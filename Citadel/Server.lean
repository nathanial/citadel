/-
  Citadel Server

  TCP server implementation using POSIX socket FFI.
-/
import Citadel.Core
import Citadel.Socket
import Citadel.SSE

namespace Citadel

open Herald.Core

/-- Serialize a response to bytes for sending over the wire -/
def serializeResponse (resp : Response) : ByteArray :=
  let statusLine := s!"{resp.version} {resp.status.code} {resp.reason}\r\n"
  let headerLines := resp.headers.foldl (init := "") fun acc h =>
    acc ++ s!"{h.name}: {h.value}\r\n"
  let header := statusLine ++ headerLines ++ "\r\n"
  header.toUTF8 ++ resp.body

/-- HTTP server -/
structure Server where
  /-- Server configuration -/
  config : ServerConfig
  /-- Request router -/
  router : Router
  /-- Middleware stack -/
  middleware : List Middleware := []
  /-- SSE connection manager (if SSE is enabled) -/
  sseManager : Option SSE.ConnectionManager := none
  /-- SSE routes: (pattern, topic) -/
  sseRoutes : List (String × String) := []

namespace Server

/-- Create a new server with configuration -/
def create (config : ServerConfig := {}) : Server :=
  { config, router := Router.empty }

/-- Create a server with default configuration -/
def new : Server := create {}

/-- Add a route -/
def route (s : Server) (method : Method) (pattern : String) (handler : Handler) : Server :=
  { s with router := s.router.add method pattern handler }

/-- Add a GET route -/
def get (s : Server) (pattern : String) (handler : Handler) : Server :=
  s.route Method.GET pattern handler

/-- Add a POST route -/
def post (s : Server) (pattern : String) (handler : Handler) : Server :=
  s.route Method.POST pattern handler

/-- Add a PUT route -/
def put (s : Server) (pattern : String) (handler : Handler) : Server :=
  s.route Method.PUT pattern handler

/-- Add a DELETE route -/
def delete (s : Server) (pattern : String) (handler : Handler) : Server :=
  s.route Method.DELETE pattern handler

/-- Add a PATCH route -/
def patch (s : Server) (pattern : String) (handler : Handler) : Server :=
  s.route Method.PATCH pattern handler

/-- Add middleware -/
def use (s : Server) (mw : Middleware) : Server :=
  { s with middleware := s.middleware ++ [mw] }

/-- Enable SSE support on the server -/
def withSSE (s : Server) (manager : SSE.ConnectionManager) : Server :=
  { s with sseManager := some manager }

/-- Register an SSE endpoint -/
def sseRoute (s : Server) (pattern : String) (topic : String := "default") : Server :=
  { s with sseRoutes := s.sseRoutes ++ [(pattern, topic)] }

/-- Get the SSE connection manager -/
def getSSEManager (s : Server) : Option SSE.ConnectionManager := s.sseManager

/-- Check if a path matches an SSE route, returning the topic if matched -/
private def matchSSERoute (s : Server) (path : String) : Option String :=
  -- Remove query string from path
  let cleanPath := path.splitOn "?" |>.head!
  s.sseRoutes.findSome? fun (pattern, topic) =>
    if pattern == cleanPath then some topic else none

/-- Send HTTP response to client socket -/
private def sendResponse (client : Socket) (resp : Response) : IO Unit := do
  let data := serializeResponse resp
  client.send data

/-- SSE keep-alive loop: sends pings and detects disconnection -/
private partial def sseKeepAliveLoop (client : Socket) (manager : SSE.ConnectionManager) (clientId : Nat) : IO Unit := do
  IO.sleep 15000  -- 15 second heartbeat
  try
    SSE.sendPing client
    sseKeepAliveLoop client manager clientId
  catch _ =>
    -- Client disconnected
    manager.removeClient clientId

/-- Handle an SSE connection - keeps connection open until client disconnects -/
private def handleSSEConnection (s : Server) (client : Socket) (topic : String) : IO Unit := do
  match s.sseManager with
  | none =>
    -- SSE not enabled, send error response
    let resp := Response.internalError "SSE not enabled"
    sendResponse client resp
  | some manager =>
    -- Send SSE headers
    SSE.sendHeaders client

    -- Register client with the connection manager
    let clientId ← manager.addClient client topic

    -- Send initial connection confirmation
    SSE.sendConnected client

    -- Start keep-alive loop (runs until client disconnects)
    try
      sseKeepAliveLoop client manager clientId
    catch _ =>
      -- Ensure cleanup on any error
      manager.removeClient clientId

/-- Handle a single request -/
private def handleRequest (s : Server) (req : Request) : IO Response := do
  match s.router.findRoute req with
  | some (route, params) =>
    let serverReq : ServerRequest := { request := req, params }
    try
      route.handler serverReq
    catch e =>
      IO.eprintln s!"Handler error: {e}"
      pure (Response.internalError)
  | none =>
    pure (Response.notFound)

/-- Read HTTP request from client socket -/
private def readRequest (client : Socket) : IO (Option Request) := do
  let mut buffer := ByteArray.empty
  let mut attempts := 0
  let maxAttempts := 100

  while attempts < maxAttempts do
    let chunk ← client.recv 4096
    if chunk.isEmpty then
      if buffer.isEmpty then
        return none  -- Client closed connection
      else
        attempts := attempts + 1
    else
      buffer := buffer ++ chunk
      -- Try to parse
      match Herald.parseRequest buffer with
      | .ok result => return some result.request
      | .error .incomplete => attempts := attempts + 1
      | .error _ => return none  -- Parse error

  return none

/-- Handle a client connection -/
private def handleConnection (s : Server) (client : Socket) : IO Unit := do
  let mut keepAlive := true

  while keepAlive do
    match ← readRequest client with
    | some req =>
      -- Check if this is an SSE endpoint
      match s.matchSSERoute req.path with
      | some topic =>
        -- This is an SSE request - handle specially (blocks until client disconnects)
        s.handleSSEConnection client topic
        keepAlive := false  -- Connection is done after SSE handler returns
      | none =>
        -- Regular HTTP request
        -- Check Connection header
        let connHeader := req.headers.get "Connection"
        let isHttp10 := req.version.minor == 0
        let closeRequested := connHeader == some "close"
        let keepAliveRequested := connHeader == some "keep-alive"

        keepAlive := if isHttp10 then keepAliveRequested else !closeRequested

        -- Handle request
        let resp ← s.handleRequest req

        -- Add Connection header if closing
        let resp := if !keepAlive then
          { resp with headers := resp.headers.add "Connection" "close" }
        else
          resp

        sendResponse client resp

    | none =>
      keepAlive := false

  client.close

/-- Run the server (blocking) -/
def run (s : Server) : IO Unit := do
  -- Create server socket
  let serverSocket ← Socket.new

  -- Bind to address
  serverSocket.bind s.config.host s.config.port

  -- Listen
  serverSocket.listen 128

  IO.println s!"Citadel server listening on {s.config.host}:{s.config.port}"

  -- Accept loop (concurrent via IO.asTask)
  while true do
    let clientSocket ← serverSocket.accept
    let _ ← IO.asTask do
      try
        s.handleConnection clientSocket
      catch e =>
        IO.eprintln s!"Connection error: {e}"
        try clientSocket.close catch _ => pure ()

end Server

end Citadel
