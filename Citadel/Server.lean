/-
  Citadel Server

  TCP server implementation using POSIX socket FFI.
-/
import Citadel.Core
import Citadel.Socket
import Citadel.SSE

namespace Citadel

open Herald.Core

/-- Debug statistics for monitoring server health -/
structure ServerStats where
  /-- Number of active HTTP connection handlers -/
  activeConnections : IO.Ref Nat
  /-- Number of active SSE connections -/
  activeSseConnections : IO.Ref Nat
  /-- Total connections accepted -/
  totalConnections : IO.Ref Nat

namespace ServerStats

def create : IO ServerStats := do
  let activeConnections ← IO.mkRef 0
  let activeSseConnections ← IO.mkRef 0
  let totalConnections ← IO.mkRef 0
  pure { activeConnections, activeSseConnections, totalConnections }

def incrementActive (s : ServerStats) : IO Nat :=
  s.activeConnections.modifyGet fun n => (n + 1, n + 1)

def decrementActive (s : ServerStats) : IO Nat :=
  s.activeConnections.modifyGet fun n => (n - 1, n - 1)

def incrementSse (s : ServerStats) : IO Nat :=
  s.activeSseConnections.modifyGet fun n => (n + 1, n + 1)

def decrementSse (s : ServerStats) : IO Nat :=
  s.activeSseConnections.modifyGet fun n => (n - 1, n - 1)

def incrementTotal (s : ServerStats) : IO Nat :=
  s.totalConnections.modifyGet fun n => (n + 1, n + 1)

def print (s : ServerStats) : IO Unit := do
  let active ← s.activeConnections.get
  let sse ← s.activeSseConnections.get
  let total ← s.totalConnections.get
  IO.println s!"[STATS] active={active} sse={sse} total={total}"

end ServerStats

/-- Global server stats (initialized on first server run) -/
initialize globalStats : IO.Ref (Option ServerStats) ← IO.mkRef none

def getOrCreateStats : IO ServerStats := do
  match ← globalStats.get with
  | some s => pure s
  | none =>
    let s ← ServerStats.create
    globalStats.set (some s)
    pure s

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
  IO.sleep 1000  -- 1 second check interval for faster disconnect detection
  try
    -- Try to send a ping - this will fail if client disconnected
    SSE.sendPing client
    sseKeepAliveLoop client manager clientId
  catch _ =>
    -- Client disconnected (send failed)
    IO.println s!"[SSE] Client {clientId} ping failed, removing"
    manager.removeClient clientId

/-- Handle an SSE connection - keeps connection open until client disconnects -/
private def handleSSEConnection (s : Server) (client : Socket) (topic : String) : IO Unit := do
  let stats ← getOrCreateStats
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
    let sseCount ← stats.incrementSse
    IO.println s!"[SSE] Client {clientId} connected to '{topic}' (sse={sseCount})"

    -- Send initial connection confirmation
    SSE.sendConnected client

    -- Start keep-alive loop (runs until client disconnects)
    try
      sseKeepAliveLoop client manager clientId
    catch _ =>
      pure ()
    -- Ensure cleanup
    manager.removeClient clientId
    let sseCount ← stats.decrementSse
    IO.println s!"[SSE] Client {clientId} disconnected (sse={sseCount})"

/-- Handle a single request -/
private def handleRequest (s : Server) (req : Request) : IO Response := do
  let startTime ← IO.monoMsNow
  IO.println s!"[REQ] {req.method} {req.path}"
  match s.router.findRoute req with
  | some (route, params) =>
    let serverReq : ServerRequest := { request := req, params }
    try
      let resp ← route.handler serverReq
      let elapsed := (← IO.monoMsNow) - startTime
      IO.println s!"[REQ] {req.method} {req.path} -> {resp.status.code} ({elapsed}ms)"
      pure resp
    catch e =>
      IO.eprintln s!"[REQ] Handler error: {e}"
      pure (Response.internalError)
  | none =>
    IO.println s!"[REQ] {req.method} {req.path} -> 404"
    pure (Response.notFound)

/-- Read HTTP request from client socket -/
private def readRequest (client : Socket) : IO (Option Request) := do
  let mut buffer := ByteArray.empty
  let mut attempts := 0
  let maxAttempts := 1000  -- Allow up to ~16MB uploads (1000 * 16KB)

  while attempts < maxAttempts do
    let chunk ← client.recv 16384  -- 16KB chunks for better performance
    if chunk.isEmpty then
      return none  -- Client closed connection (recv returned 0)
    else
      buffer := buffer ++ chunk
      -- Try to parse
      match Herald.parseRequest buffer with
      | .ok result => return some result.request
      | .error .incomplete => attempts := attempts + 1  -- Wait for more data
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

  let stats ← getOrCreateStats

  IO.println s!"Citadel server listening on {s.config.host}:{s.config.port}"

  -- Stats printer task (every 10 seconds)
  let _ ← IO.asTask do
    while true do
      IO.sleep 10000
      stats.print

  -- Accept loop (use dedicated threads to avoid blocking the thread pool)
  while true do
    let clientSocket ← serverSocket.accept
    let total ← stats.incrementTotal
    let active ← stats.incrementActive
    IO.println s!"[CONN] Accepted connection #{total} (active={active})"
    -- Use Task.Priority.dedicated so blocking I/O doesn't starve the thread pool
    let _ ← IO.asTask (prio := .dedicated) do
      try
        s.handleConnection clientSocket
      catch e =>
        IO.eprintln s!"[CONN] Connection error: {e}"
      finally
        let active ← stats.decrementActive
        IO.println s!"[CONN] Connection closed (active={active})"
        try clientSocket.close catch _ => pure ()

end Server

end Citadel
