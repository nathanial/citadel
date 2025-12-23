/-
  Citadel Server

  TCP server implementation using POSIX socket FFI.
-/
import Citadel.Core
import Citadel.Socket

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

/-- Send HTTP response to client socket -/
private def sendResponse (client : Socket) (resp : Response) : IO Unit := do
  let data := serializeResponse resp
  client.send data

/-- Handle a client connection -/
private def handleConnection (s : Server) (client : Socket) : IO Unit := do
  let mut keepAlive := true

  while keepAlive do
    match ← readRequest client with
    | some req =>
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
