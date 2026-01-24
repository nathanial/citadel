/-
  Citadel Server

  TCP server implementation using POSIX socket FFI.
-/
import Citadel.Core
import Citadel.Socket
import Citadel.SSE
import Citadel.Server.Stats
import Citadel.Server.Connection

namespace Citadel

open Herald.Core
open Connection

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

/-- Add a HEAD route -/
def head (s : Server) (pattern : String) (handler : Handler) : Server :=
  s.route Method.HEAD pattern handler

/-- Add an OPTIONS route -/
def options (s : Server) (pattern : String) (handler : Handler) : Server :=
  s.route Method.OPTIONS pattern handler

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
      -- Apply middleware chain to the handler
      let wrappedHandler := Middleware.chain s.middleware route.handler
      let resp ← wrappedHandler serverReq
      let elapsed := (← IO.monoMsNow) - startTime
      IO.println s!"[REQ] {req.method} {req.path} -> {resp.status.code} ({elapsed}ms)"
      pure resp
    catch e =>
      IO.eprintln s!"[REQ] Handler error: {e}"
      pure (Response.internalError)
  | none =>
    -- Check if path matches but method doesn't
    let allowedMethods := s.router.findMethodsForPath req.path
    if allowedMethods.isEmpty then
      IO.println s!"[REQ] {req.method} {req.path} -> 404"
      pure (Response.notFound)
    else
      let methodStrs := allowedMethods.map fun m => m.toString
      IO.println s!"[REQ] {req.method} {req.path} -> 405 (allowed: {String.intercalate ", " methodStrs})"
      pure (Response.methodNotAllowed methodStrs)

/-- Handle a client connection -/
private def handleConnection (s : Server) (client : Socket) : IO Unit := do
  let mut keepAlive := true
  let mut firstRequest := true

  while keepAlive do
    -- For subsequent requests, use keepAliveTimeout while waiting
    if !firstRequest then
      client.setTimeout s.config.keepAliveTimeout.toUInt32

    match ← readRequest client s.config with
    | .success req =>
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
        firstRequest := false

        -- Handle request
        let resp ← s.handleRequest req

        -- Add Connection header if closing
        let resp := if !keepAlive then
          { resp with headers := resp.headers.add "Connection" "close" }
        else
          resp

        sendResponse client resp

    | .connectionClosed =>
      keepAlive := false

    | .parseError =>
      IO.eprintln "[CONN] Parse error, sending 400 Bad Request"
      sendResponse client (Response.badRequest "Malformed HTTP request")
      keepAlive := false

    | .payloadTooLarge =>
      IO.eprintln s!"[CONN] Payload exceeds {s.config.maxBodySize} bytes, sending 413"
      sendResponse client (Response.payloadTooLarge)
      keepAlive := false

    | .timeout =>
      IO.eprintln "[CONN] Request timeout, sending 408"
      sendResponse client (Response.requestTimeout)
      keepAlive := false

    | .uriTooLong =>
      IO.eprintln "[CONN] URI too long, sending 414"
      sendResponse client (Response.uriTooLong)
      keepAlive := false

    | .headerValidationFailed msg =>
      IO.eprintln s!"[CONN] Header validation failed: {msg}, sending 431"
      sendResponse client (Response.headerFieldsTooLarge msg)
      keepAlive := false

  client.close

/-- Handle a TLS client connection (SSE not supported over TLS for now) -/
private def handleTlsConnection (s : Server) (client : TlsSocket) : IO Unit := do
  let anyClient := AnySocket.tls client
  let mut keepAlive := true
  let mut firstRequest := true

  while keepAlive do
    -- For subsequent requests, use keepAliveTimeout while waiting
    if !firstRequest then
      anyClient.setTimeout s.config.keepAliveTimeout.toUInt32

    match ← readRequestAny anyClient s.config with
    | .success req =>
      -- Regular HTTP request (SSE not supported over TLS for now)
      -- Check Connection header
      let connHeader := req.headers.get "Connection"
      let isHttp10 := req.version.minor == 0
      let closeRequested := connHeader == some "close"
      let keepAliveRequested := connHeader == some "keep-alive"

      keepAlive := if isHttp10 then keepAliveRequested else !closeRequested
      firstRequest := false

      -- Handle request
      let resp ← s.handleRequest req

      -- Add Connection header if closing
      let resp := if !keepAlive then
        { resp with headers := resp.headers.add "Connection" "close" }
      else
        resp

      sendResponseAny anyClient resp

    | .connectionClosed =>
      keepAlive := false

    | .parseError =>
      IO.eprintln "[TLS] Parse error, sending 400 Bad Request"
      sendResponseAny anyClient (Response.badRequest "Malformed HTTP request")
      keepAlive := false

    | .payloadTooLarge =>
      IO.eprintln s!"[TLS] Payload exceeds {s.config.maxBodySize} bytes, sending 413"
      sendResponseAny anyClient (Response.payloadTooLarge)
      keepAlive := false

    | .timeout =>
      IO.eprintln "[TLS] Request timeout, sending 408"
      sendResponseAny anyClient (Response.requestTimeout)
      keepAlive := false

    | .uriTooLong =>
      IO.eprintln "[TLS] URI too long, sending 414"
      sendResponseAny anyClient (Response.uriTooLong)
      keepAlive := false

    | .headerValidationFailed msg =>
      IO.eprintln s!"[TLS] Header validation failed: {msg}, sending 431"
      sendResponseAny anyClient (Response.headerFieldsTooLarge msg)
      keepAlive := false

  client.close

/-- Run plain HTTP server (internal) -/
private def runPlain (s : Server) : IO Unit := do
  -- Create server socket
  let serverSocket ← Jack.Socket.new

  -- Bind to address
  serverSocket.bind s.config.host s.config.port

  -- Listen
  serverSocket.listen 128

  let stats ← getOrCreateStats

  IO.println s!"Citadel HTTP server listening on {s.config.host}:{s.config.port}"

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
    let threads ← stats.incrementDedicated
    IO.println s!"[CONN] Accepted #{total} (active={active} threads={threads})"
    -- Use Task.Priority.dedicated so blocking I/O doesn't starve the thread pool
    let _ ← IO.asTask (prio := .dedicated) do
      try
        s.handleConnection clientSocket
      catch e =>
        IO.eprintln s!"[CONN] Connection error: {e}"
      finally
        let active ← stats.decrementActive
        let threads ← stats.decrementDedicated
        IO.println s!"[CONN] Closed (active={active} threads={threads})"
        try clientSocket.close catch _ => pure ()

/-- Run HTTPS server with TLS (internal) -/
private def runTls (s : Server) (tlsConfig : TlsConfig) : IO Unit := do
  -- Create TLS server socket
  let serverSocket ← TlsSocket.newServer tlsConfig.certFile tlsConfig.keyFile

  -- Bind to address
  serverSocket.bind s.config.host s.config.port

  -- Listen
  serverSocket.listen 128

  let stats ← getOrCreateStats

  IO.println s!"Citadel HTTPS server listening on {s.config.host}:{s.config.port}"

  -- Stats printer task (every 10 seconds)
  let _ ← IO.asTask do
    while true do
      IO.sleep 10000
      stats.print

  -- Accept loop (use dedicated threads to avoid blocking the thread pool)
  while true do
    -- Handle TLS accept/handshake errors gracefully (silent close)
    let acceptResult ← try
      let clientSocket ← serverSocket.accept
      pure (some clientSocket)
    catch e =>
      IO.eprintln s!"[TLS] Accept/handshake error: {e}"
      pure none

    match acceptResult with
    | none => pure ()  -- Continue accepting after handshake failure
    | some clientSocket =>
      let total ← stats.incrementTotal
      let active ← stats.incrementActive
      let threads ← stats.incrementDedicated
      IO.println s!"[TLS] Accepted #{total} (active={active} threads={threads})"
      -- Use Task.Priority.dedicated so blocking I/O doesn't starve the thread pool
      let _ ← IO.asTask (prio := .dedicated) do
        try
          s.handleTlsConnection clientSocket
        catch e =>
          IO.eprintln s!"[TLS] Connection error: {e}"
        finally
          let active ← stats.decrementActive
          let threads ← stats.decrementDedicated
          IO.println s!"[TLS] Closed (active={active} threads={threads})"
          try clientSocket.close catch _ => pure ()

/-- Run the server (blocking). Uses TLS if configured, otherwise plain HTTP. -/
def run (s : Server) : IO Unit := do
  match s.config.tls with
  | none => s.runPlain
  | some tlsConfig => s.runTls tlsConfig

end Server

end Citadel
