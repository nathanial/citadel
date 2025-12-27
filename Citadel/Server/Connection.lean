/-
  Citadel Server Connection Handling

  HTTP/HTTPS connection handling, request parsing, and response sending.
-/
import Citadel.Core
import Citadel.Socket
import Citadel.SSE
import Citadel.Server.Stats

namespace Citadel

open Herald.Core

/-- Serialize a response to bytes for sending over the wire -/
def serializeResponse (resp : Response) : ByteArray :=
  let statusLine := s!"{resp.version} {resp.status.code} {resp.reason}\r\n"
  let headerLines := resp.headers.foldl (init := "") fun acc h =>
    acc ++ s!"{h.name}: {h.value}\r\n"
  let header := statusLine ++ headerLines ++ "\r\n"
  header.toUTF8 ++ resp.body

/-- Result of reading a request from the socket -/
inductive ReadResult where
  | success (req : Request)
  | connectionClosed
  | parseError
  | payloadTooLarge
  | timeout
  | uriTooLong
  | headerValidationFailed (msg : String)

namespace Connection

/-- Send HTTP response to client socket -/
def sendResponse (client : Socket) (resp : Response) : IO Unit := do
  let data := serializeResponse resp
  client.send data

/-- Send HTTP response to any socket type -/
def sendResponseAny (client : AnySocket) (resp : Response) : IO Unit := do
  let data := serializeResponse resp
  client.send data

/-- Read HTTP request from client socket -/
def readRequest (client : Socket) (config : ServerConfig) : IO ReadResult := do
  -- Apply requestTimeout from config
  client.setTimeout config.requestTimeout.toUInt32

  let mut buffer := ByteArray.empty
  let mut attempts := 0
  let maxAttempts := 1000  -- Allow up to ~16MB uploads (1000 * 16KB)

  while attempts < maxAttempts do
    let chunk ← client.recv 16384  -- 16KB chunks for better performance
    if chunk.isEmpty then
      return .connectionClosed  -- Client closed connection (recv returned 0)
    else
      buffer := buffer ++ chunk
      -- Check body size limit
      if buffer.size > config.maxBodySize then
        return .payloadTooLarge
      -- Try to parse
      match Herald.parseRequest buffer with
      | .ok result =>
        -- Validate the parsed request
        match validateRequest result.request config with
        | some (.uriTooLong _ _) => return .uriTooLong
        | some (.tooManyHeaders count limit) =>
          return .headerValidationFailed s!"Too many headers: {count} (limit: {limit})"
        | some (.headerTooLarge name size limit) =>
          return .headerValidationFailed s!"Header '{name}' too large: {size} bytes (limit: {limit})"
        | some (.totalHeadersTooLarge size limit) =>
          return .headerValidationFailed s!"Total headers too large: {size} bytes (limit: {limit})"
        | some (.invalidUriCharacter c) =>
          return .headerValidationFailed s!"Invalid character in URI: {repr c}"
        | none => return .success result.request
      | .error .incomplete => attempts := attempts + 1  -- Wait for more data
      | .error _ => return .parseError

  return .timeout  -- Exceeded max attempts

/-- Read HTTP request from any socket type -/
def readRequestAny (client : AnySocket) (config : ServerConfig) : IO ReadResult := do
  -- Apply requestTimeout from config
  client.setTimeout config.requestTimeout.toUInt32

  let mut buffer := ByteArray.empty
  let mut attempts := 0
  let maxAttempts := 1000  -- Allow up to ~16MB uploads (1000 * 16KB)

  while attempts < maxAttempts do
    let chunk ← client.recv 16384  -- 16KB chunks for better performance
    if chunk.isEmpty then
      return .connectionClosed  -- Client closed connection (recv returned 0)
    else
      buffer := buffer ++ chunk
      -- Check body size limit
      if buffer.size > config.maxBodySize then
        return .payloadTooLarge
      -- Try to parse
      match Herald.parseRequest buffer with
      | .ok result =>
        -- Validate the parsed request
        match validateRequest result.request config with
        | some (.uriTooLong _ _) => return .uriTooLong
        | some (.tooManyHeaders count limit) =>
          return .headerValidationFailed s!"Too many headers: {count} (limit: {limit})"
        | some (.headerTooLarge name size limit) =>
          return .headerValidationFailed s!"Header '{name}' too large: {size} bytes (limit: {limit})"
        | some (.totalHeadersTooLarge size limit) =>
          return .headerValidationFailed s!"Total headers too large: {size} bytes (limit: {limit})"
        | some (.invalidUriCharacter c) =>
          return .headerValidationFailed s!"Invalid character in URI: {repr c}"
        | none => return .success result.request
      | .error .incomplete => attempts := attempts + 1  -- Wait for more data
      | .error _ => return .parseError

  return .timeout  -- Exceeded max attempts

/-- SSE keep-alive loop: sends pings and detects disconnection -/
partial def sseKeepAliveLoop (client : Socket) (manager : SSE.ConnectionManager) (clientId : Nat) : IO Unit := do
  IO.sleep 1000  -- 1 second check interval for faster disconnect detection
  try
    -- Try to send a ping - this will fail if client disconnected
    SSE.sendPing client
    sseKeepAliveLoop client manager clientId
  catch _ =>
    -- Client disconnected (send failed)
    IO.println s!"[SSE] Client {clientId} ping failed, removing"
    manager.removeClient clientId

end Connection

end Citadel
