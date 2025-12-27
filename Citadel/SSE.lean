/-
  Citadel SSE - Server-Sent Events support

  Provides types and connection management for SSE endpoints.
-/
import Citadel.Socket
import Std.Data.HashMap

namespace Citadel.SSE

/-- An SSE event to send to clients -/
structure Event where
  /-- Event type (default: "message") -/
  event : String := "message"
  /-- Event data payload -/
  data : String
  /-- Optional event ID for reconnection -/
  id : Option String := none
  /-- Optional retry interval in milliseconds -/
  retry : Option Nat := none
  deriving Repr, Inhabited

namespace Event

/-- Serialize event to SSE wire format -/
def toBytes (e : Event) : ByteArray :=
  let lines : List String := []
  -- Event type (only if not default "message")
  let lines := if e.event != "message" then
    lines ++ [s!"event: {e.event}"]
  else lines
  -- Event ID
  let lines := match e.id with
  | some id => lines ++ [s!"id: {id}"]
  | none => lines
  -- Retry interval
  let lines := match e.retry with
  | some ms => lines ++ [s!"retry: {ms}"]
  | none => lines
  -- Data lines (each line gets "data: " prefix)
  let dataLines := e.data.splitOn "\n"
  let lines := lines ++ dataLines.map (s!"data: " ++ ·)
  -- Join with \n, end with \n\n to dispatch event
  let content := "\n".intercalate lines ++ "\n\n"
  content.toUTF8

/-- Create a simple message event -/
def message (data : String) : Event :=
  { data }

/-- Create a named event with data -/
def named (eventType : String) (data : String) : Event :=
  { event := eventType, data }

/-- Create an event with an ID for reconnection -/
def withId (e : Event) (id : String) : Event :=
  { e with id := some id }

/-- Create an event with a retry hint -/
def withRetry (e : Event) (ms : Nat) : Event :=
  { e with retry := some ms }

end Event

/-- A connected SSE client -/
structure Client where
  /-- Unique client identifier -/
  id : Nat
  /-- The client socket for sending events -/
  socket : Socket
  /-- Topic/channel this client is subscribed to -/
  topic : String

/-- Thread-safe connection manager for SSE clients -/
structure ConnectionManager where
  /-- Next client ID -/
  nextId : IO.Ref Nat
  /-- Connected clients by ID -/
  clients : IO.Ref (Std.HashMap Nat Client)

namespace ConnectionManager

/-- Create a new connection manager -/
def create : IO ConnectionManager := do
  let nextId ← IO.mkRef 0
  let clients ← IO.mkRef {}
  pure { nextId, clients }

/-- Register a new SSE client -/
def addClient (cm : ConnectionManager) (socket : Socket) (topic : String) : IO Nat := do
  let id ← cm.nextId.modifyGet fun n => (n, n + 1)
  let client : Client := { id, socket, topic }
  cm.clients.modify (·.insert id client)
  pure id

/-- Remove a client (on disconnect) -/
def removeClient (cm : ConnectionManager) (clientId : Nat) : IO Unit := do
  cm.clients.modify (·.erase clientId)

/-- Get all clients for a topic -/
def getClientsForTopic (cm : ConnectionManager) (topic : String) : IO (List Client) := do
  let clients ← cm.clients.get
  pure (clients.toList.map Prod.snd |>.filter (·.topic == topic))

/-- Get all connected clients -/
def getAllClients (cm : ConnectionManager) : IO (List Client) := do
  let clients ← cm.clients.get
  pure (clients.toList.map Prod.snd)

/-- Get the number of connected clients -/
def clientCount (cm : ConnectionManager) : IO Nat := do
  let clients ← cm.clients.get
  pure clients.size

/-- Broadcast event to all clients on a specific topic (non-blocking, dedicated threads) -/
def broadcast (cm : ConnectionManager) (topic : String) (event : Event) : IO Unit := do
  let clients ← cm.getClientsForTopic topic
  let bytes := event.toBytes
  for client in clients do
    -- Use dedicated thread to avoid blocking the thread pool
    let _ ← IO.asTask (prio := .dedicated) do
      try
        client.socket.send bytes
      catch _ =>
        -- Client disconnected, remove them
        cm.removeClient client.id

/-- Broadcast to ALL connected clients (all topics, non-blocking, dedicated threads) -/
def broadcastAll (cm : ConnectionManager) (event : Event) : IO Unit := do
  let clients ← cm.getAllClients
  let bytes := event.toBytes
  for client in clients do
    -- Use dedicated thread to avoid blocking the thread pool
    let _ ← IO.asTask (prio := .dedicated) do
      try
        client.socket.send bytes
      catch _ =>
        cm.removeClient client.id

/-- Send event to a specific client -/
def sendTo (cm : ConnectionManager) (clientId : Nat) (event : Event) : IO Bool := do
  let clients ← cm.clients.get
  match clients.get? clientId with
  | some client =>
    try
      client.socket.send event.toBytes
      pure true
    catch _ =>
      cm.removeClient clientId
      pure false
  | none => pure false

end ConnectionManager

/-- SSE HTTP headers for the initial response -/
def sseHeaders : String :=
  "HTTP/1.1 200 OK\r\n" ++
  "Content-Type: text/event-stream\r\n" ++
  "Cache-Control: no-cache\r\n" ++
  "Connection: keep-alive\r\n" ++
  "X-Accel-Buffering: no\r\n\r\n"

/-- Send initial SSE headers to a client socket -/
def sendHeaders (socket : Socket) : IO Unit :=
  socket.send sseHeaders.toUTF8

/-- Send a comment (heartbeat/ping) to keep connection alive -/
def sendPing (socket : Socket) : IO Unit :=
  socket.send ": ping\n\n".toUTF8

/-- Send a "connected" comment to confirm connection -/
def sendConnected (socket : Socket) : IO Unit :=
  socket.send ": connected\n\n".toUTF8

end Citadel.SSE
