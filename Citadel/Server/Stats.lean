/-
  Citadel Server Statistics

  Debug statistics for monitoring server health.
-/

namespace Citadel

/-- Debug statistics for monitoring server health -/
structure ServerStats where
  /-- Number of active HTTP connection handlers -/
  activeConnections : IO.Ref Nat
  /-- Number of active SSE connections -/
  activeSseConnections : IO.Ref Nat
  /-- Total connections accepted -/
  totalConnections : IO.Ref Nat
  /-- Number of active dedicated threads -/
  dedicatedThreads : IO.Ref Nat
  /-- Peak dedicated threads (high water mark) -/
  peakDedicatedThreads : IO.Ref Nat

namespace ServerStats

def create : IO ServerStats := do
  let activeConnections ← IO.mkRef 0
  let activeSseConnections ← IO.mkRef 0
  let totalConnections ← IO.mkRef 0
  let dedicatedThreads ← IO.mkRef 0
  let peakDedicatedThreads ← IO.mkRef 0
  pure { activeConnections, activeSseConnections, totalConnections, dedicatedThreads, peakDedicatedThreads }

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

def incrementDedicated (s : ServerStats) : IO Nat := do
  let count ← s.dedicatedThreads.modifyGet fun n => (n + 1, n + 1)
  -- Update peak if this is a new high
  s.peakDedicatedThreads.modify fun peak => if count > peak then count else peak
  pure count

def decrementDedicated (s : ServerStats) : IO Nat :=
  s.dedicatedThreads.modifyGet fun n => (n - 1, n - 1)

def print (s : ServerStats) : IO Unit := do
  let active ← s.activeConnections.get
  let sse ← s.activeSseConnections.get
  let total ← s.totalConnections.get
  let dedicated ← s.dedicatedThreads.get
  let peak ← s.peakDedicatedThreads.get
  IO.println s!"[STATS] active={active} sse={sse} total={total} threads={dedicated} peak={peak}"

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

end Citadel
