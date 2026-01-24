/-
  Citadel Socket FFI
  Plain TCP sockets via Jack.
  TLS socket bindings using OpenSSL.
-/
import Jack

namespace Citadel

-- ============================================================================
-- Plain TCP Socket (re-exported from Jack)
-- ============================================================================

/-- Plain TCP socket, re-exported from Jack for backwards compatibility -/
abbrev Socket := Jack.Socket

-- ============================================================================
-- TLS Socket (HTTPS)
-- ============================================================================

/-- Opaque TLS socket handle -/
opaque TlsSocketPointed : NonemptyType
def TlsSocket : Type := TlsSocketPointed.type
instance : Nonempty TlsSocket := TlsSocketPointed.property

namespace TlsSocket

/-- Create a new TLS server socket with certificate and key -/
@[extern "citadel_tls_socket_new_server"]
opaque newServer (certFile keyFile : @& String) : IO TlsSocket

/-- Bind TLS socket to an address and port -/
@[extern "citadel_tls_socket_bind"]
opaque bind (sock : @& TlsSocket) (host : @& String) (port : UInt16) : IO Unit

/-- Start listening for TLS connections -/
@[extern "citadel_tls_socket_listen"]
opaque listen (sock : @& TlsSocket) (backlog : UInt32) : IO Unit

/-- Accept a new TLS connection (performs TLS handshake) -/
@[extern "citadel_tls_socket_accept"]
opaque accept (sock : @& TlsSocket) : IO TlsSocket

/-- Receive data over TLS, up to maxBytes -/
@[extern "citadel_tls_socket_recv"]
opaque recv (sock : @& TlsSocket) (maxBytes : UInt32) : IO ByteArray

/-- Send data over TLS -/
@[extern "citadel_tls_socket_send"]
opaque send (sock : @& TlsSocket) (data : @& ByteArray) : IO Unit

/-- Close the TLS socket -/
@[extern "citadel_tls_socket_close"]
opaque close (sock : TlsSocket) : IO Unit

/-- Set recv/send timeouts in seconds -/
@[extern "citadel_tls_socket_set_timeout"]
opaque setTimeout (sock : @& TlsSocket) (timeoutSecs : UInt32) : IO Unit

end TlsSocket

-- ============================================================================
-- Unified Socket Interface
-- ============================================================================

/-- Either a plain TCP socket or a TLS socket -/
inductive AnySocket where
  | plain (sock : Socket)
  | tls (sock : TlsSocket)

namespace AnySocket

/-- Receive data from either socket type -/
def recv (s : AnySocket) (maxBytes : UInt32) : IO ByteArray :=
  match s with
  | .plain sock => Jack.Socket.recv sock maxBytes
  | .tls sock => sock.recv maxBytes

/-- Send data to either socket type -/
def send (s : AnySocket) (data : ByteArray) : IO Unit :=
  match s with
  | .plain sock => Jack.Socket.send sock data
  | .tls sock => sock.send data

/-- Close either socket type -/
def close (s : AnySocket) : IO Unit :=
  match s with
  | .plain sock => Jack.Socket.close sock
  | .tls sock => sock.close

/-- Set recv/send timeouts in seconds -/
def setTimeout (s : AnySocket) (timeoutSecs : UInt32) : IO Unit :=
  match s with
  | .plain sock => Jack.Socket.setTimeout sock timeoutSecs
  | .tls sock => sock.setTimeout timeoutSecs

end AnySocket

end Citadel
