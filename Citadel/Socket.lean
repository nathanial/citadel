/-
  Citadel Socket FFI
  TCP socket bindings using POSIX sockets.
-/

namespace Citadel

/-- Opaque TCP socket handle -/
opaque SocketPointed : NonemptyType
def Socket : Type := SocketPointed.type
instance : Nonempty Socket := SocketPointed.property

namespace Socket

/-- Create a new TCP socket -/
@[extern "citadel_socket_new"]
opaque new : IO Socket

/-- Bind socket to an address and port -/
@[extern "citadel_socket_bind"]
opaque bind (sock : @& Socket) (host : @& String) (port : UInt16) : IO Unit

/-- Start listening for connections -/
@[extern "citadel_socket_listen"]
opaque listen (sock : @& Socket) (backlog : UInt32) : IO Unit

/-- Accept a new connection, returns the client socket -/
@[extern "citadel_socket_accept"]
opaque accept (sock : @& Socket) : IO Socket

/-- Receive data from socket, up to maxBytes -/
@[extern "citadel_socket_recv"]
opaque recv (sock : @& Socket) (maxBytes : UInt32) : IO ByteArray

/-- Send data to socket -/
@[extern "citadel_socket_send"]
opaque send (sock : @& Socket) (data : @& ByteArray) : IO Unit

/-- Close the socket -/
@[extern "citadel_socket_close"]
opaque close (sock : Socket) : IO Unit

/-- Get the underlying file descriptor (for debugging) -/
@[extern "citadel_socket_fd"]
opaque fd (sock : @& Socket) : UInt32

end Socket

end Citadel
