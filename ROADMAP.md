# Citadel Roadmap

This roadmap outlines improvements, bug fixes, and new features for the Citadel HTTP/1.1 server library.

---

## Test Coverage

### Integration Tests
**Files:** `Tests/Main.lean`

Add tests with actual HTTP connections:
- Start server on test port
- Send real HTTP requests
- Verify responses

### SSE Tests
**Files:** `Citadel/SSE.lean`

SSE module has no test coverage. Add tests for:
- Connection registration/unregistration
- Event broadcasting
- Keep-alive pings
- Client disconnect detection

### Edge Case Tests
- Malformed requests
- Partial data / slow clients
- Timeout handling
- Double slashes in paths
- Trailing slashes
- Empty request bodies

---

## Performance

### Trie-Based Routing
**Files:** `Citadel/Core.lean:267`

Route matching uses `findSome?` which is O(n). For servers with many routes, implement a trie or radix tree for O(log n) lookups.

### Buffer Pooling
**Files:** `Citadel/Server/Connection.lean`

Current implementation: `buffer := buffer ++ chunk` creates allocations on each recv. Use pre-allocated buffers or buffer pools.

### SSE Broadcast Optimization
**Files:** `Citadel/SSE.lean:118`

Events are serialized once per client. Serialize once per broadcast, send same bytes to all clients.

### Connection Handling Options
**Files:** `Citadel/Server.lean`

Currently creates one thread per connection. Consider:
- Thread pool with work stealing
- Async I/O (epoll/kqueue)
- Connection limits

---

## New Features

### mTLS Support
Extend TLS to support mutual TLS (client certificate verification):
- `TlsConfig.clientVerify` option
- `TlsConfig.caFile` for CA certificates
- Client certificate validation

### SNI (Server Name Indication)
Support multiple certificates per server for different hostnames.

### Compression
Support response compression:
- gzip encoding
- deflate encoding
- `Accept-Encoding` header parsing
- Configurable compression level and threshold

### Range Requests
For serving large files:
- `Range` header parsing
- 206 Partial Content responses
- `Accept-Ranges` header

### Caching Headers
- `ETag` generation and validation
- `Last-Modified` / `If-Modified-Since`
- `Cache-Control` helper methods
- 304 Not Modified responses

### Rate Limiting
Built-in or middleware-based rate limiting:
- Per-IP limits
- Configurable windows
- 429 Too Many Requests responses

### CORS Helpers
- `ResponseBuilder.cors : CorsConfig → ResponseBuilder`
- Preflight request handling
- `Access-Control-*` header helpers

---

## Code Cleanup

### Configurable Buffer Sizes
**Files:** `ffi/socket.c`

Make buffer sizes configurable:
- Receive buffer size (currently 16KB)
- Send buffer size

### Consistent Response Builder API
**Files:** `Citadel/Core.lean:158-162`

`Response.internalError` has inconsistent variants (with/without message). Standardize all response builders to take optional message body.

### SSE Loop Termination
**Files:** `Citadel/Server/Connection.lean`

`sseKeepAliveLoop` is marked `partial`. Either:
- Document why non-termination is acceptable
- Refactor to use proven termination

---

## Documentation

### Middleware Guide
Document how to:
- Implement custom middleware
- Order middleware correctly
- Access/modify requests and responses
- Short-circuit the chain

### SSE Usage Guide
Document:
- Setting up SSE endpoints
- Broadcasting events
- Client connection lifecycle
- Error handling

### Configuration Reference
Document all `ServerConfig` fields with:
- Types and defaults
- Effect on server behavior
- Example configurations

### Error Handling Patterns
Document:
- How errors propagate
- Custom error pages
- Logging integration
- Recovery strategies

### Performance Guide
Document:
- Expected throughput
- Memory usage patterns
- Scaling recommendations
- Profiling tips
