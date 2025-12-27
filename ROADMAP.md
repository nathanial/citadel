# Citadel Roadmap

This roadmap outlines improvements, bug fixes, and new features for the Citadel HTTP/1.1 server library.

---

## Critical Bugs

### Middleware Chain Not Invoked
**Files:** `Citadel/Server.lean:97, 136-138`

The `use()` method adds middleware to the server's middleware list, but `handleRequest()` calls handlers directly without invoking the middleware chain. This makes the middleware API non-functional.

**Fix:** Wrap handler execution with middleware chain invocation in `handleRequest()`.

### Configuration Fields Unused
**Files:** `Citadel/Core.lean:19-23`, `ffi/socket.c:69-72`

Several `ServerConfig` fields are defined but never enforced:
- `maxBodySize` - Request body size limits not checked
- `keepAliveTimeout` - Not applied to connections
- `requestTimeout` - Not applied to request handling

Socket timeouts are hardcoded to 5 seconds in C code instead of using config values.

**Fix:** Pass config to socket layer; enforce limits in server loop.

---

## High Priority

### Query String Parsing
**Files:** `Citadel/Core.lean:207-208`

Routes currently strip query strings for matching but don't parse or expose query parameters. Users cannot access `?key=value` data.

**Add:**
- `ServerRequest.query : String` - Raw query string
- `ServerRequest.queryParam : String → Option String` - Get single param
- `ServerRequest.queryParams : List (String × String)` - All params

### Form Data Parsing
Support common form submission formats:
- `application/x-www-form-urlencoded` parsing
- `multipart/form-data` parsing for file uploads
- `ServerRequest.formField : String → Option String`
- `ServerRequest.formFile : String → Option FormFile`

### Additional HTTP Method Helpers
**Files:** `Citadel/Core.lean`

Add route helpers for:
- `Server.head` - HEAD requests
- `Server.options` - OPTIONS requests (needed for CORS preflight)

### Additional Status Code Responses
**Files:** `Citadel/Core.lean:147-182`

Only 8 response helpers exist. Add:
- `Response.unauthorized` (401)
- `Response.forbidden` (403)
- `Response.methodNotAllowed` (405)
- `Response.conflict` (409)
- `Response.payloadTooLarge` (413)
- `Response.tooManyRequests` (429)
- `Response.serviceUnavailable` (503)

### Cookie Helpers
Add cookie parsing and setting utilities:
- `ServerRequest.cookie : String → Option String`
- `ServerRequest.cookies : List (String × String)`
- `ResponseBuilder.setCookie : String → String → CookieOptions → ResponseBuilder`
- `ResponseBuilder.clearCookie : String → ResponseBuilder`

---

## Error Handling

### Protocol Error Responses
**Files:** `Citadel/Core.lean:282`

Currently parse errors return generic 404. Implement proper HTTP error responses:
- 400 Bad Request for malformed requests
- 405 Method Not Allowed when route exists but method doesn't match
- 413 Payload Too Large when body exceeds `maxBodySize`
- 408 Request Timeout when request takes too long

### Request Validation
Add validation layer:
- Header count limits
- Header size limits
- URI length limits
- Invalid character detection

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

### Middleware Tests
Once middleware is fixed, add tests for:
- Single middleware execution
- Middleware chain ordering
- Middleware short-circuiting

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
**Files:** `Citadel/Server.lean:235`

Current implementation: `buffer := buffer ++ chunk` creates allocations on each recv. Use pre-allocated buffers or buffer pools.

### SSE Broadcast Optimization
**Files:** `Citadel/SSE.lean:118`

Events are serialized once per client. Serialize once per broadcast, send same bytes to all clients.

### Connection Handling Options
**Files:** `Citadel/Server.lean:312`

Currently creates one thread per connection. Consider:
- Thread pool with work stealing
- Async I/O (epoll/kqueue)
- Connection limits

---

## New Features

### HTTPS/TLS Support
Add TLS via OpenSSL or native bindings:
- `ServerConfig.tlsCert : Option String`
- `ServerConfig.tlsKey : Option String`
- Automatic HTTP→HTTPS redirect option

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

### Split Server.lean
**Files:** `Citadel/Server.lean` (325 lines)

Server.lean handles too many concerns. Split into:
- `Citadel/Server/Loop.lean` - Main server loop
- `Citadel/Server/Connection.lean` - Connection handling
- `Citadel/Server/Stats.lean` - Statistics tracking

### Configurable Socket Settings
**Files:** `ffi/socket.c:69-72, 149-150`

Make hardcoded values configurable:
- Socket timeout (currently 5 seconds)
- Receive buffer size (currently 16KB)
- Send buffer size

### Consistent Response Builder API
**Files:** `Citadel/Core.lean:158-162`

`Response.internalError` has inconsistent variants (with/without message). Standardize all response builders to take optional message body.

### SSE Loop Termination
**Files:** `Citadel/Server.lean:164`

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

---

## Version Milestones

### v0.2.0 - Bug Fixes
- Fix middleware invocation
- Implement config enforcement
- Add query string parsing

### v0.3.0 - Core Features
- Form data parsing
- Cookie helpers
- Additional status codes
- HEAD/OPTIONS methods

### v0.4.0 - Production Ready
- Integration test suite
- Proper error responses
- Rate limiting
- CORS support

### v0.5.0 - Performance
- Trie-based routing
- Buffer pooling
- Connection pool options

### v1.0.0 - Feature Complete
- HTTPS/TLS
- Compression
- Range requests
- Caching
- Comprehensive documentation
