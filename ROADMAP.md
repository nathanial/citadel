# Citadel Roadmap

This roadmap outlines improvements, bug fixes, and new features for the Citadel HTTP/1.1 server library.

---

## Critical Bugs

### ~~Middleware Chain Not Invoked~~ ✅ FIXED
**Files:** `Citadel/Server.lean:214`

~~The `use()` method adds middleware to the server's middleware list, but `handleRequest()` calls handlers directly without invoking the middleware chain. This makes the middleware API non-functional.~~

**Fixed:** `handleRequest()` now applies the middleware chain via `Middleware.chain s.middleware route.handler`. Added 4 middleware tests to verify correct behavior.

### ~~Configuration Fields Unused~~ ⚠️ PARTIALLY FIXED
**Files:** `Citadel/Core.lean:19-23`, `ffi/socket.c:69-72`

~~Several `ServerConfig` fields are defined but never enforced:~~
- ~~`maxBodySize` - Request body size limits not checked~~ ✅ **FIXED** - Now enforced in `readRequest`
- `keepAliveTimeout` - Not applied to connections
- `requestTimeout` - Not applied to request handling

Socket timeouts are hardcoded to 5 seconds in C code instead of using config values.

**Remaining:** Pass `keepAliveTimeout` and `requestTimeout` to socket layer.

---

## High Priority

### ~~Query String Parsing~~ ✅ IMPLEMENTED
**Files:** `Citadel/Core.lean:69-121`

~~Routes currently strip query strings for matching but don't parse or expose query parameters. Users cannot access `?key=value` data.~~

**Implemented:**
- `ServerRequest.query : String` - Raw query string (without leading `?`)
- `ServerRequest.queryParam : String → Option String` - Get single param
- `ServerRequest.queryParams : List (String × String)` - All params
- `ServerRequest.queryParamAll : String → List String` - All values for repeated keys
- `ServerRequest.fullPath : String` - Full path including query string
- `ServerRequest.path` - Now returns path without query string
- `ServerRequest.urlDecode : String → String` - URL decoding with `%XX` and `+` support

Added 14 query string tests covering edge cases.

### ~~Form Data Parsing~~ ✅ IMPLEMENTED
**Files:** `Citadel/Core.lean:123-286`

~~Support common form submission formats.~~

**Implemented:**
- `FormFile` structure with `filename`, `contentType`, and `data` fields
- `FormData` structure with `fields` and `files` lists
- `parseUrlEncodedForm` - Parse `application/x-www-form-urlencoded` bodies
- `parseMultipartForm` - Parse `multipart/form-data` with file uploads
- `ServerRequest.formData` - Get parsed form data
- `ServerRequest.formField : String → Option String` - Get a form field
- `ServerRequest.formFieldAll : String → List String` - All values for a field
- `ServerRequest.formFile : String → Option FormFile` - Get an uploaded file
- `ServerRequest.formFileAll : String → List FormFile` - All files for a field
- `ServerRequest.contentType` - Get Content-Type header
- `ServerRequest.hasFormData` - Check if request has form data

Added 14 form data tests covering urlencoded and multipart parsing.

### ~~Additional HTTP Method Helpers~~ ✅ IMPLEMENTED
**Files:** `Citadel/Core.lean:494-500`, `Citadel/Server.lean:136-142`

~~Add route helpers for HEAD and OPTIONS requests.~~

**Implemented:**
- `Router.head` / `Server.head` - HEAD requests
- `Router.options` / `Server.options` - OPTIONS requests (for CORS preflight)

Added 2 tests for HEAD and OPTIONS routing.

### ~~Additional Status Code Responses~~ ✅ IMPLEMENTED
**Files:** `Citadel/Core.lean:386-455`

~~Only 8 response helpers exist.~~

**Implemented:**
- `Response.unauthorized` (401) - with optional message
- `Response.forbidden` (403) - with optional message
- `Response.methodNotAllowed` (405) - with optional `Allow` header
- `Response.conflict` (409) - with optional message
- `Response.payloadTooLarge` (413) - with optional message
- `Response.unprocessableEntity` (422) - with optional message
- `Response.tooManyRequests` (429) - with optional `Retry-After` header
- `Response.serviceUnavailable` (503) - with optional `Retry-After` header

Added 12 tests for the new status code helpers.

### ~~Cookie Helpers~~ ✅ IMPLEMENTED
**Files:** `Citadel/Core.lean:292-440`

~~Add cookie parsing and setting utilities.~~

**Implemented:**

*Request Cookie Parsing:*
- `ServerRequest.cookies : List (String × String)` - All cookies from Cookie header
- `ServerRequest.cookie : String → Option String` - Get cookie by name
- `ServerRequest.parseCookieHeader` - Parse Cookie header format

*Response Cookie Setting:*
- `SameSite` enum (`strict`, `lax`, `none`)
- `CookieOptions` structure with `maxAge`, `domain`, `path`, `secure`, `httpOnly`, `sameSite`
- `CookieOptions.session` - Session cookie preset
- `CookieOptions.persistent` - Cookie with max age preset
- `CookieOptions.secureOnly` - Secure HTTPS-only cookie preset
- `ResponseBuilder.setCookie : String → String → CookieOptions → ResponseBuilder`
- `ResponseBuilder.clearCookie : String → ResponseBuilder` - Expire cookie

Added 11 cookie tests covering parsing and setting.

---

## Error Handling

### ~~Protocol Error Responses~~ ✅ IMPLEMENTED
**Files:** `Citadel/Core.lean`, `Citadel/Server.lean`

~~Currently parse errors return generic 404. Implement proper HTTP error responses:~~

**Implemented:**
- `Response.badRequest` (400) - Sent for malformed HTTP requests (parse errors)
- `Response.methodNotAllowed` (405) - Sent when route path matches but method doesn't; includes `Allow` header with allowed methods
- `Response.payloadTooLarge` (413) - Sent when request body exceeds `maxBodySize` config
- `Response.requestTimeout` (408) - Sent when request read times out; includes `Connection: close` header
- `Router.findMethodsForPath` - New helper to find allowed methods for a path
- `ReadResult` enum - Distinguishes success, connectionClosed, parseError, payloadTooLarge, timeout

Added 6 tests for protocol error responses.

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

### ~~HTTPS/TLS Support~~ ✅ IMPLEMENTED
**Files:** `Citadel/Socket.lean`, `Citadel/Core.lean`, `Citadel/Server.lean`, `ffi/socket.c`, `lakefile.lean`

~~Add TLS via OpenSSL or native bindings.~~

**Implemented:**
- `TlsConfig` structure with `certFile` and `keyFile` paths
- `ServerConfig.tls : Option TlsConfig` - Enable HTTPS when set
- `TlsSocket` opaque type with FFI bindings for OpenSSL
- `TlsSocket.newServer` - Create TLS server with certificate/key
- `TlsSocket.bind`, `listen`, `accept`, `recv`, `send`, `close` - Full socket API
- `AnySocket` sum type - Unified interface for plain/TLS sockets
- `Server.run` automatically uses TLS when configured
- Silent error handling: TLS handshake failures are logged and connection closed
- OpenSSL 3.x linking via Homebrew paths in lakefile.lean

**Usage:**
```lean
let config : ServerConfig := {
  port := 8443
  tls := some { certFile := "cert.pem", keyFile := "key.pem" }
}
```

**Not implemented (future work):**
- mTLS (client certificate verification)
- SNI (multiple certificates)

**HTTP→HTTPS redirect:** Implemented in Loom as `Middleware.httpsRedirect` and `App.runWithHttpsRedirect`

Added 4 TLS configuration tests

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
- ~~HTTPS/TLS~~ ✅
- Compression
- Range requests
- Caching
- Comprehensive documentation
