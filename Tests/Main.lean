/-
  Citadel Test Suite
  Main entry point for running all tests.
-/

import Citadel
import Crucible
import Staple

open Crucible
open Citadel
open Herald.Core

/-- Helper to check if a string contains a substring -/
def containsSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

-- ============================================================================
-- RoutePattern Tests
-- ============================================================================

testSuite "RoutePattern"

test "parse simple path" := do
  let pattern := RoutePattern.parse "/users"
  pattern.segments.length ≡ 1
  match pattern.segments.head? with
  | some (PathSegment.literal "users") => pure ()
  | _ => throw (IO.userError "Expected literal 'users'")

test "parse path with multiple segments" := do
  let pattern := RoutePattern.parse "/api/v1/users"
  pattern.segments.length ≡ 3

test "parse path with parameter" := do
  let pattern := RoutePattern.parse "/users/:id"
  pattern.segments.length ≡ 2
  match pattern.segments[1]? with
  | some (PathSegment.param "id") => pure ()
  | _ => throw (IO.userError "Expected param 'id'")

test "parse path with multiple parameters" := do
  let pattern := RoutePattern.parse "/users/:userId/posts/:postId"
  pattern.segments.length ≡ 4
  match pattern.segments[1]? with
  | some (PathSegment.param "userId") => pure ()
  | _ => throw (IO.userError "Expected param 'userId'")
  match pattern.segments[3]? with
  | some (PathSegment.param "postId") => pure ()
  | _ => throw (IO.userError "Expected param 'postId'")

test "parse path with wildcard" := do
  let pattern := RoutePattern.parse "/files/*"
  pattern.segments.length ≡ 2
  match pattern.segments[1]? with
  | some PathSegment.wildcard => pure ()
  | _ => throw (IO.userError "Expected wildcard")

test "match simple path" := do
  let pattern := RoutePattern.parse "/users"
  match pattern.match_ "/users" with
  | some params => params.length ≡ 0
  | none => throw (IO.userError "Expected match")

test "match path with parameter" := do
  let pattern := RoutePattern.parse "/users/:id"
  match pattern.match_ "/users/123" with
  | some params =>
    params.length ≡ 1
    params.lookup "id" ≡ some "123"
  | none => throw (IO.userError "Expected match")

test "match path with multiple parameters" := do
  let pattern := RoutePattern.parse "/users/:userId/posts/:postId"
  match pattern.match_ "/users/42/posts/99" with
  | some params =>
    params.length ≡ 2
    params.lookup "userId" ≡ some "42"
    params.lookup "postId" ≡ some "99"
  | none => throw (IO.userError "Expected match")

test "match wildcard path" := do
  let pattern := RoutePattern.parse "/files/*"
  match pattern.match_ "/files/path/to/file.txt" with
  | some _ => pure ()
  | none => throw (IO.userError "Expected match")

test "no match for wrong path" := do
  let pattern := RoutePattern.parse "/users"
  match pattern.match_ "/posts" with
  | some _ => throw (IO.userError "Expected no match")
  | none => pure ()

test "no match for shorter path" := do
  let pattern := RoutePattern.parse "/users/:id"
  match pattern.match_ "/users" with
  | some _ => throw (IO.userError "Expected no match")
  | none => pure ()

test "match strips query string" := do
  let pattern := RoutePattern.parse "/users/:id"
  match pattern.match_ "/users/123?foo=bar" with
  | some params => params.lookup "id" ≡ some "123"
  | none => throw (IO.userError "Expected match")

-- ============================================================================
-- Response Builder Tests
-- ============================================================================

testSuite "ResponseBuilder"

test "ok response" := do
  let resp := Response.ok "Hello"
  resp.status.code ≡ 200
  shouldSatisfy (resp.body == "Hello".toUTF8) "body should be Hello"
  resp.headers.get "Content-Type" ≡ some "text/plain; charset=utf-8"

test "json response" := do
  let resp := Response.json "{\"key\": \"value\"}"
  resp.status.code ≡ 200
  resp.headers.get "Content-Type" ≡ some "application/json"

test "html response" := do
  let resp := Response.html "<h1>Hello</h1>"
  resp.status.code ≡ 200
  resp.headers.get "Content-Type" ≡ some "text/html; charset=utf-8"

test "notFound response" := do
  let resp := Response.notFound
  resp.status.code ≡ 404

test "badRequest response" := do
  let resp := Response.badRequest "Invalid input"
  resp.status.code ≡ 400
  shouldSatisfy (resp.body == "Invalid input".toUTF8) "body should be error message"

test "redirect response" := do
  let resp := Response.redirect "/new-location"
  resp.status.code ≡ 302
  resp.headers.get "Location" ≡ some "/new-location"

test "permanent redirect response" := do
  let resp := Response.redirect "/new-location" (permanent := true)
  resp.status.code ≡ 301
  resp.headers.get "Location" ≡ some "/new-location"

test "noContent response" := do
  let resp := Response.noContent
  resp.status.code ≡ 204
  resp.body.size ≡ 0

test "created response" := do
  let resp := Response.created "{\"id\": 1}"
  resp.status.code ≡ 201

test "internalError response" := do
  let resp := Response.internalError
  resp.status.code ≡ 500

test "unauthorized response" := do
  let resp := Response.unauthorized
  resp.status.code ≡ 401
  shouldSatisfy (resp.body == "Unauthorized".toUTF8) "body should be Unauthorized"

test "unauthorized response with custom message" := do
  let resp := Response.unauthorized "Please login first"
  resp.status.code ≡ 401
  shouldSatisfy (resp.body == "Please login first".toUTF8) "body should be custom message"

test "forbidden response" := do
  let resp := Response.forbidden
  resp.status.code ≡ 403
  shouldSatisfy (resp.body == "Forbidden".toUTF8) "body should be Forbidden"

test "methodNotAllowed response" := do
  let resp := Response.methodNotAllowed
  resp.status.code ≡ 405

test "methodNotAllowed response with Allow header" := do
  let resp := Response.methodNotAllowed ["GET", "POST", "OPTIONS"]
  resp.status.code ≡ 405
  resp.headers.get "Allow" ≡ some "GET, POST, OPTIONS"

test "conflict response" := do
  let resp := Response.conflict
  resp.status.code ≡ 409

test "payloadTooLarge response" := do
  let resp := Response.payloadTooLarge
  resp.status.code ≡ 413

test "unprocessableEntity response" := do
  let resp := Response.unprocessableEntity
  resp.status.code ≡ 422

test "tooManyRequests response" := do
  let resp := Response.tooManyRequests
  resp.status.code ≡ 429

test "tooManyRequests response with Retry-After" := do
  let resp := Response.tooManyRequests (retryAfter := some 60)
  resp.status.code ≡ 429
  resp.headers.get "Retry-After" ≡ some "60"

test "serviceUnavailable response" := do
  let resp := Response.serviceUnavailable
  resp.status.code ≡ 503

test "serviceUnavailable response with Retry-After" := do
  let resp := Response.serviceUnavailable (retryAfter := some 120)
  resp.status.code ≡ 503
  resp.headers.get "Retry-After" ≡ some "120"

test "requestTimeout response" := do
  let resp := Response.requestTimeout
  resp.status.code ≡ 408
  shouldSatisfy (resp.body == "Request Timeout".toUTF8) "body should be Request Timeout"
  resp.headers.get "Connection" ≡ some "close"

test "requestTimeout response with custom message" := do
  let resp := Response.requestTimeout "The request took too long"
  resp.status.code ≡ 408
  shouldSatisfy (resp.body == "The request took too long".toUTF8) "body should be custom message"

test "uriTooLong response" := do
  let resp := Response.uriTooLong
  resp.status.code ≡ 414
  shouldSatisfy (resp.body == "URI Too Long".toUTF8) "body should be URI Too Long"

test "uriTooLong response with custom message" := do
  let resp := Response.uriTooLong "The URI is way too long"
  resp.status.code ≡ 414
  shouldSatisfy (resp.body == "The URI is way too long".toUTF8) "body should be custom message"

test "headerFieldsTooLarge response" := do
  let resp := Response.headerFieldsTooLarge
  resp.status.code ≡ 431
  shouldSatisfy (resp.body == "Request Header Fields Too Large".toUTF8) "body should match"

test "headerFieldsTooLarge response with custom message" := do
  let resp := Response.headerFieldsTooLarge "Too many cookies!"
  resp.status.code ≡ 431
  shouldSatisfy (resp.body == "Too many cookies!".toUTF8) "body should be custom message"

test "response has content length" := do
  let resp := Response.ok "Hello, World!"
  resp.headers.get "Content-Length" ≡ some "13"

-- ============================================================================
-- Router Tests
-- ============================================================================

testSuite "Router"

test "empty router returns 404" := do
  let router := Router.empty
  let req : Request := {
    method := Method.GET
    path := "/test"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 404

test "router matches GET route" := do
  let router := Router.empty
    |>.get "/" (fun _ => pure (Response.ok "Home"))
  let req : Request := {
    method := Method.GET
    path := "/"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 200
  shouldSatisfy (resp.body == "Home".toUTF8) "body should be Home"

test "router matches POST route" := do
  let router := Router.empty
    |>.post "/users" (fun _ => pure (Response.created))
  let req : Request := {
    method := Method.POST
    path := "/users"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 201

test "router extracts path params" := do
  let router := Router.empty
    |>.get "/users/:id" (fun req => do
      let id := req.param "id"
      match id with
      | some v => pure (Response.ok v)
      | none => pure (Response.badRequest))
  let req : Request := {
    method := Method.GET
    path := "/users/42"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 200
  shouldSatisfy (resp.body == "42".toUTF8) "body should be 42"

test "router wrong method returns 405" := do
  let router := Router.empty
    |>.get "/users" (fun _ => pure (Response.ok ""))
  let req : Request := {
    method := Method.POST
    path := "/users"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 405
  -- Check that Allow header is set
  resp.headers.get "Allow" ≡ some "GET"

test "router wrong method returns 405 with multiple allowed methods" := do
  let router := Router.empty
    |>.get "/users" (fun _ => pure (Response.ok ""))
    |>.post "/users" (fun _ => pure (Response.created))
    |>.delete "/users" (fun _ => pure (Response.noContent))
  let req : Request := {
    method := Method.PUT
    path := "/users"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 405
  -- Check that Allow header contains all methods
  match resp.headers.get "Allow" with
  | some allow =>
    shouldSatisfy (containsSubstr allow "GET") "should contain GET"
    shouldSatisfy (containsSubstr allow "POST") "should contain POST"
    shouldSatisfy (containsSubstr allow "DELETE") "should contain DELETE"
  | none => throw (IO.userError "Expected Allow header")

test "router no match returns 404" := do
  let router := Router.empty
    |>.get "/users" (fun _ => pure (Response.ok ""))
  let req : Request := {
    method := Method.GET
    path := "/posts"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 404

test "findMethodsForPath returns methods for matching path" := do
  let router := Router.empty
    |>.get "/api/items" (fun _ => pure (Response.ok ""))
    |>.post "/api/items" (fun _ => pure (Response.created))
    |>.delete "/api/items/:id" (fun _ => pure (Response.noContent))
  let methods := router.findMethodsForPath "/api/items"
  methods.length ≡ 2
  shouldSatisfy (methods.contains Method.GET) "should contain GET"
  shouldSatisfy (methods.contains Method.POST) "should contain POST"

test "findMethodsForPath returns empty for non-matching path" := do
  let router := Router.empty
    |>.get "/users" (fun _ => pure (Response.ok ""))
  let methods := router.findMethodsForPath "/posts"
  methods.length ≡ 0

test "router matches HEAD route" := do
  let router := Router.empty
    |>.head "/resource" (fun _ => pure (Response.ok ""))
  let req : Request := {
    method := Method.HEAD
    path := "/resource"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 200

test "router matches OPTIONS route" := do
  let router := Router.empty
    |>.options "/api/users" (fun _ => pure
      (ResponseBuilder.withStatus StatusCode.noContent
        |>.withHeader "Allow" "GET, POST, OPTIONS"
        |>.withHeader "Access-Control-Allow-Methods" "GET, POST"
        |>.build))
  let req : Request := {
    method := Method.OPTIONS
    path := "/api/users"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  let resp ← router.handle req
  resp.status.code ≡ 204
  resp.headers.get "Allow" ≡ some "GET, POST, OPTIONS"

-- ============================================================================
-- ServerRequest Tests
-- ============================================================================

testSuite "ServerRequest"

test "ServerRequest accessors" := do
  let req : Request := {
    method := Method.GET
    path := "/test?q=hello"
    version := Version.http11
    headers := Headers.empty.add "Host" "example.com"
    body := "body content".toUTF8
  }
  let serverReq : ServerRequest := { request := req, params := [("id", "123")] }
  serverReq.method ≡ Method.GET
  serverReq.path ≡ "/test"
  serverReq.fullPath ≡ "/test?q=hello"
  serverReq.query ≡ "q=hello"
  serverReq.queryParam "q" ≡ some "hello"
  serverReq.param "id" ≡ some "123"
  serverReq.param "missing" ≡ none
  serverReq.header "Host" ≡ some "example.com"
  serverReq.bodyString ≡ "body content"

-- ============================================================================
-- ServerConfig Tests
-- ============================================================================

testSuite "ServerConfig"

test "default config values" := do
  let config : ServerConfig := {}
  config.port ≡ 8080
  config.host ≡ "127.0.0.1"
  config.keepAliveTimeout ≡ 60
  config.requestTimeout ≡ 30

test "custom config values" := do
  let config : ServerConfig := {
    port := 3000
    host := "0.0.0.0"
    maxBodySize := 1024
  }
  config.port ≡ 3000
  config.host ≡ "0.0.0.0"
  config.maxBodySize ≡ 1024

test "default validation config values" := do
  let config : ServerConfig := {}
  config.maxUriLength ≡ 8192
  config.maxHeaderCount ≡ 100
  config.maxHeaderSize ≡ 8192
  config.maxTotalHeaderSize ≡ 65536

test "custom validation config values" := do
  let config : ServerConfig := {
    maxUriLength := 1024
    maxHeaderCount := 50
    maxHeaderSize := 4096
    maxTotalHeaderSize := 32768
  }
  config.maxUriLength ≡ 1024
  config.maxHeaderCount ≡ 50
  config.maxHeaderSize ≡ 4096
  config.maxTotalHeaderSize ≡ 32768

-- ============================================================================
-- Request Validation Tests
-- ============================================================================

testSuite "RequestValidation"

test "isValidUriChar accepts printable ASCII" := do
  shouldSatisfy (isValidUriChar 'a') "lowercase letter"
  shouldSatisfy (isValidUriChar 'Z') "uppercase letter"
  shouldSatisfy (isValidUriChar '0') "digit"
  shouldSatisfy (isValidUriChar '/') "slash"
  shouldSatisfy (isValidUriChar '?') "question mark"
  shouldSatisfy (isValidUriChar '&') "ampersand"
  shouldSatisfy (isValidUriChar '=') "equals"
  shouldSatisfy (isValidUriChar '-') "hyphen"
  shouldSatisfy (isValidUriChar '_') "underscore"
  shouldSatisfy (isValidUriChar '.') "period"
  shouldSatisfy (isValidUriChar ' ') "space"

test "isValidUriChar rejects control characters" := do
  shouldSatisfy (!isValidUriChar '\x00') "null rejected"
  shouldSatisfy (!isValidUriChar '\x01') "SOH rejected"
  shouldSatisfy (!isValidUriChar '\x1F') "unit separator rejected"
  shouldSatisfy (!isValidUriChar '\x7F') "DEL rejected"

test "validateRequest passes valid request" := do
  let config : ServerConfig := {}
  let req : Request := {
    method := Method.GET
    path := "/users/123?foo=bar"
    version := Version.http11
    headers := Headers.empty |>.add "Host" "example.com"
    body := ByteArray.empty
  }
  shouldSatisfy (validateRequest req config == none) "valid request should pass"

test "validateRequest rejects long URI" := do
  let config : ServerConfig := { maxUriLength := 10 }
  let req : Request := {
    method := Method.GET
    path := "/this/path/is/way/too/long"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  match validateRequest req config with
  | some (.uriTooLong len limit) =>
    shouldSatisfy (len > limit) "length exceeds limit"
  | _ => throw <| IO.userError "expected uriTooLong error"

test "validateRequest rejects control character in URI" := do
  let config : ServerConfig := {}
  let req : Request := {
    method := Method.GET
    path := "/path\x00with\x01nulls"
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  match validateRequest req config with
  | some (.invalidUriCharacter c) =>
    shouldSatisfy (c.toNat < 0x20) "should be control character"
  | _ => throw <| IO.userError "expected invalidUriCharacter error"

test "validateRequest rejects too many headers" := do
  let config : ServerConfig := { maxHeaderCount := 2 }
  let headers := Headers.empty
    |>.add "Header1" "value1"
    |>.add "Header2" "value2"
    |>.add "Header3" "value3"
  let req : Request := {
    method := Method.GET
    path := "/test"
    version := Version.http11
    headers := headers
    body := ByteArray.empty
  }
  match validateRequest req config with
  | some (.tooManyHeaders count limit) =>
    shouldSatisfy (count > limit) "count exceeds limit"
  | _ => throw <| IO.userError "expected tooManyHeaders error"

test "validateRequest rejects oversized header" := do
  let config : ServerConfig := { maxHeaderSize := 20 }
  let longValue := String.join (List.replicate 30 "x")
  let headers := Headers.empty |>.add "X-Long" longValue
  let req : Request := {
    method := Method.GET
    path := "/test"
    version := Version.http11
    headers := headers
    body := ByteArray.empty
  }
  match validateRequest req config with
  | some (.headerTooLarge name _ _) =>
    shouldSatisfy (name == "X-Long") "header name should match"
  | _ => throw <| IO.userError "expected headerTooLarge error"

test "validateRequest rejects excessive total header size" := do
  let config : ServerConfig := { maxTotalHeaderSize := 50 }
  let headers := Headers.empty
    |>.add "Header1" "this is a moderately long value"
    |>.add "Header2" "this is another moderately long value"
  let req : Request := {
    method := Method.GET
    path := "/test"
    version := Version.http11
    headers := headers
    body := ByteArray.empty
  }
  match validateRequest req config with
  | some (.totalHeadersTooLarge size limit) =>
    shouldSatisfy (size > limit) "size exceeds limit"
  | _ => throw <| IO.userError "expected totalHeadersTooLarge error"

test "validateRequest URI at limit passes" := do
  let config : ServerConfig := { maxUriLength := 10 }
  let req : Request := {
    method := Method.GET
    path := "/123456789"  -- exactly 10 chars
    version := Version.http11
    headers := Headers.empty
    body := ByteArray.empty
  }
  shouldSatisfy (validateRequest req config == none) "URI at limit should pass"

test "validateRequest header count at limit passes" := do
  let config : ServerConfig := { maxHeaderCount := 2 }
  let headers := Headers.empty
    |>.add "Header1" "value1"
    |>.add "Header2" "value2"
  let req : Request := {
    method := Method.GET
    path := "/test"
    version := Version.http11
    headers := headers
    body := ByteArray.empty
  }
  shouldSatisfy (validateRequest req config == none) "header count at limit should pass"

-- ============================================================================
-- Middleware Tests
-- ============================================================================

testSuite "Middleware"

test "identity middleware does nothing" := do
  let handler : Handler := fun _ => pure (Response.ok "Hello")
  let wrapped := Middleware.identity handler
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  let resp ← wrapped req
  resp.status.code ≡ 200
  shouldSatisfy (resp.body == "Hello".toUTF8) "body should be Hello"

test "middleware can modify response" := do
  -- Middleware that adds a header to the response
  let addHeader : Middleware := fun next => fun req => do
    let resp ← next req
    pure { resp with headers := resp.headers.add "X-Middleware" "applied" }
  let handler : Handler := fun _ => pure (Response.ok "Hello")
  let wrapped := addHeader handler
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  let resp ← wrapped req
  resp.headers.get "X-Middleware" ≡ some "applied"

test "middleware chain applies in correct order" := do
  -- First middleware adds "1"
  let mw1 : Middleware := fun next => fun req => do
    let resp ← next req
    let body := String.fromUTF8! resp.body ++ "1"
    pure { resp with body := body.toUTF8 }
  -- Second middleware adds "2"
  let mw2 : Middleware := fun next => fun req => do
    let resp ← next req
    let body := String.fromUTF8! resp.body ++ "2"
    pure { resp with body := body.toUTF8 }
  let handler : Handler := fun _ => pure (Response.ok "X")
  -- Chain: mw1 wraps mw2 wraps handler
  -- Execution: mw1 calls mw2 which calls handler
  -- Response flows back: handler returns "X", mw2 adds "2" -> "X2", mw1 adds "1" -> "X21"
  let chain := Middleware.chain [mw1, mw2]
  let wrapped := chain handler
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  let resp ← wrapped req
  -- mw1 is outermost, so it runs last on response -> appends "1" last
  shouldSatisfy (resp.body == "X21".toUTF8) "body should be X21 (handler X, then mw2 adds 2, then mw1 adds 1)"

test "empty middleware chain is identity" := do
  let handler : Handler := fun _ => pure (Response.ok "Unchanged")
  let chain := Middleware.chain []
  let wrapped := chain handler
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  let resp ← wrapped req
  shouldSatisfy (resp.body == "Unchanged".toUTF8) "body should be Unchanged"

-- ============================================================================
-- Query String Tests
-- ============================================================================

testSuite "QueryString"

test "query returns empty string when no query" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/users"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.query ≡ ""

test "query returns query string without leading ?" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/users?name=john&age=30"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.query ≡ "name=john&age=30"

test "path strips query string" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/users?name=john"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.path ≡ "/users"

test "fullPath includes query string" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/users?name=john"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.fullPath ≡ "/users?name=john"

test "queryParam returns single param" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/search?q=hello&limit=10"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.queryParam "q" ≡ some "hello"
  req.queryParam "limit" ≡ some "10"
  req.queryParam "missing" ≡ none

test "queryParams returns all params" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/search?a=1&b=2&c=3"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  let params := req.queryParams
  params.length ≡ 3
  params.lookup "a" ≡ some "1"
  params.lookup "b" ≡ some "2"
  params.lookup "c" ≡ some "3"

test "queryParam handles empty value" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/search?flag&name=test"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.queryParam "flag" ≡ some ""
  req.queryParam "name" ≡ some "test"

test "queryParam handles value with equals sign" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/search?expr=a=b"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.queryParam "expr" ≡ some "a=b"

test "urlDecode handles plus as space" := do
  ServerRequest.urlDecode "hello+world" ≡ "hello world"

test "urlDecode handles percent encoding" := do
  ServerRequest.urlDecode "hello%20world" ≡ "hello world"
  ServerRequest.urlDecode "%2F" ≡ "/"
  ServerRequest.urlDecode "%3D" ≡ "="

test "urlDecode handles mixed encoding" := do
  ServerRequest.urlDecode "a+b%3Dc" ≡ "a b=c"

test "queryParam decodes URL-encoded values" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/search?name=John+Doe&city=New%20York"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.queryParam "name" ≡ some "John Doe"
  req.queryParam "city" ≡ some "New York"

test "queryParamAll returns all values for repeated key" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/search?tag=rust&tag=lean&tag=haskell"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  let tags := req.queryParamAll "tag"
  tags.length ≡ 3
  tags ≡ ["rust", "lean", "haskell"]

test "query handles multiple question marks" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/search?q=what?&foo=bar"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.query ≡ "q=what?&foo=bar"
  req.queryParam "q" ≡ some "what?"

-- ============================================================================
-- Form Data Tests
-- ============================================================================

testSuite "FormData"

test "parseUrlEncodedForm parses simple fields" := do
  let fd := ServerRequest.parseUrlEncodedForm "name=John&age=30"
  fd.field "name" ≡ some "John"
  fd.field "age" ≡ some "30"

test "parseUrlEncodedForm handles URL encoding" := do
  let fd := ServerRequest.parseUrlEncodedForm "name=John+Doe&city=New%20York"
  fd.field "name" ≡ some "John Doe"
  fd.field "city" ≡ some "New York"

test "parseUrlEncodedForm handles empty values" := do
  let fd := ServerRequest.parseUrlEncodedForm "flag&name=test"
  fd.field "flag" ≡ some ""
  fd.field "name" ≡ some "test"

test "formField returns field from urlencoded body" := do
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/submit"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" "application/x-www-form-urlencoded"
      body := "username=alice&password=secret".toUTF8
    }
  }
  req.formField "username" ≡ some "alice"
  req.formField "password" ≡ some "secret"
  req.formField "missing" ≡ none

test "formFieldAll returns all values for repeated field" := do
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/submit"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" "application/x-www-form-urlencoded"
      body := "tag=rust&tag=lean&tag=haskell".toUTF8
    }
  }
  let tags := req.formFieldAll "tag"
  tags.length ≡ 3
  tags ≡ ["rust", "lean", "haskell"]

test "hasFormData returns true for urlencoded" := do
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/submit"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" "application/x-www-form-urlencoded"
      body := "name=test".toUTF8
    }
  }
  shouldSatisfy req.hasFormData "should have form data"

test "hasFormData returns true for multipart" := do
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/upload"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" "multipart/form-data; boundary=----WebKitBoundary"
      body := ByteArray.empty
    }
  }
  shouldSatisfy req.hasFormData "should have form data"

test "hasFormData returns false for json" := do
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/api"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" "application/json"
      body := "{}".toUTF8
    }
  }
  shouldSatisfy (!req.hasFormData) "should not have form data"

test "contentType returns Content-Type header" := do
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/submit"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" "text/plain"
      body := ByteArray.empty
    }
  }
  req.contentType ≡ some "text/plain"

test "parseMultipartForm parses text fields" := do
  let boundary := "----WebKitFormBoundary7MA4YWxkTrZu0gW"
  let body := s!"------WebKitFormBoundary7MA4YWxkTrZu0gW\r\nContent-Disposition: form-data; name=\"username\"\r\n\r\njohn_doe\r\n------WebKitFormBoundary7MA4YWxkTrZu0gW\r\nContent-Disposition: form-data; name=\"email\"\r\n\r\njohn@example.com\r\n------WebKitFormBoundary7MA4YWxkTrZu0gW--\r\n"
  let fd := ServerRequest.parseMultipartForm body.toUTF8 boundary
  fd.field "username" ≡ some "john_doe"
  fd.field "email" ≡ some "john@example.com"

test "parseMultipartForm parses file uploads" := do
  let boundary := "----WebKitFormBoundary7MA4YWxkTrZu0gW"
  let body := s!"------WebKitFormBoundary7MA4YWxkTrZu0gW\r\nContent-Disposition: form-data; name=\"file\"; filename=\"test.txt\"\r\nContent-Type: text/plain\r\n\r\nHello, World!\r\n------WebKitFormBoundary7MA4YWxkTrZu0gW--\r\n"
  let fd := ServerRequest.parseMultipartForm body.toUTF8 boundary
  match fd.file "file" with
  | some f =>
    f.filename ≡ "test.txt"
    f.contentType ≡ "text/plain"
    shouldSatisfy (f.data == "Hello, World!".toUTF8) "file data should match"
  | none => throw (IO.userError "Expected file")

test "formFile returns uploaded file" := do
  let boundary := "----boundary123"
  let body := s!"------boundary123\r\nContent-Disposition: form-data; name=\"avatar\"; filename=\"photo.jpg\"\r\nContent-Type: image/jpeg\r\n\r\nJPEG_DATA\r\n------boundary123--\r\n"
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/upload"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" s!"multipart/form-data; boundary={boundary}"
      body := body.toUTF8
    }
  }
  match req.formFile "avatar" with
  | some f =>
    f.filename ≡ "photo.jpg"
    f.contentType ≡ "image/jpeg"
  | none => throw (IO.userError "Expected file")

test "FormData.empty has no fields or files" := do
  let fd := ServerRequest.FormData.empty
  fd.fields.length ≡ 0
  fd.files.length ≡ 0

test "formData returns empty for non-form content" := do
  let req : ServerRequest := {
    request := {
      method := Method.POST
      path := "/api"
      version := Version.http11
      headers := Headers.empty.add "Content-Type" "application/json"
      body := "{\"key\": \"value\"}".toUTF8
    }
  }
  let fd := req.formData
  fd.fields.length ≡ 0
  fd.files.length ≡ 0

-- ============================================================================
-- Cookie Tests
-- ============================================================================

testSuite "Cookies"

test "cookie returns cookie value" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty.add "Cookie" "session=abc123; user=john"
      body := ByteArray.empty
    }
  }
  req.cookie "session" ≡ some "abc123"
  req.cookie "user" ≡ some "john"
  req.cookie "missing" ≡ none

test "cookies returns all cookies" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty.add "Cookie" "a=1; b=2; c=3"
      body := ByteArray.empty
    }
  }
  let cookies := req.cookies
  cookies.length ≡ 3
  cookies.lookup "a" ≡ some "1"
  cookies.lookup "b" ≡ some "2"
  cookies.lookup "c" ≡ some "3"

test "cookie handles value with equals sign" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty.add "Cookie" "token=abc=def=ghi"
      body := ByteArray.empty
    }
  }
  req.cookie "token" ≡ some "abc=def=ghi"

test "cookies returns empty list when no Cookie header" := do
  let req : ServerRequest := {
    request := {
      method := Method.GET
      path := "/"
      version := Version.http11
      headers := Headers.empty
      body := ByteArray.empty
    }
  }
  req.cookies.length ≡ 0

test "setCookie adds Set-Cookie header" := do
  let builder := ResponseBuilder.withStatus StatusCode.ok
    |>.setCookie "session" "abc123"
  -- Check that Set-Cookie header contains the cookie
  match builder.headers.get "Set-Cookie" with
  | some header =>
    shouldSatisfy (header.startsWith "session=abc123") "should start with name=value"
  | none => throw (IO.userError "Expected Set-Cookie header")

test "setCookie with options includes attributes" := do
  let opts : CookieOptions := {
    maxAge := some 3600
    path := some "/app"
    secure := true
    httpOnly := true
    sameSite := some .strict
  }
  let builder := ResponseBuilder.withStatus StatusCode.ok
    |>.setCookie "token" "xyz" opts
  match builder.headers.get "Set-Cookie" with
  | some header =>
    shouldSatisfy (containsSubstr header "token=xyz") "should contain name=value"
    shouldSatisfy (containsSubstr header "Max-Age=3600") "should contain Max-Age"
    shouldSatisfy (containsSubstr header "Path=/app") "should contain Path"
    shouldSatisfy (containsSubstr header "Secure") "should contain Secure"
    shouldSatisfy (containsSubstr header "HttpOnly") "should contain HttpOnly"
    shouldSatisfy (containsSubstr header "SameSite=Strict") "should contain SameSite"
  | none => throw (IO.userError "Expected Set-Cookie header")

test "setCookie with domain" := do
  let opts : CookieOptions := { domain := some ".example.com" }
  let builder := ResponseBuilder.withStatus StatusCode.ok
    |>.setCookie "cookie" "value" opts
  match builder.headers.get "Set-Cookie" with
  | some header =>
    shouldSatisfy (containsSubstr header "Domain=.example.com") "should contain Domain"
  | none => throw (IO.userError "Expected Set-Cookie header")

test "clearCookie sets Max-Age to 0" := do
  let builder := ResponseBuilder.withStatus StatusCode.ok
    |>.clearCookie "session"
  match builder.headers.get "Set-Cookie" with
  | some header =>
    shouldSatisfy (containsSubstr header "session=") "should contain cookie name"
    shouldSatisfy (containsSubstr header "Max-Age=0") "should set Max-Age=0"
  | none => throw (IO.userError "Expected Set-Cookie header")

test "CookieOptions.session creates session cookie" := do
  let opts := CookieOptions.session
  shouldSatisfy opts.httpOnly "should be HttpOnly"
  shouldSatisfy (opts.maxAge == none) "should have no Max-Age"

test "CookieOptions.persistent creates cookie with max age" := do
  let opts := CookieOptions.persistent 86400
  opts.maxAge ≡ some 86400

test "CookieOptions.secureOnly creates secure cookie" := do
  let opts := CookieOptions.secureOnly
  shouldSatisfy opts.secure "should be Secure"
  shouldSatisfy opts.httpOnly "should be HttpOnly"
  opts.sameSite ≡ some SameSite.strict

-- ============================================================================
-- TLS Configuration Tests
-- ============================================================================

testSuite "TLS"

test "TlsConfig structure" := do
  let config : TlsConfig := { certFile := "/path/to/cert.pem", keyFile := "/path/to/key.pem" }
  config.certFile ≡ "/path/to/cert.pem"
  config.keyFile ≡ "/path/to/key.pem"

test "ServerConfig with TLS defaults to none" := do
  let config : ServerConfig := {}
  shouldSatisfy (config.tls == none) "tls should default to none"

test "ServerConfig with TLS configured" := do
  let tlsConfig : TlsConfig := { certFile := "cert.pem", keyFile := "key.pem" }
  let config : ServerConfig := { tls := some tlsConfig }
  match config.tls with
  | some tls =>
    tls.certFile ≡ "cert.pem"
    tls.keyFile ≡ "key.pem"
  | none => throw (IO.userError "Expected TLS config")

test "ServerConfig with TLS and custom port" := do
  let tlsConfig : TlsConfig := { certFile := "cert.pem", keyFile := "key.pem" }
  let config : ServerConfig := {
    port := 8443
    host := "0.0.0.0"
    tls := some tlsConfig
  }
  config.port ≡ 8443
  config.host ≡ "0.0.0.0"
  shouldSatisfy (config.tls.isSome) "should have TLS config"



-- Main entry point
def main : IO UInt32 := do
  IO.println "Citadel HTTP Server Tests"
  IO.println "========================="
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result != 0 then
    IO.println "Some tests failed!"
    return 1
  else
    IO.println "All tests passed!"
    return 0
