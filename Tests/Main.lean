/-
  Citadel Test Suite
  Main entry point for running all tests.
-/

import Citadel
import Crucible

open Crucible
open Citadel
open Herald.Core

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

test "router wrong method returns 404" := do
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
  resp.status.code ≡ 404

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
  serverReq.path ≡ "/test?q=hello"
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

#generate_tests

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
