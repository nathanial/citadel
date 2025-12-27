/-
  Citadel Core Types

  Core types for the HTTP server.
-/
import Herald

namespace Citadel

open Herald.Core

/-- Server configuration -/
structure ServerConfig where
  /-- Port to listen on -/
  port : UInt16 := 8080
  /-- Host to bind to -/
  host : String := "127.0.0.1"
  /-- Maximum request body size in bytes -/
  maxBodySize : Nat := 10 * 1024 * 1024  -- 10 MB
  /-- Keep-alive timeout in seconds -/
  keepAliveTimeout : Nat := 60
  /-- Request timeout in seconds -/
  requestTimeout : Nat := 30
  deriving Repr, Inhabited

/-- Path parameters extracted from route matching -/
abbrev Params := List (String × String)

/-- Extended request with route parameters -/
structure ServerRequest where
  /-- The underlying HTTP request -/
  request : Request
  /-- Path parameters from route matching -/
  params : Params := []
  deriving Inhabited

namespace ServerRequest

/-- Get the request method -/
def method (r : ServerRequest) : Method := r.request.method

/-- Get the full request path including query string -/
def fullPath (r : ServerRequest) : String := r.request.path

/-- Get the request path without the query string -/
def path (r : ServerRequest) : String :=
  match r.request.path.splitOn "?" with
  | p :: _ => p
  | [] => r.request.path

/-- Get the request headers -/
def headers (r : ServerRequest) : Headers := r.request.headers

/-- Get the request body -/
def body (r : ServerRequest) : ByteArray := r.request.body

/-- Get the body as a string -/
def bodyString (r : ServerRequest) : String :=
  String.fromUTF8! r.request.body

/-- Get a path parameter by name -/
def param (r : ServerRequest) (name : String) : Option String :=
  r.params.lookup name

/-- Get a header by name -/
def header (r : ServerRequest) (name : String) : Option String :=
  r.request.headers.get name

/-- Get the raw query string (without the leading '?') -/
def query (r : ServerRequest) : String :=
  match r.request.path.splitOn "?" with
  | _ :: rest => String.intercalate "?" rest
  | _ => ""

/-- Decode a percent-encoded character (%XX) -/
private def decodeHexChar (c1 c2 : Char) : Option Char :=
  let hexVal (c : Char) : Option UInt8 :=
    if '0' ≤ c && c ≤ '9' then some (c.toNat.toUInt8 - '0'.toNat.toUInt8)
    else if 'a' ≤ c && c ≤ 'f' then some (c.toNat.toUInt8 - 'a'.toNat.toUInt8 + 10)
    else if 'A' ≤ c && c ≤ 'F' then some (c.toNat.toUInt8 - 'A'.toNat.toUInt8 + 10)
    else none
  match hexVal c1, hexVal c2 with
  | some h1, some h2 => some (Char.ofNat (h1 * 16 + h2).toNat)
  | _, _ => none

/-- URL-decode a string (handles %XX and + for space) -/
def urlDecode (s : String) : String :=
  let chars := s.toList
  go chars []
where
  go : List Char → List Char → String
  | [], acc => String.ofList acc.reverse
  | '+' :: rest, acc => go rest (' ' :: acc)
  | '%' :: c1 :: c2 :: rest, acc =>
    match decodeHexChar c1 c2 with
    | some c => go rest (c :: acc)
    | none => go rest (c2 :: c1 :: '%' :: acc)  -- Keep as-is if invalid
  | c :: rest, acc => go (rest) (c :: acc)

/-- Parse a query string into key-value pairs -/
def parseQueryString (qs : String) : List (String × String) :=
  if qs.isEmpty then []
  else
    qs.splitOn "&"
      |>.filterMap fun pair =>
        match pair.splitOn "=" with
        | [key] => some (urlDecode key, "")
        | key :: rest => some (urlDecode key, urlDecode (String.intercalate "=" rest))
        | [] => none

/-- Get all query parameters as a list of key-value pairs -/
def queryParams (r : ServerRequest) : List (String × String) :=
  parseQueryString r.query

/-- Get a query parameter by name -/
def queryParam (r : ServerRequest) (name : String) : Option String :=
  r.queryParams.lookup name

/-- Get all values for a query parameter (for repeated keys like ?tag=a&tag=b) -/
def queryParamAll (r : ServerRequest) (name : String) : List String :=
  r.queryParams.filterMap fun (k, v) => if k == name then some v else none

end ServerRequest

/-- Response builder for convenient response construction -/
structure ResponseBuilder where
  /-- Status code -/
  status : StatusCode := StatusCode.ok
  /-- Response headers -/
  headers : Headers := Headers.empty
  /-- Response body -/
  body : ByteArray := ByteArray.empty
  deriving Inhabited

namespace ResponseBuilder

/-- Create a response builder with a status code -/
def withStatus (code : StatusCode) : ResponseBuilder :=
  { status := code }

/-- Set the response body as bytes -/
def withBody (b : ResponseBuilder) (data : ByteArray) : ResponseBuilder :=
  { b with body := data }

/-- Set the response body as a string -/
def withText (b : ResponseBuilder) (text : String) : ResponseBuilder :=
  { b with body := text.toUTF8 }

/-- Add a header -/
def withHeader (b : ResponseBuilder) (name value : String) : ResponseBuilder :=
  { b with headers := b.headers.add name value }

/-- Set Content-Type header -/
def withContentType (b : ResponseBuilder) (ct : String) : ResponseBuilder :=
  b.withHeader "Content-Type" ct

/-- Build the final response -/
def build (b : ResponseBuilder) : Response :=
  let headers := b.headers.add "Content-Length" (toString b.body.size)
  { status := b.status
    reason := b.status.defaultReason
    version := Version.http11
    headers := headers
    body := b.body }

end ResponseBuilder

namespace Response

/-- Create a 200 OK response with text body -/
def ok (body : String) : Response :=
  ResponseBuilder.withStatus StatusCode.ok
    |>.withText body
    |>.withContentType "text/plain; charset=utf-8"
    |>.build

/-- Create a 200 OK response with HTML body -/
def html (body : String) : Response :=
  ResponseBuilder.withStatus StatusCode.ok
    |>.withText body
    |>.withContentType "text/html; charset=utf-8"
    |>.build

/-- Create a 200 OK response with JSON body -/
def json (body : String) : Response :=
  ResponseBuilder.withStatus StatusCode.ok
    |>.withText body
    |>.withContentType "application/json"
    |>.build

/-- Create a 201 Created response -/
def created (body : String := "") : Response :=
  ResponseBuilder.withStatus StatusCode.created
    |>.withText body
    |>.withContentType "application/json"
    |>.build

/-- Create a 204 No Content response -/
def noContent : Response :=
  ResponseBuilder.withStatus StatusCode.noContent
    |>.build

/-- Create a 400 Bad Request response -/
def badRequest (message : String := "Bad Request") : Response :=
  ResponseBuilder.withStatus StatusCode.badRequest
    |>.withText message
    |>.withContentType "text/plain; charset=utf-8"
    |>.build

/-- Create a 404 Not Found response -/
def notFound (message : String := "Not Found") : Response :=
  ResponseBuilder.withStatus StatusCode.notFound
    |>.withText message
    |>.withContentType "text/plain; charset=utf-8"
    |>.build

/-- Create a 500 Internal Server Error response -/
def internalError (message : String := "Internal Server Error") : Response :=
  ResponseBuilder.withStatus StatusCode.internalServerError
    |>.withText message
    |>.withContentType "text/plain; charset=utf-8"
    |>.build

/-- Create a redirect response -/
def redirect (location : String) (permanent : Bool := false) : Response :=
  let status := if permanent then StatusCode.movedPermanently else StatusCode.found
  ResponseBuilder.withStatus status
    |>.withHeader "Location" location
    |>.build

end Response

/-- A request handler function -/
def Handler := ServerRequest → IO Response

/-- Route path segment -/
inductive PathSegment where
  | literal (s : String)
  | param (name : String)
  | wildcard
  deriving Repr, BEq

/-- A parsed route pattern -/
structure RoutePattern where
  segments : List PathSegment
  deriving Repr

namespace RoutePattern

/-- Parse a route pattern string like "/users/:id/posts" -/
def parse (pattern : String) : RoutePattern :=
  let parts := pattern.splitOn "/"
    |>.filter (· ≠ "")
  let segments := parts.map fun part =>
    if part == "*" then
      PathSegment.wildcard
    else if part.startsWith ":" then
      PathSegment.param (part.drop 1)
    else
      PathSegment.literal part
  { segments }

/-- Match a path against this pattern, extracting parameters -/
def match_ (rp : RoutePattern) (path : String) : Option Params :=
  let parts := path.splitOn "/"
    |>.filter (· ≠ "")
    -- Strip query string
    |>.map fun p => (p.splitOn "?").head!
  go rp.segments parts []
where
  go : List PathSegment → List String → Params → Option Params
  | [], [], acc => some acc.reverse
  | PathSegment.wildcard :: _, _, acc => some acc.reverse  -- Wildcard matches rest
  | PathSegment.literal s :: segs, p :: parts, acc =>
    if s == p then go segs parts acc else none
  | PathSegment.param name :: segs, p :: parts, acc =>
    go segs parts ((name, p) :: acc)
  | _, _, _ => none

end RoutePattern

/-- A route definition -/
structure Route where
  /-- HTTP method to match -/
  method : Method
  /-- Path pattern -/
  pattern : RoutePattern
  /-- Handler function -/
  handler : Handler

/-- Router that matches requests to handlers -/
structure Router where
  /-- Registered routes -/
  routes : List Route := []

namespace Router

/-- Create an empty router -/
def empty : Router := { routes := [] }

/-- Add a route -/
def add (r : Router) (method : Method) (pattern : String) (handler : Handler) : Router :=
  { routes := r.routes ++ [{ method, pattern := RoutePattern.parse pattern, handler }] }

/-- Add a GET route -/
def get (r : Router) (pattern : String) (handler : Handler) : Router :=
  r.add Method.GET pattern handler

/-- Add a POST route -/
def post (r : Router) (pattern : String) (handler : Handler) : Router :=
  r.add Method.POST pattern handler

/-- Add a PUT route -/
def put (r : Router) (pattern : String) (handler : Handler) : Router :=
  r.add Method.PUT pattern handler

/-- Add a DELETE route -/
def delete (r : Router) (pattern : String) (handler : Handler) : Router :=
  r.add Method.DELETE pattern handler

/-- Add a PATCH route -/
def patch (r : Router) (pattern : String) (handler : Handler) : Router :=
  r.add Method.PATCH pattern handler

/-- Find a matching route for a request -/
def findRoute (r : Router) (req : Request) : Option (Route × Params) :=
  r.routes.findSome? fun route =>
    if route.method == req.method then
      match route.pattern.match_ req.path with
      | some params => some (route, params)
      | none => none
    else
      none

/-- Handle a request, returning a response -/
def handle (r : Router) (req : Request) : IO Response := do
  match r.findRoute req with
  | some (route, params) =>
    let serverReq : ServerRequest := { request := req, params }
    route.handler serverReq
  | none =>
    pure (Response.notFound)

end Router

/-- Middleware function type -/
def Middleware := Handler → Handler

namespace Middleware

/-- Identity middleware that does nothing -/
def identity : Middleware := id

/-- Compose two middleware functions -/
def compose (m1 m2 : Middleware) : Middleware :=
  fun handler => m1 (m2 handler)

/-- Chain a list of middleware -/
def chain (middlewares : List Middleware) : Middleware :=
  middlewares.foldr compose identity

end Middleware

/-- Server error type -/
inductive ServerError where
  | bindFailed (msg : String)
  | acceptFailed (msg : String)
  | parseError (msg : String)
  | handlerError (msg : String)
  | timeout
  | connectionClosed
  deriving Repr

instance : ToString ServerError where
  toString
    | .bindFailed msg => s!"Bind failed: {msg}"
    | .acceptFailed msg => s!"Accept failed: {msg}"
    | .parseError msg => s!"Parse error: {msg}"
    | .handlerError msg => s!"Handler error: {msg}"
    | .timeout => "Request timeout"
    | .connectionClosed => "Connection closed"

end Citadel
