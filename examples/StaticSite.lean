/-
  Citadel Example: Static Website
  A simple website serving static HTML pages.
-/
import Citadel
import Staple.Json

open Citadel

def homePage : String := "
<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>Citadel - Home</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      line-height: 1.6;
      color: #333;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      min-height: 100vh;
    }
    .container {
      max-width: 800px;
      margin: 0 auto;
      padding: 2rem;
    }
    header {
      background: rgba(255,255,255,0.95);
      padding: 1rem 2rem;
      box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    }
    nav { display: flex; gap: 2rem; align-items: center; }
    nav a { color: #667eea; text-decoration: none; font-weight: 500; }
    nav a:hover { text-decoration: underline; }
    .logo { font-size: 1.5rem; font-weight: bold; color: #333; }
    main {
      background: rgba(255,255,255,0.95);
      margin: 2rem auto;
      padding: 3rem;
      border-radius: 12px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.15);
      max-width: 800px;
    }
    h1 { color: #667eea; margin-bottom: 1rem; }
    p { margin-bottom: 1rem; color: #555; }
    .highlight {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 1.5rem;
      border-radius: 8px;
      margin: 1.5rem 0;
    }
    code {
      background: #f4f4f4;
      padding: 0.2rem 0.5rem;
      border-radius: 4px;
      font-family: 'SF Mono', Monaco, monospace;
    }
    footer {
      text-align: center;
      padding: 2rem;
      color: rgba(255,255,255,0.8);
    }
  </style>
</head>
<body>
  <header>
    <nav>
      <span class=\"logo\">Citadel</span>
      <a href=\"/\">Home</a>
      <a href=\"/about\">About</a>
      <a href=\"/api/status\">API Status</a>
    </nav>
  </header>
  <main class=\"container\">
    <h1>Welcome to Citadel</h1>
    <p>Citadel is a lightweight HTTP server library for <strong>Lean 4</strong>.</p>
    <div class=\"highlight\">
      <p>This page is being served by a Lean 4 application using the Citadel HTTP server and Herald HTTP parser.</p>
    </div>
    <p>Features:</p>
    <ul style=\"margin-left: 2rem; margin-bottom: 1rem;\">
      <li>Simple routing with path parameters</li>
      <li>HTTP/1.1 with keep-alive support</li>
      <li>Request/response builders</li>
      <li>Middleware support</li>
    </ul>
    <p>Try visiting <code>/about</code> or <code>/api/status</code> to see more examples.</p>
  </main>
  <footer>
    <p>Powered by Citadel & Herald - Written in Lean 4</p>
  </footer>
</body>
</html>
"

def aboutPage : String := "
<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>Citadel - About</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      line-height: 1.6;
      color: #333;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      min-height: 100vh;
    }
    header {
      background: rgba(255,255,255,0.95);
      padding: 1rem 2rem;
      box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    }
    nav { display: flex; gap: 2rem; align-items: center; }
    nav a { color: #667eea; text-decoration: none; font-weight: 500; }
    nav a:hover { text-decoration: underline; }
    .logo { font-size: 1.5rem; font-weight: bold; color: #333; }
    main {
      background: rgba(255,255,255,0.95);
      margin: 2rem auto;
      padding: 3rem;
      border-radius: 12px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.15);
      max-width: 800px;
    }
    h1 { color: #667eea; margin-bottom: 1rem; }
    h2 { color: #764ba2; margin: 1.5rem 0 0.5rem; }
    p { margin-bottom: 1rem; color: #555; }
    pre {
      background: #2d2d2d;
      color: #f8f8f2;
      padding: 1rem;
      border-radius: 8px;
      overflow-x: auto;
      margin: 1rem 0;
    }
    footer {
      text-align: center;
      padding: 2rem;
      color: rgba(255,255,255,0.8);
    }
  </style>
</head>
<body>
  <header>
    <nav>
      <span class=\"logo\">Citadel</span>
      <a href=\"/\">Home</a>
      <a href=\"/about\">About</a>
      <a href=\"/api/status\">API Status</a>
    </nav>
  </header>
  <main>
    <h1>About Citadel</h1>
    <p>Citadel is part of a Lean 4 workspace containing interconnected libraries for building applications.</p>

    <h2>Architecture</h2>
    <p>Citadel uses <strong>Herald</strong> for HTTP message parsing, which implements a full HTTP/1.1 parser with:</p>
    <ul style=\"margin-left: 2rem; margin-bottom: 1rem;\">
      <li>Request and response parsing</li>
      <li>Chunked transfer encoding</li>
      <li>Header line folding</li>
      <li>Keep-alive connection handling</li>
    </ul>

    <h2>Example Code</h2>
    <pre>
import Citadel

def main : IO Unit := do
  let server := Server.new
    |>.get \"/\" (fun _ => pure (Response.html homePage))
    |>.get \"/users/:id\" (fun req => do
      let id := req.param \"id\" |>.getD \"unknown\"
      pure (Response.json s!\"{\\\"id\\\": \\\"{id}\\\"}\"))
  server.run
    </pre>

    <h2>The Stack</h2>
    <p>This server demonstrates:</p>
    <ul style=\"margin-left: 2rem;\">
      <li><strong>Citadel</strong> - HTTP server with routing</li>
      <li><strong>Herald</strong> - HTTP/1.1 parser</li>
      <li><strong>POSIX Sockets</strong> - TCP networking via FFI</li>
      <li><strong>Lean 4</strong> - Pure functional programming</li>
    </ul>
  </main>
  <footer>
    <p>Powered by Citadel & Herald - Written in Lean 4</p>
  </footer>
</body>
</html>
"

def notFoundPage : String := "
<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>404 - Not Found</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
    }
    .error-box {
      background: rgba(255,255,255,0.95);
      padding: 3rem;
      border-radius: 12px;
      text-align: center;
      box-shadow: 0 4px 20px rgba(0,0,0,0.15);
    }
    h1 { font-size: 4rem; color: #667eea; }
    p { color: #555; margin: 1rem 0; }
    a { color: #667eea; }
  </style>
</head>
<body>
  <div class=\"error-box\">
    <h1>404</h1>
    <p>Page not found</p>
    <a href=\"/\">Return home</a>
  </div>
</body>
</html>
"

def main : IO Unit := do
  IO.println "Starting Citadel example server..."

  let server := Server.create { port := 8080, host := "127.0.0.1" }
    -- Home page
    |>.get "/" (fun _ => pure (Response.html homePage))

    -- About page
    |>.get "/about" (fun _ => pure (Response.html aboutPage))

    -- API endpoint returning JSON
    |>.get "/api/status" (fun _ => pure (Response.json "{\"status\": \"ok\", \"server\": \"citadel\", \"version\": \"0.1.0\"}"))

    -- API endpoint with path parameter
    |>.get "/api/users/:id" (fun req => do
      let id := req.param "id" |>.getD "unknown"
      let name := s!"User {id}"
      pure (Response.json (jsonStr! { "user_id" : id, name })))

    -- POST endpoint
    |>.post "/api/echo" (fun req => do
      let body := req.bodyString
      let echoed := body
      pure (Response.json (jsonStr! { echoed })))

  IO.println "Routes:"
  IO.println "  GET  /           - Home page"
  IO.println "  GET  /about      - About page"
  IO.println "  GET  /api/status - JSON status"
  IO.println "  GET  /api/users/:id - User by ID"
  IO.println "  POST /api/echo   - Echo body"
  IO.println ""

  server.run
