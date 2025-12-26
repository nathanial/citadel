# Citadel

An HTTP/1.1 server library for Lean 4.

## Features

- HTTP/1.1 server with keep-alive support
- Request routing with path parameters
- Middleware support
- Static file serving
- Built on [Herald](../herald) for HTTP parsing

## Installation

Add to your `lakefile.lean`:

```lean
require citadel from git "https://github.com/username/citadel" @ "main"
```

## Quick Start

```lean
import Citadel

open Citadel

def main : IO Unit := do
  let server ← Server.create { port := 8080 }

  server.get "/" fun _req => do
    Response.ok "Hello, World!"

  server.get "/users/:id" fun req => do
    let id := req.params.get! "id"
    Response.json (jsonStr! { id })

  server.run
```

## License

MIT License - see [LICENSE](LICENSE) for details.
