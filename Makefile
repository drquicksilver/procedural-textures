# One-command entry points for the texture editor.
#
#   make app    build everything and serve the editor at http://localhost:8080/
#   make dev    serve the API and a hot-reloading frontend at http://localhost:5173/
#   make test   run the Haskell and frontend test suites, and type-check the frontend
#   make e2e    run the slower end-to-end browser tests (needs Chrome)

PORT ?= 8080

E2E_PORT ?= 8095

.PHONY: app dev test e2e frontend frontend-deps

app: frontend
	stack build
	stack exec texture-server -- --port $(PORT)

dev: frontend-deps
	stack build
	trap 'kill 0' EXIT; \
	stack exec texture-server -- --port $(PORT) & \
	API_PORT=$(PORT) npm --prefix frontend run dev

test: frontend-deps
	stack test
	npm --prefix frontend test
	npm --prefix frontend run typecheck

# Starts a server, waits (up to a minute) until its API answers, runs the
# browser tests against it and stops it again.
e2e: frontend
	stack build
	stack exec texture-server -- --port $(E2E_PORT) & server=$$!; \
	trap "kill $$server" EXIT; \
	for attempt in $$(seq 120); do \
	  curl -sf http://localhost:$(E2E_PORT)/api/schema > /dev/null && break; \
	  if ! kill -0 $$server 2> /dev/null; then echo "texture-server exited"; exit 1; fi; \
	  sleep 0.5; \
	done; \
	curl -sf http://localhost:$(E2E_PORT)/api/schema > /dev/null || { echo "texture-server did not start"; exit 1; }; \
	E2E_URL=http://localhost:$(E2E_PORT)/ npm --prefix frontend run e2e

frontend: frontend-deps
	npm --prefix frontend run build

frontend-deps: frontend/node_modules/.package-lock.json

frontend/node_modules/.package-lock.json: frontend/package.json frontend/package-lock.json
	npm --prefix frontend ci
