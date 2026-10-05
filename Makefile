# One-command entry points for the texture editor.
#
#   make app    build everything and serve the editor at http://localhost:8080/
#   make dev    serve the API and a hot-reloading frontend at http://localhost:5173/
#   make test   run the Haskell and frontend test suites
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
	npm --prefix frontend run dev

test: frontend-deps
	stack test
	npm --prefix frontend test

e2e: frontend
	stack build
	stack exec texture-server -- --port $(E2E_PORT) & server=$$!; \
	trap "kill $$server" EXIT; \
	sleep 1; \
	E2E_URL=http://localhost:$(E2E_PORT)/ npm --prefix frontend run e2e

frontend: frontend-deps
	npm --prefix frontend run build

frontend-deps: frontend/node_modules/.package-lock.json

frontend/node_modules/.package-lock.json: frontend/package.json frontend/package-lock.json
	npm --prefix frontend ci
