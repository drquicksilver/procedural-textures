# One-command entry points for the texture editor.
#
#   make app    build everything and serve the editor at http://localhost:8080/
#   make dev    serve the API and a hot-reloading frontend at http://localhost:5173/
#   make test   run the Haskell and frontend test suites

PORT ?= 8080

.PHONY: app dev test frontend frontend-deps

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

frontend: frontend-deps
	npm --prefix frontend run build

frontend-deps: frontend/node_modules/.package-lock.json

frontend/node_modules/.package-lock.json: frontend/package.json frontend/package-lock.json
	npm --prefix frontend ci
