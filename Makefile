# The editor is static: app/dev/e2e start only Node, never texture-server.
PORT ?= 8080
E2E_PORT ?= 8095
.PHONY: app dev test e2e frontend frontend-deps assets

app: frontend
	npm --prefix frontend run preview -- --host 127.0.0.1 --port $(PORT) --strictPort

dev: frontend-deps
	npm --prefix frontend run dev

test: frontend-deps
	stack build
	stack test
	npm --prefix frontend test
	npm --prefix frontend run build

e2e: frontend
	E2E_PORT=$(E2E_PORT) npm --prefix frontend run e2e

assets:
	stack run procedural-textures -- assets

frontend: frontend-deps
	npm --prefix frontend run build

frontend-deps: frontend/node_modules/.package-lock.json
frontend/node_modules/.package-lock.json: frontend/package.json frontend/package-lock.json
	npm --prefix frontend ci

.PHONY: pages
pages: frontend
	npm --prefix frontend run pages:build
