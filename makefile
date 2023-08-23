MODE?=release
ifeq ($(MODE),release)
  args=--release
endif
OUT_DIR?=/mnt/y/srv/el
IP?=::1
PORT?=62820
R?=cargo
all:build
build:;cargo build $(args)
wasm:;cd wasm && npm ci && CARGO_TARGET_DIR=target-trunk trunk build $(args)
serve:;cd wasm && trunk serve --address $(IP) --port $(PORT)
