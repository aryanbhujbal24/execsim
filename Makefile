.PHONY: build test bench alloc demo doc fmt clean

build:
	dune build

test:
	dune test

# Throughput. Build in release so the numbers mean something.
bench:
	dune build --profile release
	./_build/default/bench/bench.exe

# Minor-heap words per operation, attributed by call site.
alloc:
	dune build --profile release
	./_build/default/bench/alloc.exe

demo:
	dune exec bin/main.exe

doc:
	dune build @doc
	@echo "open _build/default/_doc/_html/index.html"

fmt:
	dune build @fmt --auto-promote || true

clean:
	dune clean
