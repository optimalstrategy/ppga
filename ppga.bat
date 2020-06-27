@echo off

cargo run --release --quiet --manifest-path .\ppga\Cargo.toml --features=build-binary -- %*
