#!env /usr/bin/bash
cargo run --release --quiet --manifest-path ./ppga/Cargo.toml --features=build-binary -- $*
