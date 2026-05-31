# Changelog

## 1.2.0

- Update Rust toolchain to `nightly-2025-12-06`. ([237c478](https://github.com/trailofbits/elaborate/commit/237c478d0b9de125a45e3c6001b9c2a0aaec9e7a))
- Deny `clippy::disallowed-methods` rather than `warnings` in `disallowed_methods` function ([e66431d](https://github.com/trailofbits/elaborate/commit/e66431dc01d556d5c15f6bd985893d12802cf1ec))

## 1.1.0

- Update Rust toolchain to `nightly-2025-11-22`. ([9cd2409](https://github.com/trailofbits/elaborate/commit/9cd24094ba711d177bf1e0fff20b3f34b7faf30b))

## 1.0.0

- Update Rust toolchain to `nightly-2025-10-26`. ([b514a66](https://github.com/trailofbits/elaborate/commit/b514a66be76b0bf51b89048003c96468dc2a51a3))

## 0.2.1

- Set `RUSTFLAGS` to `--deny=warnings` in `disallowed_methods` ([#136](https://github.com/trailofbits/elaborate/pull/136))

## 0.2.0

- Add `disallowed_methods` function to identify functions that could be replaced with wrapped ones ([#118](https://github.com/trailofbits/elaborate/pull/118))

## 0.1.0

- Initial release
