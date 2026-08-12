# Changelog

## 2.2.0

- FEATURE: Upgrade to toolchain `nightly-2026-03-05` ([63eff51](https://github.com/trailofbits/elaborate/commit/63eff51546d668b5755e4c10db5d55f1b9dba4ce))

## 2.1.0

- FEATURE: Upgrade toolchain to `nightly-2026-01-19` ([55a65d6](https://github.com/trailofbits/elaborate/commit/55a65d673964ca4f5e69ad5869f698c65bf5f690))
- Add `--no-deps` to `clippy` invocation in `disallowed_methods` ([6b5d34f](https://github.com/trailofbits/elaborate/commit/6b5d34f2920ba96bbe82fb0b2be7578816e91a07))

## 2.0.0

- BREAKING: Eliminate use of private `generated` module so that `elaborate` symbol paths begin with `elaborate::std`. This is a breaking change for anyone using `clippy::disallowed_methods` to prevent use of an `elaborate` method. ([9cb8c05](https://github.com/trailofbits/elaborate/commit/9cb8c0573796fa6921b0921c22a57f067355f1b6))

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
