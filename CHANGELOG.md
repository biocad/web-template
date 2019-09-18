# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.1.7] - 2019-09-18
### Changed
- `throwJson` type is more polymorphic to match `throwIO` and similar.

## [0.1.1.6] - 2019-09-13
### Added
- `MonadWebError` type class with `throwJson` function.

## [0.1.1.5] - 2019-08-08
### Added
- Added `toApplication` function - convert function from CustomWebServer to Application.

## [0.1.1.4] - 2019-01-28
### Added
- Implement `restartOnError`, add `pure` web monads.

## [0.1.1.3] - 2019-01-22
### Added
- Customizable middleware, pretty default request logging.

## [0.1.1.2] - 2018-09-12
### Fixed
- Processing of different versions for the same routes.

## [0.1.1.1] - 2018-09-12
### Changed
- Logging format. Now it is look like: `{"app":"scotty","msg":"GET /v1/ping 200","timestamp":0,"level":"INFO","datetime":"1970-01-01T00:00:00+0000"}`. TODO: use correct time.
