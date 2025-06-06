# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.3.16] - 2025-03-27
### Changed
- `Permit` does not overwrite 403 response description of deeper layers.

## [0.1.3.15] - 2023-11-20
### Changed
- `WithDescription` does not clash between different fields with the same type.

## [0.1.3.14] - 2023-06-14
### Added
- Debug log formatter `debugLogHandler`, customizable log middleware `logMiddlewareCustom`.

## [0.1.3.13] - 2023-01-23
### Changed
- Add check for aeson 2, for GHC 9.2.5.

## [0.1.3.12] - 2022-07-17
### Changed
- Update imports to support `lens-aeson` >=1.2.

## [0.1.3.11] - 2022-02-06
### Added
- Custom `swagger-ui` wrapper that allows to use BIOCAD's OIDC authorization from the UI.
- Compatibility with `openapi3` 3.2.0 and higher.

## [0.1.3.10] - 2021-08-03
### Added
- Create OIDC config with provided tls manager.

## [0.1.3.9] - 2021-07-15
### Changed
- Pass access token to handlers.

## [0.1.3.8] - 2021-06-22
### Added
- Possibility to accept Service Token in `OIDCAuth`. Token is considered a Service Token if it
  lacks the `object_guid` claim. `preferred_username` claim is used as user id instead.

## [0.1.3.7] - 2021-06-10
### Added
- Add a way to describe fields in Swagger schemas.

## [0.1.3.6] - 2021-06-09
### Changed
- Add default expiration time for OIDC discovery document and JWKS when provider does not set
  expiration.

## [0.1.3.5] - 2021-06-08
### Changed
- CBDINFRA-318: added OpenID Connect authorization support for servant;
- CBDINFRA-318: added role based authentication on top of OIDC.

## [0.1.3.4] - 2021-04-18
### Changed
- Move to `servant-swagger-ui-0.3.5.3.47.1`.

## [0.1.3.3] - 2020-12-12
### Changed
- Log user id for requests where it's set.

## [0.1.3.2] - 2020-11-19
### Fixed
- Flush `stdout` after writing logs.

## [0.1.3.1] - 2020-11-19
### Changed
- Rewrite logging middleware: do not force reading the whole request before passing it to the
  application.

## [0.1.3.0] - 2020-10-14
### Added
- `servant` support.

## [0.1.2.3] - 2020-08-17
### Added
- `defaultHandleLog400` middleware to log response bodies of 4xx and 5xx responses.

## [0.1.2.2] - 2020-05-19
### Changed
- Ignore `"Warp: Client closed connection prematurely"` exception.

## [0.1.2.1] - 2020-04-15
### Added
- `runWebServerWith` function that runs a web server with custom Warp settings.

## [0.1.2.0] - 2020-04-09
### Changed
- `warp` is configured to print exceptions to `stdout` in json format;
- `restartOnError` does not restart on `Ctrl-C`;
- `defaultHandleLog` prints `status` and `url` in separate json fields.

## [0.1.1.9] - 2020-03-31
### Fixed
- Compilation with `--pedantic`.
### Added
- GitLab CI.

## [0.1.1.8] - 2019-09-20
### Added
- Middleware which adds CORS header to every response.

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
