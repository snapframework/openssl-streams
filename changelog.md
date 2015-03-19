# openssl-streams changelog #

## 1.2.1.0
  - Bump upper bound of the `io-streams` dependency for the bytestring-builder
    migration.

## 1.2.0.0
  - Added `withConnection`, a convenience function for initiating an SSL
    connection to a given `(host, port)` pair. The socket and SSL connection
    are closed after the user handler runs.
  - Moved changelog from cabal file to changelog.md
  - Updated the dependency list to allow latest `io-streams` and `network`.

## 1.1.0.2
Fixed a bug in the `connect` routine uncovered by the recent network upgrade.

## 1.1.0.1
Widened @network@ dependency to include 2.5.

## 1.1.0.0
Updated `openssl-streams` to work with `io-streams` 1.1.0.0.

## 1.0.0.0
First release.
