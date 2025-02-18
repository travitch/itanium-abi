## 0.1.3 (2025-02-17)

### Improvements

- Allow building with GHC-9.8

## 0.1.2 (2023-03-23)

### Features / Improvements

- Allow building with text-2.0 (and thus GHC-9.4)
- Decode names referencing integer literal template parameters
- Decode names of static function definitions
- Improve pretty printing for templated constructors and destructors
- Improve pretty printing for template arguments
- Decode names of const declarations for non-builtin datatypes
- Replace some calls to `error` with structured exceptions

### Fixes

- More strictly parse substitution IDs

## 0.1.1.1

- Bump bounds on boomerang to force a newer version

  Some builds (especially under CI) were picking up older versions of boomerang
  that do not compile with modern versions of GHC
