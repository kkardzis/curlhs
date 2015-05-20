# Change Log (curlhs)


## [Release 0.1.6][0.1.6] 2015-05-20

### Added
- Support for `CURLOPT_HEADERFUNCTION` added ([Issue #5]).
- Change log added.

### Changed
- Requirement to explicit use of `loadlib`/`freelib` removed.
The process of loading and initializing *libcurl* is automatically
performed when *curlhs* is loaded/linked, and the process of cleanup
and unloading is performed when *curlhs* is unloaded/unlinked.
Indirectly inspired by the discussion on [Issue #3].

- Tests, examples and documentation updated accordingly.

### Deprecated
- Module `Network.CURL000` and its contents (`loadlib`/`freelib` etc.)
will be removed in the future versions.

### Fixed
- Fixed bug which caused segfaults when more than one *callback type*
or *slist type* option was used on the same handle.



## [Release 0.1.5][0.1.5] 2015-04-27

### Fixed
- Tests updated for newer `hspec` package version ([Issue #4]).



## [Release 0.1.4][0.1.4] 2015-04-23

### Fixed
- Building errors on OS X platform fixed ([Issue #2]).
- Large negative literals warning fixed.
- GHC 7.10 warnings fixed.



## [Release 0.1.3][0.1.3] 2014-10-03

### Fixed
- Documentation links fixed.



## [Release 0.1.2][0.1.2] 2014-10-02

### Added
- Documentation browser added (simple markdown viewer).
- Short tutorial and few simple examples added.
- Basic test suite added.

### Changed
- `CURLOPT_SSL_VERIFYHOST` option type changed to `Bool`.



## [Release 0.1.1][0.1.1] 2014-08-14

### Fixed
- Version boundaries for `base` package added.



## [Release 0.1.0][0.1.0] 2014-08-14

- Almost complete rewrite. All before is a dark history.




[HEAD]:  https://github.com/kkardzis/curlhs/compare/curlhs-0.1.6...HEAD
[0.1.6]: https://github.com/kkardzis/curlhs/compare/curlhs-0.1.5...curlhs-0.1.6
[0.1.5]: https://github.com/kkardzis/curlhs/compare/curlhs-0.1.4...curlhs-0.1.5
[0.1.4]: https://github.com/kkardzis/curlhs/compare/curlhs-0.1.3...curlhs-0.1.4
[0.1.3]: https://github.com/kkardzis/curlhs/compare/curlhs-0.1.2...curlhs-0.1.3
[0.1.2]: https://github.com/kkardzis/curlhs/compare/curlhs-0.1.1...curlhs-0.1.2
[0.1.1]: https://github.com/kkardzis/curlhs/compare/curlhs-0.1.0...curlhs-0.1.1
[0.1.0]: https://github.com/kkardzis/curlhs/compare/curlhs-0.0.2...curlhs-0.1.0

[Issue #2]: https://github.com/kkardzis/curlhs/issues/2
[Issue #3]: https://github.com/kkardzis/curlhs/issues/3
[Issue #4]: https://github.com/kkardzis/curlhs/issues/4
[Issue #5]: https://github.com/kkardzis/curlhs/issues/5

