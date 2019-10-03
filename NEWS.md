# r2dii.dataraw 0.0.1.9001

This version makes the responsibility of r2dii.dataraw clearer, and the installation lighter.

### Some functions moved elsewhere

All functions that don't relate to datasets moved to r2dii.utils:

* `degrees()`.
* all functions that handle paths.
* all functions that handle the configuration file.

### Some functions were removed

r2dii.dataraw no longer stores snapshots, it only creates them. To reflect this, some functions were removed:

* `path_snapshot()`: It's now useless, as we no longer store snapshots.
* `set_version()`: We no longer need a complex version number.

### Other changes

* Examples are now documented in the r2dii package.

* New dataset `data_dictionary` (#21, thanks @jhoff-2dii).

* New dataset `loanbook_demo` (#30, thanks @2diiKlaus).

* The option `r2dii.dataraw.config` is now called `r2dii_config`.

* `example_config()` gains `path` to choose a specific configuration file. Called with no argument, it shows available configuration files.

* New `LoanMarket()` (#17).

* New `LoanMarketClimate()` (#16).

* Dataset-functions now fail more gracefully if the requested file doesn't exist.

* Polish installation instructions.

* `BENCH.REGIONS` is now based on data accessed on 2019-08-08 from "C:\Users\Mauro\Dropbox (2° Investing)\2° Investing Team\People\Klaus\GitHub\Reference\ReferenceData\BenchRegions.rda" (@2diiKlaus; https://github.com/2DegreesInvesting/Reference/issues/39).

# r2dii.dataraw 0.0.1.2019-06-21.2019-08-07

* The package name is now r2dii.dataraw.

* The package repo is now https://github.com/2DegreesInvesting/r2dii.dataraw.

* The package goal now reflects the focus on raw or lightly prepared data.

# Reference 0.0.0.9002.2019-06-21.2019-08-01

* README (site's home) now shows only main features. Details are now given in the new _Get started_ section, on the site's navigation bar.

* README (site's home) now shows limitations and possible enhancements to make them obvious and motivate discussion.

* New `take_snapshot()` replaces `snapshot_data()` to use the imperative-verb style.

* The option to set a configuration file globally is now `Reference.config` (replaces `Reference.path_config`). 

* Percent tests coverage is now measured and reported on README.

* QA issues can now be searched using the [2diiQA label](https://github.com/2DegreesInvesting/Reference/search?q=label%3A2diiQA+is%3Aopen&type=Issues).

* `REF.PATH()` and `DATA.PREP.PATH()` are no longer useful and were removed
(commits 67694724, 05ea508c). These functions pointed to a sub-directory of the Reference
repository. In the context of an R package, this is an anti-pattern and is
therefore discouraged. `DATA.PREP.PATH()` was used to `sector.bridge` and `RevenueSplit`. These datasets are now sourced directly from GitHub in data-raw/ and exported with the package.

* New "Changelog" section, on the site's navigation bar, reports news.

