0.3.0
---

* Major changes
  - Redesign changelog affecting part (see [#106]( https://github.com/GetShopTV/changelogged/pull/106 ));
    - Move ignore_commits to config for each changelog (see [`7dfb6f7`]( https://github.com/GetShopTV/changelogged/commit/7dfb6f7 ));
    - Implement ignored commits (closes #81) and refactor checkLocalChangelogF (see [`de7bbbd`]( https://github.com/GetShopTV/changelogged/commit/de7bbbd ));
    - Update README (see [`c519e90`]( https://github.com/GetShopTV/changelogged/commit/c519e90 ));
    - Ignore .changelogged.yaml in changelogged.yaml (see [`a87c565`]( https://github.com/GetShopTV/changelogged/commit/a87c565 ));
    - Fix bug with multiple watch and ignored files, use cleaner git commands (see [`1c0d594`]( https://github.com/GetShopTV/changelogged/commit/1c0d594 ));
    - Ignore changes to ChangeLog itself by default (only own, no mention in config. Closes #77) (see [`fb861db`]( https://github.com/GetShopTV/changelogged/commit/fb861db ));
    - Ignore all given changelogs by default (see [`2fed677`]( https://github.com/GetShopTV/changelogged/commit/2fed677 ));
    - Refactor config loading (see [`1f03cdc`]( https://github.com/GetShopTV/changelogged/commit/1f03cdc ));
    - Split defaultMain to main and consistent changelog checking part (see [`1fb5141`]( https://github.com/GetShopTV/changelogged/commit/1fb5141 ));
    - Refactor changelog affecting functions (see [`cf31c84`]( https://github.com/GetShopTV/changelogged/commit/cf31c84 ));
    - Restructure functional parts (see [`f7ef415`]( https://github.com/GetShopTV/changelogged/commit/f7ef415 ));
    - Make bump-versions and update-changelog proper commands (see [`bede39c`]( https://github.com/GetShopTV/changelogged/commit/bede39c ));
    - Add success pretty printer (see [`9addf91`]( https://github.com/GetShopTV/changelogged/commit/9addf91 ));
  - Enhancements (see [#89]( https://github.com/GetShopTV/changelogged/pull/89 ));
    - Add target changelog as command line argument (see [`1ed3345`]( https://github.com/GetShopTV/changelogged/commit/1ed3345 ));
    - Redesign --level option (see [`3f25201`]( https://github.com/GetShopTV/changelogged/commit/3f25201 ));
    - Add --version option (see [`0e828fb`]( https://github.com/GetShopTV/changelogged/commit/0e828fb ));
    - Add config (see [`b000fb2`]( https://github.com/GetShopTV/changelogged/commit/b000fb2 ));
    - Remove dead code (see [`85e067b`]( https://github.com/GetShopTV/changelogged/commit/85e067b ));
    - Add option for config file location (closes #75) (see [`569ce6e`]( https://github.com/GetShopTV/changelogged/commit/569ce6e ));
  - Move computations from IO and enhance correspondingly (see [#93]( https://github.com/GetShopTV/changelogged/pull/93 ));
    - Add Appl type to lift computations to (see [`095fc88`]( https://github.com/GetShopTV/changelogged/commit/095fc88 ));
    - Move computations to Appl context (closes #92) (see [`518f82e`]( https://github.com/GetShopTV/changelogged/commit/518f82e ));
    - Make terminal colors optional (closes #63) (see [`c846c86`]( https://github.com/GetShopTV/changelogged/commit/c846c86 ));
    - Reexport common functions to work with Reader monad from Options (see [`41df17d`]( https://github.com/GetShopTV/changelogged/commit/41df17d ));
    - Add dry-run option (closes #72) (see [`6bed18e`]( https://github.com/GetShopTV/changelogged/commit/6bed18e ));
  - Enhancements (see [#101]( https://github.com/GetShopTV/changelogged/pull/101 ));
    - Remove default changelogs (closes #96) (see [`bbae67d`]( https://github.com/GetShopTV/changelogged/commit/bbae67d ));
    - Derive FromJSON instances and add level headers to config (see [`71ea07a`]( https://github.com/GetShopTV/changelogged/commit/71ea07a ));
    - Use custom level headers (see [`357253a`]( https://github.com/GetShopTV/changelogged/commit/357253a ));
    - Derive FromJSON with TH to transform field names (see [`17cc325`]( https://github.com/GetShopTV/changelogged/commit/17cc325 ));
    - Fix and refactor function to infer version from changelog (see [`3bb1036`]( https://github.com/GetShopTV/changelogged/commit/3bb1036 ));
    - Use Enum instance for Level in getLevelOfChanges function (see [`e655ead`]( https://github.com/GetShopTV/changelogged/commit/e655ead ));
    - Factor out json field modifier to Pure (see [`611f10c`]( https://github.com/GetShopTV/changelogged/commit/611f10c ));
    - Split version_pattern, derive ToJSON instances (see [`fea956e`]( https://github.com/GetShopTV/changelogged/commit/fea956e ));
    - Do not require --format=suggest for writing to changelog and refactor a bit (see [`dbc381b`]( https://github.com/GetShopTV/changelogged/commit/dbc381b ));
    - Remove checkChangelogWrap (see [`97de3e3`]( https://github.com/GetShopTV/changelogged/commit/97de3e3 ));
* Minor changes
  - Update to LTS-11.7 (see [`43193cc`]( https://github.com/GetShopTV/changelogged/commit/43193cc ));
  - Add debug info and --verbose flag (see [#97]( https://github.com/GetShopTV/changelogged/pull/97 ));
* Fixes
  - Remove accidental file and surround links with spaces in output (see [`da006e1`]( https://github.com/GetShopTV/changelogged/commit/da006e1 ));
  - Remove images dir from package.yaml (see [`4af996e`]( https://github.com/GetShopTV/changelogged/commit/4af996e ));
  - Fix adding spaces around links (see [`180e73f`]( https://github.com/GetShopTV/changelogged/commit/180e73f ));
* Docs
  - Update demo (see [`b103e6c`]( https://github.com/GetShopTV/changelogged/commit/b103e6c ));
  - Remove outdated note from README and get back images dir (see [`2c7a03c`]( https://github.com/GetShopTV/changelogged/commit/2c7a03c ));
  - Update docs for v0.2.0 (see [#84]( https://github.com/GetShopTV/changelogged/pull/84 ));
  - Update README.md (see [`f59ee55`]( https://github.com/GetShopTV/changelogged/commit/f59ee55 ));
  - Remove Guiding examples from README. Reason - outdated images are worse than no images (see [`503ecfa`]( https://github.com/GetShopTV/changelogged/commit/503ecfa ));
0.2.0
---

* Major changes
  - Redesign configuration format (see [#76](https://github.com/GetShopTV/changelogged/pull/76));
    - universal changelog configuration;
    - separate version files from watched files (those can be different);
    - ignored files;
    - add optional version `branch` config (for Git Flow projects where development and feature branches might not have version tags associated with them);
    - pretty printing config and git info summary on start;
    - fix PR and commit links for projects with `git@github.com:` remote url;
  - Improve CLI (see [#64](https://github.com/GetShopTV/changelogged/pull/64));

* Minor changes
  - Enable Travis CI (see [#65](https://github.com/GetShopTV/changelogged/pull/65));
  - Move code from Main to Changelogged.Main (see [`1194b20`](https://github.com/GetShopTV/changelogged/commit/1194b20));
  - Ignore tag at `HEAD` (see [`35cae7c`](https://github.com/GetShopTV/changelogged/commit/35cae7c));

* Fixes
  - Remove orphan instances (see [`c9ffce3`](https://github.com/GetShopTV/changelogged/commit/c9ffce3));
  - Add images to `extra-source-files` and improve description (see [`b4a2efb`](https://github.com/GetShopTV/changelogged/commit/b4a2efb));
  - Fix image paths in README (see [`6028f57`](https://github.com/GetShopTV/changelogged/commit/6028f57));
  - Track changes for `HEAD` instead of origin/master (see [`0f5c839`](https://github.com/GetShopTV/changelogged/commit/0f5c839));
  - Use `--abbrev=0` to get clear tag name (see [`4f50f22`](https://github.com/GetShopTV/changelogged/commit/4f50f22));
  - Fix version bump to just use `version_pattern` (see [`976633f`](https://github.com/GetShopTV/changelogged/commit/976633f));
