0.3.0
---

* Major changes
  - Move computations from IO and enhance correspondingly (see [#93]( https://github.com/GetShopTV/changelogged/pull/93 ));
  - Enhancements (see [#101]( https://github.com/GetShopTV/changelogged/pull/101 ));
    - Remove default changelogs
    - Add custom level headers
    - Split version_pattern
    - Derive FromJSON instances
  - Redesign changelog affecting part (see [#106]( https://github.com/GetShopTV/changelogged/pull/106 ));
* Minor changes
  - Enhancements (see [#89]( https://github.com/GetShopTV/changelogged/pull/89 ));
    - Add --version
    - Redesign --level option
    - Add option for config file location
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
