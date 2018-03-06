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
