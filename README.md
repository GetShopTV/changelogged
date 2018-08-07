# changelogged

[![Hackage](https://img.shields.io/hackage/v/changelogged.svg)](http://hackage.haskell.org/package/changelogged)
[![Build Status](https://travis-ci.org/GetShopTV/changelogged.svg?branch=master)](https://travis-ci.org/GetShopTV/changelogged)

`changelogged` is a changelog manager for Git projects.

![`changelogged` demo.](images/changelogged-v0.2.0-demo.gif)

## Quick start

You're encouraged to use `changelogged` when preparing a new release of your project.

For most projects you can start by simply running `changelogged` with no options or configuration files:

```
changelogged
```

That should print out some inferred information about your project
and also list any recent changes that are not included in you changelog.

You can then prepend missing changelog entries automatically with

```
changelogged update-changelog
```

Now you can see new entries in your changelog, make edits and group changes.
Even if you see simple messages on a screen, detailed (with links, see demo) are written to changelog.
When you're done you can automatically bump project's version with

```
changelogged bump-versions
```

That's it! Now you have a proper changelog with no forgotten changes.

### A note for Git Flow projects

With Git Flow your development and feature branches
will likely not have version tags associated with them.
This can confuse `changelogged` and result in more suggestions than needed.

To avoid this situation add [`branch: master` line](https://github.com/GetShopTV/changelogged/blob/master/.changelogged.template.yaml#L37-L41)
in your `.changelogged.yaml`.

## Configuration file

For any non-trivial project you're likely to want to configure the way `changelogged`
treats changes in that project.
Normally configuration file is located at `.changelogged.yaml` (note the leading dot).

See [.changelogged.template.yaml](.changelogged.template.yaml)
for a template configuration file with description of all fields.

## Feature reference

### Help message

```
changelogged --help
```

```
changelogged - Changelog Manager for Git Projects

Usage: changelogged [ACTION] [--format FORMAT] [--dry-run] [TARGET_CHANGELOG]
                    [--config changelogged.yaml config file location]

Available options:
  -h,--help                Show this help text
  ACTION                   If present could be update-changelog or
                           bump-versions.
  --format FORMAT          Format for missing changelog entry warnings. FORMAT
                           can be 'simple' or 'suggest'. (default: simple)
  --level CHANGE_LEVEL     Level of changes (to override one inferred from
                           changelog). CHANGE_LEVEL can be 'app', 'major',
                           'minor', 'fix' or 'doc'.
  --from-bc                Look for missing changelog entries from the start of
                           the project.
  --force                  Bump versions ignoring possibly outdated changelogs.
                           Usable with bump-versions only
  --no-check               Do not check if changelogs have any missing entries.
  --no-colors              Print all messages in standard terminal color.
  --dry-run                Do not change files while running.
  TARGET_CHANGELOG         Path to target changelog.
  --config changelogged.yaml config file location
                           Path to config file.
  --verbose                Turn verbose mode on (useful for developers).
  --version                Print version.
```

### Checking changelogs

This is default feature. Changelogged will output all missing pull requests and commits with their messages.

You can skip it with `--no-check` option or ignore results while bumping with `--force` option. Also you can check changelog from the first commit with `--from-bc`.

### Bumping versions

If changelogs are up to date changelogged will bump versions all over the project with command `bump-versions`.

You can variously combine changelog checking and bumping versions. For example you may just want to be sure changelogs are up to date. You can just run `changelogged`
By default new version is inferred from changelog.
Default versioning: `app.major.minor.fix.doc`.
If you use another versioning system or headers in changelog you can change `level_headers` in `.changelogged.yaml`

You can specify new version explicitly with `--level` option. It's also the only way to bump version with `--no-check` option.

### Multiple changelogs and subversions

If you have a lot of changelog entries in `changelogged.yaml` you get it. There is no option to set explicit subversion for any changelog. `--api-level` is already deprecated.
Also there will be option to check changelog passed by name in commang line args if it presents in config.

### Writing changelogs.

`--format=suggest` provides another format for records you see on the screen.
`update-changelog` command will write these records to changelog. With any `format` option new entries will match `--format=suggest`.
It's recommended to edit it manually after.

## Development

### Requirements

It works with Git projects only.
It was never tested on Windows. Ideally it will work if you have Git Bash installed.

### Getting and building

#### Installing from Hackage

You can build a version from Hackage using Stack or cabal-install:

```
stack install changelogged
```

```
cabal install changelogged
```

#### Using latest code from GitHub

Clone this repository:

```
git clone https://github.com/GetShopTV/changelogged.git
```

`cd` into cloned repository and build with Stack:

```
stack build
```

You can now run `changelogged` via Stack with

```
stack exec changelogged
```

or you can install it with

```
stack install
```

## Contributing

Bug reports and feature requests are welcome on
[GitHub](https://github.com/GetShopTV/changelogged/issues)

Pull requests are welcome!

_GetShop.TV Team_
