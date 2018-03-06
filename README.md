# changelogged

[![Hackage](https://img.shields.io/hackage/v/changelogged.svg)](http://hackage.haskell.org/package/changelogged)
[![Build Status](https://travis-ci.org/GetShopTV/changelogged.svg?branch=master)](https://travis-ci.org/GetShopTV/changelogged)

## What and how to?

### Quick start
```
changelogged

changelogged --format suggest

changelogged --update-changelog
git diff ChangeLog.md
sed -i '1i * Minor changes:'

changelogged --bump-versions
```

### Purpose
This is the tool for tracking project history and release documentation.
It guides developer through writing changelogs are bump version in project files.
With proper usage it guarantees that every pull request or commit from git history is mentioned in project changelogs and that versions all over the project are up to date.
It can also write changelogs based on git history and infer new version from changelog if it contains rubrication by level of changes.

### Configuration file
[example](.changelogged.template.yaml)

Config is optional.
With no config tool will try to check file named `ChangeLog.md` as far as it's part of Stack project template and bump versions in `package.yaml` file in root.

Config (named `changelogged.yaml`) is split by changelogs. It's well commented in example. No files are ignored or watched by default.

### Features reference

#### Help message

```
changelogged --help
```

```
changelogged - Changelog Manager for Git Projects

Usage: changelogged [--format FORMAT] [--update-changelog] [--bump-versions]

Available options:
  -h,--help                Show this help text
  --format FORMAT          Missing entries report format. FORMAT can be 'simple'
                           or 'suggest'. (default: simple)
  --update-changelog       Prepend missing entries to changelogs. Available with
                           --format=suggest.
  --bump-versions          Bump versions according to change level.
  --level CHANGE_LEVEL     Level of changes (for packages). CHANGE_LEVEL can be
                           'app', 'major', 'minor', 'fix' or 'doc'.
  --api-level CHANGE_LEVEL Level of changes (for API). CHANGE_LEVEL can be
                           'app', 'major', 'minor', 'fix' or 'doc'.
  --from-bc                Check changelogs for the entire history of the
                           project.
  --force                  Bump versions even when changelogs are outdated.
  --no-check               Do not check changelogs.
```

See examples [below](#guiding-examples)

#### Checking changelogs
This is default feature. Changelogged will output all missing pull requests and commits with their messages.

You can skip it with `--no-check` option or ignore results with `--force` option. Also you can check changelog from the first commit with `--from-bc`.

#### Bumping versions
If changelogs are up to day changelogged will bump versions all over the project with option `--bump-versions`.

You can variously combine changelog checking and bumping versions. For example you may just want to be sure changelogs are up to date. You can just run `changelogged`
By default new version is inferred from changelog. It will work if you have some of `* App...` `* Major...`, `* Minor...`, `* Fix...`, and `* Doc...` sections in changelog and name versions correspondingly.
Suggested versioning: `app.major.minor.fix.doc`.
Otherwise you can specify new version explicitly with `--level` option. It's also the only way to work with `--no-check` option.
This option is subject to change significantly and is broken now.

#### Multiple changelogs and subversions
If you have a lot of changelog entries in `changelogged.yaml` you get it. There is no option to set explicit subversion for any changelog. `--api-level` is already deprecated.
Also there will be option to check changelog passed by name in commang line args if it presents in config.

#### Writing changelogs.
`--format suggest` provides another format for records you see on the screen.
It can be used with `--update-changelog` option to write these strings to the top of changelog they are relevant to.
It's recommended to edit it manually after.
This option cannot be used with `--format simple` which is default.

### Guiding examples:

#### Common run:
```
changelogged (master):$ changelogged
```
![image1](images/common_run.png)

#### Suggest changelog entries:
```
changelogged (master):$ changelogged --format suggest
```
![image2](images/suggest.png)

Try to bump with no entries in changelog:
```
changelogged (master):$ changelogged --format suggest --bump-versions
```
![image3](images/failed_bump.png)

Force with no entries in changelog:
```
changelogged (master):$ changelogged --format suggest --bump-versions --force
```
![image3](images/no_force.png)

#### Write suggested entries to changelog (works only with `--format suggest`)
```
changelogged (master):$ changelogged --format suggest --update-changelog
```
![image5](images/suggest.png)
```
changelogged (master):$ git diff ChangeLog.md
```
![image6](images/chlog_diff.png)
It requires some manual editing after.

#### Bump version infering it from changelog:
```
changelogged (master):$ changelogged --format suggest --bump-versions
```
![image7](images/bump.png)
```
changelogged (master):$ git diff ChangeLog.md
```
![image8](images/release.png)

Try to bump without checking changelogs. Seems that `--force` option is always preferrable. But it waits for use cases.
```
chagelogged --no-check
```
![image9](images/no_check.png)

### Typical daily workflow to keep project and API changelogs up to date (assuming existing changelogged.yaml):
See missing entries:
```
changelogged --format suggest
```
Record these changes.
```
changelogged --format suggest --update-changelog
```
And then edit changelogs manually.

### Suggested simple workflow on release (for project with no `changelogged.yaml` and API changelog):
See missing entries:
```
changelogged
```
Record these changes:
```
changelogged --format suggest --update-changelog
```
Manually edit changelog

Bump versions:
```
changelogged --bump-versions
```
_Next part is subject to change:_

Commit files with bumped versions.

Record commit with version bumps:
```
changelogged --format suggest --update-changelog
```
Edit changelog - move new entry under version milestone.

Commit changelog, push and release.

## Setting up

### Requirements
It works with git projects only.
It was never tested on Windows. Ideally it will work if you have Git Bash installed.
As tool was designed for Haskell ecosystem first there are these extensions supported: `.hs`, `.cabal`, `.yaml` and `.json`. You cannot bump version in any other file.
But new extensions can be easily provided as far as changelogged aims to have the most wide users community. See [Add new extension](#add-new-extension) section.
It's subject to change - there will not be extension specific code in next version.

### Getting and building
First of all you should go to project [Github](https://github.com/GetShopTV/changelogged)

Then you can simply download it from release. After execute something like `cp bin/changelogged ~/.local/bin`.

Or you can clone repo and build it from source. You need latest [Stack](https://docs.haskellstack.org/en/stable/README/) installed.
To build run
```
stack install
```

## Contributing

### Common
Bug reports and feature requests are welcome on [Github](https://github.com/GetShopTV/changelogged/issues)

You are free to fork project and make a pull request.

### Add new extension
You may want to use this tool inside project written not in Haskell. You are very welcome.

Version bumping is restricted with extension of file. There is no euristic rules like "version is placed in `version = `" to have determined behaviour now.
You may request for extension you want to be supported in [special issue](https://github.com/GetShopTV/changelogged/issues/35). It will be here ASAP.
Or you may write it yourself.
You are welcome to ask about how in comments in that issue (or browse comments).
