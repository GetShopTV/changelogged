# changelogged

[![Hackage](https://img.shields.io/hackage/v/changelogged.svg)](http://hackage.haskell.org/package/changelogged)
[![Build Status](https://travis-ci.org/GetShopTV/changelogged.svg?branch=master)](https://travis-ci.org/GetShopTV/changelogged)

`changelogged` is a changelog manager for Git projects.

![`changelogged` demo.](images/demo_new.gif)

## Quick start

You're encouraged to use `changelogged` when preparing a new release of your project.

If you're using bash you probably want to enable autocompletion. You can do it with the following or equivalent:
```
changelogged --bash-completion-script changelogged >> ~/.bash_completion
```
In new terminal sessions you will have it.

For most projects you can simply run `changelogged` with no options or configuration files:

```
changelogged
```

After you refuse to do to interactive mode it will add missing entries to your changelogs and open editor for each if them.
After you can bummp versions over whole project (usable if you have more than one version file).

That's it! Now you have a proper changelog with no forgotten changes.

### A note for Git Flow projects

With Git Flow your development and feature branches
will likely not have version tags associated with them.
This can confuse `changelogged` and result in more suggestions than needed.

To avoid this situation add [`branch: master` line](https://github.com/GetShopTV/changelogged/blob/master/.changelogged.template.yaml#L37-L41)
in your `.changelogged.yaml`.

## Versioning

Let A.B.C.D.E be version number.

They are named in a prompt correspondingly `app`, `major`, `minor`, `fix` and `doc`

A.B.C versions are bumped through zero. If you have version A.B.9 and bump B next version will be A.B+1.0.
A.B version will never be reached since C was introduced once.

non-PVP versions are bumped through one. A.B.C.9 -> A.B.C+1 -> A.B.C+1.1

## Configuration file

For any non-trivial project you're likely to want to configure the way `changelogged`
treats changes in that project.
Normally configuration file is located at `.changelogged.yaml` in the root of the project. (note the leading dot).
You can define your own location with option `--config`.

See [.changelogged.template.yaml](.changelogged.template.yaml)
for a template configuration file with description of all fields.
All paths inside can be relative against project root directory.

## Interactive mode

![`changelogged` demo.](images/demo_interactive.gif)

If you want a lot more customized changelog try to go to interactive mode.

It allows to walk through history and do something with each entry.
Options:
 * `(w)rite` - write entry to changelog and go to next. You can omit `w` and do it by pressing enter.
 * `(r)emind` - show commit in pager.
 * `(s)kip` - skip entry and go to next.
 * `(e)xpand` - write merge commit entry, expand to subchanges and walk through them.
 * `(i)gnore` - ignore commit. It will be written to config and will never be suggested as missing.
 * `(a)ll` - add remaining entries to changelog. Raised inside merge commit it will add rest of subchanges.
 * `(q)uit` - skip remaining entries. Inside merge commit it will quit it only.

### Help message

```
changelogged --help
```

```
Changelog Manager for Git Projects

Usage: changelogged [--dry-run] [TARGET_CHANGELOG]
                    [--config changelogged.yaml config file location]
  Changelogged

Available options:
  -h,--help                Show this help text
  --list-misses            List missing entries, don't modify changelogs.
  --from-version CHECK_FROM_TAG
                           Tag or commit from which to check changelogs.
  --from-beginning         Check all changelogs from start of the project.
  --no-colors              Print all messages in standard terminal color.
  --dry-run                Do not change files while running.
  TARGET_CHANGELOG         Path to target changelog.
  --config changelogged.yaml config file location
                           Path to config file.
  --verbose                Turn verbose mode on (useful for developers).
  --version                Print version.

```

## Getting and building

### Requirements

It works with Git projects only.

Current version supports only GitHub and UNIX like OS.
We are working on it.

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

Code of conduct and contributing rules are coming soon.

Pull requests are welcome!

_GetShop.TV Team_
