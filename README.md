This is utility for Unix-like systems. Written under Ubuntu 16.04.

## Usage:

First clone this directory to project directory and ensure `CHANGELOG.md` and maybe `API_CHANGELOG.md` are in project root.

To build and install `stack setup; stack build choo-choo; stack install choo-choo` in `choo-choo/`.

Set up configuration file `./paths`.

Bump versions in packages given with option (`-p`, `--packages`) or in `./paths`.

It can check changelogs from last release or from start of project (`-e` `--from-bc`).

It will check changelog correctly if only all it's entries mention their PR or commits. And changelog will be considered as outdated if any of them is missed.
You still can bump versions if you decide to. Use the `--force` (or `-f`).

You can supress changelog checking with `-c` or `--no-check`. If you use it, you must declare version modifiers (`app`, `major`, `minor`, `fix`, `doc`) while bump.

And it will infer version from changelog if only changes are marked with modifiers "Major changes", "Minor changes", "Fixes" and "Docs".

`API_CHANGELOG.md` will be checked only if swagger file (or any other file : file changed <=> API changed) with version variable presents in `./paths`

Same if you want to bump API version.

For `.json` files in `swaggerFileName` field of `./paths` existence of string `"<var>": "{{ version }}"` is assumed.

For `.hs` files - `<var> = "<version>"`.

It can bump project version not only in `.cabal` files if run without `-p` option. Supports `.hs` and `.json`.

And then every time to run it call `stack exec choo-choo-exe [-- options]` from root of repository.

# Options:
```
Usage: choo-choo-exe [-p|--packages PACKAGES] [-l|--level LEVEL] [-a|--api API]
                     [-c|--no-check] [-e|--from-bc] [-f|--force]

Available options:
  -h,--help                Show this help text
  -p,--packages PACKAGES   List of packages to bump.
  -l,--level LEVEL         Level of changes.
  -a,--api API             Level of changes in API.
  -c,--no-check            Do not check changelogs.
  -e,--from-bc             Check changelogs from start of project.
  -f,--force               Bump version even if changelogs are outdated. Cannot
                           be mixed with -c.
```
