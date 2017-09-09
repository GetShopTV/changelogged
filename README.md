This is executable utility for Unix-like systems. Written under Ubuntu 16.04.
Updates version in `.cabal` files.

## Usage:

First clone this directory to project directory and ensure `CHANGELOG.md` and maybe `API_CHANGELOG.md` to be in project root.
And `./run.sh`

Then text of autogenerated help with editions:

```
This script can check your changelogs and bump versions in project.
It's rigorous. Check will fail if any commit or pull request from git history is missed.
It assumes to be run in root directory of project and that changelog is here.
And `API_CHANGELOG.md` will be checked only if it's swagger file (or any other indicating changes file) presents in `paths` config.
Project must be git repository.
You can specify these levels of changes: app, major, minor, fix, doc.
It can infer version from changelog.
But it will refuse to do it if it's not sure changelogs are up to date.

Usage: bump_versions.hs [-p|--packages PACKAGES] [-l|--level LEVEL]
                        [-c|--no-check] [-e|--from-bc] [-f|--force]

Available options:
  -h,--help                Show this help text
  -p,--packages PACKAGES   List of packages to bump.
  -l,--level LEVEL         Level of changes.
  -c,--no-check            Do not check changelogs.
  -e,--from-bc             Check changelogs from start of project.
  -f,--force               Bump version even if changelogs are outdated. Cannot
                           be mixed with -c.
```
