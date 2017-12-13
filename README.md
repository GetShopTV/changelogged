This it the tool for tracking project history.

It can check if your changelogs are up to date, suggest changes you've missed and bump versions in given files automatically.
It takes all data from 'git'.

It was designed for cabal projects first, so it bumps version in `.cabal` files specially.
You can specify all paths and package names in config named `changelogged.yaml`.

Text of help message:
```
This tool can check your changelogs and bump versions in project.
It assumes to be run in root directory of project and that changelog is here.
You can specify these levels of changes: app, major, minor, fix, doc.
It can infer version from changelog.
But it will refuse to do it if it's not sure changelogs are up to date.

Usage: changelogged [-p|--packages PACKAGES] [-l|--level ARG]
                    [-a|--api-level ARG] [--format ARG] [-W|--with-api]
                    [-c|--no-check] [-C|--no-bump] [-e|--from-bc] [-f|--force]

Available options:
  -h,--help                Show this help text
  -p,--packages PACKAGES   List of packages to bump (separated by space).
  -l,--level ARG           Level of changes (for packages). One of (app major
                           minor fix doc)
  -a,--api-level ARG       Level of changes (for API). One of (app major minor
                           fix doc)
  --format ARG             Warning format. One of (simple
                           suggest) (default: simple)
  -W,--with-api            Assume there is API and work with it also.
  -c,--no-check            Do not check changelogs.
  -C,--no-bump             Do not bump versions. Only check changelogs.
  -e,--from-bc             Check changelogs from start of project.
  -f,--force               Bump version even if changelogs are outdated. Cannot
                           be mixed with -c.
```
