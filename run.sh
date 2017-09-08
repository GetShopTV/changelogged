#!/bin/bash

# Runner of script with prerequisites.

stack install tuple turtle
./bump_versions.hs "$@"
