#!/bin/bash
DESCRIPTION=($(git diff --cached --name-only | grep 'DESCRIPTION'))
MSG="use 'git commit --no-verify' to override this check"

if [[ ${#DESCRIPTION[@]} == 0 ]]; then
  exit 0
fi

if [[ DESCRIPTION -nt codemeta.json ]]; then
  echo -e "codemeta.json is out of date; please re-run codemetar::write_codemeta\n$MSG"
  exit 1
fi
