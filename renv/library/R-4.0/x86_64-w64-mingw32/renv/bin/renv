#!/usr/bin/env sh

if [ -z "${RENV_CONFIG_SYNCHRONIZED_CHECK}" ]; then
  RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
  export RENV_CONFIG_SYNCHRONIZED_CHECK
fi

R --no-save --no-restore -s -e "renv:::renv_cli_exec()" --args "$@"
