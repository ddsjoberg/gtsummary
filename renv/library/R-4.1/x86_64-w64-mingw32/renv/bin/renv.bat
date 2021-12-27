set "RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE"
R --no-save --no-restore -s -e "renv:::renv_cli_exec()" --args %*
