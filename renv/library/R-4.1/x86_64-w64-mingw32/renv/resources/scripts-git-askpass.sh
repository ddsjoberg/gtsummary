#!/usr/bin/env sh

case "$1" in
  Username*) echo "${GIT_USERNAME:-${GIT_USER}}" ;;
  Password*) echo "${GIT_PASSWORD:-${GIT_PASS}}" ;;
esac

