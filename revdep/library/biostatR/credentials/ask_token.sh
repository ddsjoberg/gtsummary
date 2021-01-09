#!/bin/sh
# Prompt MUST end with : to support askpass_mac!!
exec "$GIT_ASKTOKEN" "Please enter your ${GIT_ASKTOKEN_NAME:-TOKEN}:"
