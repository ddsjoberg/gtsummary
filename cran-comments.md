## Test environments
* Windows 10 (local), 4.0
* Ubuntu 16.04.6 LTS (on github actions), 3.6, 4.0
* Windows Server 2019 (on github actions), 4.0
* macOS (on github actions), 4.0
* win-builder devel

## R CMD check results
Maintainer: 'Daniel D. Sjoberg <danield.sjoberg@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://codecov.io/gh/ddsjoberg/gtsummary?branch=master
    From: README.md
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).

## Additional Comments

The URL flagged as potentially invalid was double-checked and is indeed valid (and has not changed since the last CRAN submission).

Thank you for your time!
