
# 2.0.1

* `is_online()` now tries the Apple captive test first, because it works better
  when DNS is not masked, but HTTP is (#13).

# 2.0.0

* New `nsl()` function to perform DNS queries.

* New `my_ip()` function to query the computer's public IP address.

* New `apple_captive_test()` function to check Apple's captive test
  web page to see if the computer is online.

* Better `is_online()` implementation, it uses DNS and HTTPS instead
  of an ICMP ping via an external ping program.

* Now `ip_up()` checks first if the computer is connected to the
  internet, via `is_online()`.

# 1.2.0

* New `is_up()` function to check if a web (other other TCP) server is up.

* Timeout now works correctly on Linux systems (#7).

* `ping()` uses processx now to run the external ping program, so the
  the ping error messages do not litter the R console (#8, #9).

# 1.1.2

No user visible changes.

# 1.1.0

* New `is_online()` function to check if the computer is online, by
  pinging two DNS servers.

* TCP Timeout now works for the connect phase as well.

# 1.0.0

First release on CRAN.
