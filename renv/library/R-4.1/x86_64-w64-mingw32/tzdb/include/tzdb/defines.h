#ifndef TZDB_DEFINES_H
#define TZDB_DEFINES_H

// All clients should use the same set of defines. They can technically set
// them themselves if they require it, but we default to the same ones that
// are in the Makevars. This is important to ensure that `date.h` includes
// tools like `scan_keyword()` (by turning on `ONLY_C_LOCALE`), and `tz.h`
// doesn't include tools for downloading the time zone database.
//
// When we eventually switch to using the binary time zone database, we will
// switch `USE_OS_TZDB` to `1`.

#ifndef AUTO_DOWNLOAD
# define AUTO_DOWNLOAD 0
#endif

#ifndef HAS_REMOTE_API
# define HAS_REMOTE_API 0
#endif

#ifndef ONLY_C_LOCALE
# define ONLY_C_LOCALE 1
#endif

#ifndef USE_OS_TZDB
# define USE_OS_TZDB 0
#endif

#endif
