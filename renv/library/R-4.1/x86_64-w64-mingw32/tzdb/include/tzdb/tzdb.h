#ifndef TZDB_TZDB_H
#define TZDB_TZDB_H

#include <tzdb/date.h>
#include <tzdb/tz.h>

#include <R_ext/Rdynload.h>

#include <string>
#include <stdbool.h>

// -----------------------------------------------------------------------------

namespace tzdb {

static
inline
bool
locate_zone(const std::string& name, const date::time_zone*& p_time_zone) {
  typedef bool fn_t(const std::string&, const date::time_zone*&);
  static fn_t *fn = (fn_t*) R_GetCCallable("tzdb", "api_locate_zone");
  return fn(name, p_time_zone);
}

static
inline
bool
get_local_info(const date::local_seconds& tp,
               const date::time_zone* p_time_zone,
               date::local_info& info) {
  typedef bool fn_t(const date::local_seconds&, const date::time_zone*, date::local_info&);
  static fn_t *fn = (fn_t*) R_GetCCallable("tzdb", "api_get_local_info");
  return fn(tp, p_time_zone, info);
}

static
inline
bool
get_sys_info(const date::sys_seconds& tp,
             const date::time_zone* p_time_zone,
             date::sys_info& info) {
  typedef bool fn_t(const date::sys_seconds&, const date::time_zone*, date::sys_info&);
  static fn_t *fn = (fn_t*) R_GetCCallable("tzdb", "api_get_sys_info");
  return fn(tp, p_time_zone, info);
}

} // namespace tzdb

// -----------------------------------------------------------------------------

#endif
