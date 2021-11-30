/* -*- mode: c++ -*- */

#ifndef R_PROGRESS_H
#define R_PROGRESS_H

#include <unistd.h>
#include <sys/time.h>

#ifdef Win32
#  include <io.h>
#endif

#include <string>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <cstring>
#include <cstdlib>
#include <cerrno>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Print.h>

// For gettimeofday implementation on windows
#ifdef Win32

// MSVC defines this in winsock2.h!?
typedef struct timeval {
    long tv_sec;
    long tv_usec;
} timeval;
int gettimeofday(struct timeval * tp, struct timezone * tzp);

#endif

namespace RProgress {

class RProgress {

 public:

   RProgress(std::string format,
       double total,
       int width,
       std::string cursor_char,
       std::string complete_char,
       std::string incomplete_char,
       bool clear,
       double show_after) :

    first(true), format(format), total(total), current(0), count(0),
    width(width), cursor_char(cursor_char), complete_char(complete_char),
    incomplete_char(incomplete_char), clear(clear), show_after(show_after),
    last_draw(""), start(0), toupdate(false), complete(false), reverse(false) {

    supported = is_supported();
    use_stderr = default_stderr();
  }

   RProgress(std::string format = "[:bar] :percent",
       double total = 100,
       int width = Rf_GetOptionWidth() - 2,
       char complete_char = '=',
       char incomplete_char = '-',
       bool clear = true,
       double show_after = 0.2) :

    first(true), format(format), total(total), current(0), count(0),
    width(width), cursor_char(1, complete_char), complete_char(1, complete_char),
    incomplete_char(1, incomplete_char), clear(clear), show_after(show_after),
    last_draw(""), start(0), toupdate(false), complete(false), reverse(false) {

    supported = is_supported();
    use_stderr = default_stderr();
  }


  ~RProgress() { }

  void set_format(std::string format)    { this->format = format;         }
  void set_total(double total)           { this->total = total;           }
  void set_width(int width)              { this->width = width;           }
  void set_cursor_char(const char* cursor_char) {
    this->cursor_char = cursor_char;
  }
  void set_complete_char(const char* complete_char) {
    this->complete_char = complete_char;
  }
  void set_incomplete_char(const char* incomplete_char) {
    this->incomplete_char = incomplete_char;
  }
  void set_clear(bool clear)             { this->clear = clear;           }
  void set_show_after(double show_after) { this->show_after = show_after; }

  void set_reverse(bool reverse)         { this->reverse = reverse;       }

  void tick(double len = 1) {
    // Start the timer
    if (first) { start = time_now(); }

    current += len;
    count++;

    // We only update after show_after secs
    toupdate = toupdate || time_now() - start > show_after;

    if (current >= total) complete = true;

    // Need to render at the beginning and at the end, always
    if (first || toupdate || complete) this->render();

    if (complete) this->terminate();

    first = false;
  }

  void update(double ratio) {
    double goal = ratio * total;
    this->tick(goal - current);
  }

 private:

  bool first;			// Is the next one the first tick?
  bool supported;		// \r supported at all?
  std::string format;		// Format template
  double total;			// Total number of ticks
  double current;		// Current number of ticks
  int count;                    // Total number of calls
  int width;			// Width of progress bar
  bool use_stderr;		// Whether to print to stderr
  std::string cursor_char;		// Character for cursor tick
  std::string complete_char;		// Character for completed ticks
  std::string incomplete_char;		// Character for incomplete ticks
  bool clear;			// Should we clear the line at the end?
  double show_after;		// Delay to show/increase the progress bar
  std::string last_draw;	// Last progress bar drawn

  double start;			// Start time
  bool toupdate;		// Are we updating? (After show_after.)
  bool complete;		// Are we complete?
  bool reverse;  // go from right to left rather than left to right

  void render() {
    if (!supported) return;

    std::string str = format;

    std::stringstream buffer;

    double ratio_now = ratio();

    // percent
    buffer << std::setw(3) << ratio_now * 100 << "%";
    replace_all(str, ":percent", buffer.str());
    buffer.str(""); buffer.clear();

    // elapsed
    double elapsed_secs = time_now() - start;
    std::string elapsed = vague_dt(elapsed_secs);
    replace_all(str, ":elapsed", elapsed);

    // eta
    double percent = round(ratio_now * 100);
    double eta_secs = percent == 100 ? 0 :
      elapsed_secs * (total / current - 1.0);
    std::string eta = std::isinf(eta_secs) ? "?s" : vague_dt(eta_secs);
    replace_all(str, ":eta", eta);

    // rate
    if (elapsed_secs == 0) {
      buffer << "?";
    } else {
      double rate_num = elapsed_secs == 0 ? 0 : current / elapsed_secs;
      buffer << pretty_bytes(rate_num) << "/s";
    }
    replace_all(str, ":rate", buffer.str());
    buffer.str(""); buffer.clear();

    // current
    buffer << round(current);
    replace_all(str, ":current", buffer.str());
    buffer.str(""); buffer.clear();

    // total
    buffer << round(total);
    replace_all(str, ":total", buffer.str());
    buffer.str(""); buffer.clear();

    // bytes
    replace_all(str, ":bytes", pretty_bytes(current));

    // spin
    replace_all(str, ":spin", spin_symbol());

    // bar
    std::string str_no_bar = str;
    replace_all(str_no_bar, ":bar", "");
    long int bar_width = width - str_no_bar.length();
    if (bar_width < 0) bar_width = 0;

    double complete_len = round(bar_width * ratio_now);
    std::string bar;

    if (reverse) {
      for (long int i = (long int) complete_len; i < bar_width; i++) {
        bar += incomplete_char;
      }
      if (complete_len > 0) {
        bar += cursor_char;
      }
      for (int i = 0; i < (complete_len - 1); i++) { bar += complete_char; }
    } else {
      for (int i = 0; i < (complete_len - 1); i++) { bar += complete_char; }
      if (complete_len > 0) {
        bar += cursor_char;
      }
      for (long int i = (long int) complete_len; i < bar_width; i++) {
        bar += incomplete_char;
      }
    }
    replace_all(str, ":bar", bar);

    if (last_draw != str) {
      if (last_draw.length() > str.length()) { clear_line(use_stderr, width); }
      cursor_to_start(use_stderr);
      if (use_stderr) {
	REprintf(str.c_str());
      } else {
	Rprintf(str.c_str());
      }
      last_draw = str;
    }
  }

  void terminate() {
    if (! supported) return;
    if (clear) {
      clear_line(use_stderr, width);
      cursor_to_start(use_stderr);
    } else {
      if (use_stderr) {
	REprintf("\n");
      } else {
	Rprintf("\n");
      }
    }
  }

  double ratio() {
    double ratio = current / total;
    if (ratio < 0) ratio = 0;
    if (ratio > 1) ratio = 1;
    return ratio;
  }

  std::string spin_symbol() {
    const char symbols[4] = {'-', '\\', '|', '/'};
    return std::string(1, symbols[(count - 1) % 4]);
  }

  void clear_line(bool use_stderr, int width) {

    char *spaces = (char*) calloc(width + 2, sizeof(char));
    if (!spaces) Rf_error("Progress bar: out of memory");
    for (int i = 1; i <= width; i++) spaces[i] = ' ';
    spaces[0] = '\r';
    spaces[width + 1] = '\0';

    if (use_stderr) {
      REprintf(spaces);
    } else {
      Rprintf(spaces);
    }
    free(spaces);
  }

  void cursor_to_start(bool use_stderr) {

    if (use_stderr) {
      REprintf("\r");
    } else {
      Rprintf("\r");
    }
  }

  bool is_r_studio() {

    char *v = std::getenv("RSTUDIO");

    return v != 0 && v[0] == '1' && v[1] == '\0';
  }

  bool is_r_app() {

    char *v = std::getenv("R_GUI_APP_VERSION");

    return v != 0;
  }

  // In R Studio we should print to stdout, because priting a \r
  // to stderr is buggy (reported)

  bool default_stderr() {

    return !is_r_studio();
  }

  // If stdout is a terminal, or R Studio or macOS R.app
  // On windows, stdout is a terminal, apparently

  bool is_supported() {

    return is_option_enabled() &&
      (isatty(1) || is_r_studio() || is_r_app());
  }

  // gettimeofday for windows, from
  // https://stackoverflow.com/questions/10905892

#ifdef Win32

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <stdint.h> // portable: uint64_t   MSVC: __int64

  int gettimeofday(struct timeval * tp, struct timezone * tzp) {
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
  }

#endif

  static double time_now() {
    struct timeval now;
    gettimeofday(&now, /* tzp = */ 0);
    return now.tv_sec + now.tv_usec / 1000000.0;
  }

  static void replace_all(std::string& str, const std::string& from,
		   const std::string& to) {
    if (from.empty()) return;

    size_t start_pos = 0;

    while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
      str.replace(start_pos, from.length(), to);
      start_pos += to.length();
    }
  }

public:

  static std::string vague_dt(double seconds) {
    double minutes = seconds / 60;
    double hours = minutes / 60;
    double days = hours / 24;
    double years = days / 365.25;

    std::stringstream buffer;

    buffer << std::setw(2);

    if (seconds < 50) {
      buffer << round(seconds) << "s";
    } else if (minutes < 50) {
      buffer << round(minutes) << "m";
    } else if (hours < 18) {
      buffer << round(hours) << "h";
    } else if (days < 30) {
      buffer << round(days) << "d";
    } else if (days < 335) {
      buffer << round(days/30) << "M";
    } else {
      buffer << round(years) << "y";
    }

    return buffer.str();
  }

  static std::string pretty_bytes(double rate) {

    errno = 0;
    long bytes = lround(rate);
    if (errno == ERANGE) {
      bytes = LONG_MAX;
    }

    if (bytes == 0) { return "0B"; }

    std::string units[] = { "B", "kB", "MB", "GB", "TB", "PB", "EB",
			    "ZB", "YB" };
    long int num_units = (long int)(sizeof(units) / sizeof(units[0]));
    double idx = std::floor(std::log(bytes) / std::log(1000.0));
    if (idx >= num_units) { idx = num_units - 1; }

    double res = round(bytes / std::pow(1000.0, idx) * 100.0) / 100.0;
    std::stringstream buffer;

    buffer.precision(2);
    buffer << std::fixed << res << units[(long) idx];
    return buffer.str();
  }

  static bool is_option_enabled() {
    SEXP opt = PROTECT(Rf_GetOption1(Rf_install("progress_enabled")));
    if (Rf_isNull(opt)) {
      UNPROTECT(1);
      return true;
    }
    Rboolean t = R_compute_identical(opt, Rf_ScalarLogical(1), 16);
    UNPROTECT(1);
    return t;
  }

}; // class RProgress

}  // namespace RProgress

#endif
