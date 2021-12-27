# nycflights13 1.0.2

* Fixes for R CMD check changes.

# nycflights13 1.0.1

* `airports$tz` with values `\N` replaced with `NA` (#36).

* `weather$year` and `weather$month` are now integers to match `flights$year`
  and `flights$month` (@jozefhajnala, #34)

# nycflights13 1.0.0

* `weather$time_hour` and `flights$time_hour` are now stored in the 
  America/New_York timezone (#19).

* `weather` data updated from latest Iowa State mesonet (@rmcd1024, #24).
  `wind_gust` is now correctly captured from the underlying data, rather than 
  being a copy of `wind_speed`. `precip` is better captures the hourly 
  preciptation (which tends to be recorded at 51 minutes past the hour) (#27) 

# nycflights13 0.2.2

* Import function from tibble to suppress R CMD check NOTE.

# nycflights13 0.2.1

* nycflights imports tibble so you get nice printing even when no other
  tidyverse package is loaded.

* `airports` now has a `tzone` column that contains the IANA time zone
  for the airport (#15).

# nycflights13 0.2.0

* `airlines`: `carrier` columns are characters instead of factors (#2).

* `airports`: duplicate entry for BFT removed (#7).

* `flights`:
    
    * new `time_hour` variable combines `year`, `month`, `day`, and 
      `hour` into a single variable (#11).

    * new `sched_dep_time` and `sched_arr_time` variables give scheduled 
      departure and  arrival times - these are more appropriate for connecting 
      to weather data. `hour` and `minute` are now computed from the scheduled 
      departure time, not the actual departure time.

    * missing `tailnum` now recorded as `NA`, not `""` (#10).

* `weather`:
  
  * Includes weather data for all airports.

  * New `time_hour` variable combines `year`, `month`, `day`, `hour` into 
    a single POSIXct variable.

  * Saved as ungrouped.
