# dplyr::storms[1:10, ] %>% dput() # this was run on 2023-04-23 on dplyr v1.1.2

df_dplyr_storms <-
  structure(
    list(
      name = c("Amy", "Amy", "Amy", "Amy", "Amy", "Amy", "Amy", "Amy", "Amy", "Amy"),
      year = c(1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975, 1975),
      month = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6),
      day = c(27L, 27L, 27L, 27L, 28L, 28L, 28L, 28L, 29L, 29L),
      hour = c(0, 6, 12, 18, 0, 6, 12, 18, 0, 6),
      lat = c(27.5, 28.5, 29.5, 30.5, 31.5, 32.4, 33.3, 34, 34.4, 34),
      long = c(-79, -79, -79, -79, -78.8, -78.7, -78, -77, -75.8, -74.8),
      status =
        structure(
          c(7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 8L, 8L),
          levels = c(
            "disturbance", "extratropical", "hurricane", "other low", "subtropical depression",
            "subtropical storm", "tropical depression", "tropical storm", "tropical wave"),
          class = "factor"
        ),
      category = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
      wind = c(25L, 25L, 25L, 25L, 25L, 25L, 25L, 30L, 35L, 40L),
      pressure = c(1013L, 1013L, 1013L, 1013L, 1012L, 1012L, 1011L, 1006L, 1004L, 1002L),
      tropicalstorm_force_diameter = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_),
      hurricane_force_diameter = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
    ),
    row.names = c(NA, -10L),
    class = c("tbl_df", "tbl", "data.frame")
  )
