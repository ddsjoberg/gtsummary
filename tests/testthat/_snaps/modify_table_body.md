# modify_table_body() works

    Code
      as.data.frame(modify_header(modify_table_body(modify_table_body(
        modify_column_hide(tbl_regression(glm(response ~ trt + marker, trial, family = binomial)),
        c("conf.low", "conf.high")), ~ dplyr::mutate(.x, n_nonevent = N - nevent)),
      dplyr::relocate, nevent, n_nonevent, .after = label), n_nonevent = "**Control N**",
      nevent = "**Case N**"))
    Output
            **Characteristic** **Case N** **Control N** **log(OR)** **p-value**
      1 Chemotherapy Treatment       57.0           126        <NA>        <NA>
      2                 Drug A       57.0           126        <NA>        <NA>
      3                 Drug B       57.0           126        0.34         0.3
      4   Marker Level (ng/mL)       57.0           126        0.32       0.080

# modify_table_body() messaging

    Code
      modify_table_body(tbl_summary(trial, include = marker), ~ stop(
        "I made an error."))
    Condition
      Error in `modify_table_body()`:
      ! The following error occured while executing `fun` on `x$table_body`:
      x I made an error.

