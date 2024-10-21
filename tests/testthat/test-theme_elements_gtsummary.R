# Package-wide Unit Tests-------------------------------------------------------
## pkgwide-fn:prependpvalue_fun-------------------------------------------------
test_that("pkgwide-fn:prependpvalue_fun works", {

    # Create a theme#
    my_theme_1 <-
      list(
        # Prepend p value, with 3 place digits
        "pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 3, prepend_p = TRUE)
      )

    # Set the theme
    set_gtsummary_theme(my_theme_1)

    # Store the table#
    gts_1 <-
      trial |>
      tbl_summary(
        by = trt,
        include = c(trt, age),
      ) |>
      add_p()

    # Test that the p-value has 3 digits#
    expect_true(
    inline_text(gts_1, variable = age, column = "p.value") |>
      grepl(pattern = "p=0.\\d{3}", x = _)
  )
    # Reset the theming#
    reset_gtsummary_theme()
}
)

## pkgwide-fn:pvalue_fun--------------------------------------------------------
test_that("pkgwide-fn:pvalue_fun works", {

  # Create a theme#
  my_theme_2 <-
    list(
      # Prepend p value, with 3 place digits
      "pkgwide-fn:pvalue_fun" = label_style_pvalue(digits = 1, prefix = "Totally Awesome P Value = ", "OutDec" = ",")
    )

  # Set the theme
  set_gtsummary_theme(my_theme_2)

  # Store the table#
  gts_2 <-
    trial |>
    tbl_summary(
      by = trt,
      include = c(trt, age),
    ) |>
    add_p()

  # Test that the p-value has the correct digits and prefix#
  expect_true(
  inline_text.gtsummary(gts_2, variable = age, column = "p.value") |>
    grepl(pattern = "Totally Awesome P Value = 0.\\d{1}", x = _)
  )


  # Reset the theming#
  reset_gtsummary_theme()
}
)

## pkgwide-lgl:quiet------------------------------------------------------------
test_that("pkgwide-lgl:quiet works", {

  # Create a theme#
  my_theme_3 <-
    list(
      # set messaging to quiet
      "pkgwide-lgl:quiet" = TRUE
    )

  # Set the theme
  set_gtsummary_theme(my_theme_3)

  # Test that the lgl value can be found#
  expect_equal(
    get_theme_element("pkgwide-lgl:quiet"), TRUE
  )
  # Reset the theming#
  reset_gtsummary_theme()
}
)

## pkgwide-str:ci.sep-----------------------------------------------------------
test_that("pkgwide-str:ci.sep works", {

  # Create a theme#
  my_theme_4 <-
    list(
      # Set CI sep to be something *cute*
      "pkgwide-str:ci.sep" = "~*~"
    )

  # Set the theme
  set_gtsummary_theme(my_theme_4)

  # Create the table#
  gts_4 <-
    glm(response ~ age + stage, trial, family = binomial) |>
      tbl_regression(x = _, exponentiate = TRUE)

  # Test that the CI has the correct pattern somewhere#
  expect_true(
    gts_4$table_body$ci |> grepl("~*~", x = _) |> any()
  )
  # Reset the theming#
  reset_gtsummary_theme()
}
)

## pkgwide-str:language---------------------------------------------------------
test_that("pkgwide-str:ci.sep works", {

  # Create a theme#
  my_theme_5 <-
    list(
      # configurar el idioma en español#
      "pkgwide-str:language" = "es"
    )

  # Set the theme
  set_gtsummary_theme(my_theme_5)

  # Create the table#
  gts_5 <-
    trial |>
    tbl_summary(
      by = trt,
      include = c(trt, age),
    )

  # Test that the CI has the correct pattern somewhere#
  expect_snapshot(
    show_header_names(gts_5)
  )
  # Reset the theming#
  reset_gtsummary_theme()
}
)

