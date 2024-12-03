# Package-wide Unit Tests-------------------------------------------------------
## pkgwide-fn:prependpvalue_fun-------------------------------------------------
test_that("pkgwide-fn:prependpvalue_fun works", {

  # Create a theme#
  my_theme_1 <-
    list(
      # Prepend p value, with 3 place digits
      "pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 3, prepend_p = TRUE)
    )

  # Create a summary table#
  gts_1 <-
    trial |>
    tbl_summary(
      by = trt,
      include = c(trt, age)
    ) |>
    add_p()

  # Apply the theme to the table and pull out the p-value#
  gts_1_pvalue <-
    with_gtsummary_theme(
      x = my_theme_1,
      expr = inline_text(x = gts_1, variable = age, column = "p.value")
    )

  # Test that the p-value has 3 digits#
  expect_equal(gts_1_pvalue, "p=0.718")
}
)

## Other pkgwide Tests----------------------------------------------------------
# Create a theme#
my_theme_2 <-
  list(
    # Pvalue with 2 place digits and custom decimal mark
    "pkgwide-fn:pvalue_fun" = label_style_pvalue(digits = 2, decimal.mark = "++"),
    # set messaging to quiet
    "pkgwide-lgl:quiet" = TRUE,
    # configurar el idioma en español#
    "pkgwide-str:language" = "es"
  )


# Apply the theme to the table and pull out the p-vale#
gts_2_pvalue <-
  with_gtsummary_theme(
    x = my_theme_2,
    expr = trial |>
      tbl_summary(
        by = trt,
        include = c(trt, age),
      ) |>
      add_p() |>
      inline_text.gtsummary(x = _, variable = age, column = "p.value")
  )

### pkgwide-fn:pvalue_fun-------------------------------------------------------
test_that("pkgwide-fn:pvalue_fun works", {

  # Test that the p-value has the decimal mark#
  expect_equal(gts_2_pvalue, "0++72")

}
)

## pkgwide-lgl:quiet------------------------------------------------------------
test_that("pkgwide-lgl:quiet works", {

  # Test that the lgl value can be found#
  expect_silent(
    with_gtsummary_theme(
      x = my_theme_2,
      expr = get_theme_element("pkgwide-lgl:quiet")
    )
  )
}
)

## pkgwide-str:language---------------------------------------------------------
test_that("pkgwide-str:language works", {

  # Pull out the headers to see if the language changed#
  vec_gts2_headers <-
    with_gtsummary_theme(
      x = my_theme_2,
      expr = trial |>
        tbl_summary(
          by = trt,
          include = c(trt, age)
        ) |>
        add_p()
    )[["table_styling"]][["header"]][["label"]]

  # Test that some of the headers were translated to Spanish#
  expect_contains(vec_gts2_headers, "**Característica**")
}
)


## pkgwide-str:ci.sep-----------------------------------------------------------
test_that("pkgwide-str:ci.sep works", {

  # Create a theme#
  my_theme_3 <-
    list(
      # Set CI sep to be something *cute*#
      "pkgwide-str:ci.sep" = " ~*~"
    )

  # Apply the theme to the table and grab the CI value for age#
  gts_ci_value <-
    with_gtsummary_theme(
      x = my_theme_3,
      expr = glm(response ~ age + stage, trial, family = binomial) |>
        tbl_regression(x = _, exponentiate = TRUE) |>
        inline_text.gtsummary(x = _, variable = age, column = "ci")
    )

  # Test that the CI has the correct pattern#
  expect_equal(gts_ci_value, "1.00 ~*~ 1.04")
}
)

## pkgwide-str:print_engine-----------------------------------------------------
test_that("pkgwide-str:print_engine works", {

  # Create a theme#
  my_theme_4 <-
    list(
      # Have the print engine be kable#
      "pkgwide-str:print_engine" = "kable"
    )

  # Set the theme to check that table is in kable format#
  gts_4_w_theme <-
    with_gtsummary_theme(
      x = my_theme_4,
      expr = trial |>
        dplyr::select(death, trt) |>
        tbl_summary(by = trt) |>
        print()
    )

  # Create the same table with as_kable instead#
  gts_4_w_kable <-
    trial |>
    dplyr::select(death, trt) |>
    tbl_summary(by = trt) |>
    as_kable()

  # Test that the print engine output matches the kable version of the table#
  expect_equal(gts_4_w_theme, gts_4_w_kable)
}
)

## pkgwide-str:theme_name-------------------------------------------------------
test_that("pkgwide-str:theme_name works", {

  # Create a theme#
  my_theme_5 <-
    list(
      # Set a theme name#
      "pkgwide-str:theme_name" = "Super Cool Themey Theme"
    )

  # Grab the theme name#
  gts_5_theme_name <-
    with_gtsummary_theme(
      x = my_theme_5,
      expr = get_gtsummary_theme()
    )[["pkgwide-str:theme_name"]]

  # Test that the theme name matches as expected#
  expect_equal(gts_5_theme_name, "Super Cool Themey Theme")
}
)


## pkgwide-fun:pre_conversion---------------------------------------------------
test_that("pkgwide-fun:pre_conversion works", {

  # Create a theme#
  my_theme_6 <-
    list(
      # Set a fx to use in pre conversion#
      "pkgwide-fun:pre_conversion" = add_ci,
      # Have the print engine be kable#
      "pkgwide-str:print_engine" = "kable"
    )

  # Apply the theme to the table and print it to see pre conversions#
  gts_6 <-
    with_gtsummary_theme(
      x = my_theme_6,
      expr = trial |>
        dplyr::select(death, trt) |>
        tbl_summary(by = trt) |>
        print()
    )

  # Test that the table includes the CI column#
  expect_snapshot(
    gts_6
  )

}
)
