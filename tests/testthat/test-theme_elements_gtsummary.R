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

## Other pkgwide Tests----------------------------------------------------------
# Create a theme#
my_theme_2 <-
  list(
    # Prepend p value, with 3 place digits
    "pkgwide-fn:pvalue_fun" = label_style_pvalue(digits = 1, prefix = "Totally Awesome P Value = ", "OutDec" = ","),
    # set messaging to quiet
    "pkgwide-lgl:quiet" = TRUE,
    # configurar el idioma en español#
    "pkgwide-str:language" = "es"
  )

# Set the theme
set_gtsummary_theme(my_theme_2)

# Store the table#
gts_2 <-
  trial |>
  tbl_summary(
    by = trt,
    statistic = age ~ "{mean} ({sd})",
    include = c(trt, age),
  ) |>
  add_p()

### pkgwide-fn:pvalue_fun--------------------------------------------------------
test_that("pkgwide-fn:pvalue_fun works", {

  # Test that the p-value has the correct digits and prefix#
  expect_true(
    inline_text.gtsummary(gts_2, variable = age, column = "p.value") |>
      grepl(pattern = "Totally Awesome P Value = 0.\\d{1}", x = _)
  )

}
)

## pkgwide-lgl:quiet------------------------------------------------------------
test_that("pkgwide-lgl:quiet works", {

  # Test that the lgl value can be found#
  expect_true(
    get_theme_element("pkgwide-lgl:quiet")
  )
}
)

## pkgwide-str:language---------------------------------------------------------
test_that("pkgwide-str:language works", {

  # Test that the CI has the correct pattern somewhere#
  expect_snapshot(
    show_header_names(gts_2)
  )

}
)

# Reset the theme
reset_gtsummary_theme()

## pkgwide-str:ci.sep-----------------------------------------------------------
test_that("pkgwide-str:ci.sep works", {

  # Create a theme#
  my_theme_3 <-
    list(
      # Set CI sep to be something *cute*
      "pkgwide-str:ci.sep" = "~*~",
      # Have the print engine be kable
      "pkgwide-str:print_engine" = "kable"
    )

  # Set the theme
  set_gtsummary_theme(my_theme_3)

  gts_3 <-
    with_gtsummary_theme(
      my_theme_3,
      glm(response ~ age + stage, trial, family = binomial) |>
        tbl_regression(x = _, exponentiate = TRUE),
    )

  # Test that the CI has the correct pattern somewhere#
  expect_true(
    gts_3$table_body$ci |>
      grepl("~*~", x = _) |>
      any()
  )

  # Reset the theme
  reset_gtsummary_theme()

}
)

# Reset the theming#
reset_gtsummary_theme()

## pkgwide-str:print_engine-----------------------------------------------------
test_that("pkgwide-str:print_engine works", {

  # Create a theme#
  my_theme_4 <-
    list(
      # Have the print engine be kable
      "pkgwide-str:print_engine" = "kable"
    )

  # set the theme
  set_gtsummary_theme(my_theme_4)

  # Set the theme to check that table is in kable format
  gts_4 <-
    with_gtsummary_theme(
      my_theme_4,
      trial |>
        dplyr::select(death, trt) |>
        tbl_summary(by = trt)
    )

  # This only works when a theme is set explicitly
  # And not when it is just temporarily set with 'with_gtsummary_theme'
  # Is this intentional behavior?
  expect_true(grepl("|:-", gts_4) |> any())

  # Reset the theme
  reset_gtsummary_theme()
}
)

## pkgwide-str:theme_name-------------------------------------------------------
test_that("pkgwide-str:theme_name works", {

  # Create a theme#
  my_theme_5 <-
    list(
      # Set a theme name
      "pkgwide-str:theme_name" = "Super Cool Themey Theme"
    )

  expect_snapshot(
    set_gtsummary_theme(my_theme_5)
  )

  # Reset the theme
  reset_gtsummary_theme()
}
)


## pkgwide-fun:pre_conversion---------------------------------------------------
test_that("pkgwide-fun:pre_conversion works", {

  # Create a theme#
  my_theme_6 <-
    list(
      # Set a fx to use in pre conversion
      "pkgwide-fun:pre_conversion" = add_ci
    )

  # Set the theme
  set_gtsummary_theme(my_theme_6)
  gts_6 <-
    trial |>
    dplyr::select(death, trt) |>
    tbl_summary(by = trt)

  # This only works when a theme is set explicitly
  # And not when it is just temporarily set with 'with_gtsummary_theme'
  # Is this intentional behavior?
  expect_snapshot(
    gts_6
  )

  # Reset the theme
  reset_gtsummary_theme()
}
)
