# gtsummary + Quarto/Rmarkdown

The **{gtsummary}** package was written to be a companion to the
**{gt}** package from RStudio. But not all output types are supported by
the **{gt}** package. Therefore, we have made it possible to print
**{gtsummary}** tables with various engines.

## Output Types

Here’s a summary of the various Quarto and R Markdown output types and
the print engines that support them.

[TABLE]

Any **{gtsummary}** table can be converted to one of the types in the
table above. For example, the code below prints a **{gtsummary}** table
as a **{flextable}** table, instead of the default **{gt}** table.

``` r
tbl_summary(trial) %>%
  as_flex_table()
```

## Example R Markdown Report

An example R markdown report using **{gtsummary}** has been included
with the package. To open the example file, run the following command in
the R console.

``` r
library(gtsummary)
system.file(package = "gtsummary") %>%
  file.path("rmarkdown_example/gtsummary_rmarkdown_html.Rmd") %>%
  file.edit()
```

## LaTeX

To print {gtsummary} tables using LaTeX, utilize one of the supporting
print engines.

``` r
# build gtsummary table
tbl <- tbl_summary(trial)

# using the {gt} package
as_gt(tbl) %>% gt::as_latex()

# using the {huxtable} package
as_hux_table(tbl) %>% huxtable::to_latex()

# using the {kableExtra} package
as_kable_extra(tbl, format = "latex")

# using the knitr::kable function
as_kable(tbl, format = "latex")
```

## Images

Use the {gt} package’s
[`gt::gtsave()`](https://gt.rstudio.com/reference/gtsave.html) function
to save images of {gtsummary} tables.

``` r
tbl_summary(trial) |>  # build gtsummary table
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as image
    filename = "my_table_image.png"
  )
```

## Tips

When printing {gt} or {gtsummary} tables in a loop, use
[`print()`](https://rdrr.io/r/base/print.html) and `results = 'asis'` in
the R markdown chunk.

```` r
```{r loop_print, results = 'asis'}
for (i in 1) {
  tbl <- tbl_summary(trial)   # build gtsummary table
  print(tbl)                  # print table
}
```
````

If `print(tbl)` does not work for you, try either
`knitr::knit_print(tbl)` or `cat(knitr::knit_print(tbl))`.

Icons from [icons8](https://icons8.com/)
