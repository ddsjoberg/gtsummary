# Modify table caption

Captions are assigned based on output type.

- `gt::gt(caption=)`

- `flextable::set_caption(caption=)`

- `huxtable::set_caption(value=)`

- `knitr::kable(caption=)`

## Usage

``` r
modify_caption(x, caption, text_interpret = c("md", "html"))
```

## Arguments

- x:

  (`gtsummary`)  
  A gtsummary object

- caption:

  (`string`/`character`)  
  A string for the table caption/title. NOTE: The `gt` print engine
  supports a vector of captions. But not every print engine supports
  this feature, and for those outputs, only a string is accepted.

- text_interpret:

  (`string`)  
  String indicates whether text will be interpreted with
  [`gt::md()`](https://gt.rstudio.com/reference/md.html) or
  [`gt::html()`](https://gt.rstudio.com/reference/html.html). Must be
  `"md"` (default) or `"html"`. Applies to tables printed with `{gt}`.

## Value

Updated gtsummary object

## Examples

``` r
trial |>
  tbl_summary(by = trt, include = c(marker, stage)) |>
  modify_caption(caption = "**Baseline Characteristics** N = {N}")


  
Baseline Characteristics N = 200

  
Characteristic
```
