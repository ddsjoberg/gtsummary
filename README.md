
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/ddsjoberg/gtsummary/branch/master/graph/badge.svg)](https://codecov.io/gh/ddsjoberg/gtsummary?branch=master)
[![R build
status](https://github.com/ddsjoberg/gtsummary/workflows/R-CMD-check/badge.svg)](https://github.com/ddsjoberg/gtsummary/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![](https://cranlogs.r-pkg.org/badges/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![DOI:10.32614/RJ-2021-053](https://zenodo.org/badge/DOI/10.32614/RJ-2021-053.svg)](https://doi.org/10.32614/RJ-2021-053)
<!-- badges: end -->

## gtsummary <a href='https://github.com/ddsjoberg/gtsummary'><img src='man/figures/logo.png' align="right" height="120" /></a>

The {gtsummary} package provides an elegant and flexible way to create
publication-ready analytical and summary tables using the **R**
programming language. The {gtsummary} package summarizes data sets,
regression models, and more, using sensible defaults with highly
customizable capabilities.

-   [**Summarize data frames or
    tibbles**](http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
    easily in **R**. Perfect for presenting descriptive statistics,
    comparing group **demographics** (e.g creating a **Table 1** for
    medical journals), and more. Automatically detects continuous,
    categorical, and dichotomous variables in your data set, calculates
    appropriate descriptive statistics, and also includes amount of
    missingness in each variable.

-   [**Summarize regression
    models**](http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
    in R and include reference rows for categorical variables. Common
    regression models, such as logistic regression and Cox proportional
    hazards regression, are automatically identified and the tables are
    pre-filled with appropriate column headers (i.e. Odds Ratio and
    Hazard Ratio).

-   [**Customize gtsummary
    tables**](http://www.danieldsjoberg.com/gtsummary/reference/index.html#section-general-formatting-styling-functions)
    using a growing list of formatting/styling functions.
    **[Bold](http://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
    labels,
    **[italicize](http://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
    levels, **[add
    p-value](http://www.danieldsjoberg.com/gtsummary/reference/add_p.html)**
    to summary tables,
    **[style](http://www.danieldsjoberg.com/gtsummary/reference/style_percent.html)**
    the statistics however you choose,
    **[merge](http://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html)**
    or
    **[stack](http://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.html)**
    tables to present results side by side… there are so many
    possibilities to create the table of your dreams!

-   **[Report statistics
    inline](http://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)**
    from summary tables and regression summary tables in **R markdown**.
    Make your reports completely reproducible!

By leveraging [{broom}](https://broom.tidymodels.org/),
[{gt}](https://gt.rstudio.com/), and
[{labelled}](http://larmarange.github.io/labelled/) packages,
{gtsummary} creates beautifully formatted, ready-to-share summary and
result tables in a single line of R code!

Check out the examples below, review the
[vignettes](http://www.danieldsjoberg.com/gtsummary/articles/) for a
detailed exploration of the output options, and view the
[gallery](http://www.danieldsjoberg.com/gtsummary/articles/gallery.html)
for various customization examples.

## Installation

The {gtsummary} package was written as a companion to the
[{gt}](https://gt.rstudio.com/) package from RStudio. You can install
{gtsummary} with the following code.

``` r
install.packages("gtsummary")
```

Install the development version of {gtsummary} with:

``` r
remotes::install_github("ddsjoberg/gtsummary")
```

## Examples

### Summary Table

Use
[`tbl_summary()`](http://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
to summarize a data frame.

<img src = "https://github.com/ddsjoberg/gtsummary/raw/master/data-raw/misc_files/tbl_summary_demo1.gif" alt = "animated" width = "100%">

Example basic table:

``` r
library(gtsummary)
# make dataset with a few variables to summarize
trial2 <- trial %>% select(age, grade, response, trt)

# summarize the data with our package
table1 <- tbl_summary(trial2)
```

<img src="man/figures/README-tbl_summary_print_simple-1.png" width="30%" />

There are many **customization options** to **add information** (like
comparing groups) and **format results** (like bold labels) in your
table. See the
[`tbl_summary()`](http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
tutorial for many more options, or below for one example.

``` r
table2 <- 
  tbl_summary(
    trial2,
    by = trt, # split table by group
    missing = "no" # don't list missing data separately
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 
```

<img src="man/figures/README-tbl_summary_print_extra-1.png" width="60%" />

### Regression Models

Use
[`tbl_regression()`](http://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
to easily and beautifully display regression model results in a table.
See the
[tutorial](http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
for customization options.

``` r
mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE)
```

<img src="man/figures/README-tbl_regression_printa-1.png" width="40%" />

### Side-by-side Regression Models

You can also present side-by-side regression model results using
`tbl_merge()`

``` r
library(survival)

# build survival model table
t2 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  tbl_regression(exponentiate = TRUE)

# merge tables 
tbl_merge_ex1 <-
  tbl_merge(
    tbls = list(t1, t2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )
```

<img src="man/figures/README-tbl_merge_ex1-1.png" width="60%" />

Review even more output options in the **[table
gallery](http://www.danieldsjoberg.com/gtsummary/articles/gallery.html)**.

## gtsummary + R Markdown

The **{gtsummary}** package was written to be a companion to the
**{gt}** package from RStudio. But not all output types are supported by
the **{gt}** package. Therefore, we have made it possible to print
**{gtsummary}** tables with various engines.

Review the **[gtsummary + R
Markdown](http://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html)**
vignette for details.

<a href="http://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html">
<img src="man/figures/gt_output_formats.PNG" width="55%" /> </a>

## Save Individual Tables

{gtsummary} tables can also be saved directly to file as an image, RTF,
LaTeX, and Word file.

``` r
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = ".") # use extensions .html .tex .ltx .rtf
```

For a Word file, use

``` r
tbl %>%
  as_flex_table() %>%
  flextable::save_as_docx()
```

## Additional Resources

-   The best resources are the gtsummary vignettes: [table
    gallery](https://www.danieldsjoberg.com/gtsummary/articles/gallery.html),
    [`tbl_summary()`
    tutorial](https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html),
    [`tbl_regression()`
    tutorial](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html),
    [`inline_text()`
    tutorial](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html),
    [gtsummary
    themes](https://www.danieldsjoberg.com/gtsummary/articles/themes.html),
    [gtsummary+R
    markdown](https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html).

-   The R Journal Article [*Reproducible Summary Tables with the
    gtsummary
    Package*](https://github.com/ddsjoberg/gtsummary/raw/master/data-raw/RJ-2021-053.pdf).

-   The [RStudio Education
    Blog](https://education.rstudio.com/blog/2020/07/gtsummary/)
    includes a post with a brief introduction to the package.

-   A [recording of a
    presentation](https://www.youtube.com/watch?v=tANo9E1SYJE) given to
    the Weill Cornell Biostatistics Department and the Memorial Sloan
    Kettering R Users Group.

    <iframe width="560" height="315" src="https://www.youtube.com/embed/tANo9E1SYJE" title="YouTube video player" frameborder="0" allow="accelerometer; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## Cite gtsummary

``` text
> citation("gtsummary")

To cite gtsummary in publications use:

  Sjoberg DD, Whiting K, Curry M, Lavery JA, Larmarange J. Reproducible summary tables with the gtsummary package.
  The R Journal 2021;13:570–80. https://doi.org/10.32614/RJ-2021-053.

A BibTeX entry for LaTeX users is

  @Article{gtsummary,
    author = {Daniel D. Sjoberg and Karissa Whiting and Michael Curry and Jessica A. Lavery and Joseph Larmarange},
    title = {Reproducible Summary Tables with the gtsummary Package},
    journal = {{The R Journal}},
    year = {2021},
    url = {https://doi.org/10.32614/RJ-2021-053},
    doi = {10.32614/RJ-2021-053},
    volume = {13},
    issue = {1},
    pages = {570-580},
  }
```

## Contributing

Big thank you to [@jeffreybears](https://www.jeffreybears.com/) for the
hex sticker!

Please note that the {gtsummary} project is released with a [Contributor
Code of
Conduct](http://www.danieldsjoberg.com/gtsummary/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms. Thank
you to all contributors!  
[@ablack3](https://github.com/ablack3),
[@aghaynes](https://github.com/aghaynes),
[@ahinton-mmc](https://github.com/ahinton-mmc),
[@albertostefanelli](https://github.com/albertostefanelli),
[@alexis-catherine](https://github.com/alexis-catherine),
[@anaavu](https://github.com/anaavu),
[@andrader](https://github.com/andrader),
[@angelgar](https://github.com/angelgar),
[@arbet003](https://github.com/arbet003),
[@arnmayer](https://github.com/arnmayer),
[@asshah4](https://github.com/asshah4),
[@awcm0n](https://github.com/awcm0n),
[@barthelmes](https://github.com/barthelmes),
[@BeauMeche](https://github.com/BeauMeche),
[@benediktclaus](https://github.com/benediktclaus),
[@bwiernik](https://github.com/bwiernik),
[@calebasaraba](https://github.com/calebasaraba),
[@CarolineXGao](https://github.com/CarolineXGao),
[@Chris-M-P](https://github.com/Chris-M-P),
[@chrisleitzinger](https://github.com/chrisleitzinger),
[@clmawhorter](https://github.com/clmawhorter),
[@CodieMonster](https://github.com/CodieMonster),
[@coeus-analytics](https://github.com/coeus-analytics),
[@coreysparks](https://github.com/coreysparks),
[@ctlamb](https://github.com/ctlamb),
[@davidgohel](https://github.com/davidgohel),
[@davidkane9](https://github.com/davidkane9),
[@dax44](https://github.com/dax44),
[@ddsjoberg](https://github.com/ddsjoberg),
[@DeFilippis](https://github.com/DeFilippis),
[@denis-or](https://github.com/denis-or),
[@dieuv0](https://github.com/dieuv0),
[@djbirke](https://github.com/djbirke),
[@eamoncaddigan](https://github.com/eamoncaddigan),
[@ElfatihHasabo](https://github.com/ElfatihHasabo),
[@emilyvertosick](https://github.com/emilyvertosick),
[@ercbk](https://github.com/ercbk),
[@fh-jsnider](https://github.com/fh-jsnider),
[@gjones1219](https://github.com/gjones1219),
[@gorkang](https://github.com/gorkang),
[@GuiMarthe](https://github.com/GuiMarthe),
[@hughjonesd](https://github.com/hughjonesd),
[@iaingallagher](https://github.com/iaingallagher),
[@IndrajeetPatil](https://github.com/IndrajeetPatil),
[@IsadoraBM](https://github.com/IsadoraBM),
[@j-tamad](https://github.com/j-tamad),
[@jalavery](https://github.com/jalavery),
[@jeanmanguy](https://github.com/jeanmanguy),
[@jemus42](https://github.com/jemus42),
[@jennybc](https://github.com/jennybc),
[@JeremyPasco](https://github.com/JeremyPasco),
[@JesseRop](https://github.com/JesseRop),
[@jflynn264](https://github.com/jflynn264),
[@jjallaire](https://github.com/jjallaire),
[@joelgautschi](https://github.com/joelgautschi),
[@JonGretar](https://github.com/JonGretar),
[@juseer](https://github.com/juseer),
[@jwilliman](https://github.com/jwilliman),
[@karissawhiting](https://github.com/karissawhiting),
[@khizzr](https://github.com/khizzr),
[@kmdono02](https://github.com/kmdono02),
[@kwakuduahc1](https://github.com/kwakuduahc1),
[@larmarange](https://github.com/larmarange),
[@leejasme](https://github.com/leejasme),
[@loukesio](https://github.com/loukesio),
[@lspeetluk](https://github.com/lspeetluk),
[@ltin1214](https://github.com/ltin1214),
[@lucavd](https://github.com/lucavd),
[@maia-sh](https://github.com/maia-sh),
[@margarethannum](https://github.com/margarethannum),
[@Marsus1972](https://github.com/Marsus1972),
[@matthieu-faron](https://github.com/matthieu-faron),
[@mbac](https://github.com/mbac),
[@MelissaAssel](https://github.com/MelissaAssel),
[@michaelcurry1123](https://github.com/michaelcurry1123),
[@moleps](https://github.com/moleps),
[@MyKo101](https://github.com/MyKo101),
[@oranwutang](https://github.com/oranwutang),
[@Pascal-Schmidt](https://github.com/Pascal-Schmidt),
[@proshano](https://github.com/proshano),
[@raphidoc](https://github.com/raphidoc),
[@ryzhu75](https://github.com/ryzhu75),
[@sachijay](https://github.com/sachijay),
[@sammo3182](https://github.com/sammo3182),
[@sbalci](https://github.com/sbalci),
[@ShixiangWang](https://github.com/ShixiangWang),
[@simonpcouch](https://github.com/simonpcouch),
[@slb2240](https://github.com/slb2240),
[@slobaugh](https://github.com/slobaugh),
[@StaffanBetner](https://github.com/StaffanBetner),
[@storopoli](https://github.com/storopoli),
[@tamytsujimoto](https://github.com/tamytsujimoto),
[@TarJae](https://github.com/TarJae),
[@THIB20](https://github.com/THIB20),
[@tjmeyers](https://github.com/tjmeyers),
[@tldrcharlene](https://github.com/tldrcharlene),
[@tormodb](https://github.com/tormodb),
[@UAB-BST-680](https://github.com/UAB-BST-680),
[@uriahf](https://github.com/uriahf),
[@xkcococo](https://github.com/xkcococo),
[@yonicd](https://github.com/yonicd),
[@zabore](https://github.com/zabore),
[@zachariae](https://github.com/zachariae),
[@zeyunlu](https://github.com/zeyunlu),
[@zlkrvsm](https://github.com/zlkrvsm), and
[@zongell-star](https://github.com/zongell-star)
