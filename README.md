
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/ddsjoberg/gtsummary/workflows/R-CMD-check/badge.svg)](https://github.com/ddsjoberg/gtsummary/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![Codecov test
coverage](https://codecov.io/gh/ddsjoberg/gtsummary/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ddsjoberg/gtsummary?branch=main)
[![](https://cranlogs.r-pkg.org/badges/gtsummary)](https://cran.r-project.org/package=gtsummary)
[![DOI:10.32614/RJ-2021-053](https://zenodo.org/badge/DOI/10.32614/RJ-2021-053.svg)](https://doi.org/10.32614/RJ-2021-053)
<!-- badges: end -->

## gtsummary <a href='https://github.com/ddsjoberg/gtsummary'><img src='man/figures/logo.png' align="right" height="120" /></a>

The {gtsummary} package provides an elegant and flexible way to create
publication-ready analytical and summary tables using the **R**
programming language. The {gtsummary} package summarizes data sets,
regression models, and more, using sensible defaults with highly
customizable capabilities.

- [**Summarize data frames or
  tibbles**](https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
  easily in **R**. Perfect for presenting descriptive statistics,
  comparing group **demographics** (e.g creating a **Table 1** for
  medical journals), and more. Automatically detects continuous,
  categorical, and dichotomous variables in your data set, calculates
  appropriate descriptive statistics, and also includes amount of
  missingness in each variable.

- [**Summarize regression
  models**](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
  in R and include reference rows for categorical variables. Common
  regression models, such as logistic regression and Cox proportional
  hazards regression, are automatically identified and the tables are
  pre-filled with appropriate column headers (i.e. Odds Ratio and Hazard
  Ratio).

- [**Customize gtsummary
  tables**](https://www.danieldsjoberg.com/gtsummary/reference/index.html#section-general-formatting-styling-functions)
  using a growing list of formatting/styling functions.
  **[Bold](https://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
  labels,
  **[italicize](https://www.danieldsjoberg.com/gtsummary/reference/bold_italicize_labels_levels.html)**
  levels, **[add
  p-value](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)**
  to summary tables,
  **[style](https://www.danieldsjoberg.com/gtsummary/reference/style_percent.html)**
  the statistics however you choose,
  **[merge](https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html)**
  or
  **[stack](https://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.html)**
  tables to present results side by side… there are so many
  possibilities to create the table of your dreams!

- **[Report statistics
  inline](https://www.danieldsjoberg.com/gtsummary/articles/inline_text.html)**
  from summary tables and regression summary tables in **R markdown**.
  Make your reports completely reproducible!

By leveraging [{broom}](https://broom.tidymodels.org/),
[{gt}](https://gt.rstudio.com/), and
[{labelled}](http://larmarange.github.io/labelled/) packages,
{gtsummary} creates beautifully formatted, ready-to-share summary and
result tables in a single line of R code!

Check out the examples below, review the
[vignettes](https://www.danieldsjoberg.com/gtsummary/articles/) for a
detailed exploration of the output options, and view the
[gallery](https://www.danieldsjoberg.com/gtsummary/articles/gallery.html)
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
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
to summarize a data frame.

<img src = "https://github.com/ddsjoberg/gtsummary/raw/main/data-raw/misc_files/tbl_summary_demo1.gif" alt = "animated" width = "100%">

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
[`tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html)
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
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
to easily and beautifully display regression model results in a table.
See the
[tutorial](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
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
gallery](https://www.danieldsjoberg.com/gtsummary/articles/gallery.html)**.

## gtsummary + R Markdown

The **{gtsummary}** package was written to be a companion to the
**{gt}** package from RStudio. But not all output types are supported by
the **{gt}** package. Therefore, we have made it possible to print
**{gtsummary}** tables with various engines.

Review the **[gtsummary + R
Markdown](https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html)**
vignette for details.

<a href="https://www.danieldsjoberg.com/gtsummary/articles/rmarkdown.html">
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

- The best resources are the gtsummary vignettes: [table
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

- The R Journal Article [*Reproducible Summary Tables with the gtsummary
  Package*](https://github.com/ddsjoberg/gtsummary/raw/main/data-raw/RJ-2021-053.pdf).

- The [RStudio Education
  Blog](https://education.rstudio.com/blog/2020/07/gtsummary/) includes
  a post with a brief introduction to the package.

- A [recording of a
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

Big thank you to
[@jeffreybears](https://www.etsy.com/shop/jeffreybears/) for the hex
sticker!

Please note that the {gtsummary} project is released with a [Contributor
Code of
Conduct](https://www.danieldsjoberg.com/gtsummary/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms. Thank
you to all contributors!  
[@ablack3](https://github.com/ablack3),
[@ABorakati](https://github.com/ABorakati),
[@adilsonbauhofer](https://github.com/adilsonbauhofer),
[@aghaynes](https://github.com/aghaynes),
[@ahinton-mmc](https://github.com/ahinton-mmc),
[@aito123](https://github.com/aito123),
[@akarsteve](https://github.com/akarsteve),
[@akefley](https://github.com/akefley),
[@albertostefanelli](https://github.com/albertostefanelli),
[@alexis-catherine](https://github.com/alexis-catherine),
[@amygimma](https://github.com/amygimma),
[@anaavu](https://github.com/anaavu),
[@andrader](https://github.com/andrader),
[@angelgar](https://github.com/angelgar),
[@arbet003](https://github.com/arbet003),
[@arnmayer](https://github.com/arnmayer),
[@aspina7](https://github.com/aspina7),
[@asshah4](https://github.com/asshah4),
[@AurelienDasre](https://github.com/AurelienDasre),
[@awcm0n](https://github.com/awcm0n),
[@barthelmes](https://github.com/barthelmes),
[@bcjaeger](https://github.com/bcjaeger),
[@BeauMeche](https://github.com/BeauMeche),
[@benediktclaus](https://github.com/benediktclaus),
[@berg-michael](https://github.com/berg-michael),
[@bhattmaulik](https://github.com/bhattmaulik),
[@BioYork](https://github.com/BioYork),
[@brachem-christian](https://github.com/brachem-christian),
[@browne123](https://github.com/browne123),
[@bwiernik](https://github.com/bwiernik),
[@bx259](https://github.com/bx259),
[@calebasaraba](https://github.com/calebasaraba),
[@CarolineXGao](https://github.com/CarolineXGao),
[@ChongTienGoh](https://github.com/ChongTienGoh),
[@Chris-M-P](https://github.com/Chris-M-P),
[@chrisleitzinger](https://github.com/chrisleitzinger),
[@cjprobst](https://github.com/cjprobst),
[@clmawhorter](https://github.com/clmawhorter),
[@CodieMonster](https://github.com/CodieMonster),
[@coeus-analytics](https://github.com/coeus-analytics),
[@coreysparks](https://github.com/coreysparks),
[@ctlamb](https://github.com/ctlamb),
[@davidgohel](https://github.com/davidgohel),
[@davidkane9](https://github.com/davidkane9),
[@DavisVaughan](https://github.com/DavisVaughan),
[@dax44](https://github.com/dax44),
[@dchiu911](https://github.com/dchiu911),
[@ddsjoberg](https://github.com/ddsjoberg),
[@DeFilippis](https://github.com/DeFilippis),
[@denis-or](https://github.com/denis-or),
[@dereksonderegger](https://github.com/dereksonderegger),
[@dieuv0](https://github.com/dieuv0),
[@discoleo](https://github.com/discoleo),
[@djbirke](https://github.com/djbirke),
[@dmenne](https://github.com/dmenne),
[@edrill](https://github.com/edrill),
[@ElfatihHasabo](https://github.com/ElfatihHasabo),
[@emilyvertosick](https://github.com/emilyvertosick),
[@ercbk](https://github.com/ercbk),
[@erikvona](https://github.com/erikvona),
[@eweisbrod](https://github.com/eweisbrod),
[@feizhadj](https://github.com/feizhadj),
[@fh-jsnider](https://github.com/fh-jsnider),
[@ge-generation](https://github.com/ge-generation),
[@Generalized](https://github.com/Generalized),
[@ghost](https://github.com/ghost),
[@gjones1219](https://github.com/gjones1219),
[@gorkang](https://github.com/gorkang),
[@GuiMarthe](https://github.com/GuiMarthe),
[@hass91](https://github.com/hass91),
[@HichemLa](https://github.com/HichemLa),
[@huftis](https://github.com/huftis),
[@hughjonesd](https://github.com/hughjonesd),
[@iaingallagher](https://github.com/iaingallagher),
[@ilyamusabirov](https://github.com/ilyamusabirov),
[@IndrajeetPatil](https://github.com/IndrajeetPatil),
[@IsadoraBM](https://github.com/IsadoraBM),
[@j-tamad](https://github.com/j-tamad),
[@jalavery](https://github.com/jalavery),
[@jeanmanguy](https://github.com/jeanmanguy),
[@jemus42](https://github.com/jemus42),
[@jenifav](https://github.com/jenifav),
[@jennybc](https://github.com/jennybc),
[@JeremyPasco](https://github.com/JeremyPasco),
[@JesseRop](https://github.com/JesseRop),
[@jflynn264](https://github.com/jflynn264),
[@jhelvy](https://github.com/jhelvy),
[@jjallaire](https://github.com/jjallaire),
[@jmbarajas](https://github.com/jmbarajas),
[@jmbarbone](https://github.com/jmbarbone),
[@JoanneF1229](https://github.com/JoanneF1229),
[@joelgautschi](https://github.com/joelgautschi),
[@jojosgithub](https://github.com/jojosgithub),
[@JonGretar](https://github.com/JonGretar),
[@jordan49er](https://github.com/jordan49er),
[@jthomasmock](https://github.com/jthomasmock),
[@juseer](https://github.com/juseer),
[@jwilliman](https://github.com/jwilliman),
[@karissawhiting](https://github.com/karissawhiting),
[@kendonB](https://github.com/kendonB),
[@kentm4](https://github.com/kentm4),
[@kmdono02](https://github.com/kmdono02),
[@kwakuduahc1](https://github.com/kwakuduahc1),
[@lamhine](https://github.com/lamhine),
[@larmarange](https://github.com/larmarange),
[@leejasme](https://github.com/leejasme),
[@loukesio](https://github.com/loukesio),
[@lspeetluk](https://github.com/lspeetluk),
[@ltin1214](https://github.com/ltin1214),
[@lucavd](https://github.com/lucavd),
[@LuiNov](https://github.com/LuiNov),
[@maia-sh](https://github.com/maia-sh),
[@Marsus1972](https://github.com/Marsus1972),
[@matthieu-faron](https://github.com/matthieu-faron),
[@mbac](https://github.com/mbac),
[@mdidish](https://github.com/mdidish),
[@MelissaAssel](https://github.com/MelissaAssel),
[@mfansler](https://github.com/mfansler),
[@michaelcurry1123](https://github.com/michaelcurry1123),
[@mljaniczek](https://github.com/mljaniczek),
[@moleps](https://github.com/moleps),
[@motocci](https://github.com/motocci),
[@msberends](https://github.com/msberends),
[@mvuorre](https://github.com/mvuorre),
[@myensr](https://github.com/myensr),
[@MyKo101](https://github.com/MyKo101),
[@nalimilan](https://github.com/nalimilan),
[@oranwutang](https://github.com/oranwutang),
[@palantre](https://github.com/palantre),
[@parmsam](https://github.com/parmsam),
[@Pascal-Schmidt](https://github.com/Pascal-Schmidt),
[@PaulC91](https://github.com/PaulC91),
[@pedersebastian](https://github.com/pedersebastian),
[@perlatex](https://github.com/perlatex),
[@philsf](https://github.com/philsf),
[@polc1410](https://github.com/polc1410),
[@Polperobis](https://github.com/Polperobis),
[@postgres-newbie](https://github.com/postgres-newbie),
[@proshano](https://github.com/proshano),
[@raphidoc](https://github.com/raphidoc),
[@RaviBot](https://github.com/RaviBot),
[@rich-iannone](https://github.com/rich-iannone),
[@RiversPharmD](https://github.com/RiversPharmD),
[@rmgpanw](https://github.com/rmgpanw),
[@roman2023](https://github.com/roman2023),
[@ryzhu75](https://github.com/ryzhu75),
[@sachijay](https://github.com/sachijay),
[@saifelayan](https://github.com/saifelayan),
[@sammo3182](https://github.com/sammo3182),
[@sandhyapc](https://github.com/sandhyapc),
[@sbalci](https://github.com/sbalci),
[@sda030](https://github.com/sda030),
[@shannonpileggi](https://github.com/shannonpileggi),
[@shaunporwal](https://github.com/shaunporwal),
[@shengchaohou](https://github.com/shengchaohou),
[@ShixiangWang](https://github.com/ShixiangWang),
[@simonpcouch](https://github.com/simonpcouch),
[@slb2240](https://github.com/slb2240),
[@slobaugh](https://github.com/slobaugh),
[@spiralparagon](https://github.com/spiralparagon),
[@StaffanBetner](https://github.com/StaffanBetner),
[@Stephonomon](https://github.com/Stephonomon),
[@storopoli](https://github.com/storopoli),
[@szimmer](https://github.com/szimmer),
[@tamytsujimoto](https://github.com/tamytsujimoto),
[@TAOS25](https://github.com/TAOS25),
[@TarJae](https://github.com/TarJae),
[@themichjam](https://github.com/themichjam),
[@THIB20](https://github.com/THIB20),
[@tibirkrajc](https://github.com/tibirkrajc),
[@tjmeyers](https://github.com/tjmeyers),
[@tldrcharlene](https://github.com/tldrcharlene),
[@tormodb](https://github.com/tormodb),
[@toshifumikuroda](https://github.com/toshifumikuroda),
[@UAB-BST-680](https://github.com/UAB-BST-680),
[@uakimix](https://github.com/uakimix),
[@uriahf](https://github.com/uriahf),
[@Valja64](https://github.com/Valja64),
[@vvm02](https://github.com/vvm02),
[@xkcococo](https://github.com/xkcococo),
[@yatirbe](https://github.com/yatirbe),
[@yonicd](https://github.com/yonicd),
[@yoursdearboy](https://github.com/yoursdearboy),
[@yuryzablotski](https://github.com/yuryzablotski),
[@zabore](https://github.com/zabore),
[@zachariae](https://github.com/zachariae),
[@zaddyzad](https://github.com/zaddyzad),
[@zawkzaw](https://github.com/zawkzaw),
[@zeyunlu](https://github.com/zeyunlu),
[@zhengnow](https://github.com/zhengnow),
[@zlkrvsm](https://github.com/zlkrvsm),
[@zongell-star](https://github.com/zongell-star), and
[@Zoulf001](https://github.com/Zoulf001)
