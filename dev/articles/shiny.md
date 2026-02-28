# gtsummary + Shiny

Any gtsummary table can be placed into a [Shiny
application](https://shiny.rstudio.com/). The trick is that the
gtsummary must first be converted into a gt table using the
[`gtsummary::as_gt()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/as_gt.md)
function. Once the table is a proper gt table, we can use the methods
from the gt package to place the table in the Shiny app. Read more about
creating Shiny applications with gt tables here:
<https://gt.rstudio.com/reference/gt_output.html>.

## Shiny Example

Below is an example of a Shiny app using the gtsummary package to
provide dynamic patient characteristic summaries.

### Please Wait

![loading](/__static__/frontend/images/spinner.gif?v=ce6bcde20b2f6c562913c06be83f9e7c8a19b008017407a3094b76fa82bbd6b7f4048e032e07e534d4ab5442b9105294d612863735077ab13a47653a14c5866e)

If the Shiny app has run out of its free server usage, see below for
instructions on running the app locally on your machine.

## Shiny Code

``` r
library(gtsummary)
library(shiny)
library(gt)

# create the Shiny app
ui <- fluidPage(
  # Inputs: Select Age Range
  sidebarPanel(
    sliderInput(
      inputId = "age_range",
      label = "Age Range",
      min = 5, max = 85,
      value = c(5, 85), step = 5
    )
  ),

  # place summary table on main page
  gt_output(outputId = "my_gt_table")
)

server <- function(input, output, session) {
  output$my_gt_table <-
    render_gt(
      trial %>%
        # filter out patients outside of age range
        dplyr::filter(
          dplyr::between(age, input$age_range[1], input$age_range[2])
        ) %>%
        # build a gtsummary table
        tbl_summary(
          by = trt,
          type = age ~ "continuous",
          include = c(age, grade, response),
          missing = "no"
        ) %>%
        add_stat_label() %>%
        add_n() %>%
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Table 1. Patient Characteristics**"))
    )
}

# Run the application
shinyApp(ui = ui, server = server)
```

To run this Shiny app locally on your machine, save the script above as
`app.R` in a R Project, open in RStudio, and run the application.
