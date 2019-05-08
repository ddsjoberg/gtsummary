library(glue)
library(here)
library(stringr)
library(purrr)
library(gt)
library(webshot)

# Create Doc .pngs  -------------------------------------------------------------
# script to run example code chunk from function documentation and save outputs as png

# function to run example code chunk from single documentation file (.Rd) and save output.
update_table_png <- function(input_file_path) {
  file_text <- readLines(input_file_path)

  if (any(str_detect(file_text, "examples\\{"))) {

    # find, extract and clean example code from doc text
    where_is_example <- which(str_detect(file_text, "examples\\{"))
    file_text_trunc <- file_text[where_is_example:length(file_text)]
    example_code <- file_text_trunc[2:(min(which(file_text_trunc == "}")) - 1)] %>%
      str_remove_all("\\\\")

    # evaluate example code and save to environment -> html (temprary) -> png
    # use map() because there may be multiple resulting objects from one example code chunk
    res_obj <- evalute_examples_code(example_code)
    res_obj <- res_obj[which(str_detect(names(res_obj), "_ex"))]
    map2(res_obj, names(res_obj), ~ save_html_then_png(.x, .y))
  }

  # if there is no example code to be run in a given doc, print message
  else {
    print(glue("no examples in '{input_file_path}'"))
  }
}


# Helper Functions -------------------------------------------------------------

# helper function to evaluate example code
evalute_examples_code <- function(code) {
  eval(parse(text = code))
  exp_outputs <- as.list(environment())
  exp_outputs["code"] <- NULL
  return(exp_outputs)
}

# helper function to turn a resulting table object to a gt object, then save as html/png
# this will be replaced once new gtsave() png option available.
save_html_then_png <- function(x, obj_name) {
  if ("gt_tbl" %in% class(x)) {
    x %>%
      gtsave(glue::glue("{here()}/man/figures/", "{obj_name}", ".html"))
  } else {
    x %>%
      as_gt() %>%
      gtsave(glue::glue("{here()}/man/figures/", "{obj_name}", ".html"))
  }

  webshot::webshot(
    # url = glue::glue("file://{here()}/man/figures/", "{obj_name}", ".html"),
    url = glue::glue("{here()}/man/figures/", "{obj_name}", ".html"),
    file = glue::glue("{here()}/man/figures/", "{obj_name}", ".png"),
    selector = "table", zoom = 2, expand = NULL
  )

  # remove .html when done
  invisible(file.remove(glue::glue("{here()}/man/figures/", "{obj_name}", ".html")))
}


# Run Code to Create Figures for all files  -----------------------------------

all_files <- list.files(here("man"), full.names = TRUE) %>%
  purrr::discard(!stringr::str_ends(., pattern = ".Rd"))
map(all_files, ~ update_table_png(.x))

# Or Run on individual files as needed::
# update_table_png(here("man", "add_global.tbl_regression.Rd"))
