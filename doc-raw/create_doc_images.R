library(gtsummary)

# Create Doc .pngs  -------------------------------------------------------------
# script to run example code chunk from function documentation and save outputs as png

# function to run example code chunk from single documentation file (.Rd) and save output.
update_table_png <- function(input_file_path) {
  file_text <- readLines(input_file_path)

  if (any(stringr::str_detect(file_text, "examples\\{"))) {
    print(glue::glue("Working on {input_file_path}"))

    # find, extract and clean example code from doc text
    where_is_example <- which(stringr::str_detect(file_text, "examples\\{"))
    file_text_trunc <- file_text[where_is_example:length(file_text)]


    example_code <- file_text_trunc[2:(min(which(file_text_trunc == "}")) - 1)] %>%
      stringr::str_remove_all("\\\\")

    example_code <-  example_code[!stringr::str_detect(example_code, "donttest")]

    # evaluate example code and save to environment -> html (temprary) -> png
    # use map() because there may be multiple resulting objects from one example code chunk
    res_obj <- evalute_examples_code(example_code)
    res_obj <- res_obj[which(stringr::str_detect(names(res_obj), "_ex"))]

    purrr::map2(res_obj, names(res_obj), ~ save_html_then_png(.x, .y))
  }

  # if there is no example code to be run in a given doc, print message
  else {
    print(glue::glue("no examples in '{input_file_path}'"))
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
      gt::gtsave(glue::glue("{here::here()}/man/figures/", "{obj_name}", ".html"))
  } else {
    x %>%
      as_gt() %>%
      gt::gtsave(glue::glue("{here::here()}/man/figures/", "{obj_name}", ".html"))
  }

  webshot::webshot(
    url = glue::glue("{here::here()}/man/figures/", "{obj_name}", ".html"),
    file = glue::glue("{here::here()}/man/figures/", "{obj_name}", ".png"),
    selector = "table", zoom = 2, expand = NULL
  )

  # remove .html when done
  invisible(file.remove(glue::glue("{here::here()}/man/figures/", "{obj_name}", ".html")))
}


# Run Code to Create Figures for all files  -----------------------------------

all_files <-
  list.files(here::here("man"), full.names = TRUE) %>%
  purrr::discard(!stringr::str_ends(., pattern = ".Rd")) %>%
  # TODO: Figure out why this the code does not work for add_p_test methods
  purrr::discard(~endsWith(., "add_p.Rd"))

purrr::walk(all_files, ~ update_table_png(.x))

# Or Run on individual files as needed:
# update_table_png(here::here("man", "add_p.Rd"))

# README gt tables -------------------------------------------------------------
temp_dir <- tempdir()
# tbl_summary
tbl_summary(
  data = trial[c("trt", "age", "grade", "response")],
  by = trt
) %>%
  add_p() %>%
  as_gt() %>%
  gt::gtsave(file.path(temp_dir, "README-tbl_summary.html"))

webshot::webshot(
  url = file.path(temp_dir, "README-tbl_summary.html"),
  file = file.path(here::here(), "man/figures/README-tbl_summary.png"),
  selector = "table", zoom = 2, expand = NULL
)


# tbl_regression
glm(
  response ~ trt + age + grade,
  data = trial,
  family = binomial(link = "logit")
) %>%
  tbl_regression(exponentiate = TRUE) %>%
  as_gt() %>%
  gt::gtsave(file.path(temp_dir, "README-tbl_regression.html"))

webshot::webshot(
  url = file.path(temp_dir, "README-tbl_regression.html"),
  file = file.path(here::here(), "man/figures/README-tbl_regression.png"),
  selector = "table", zoom = 2, expand = NULL
)
