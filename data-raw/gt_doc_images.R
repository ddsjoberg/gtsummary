# THIS SCRIPT CREATES HELP FILE IMAGES FOR GT OBJECTS (INCLUDING GTSUMMARY)
library(gtsummary)
rm(list = ls())

# list of all help files
gt_functions <-
  list.files(here::here("man")) %>%
  purrr::keep(~stringr::str_ends(., stringr::fixed(".Rd"))) %>%
  stringr::str_remove(".Rd")

# create temp gtsummary directory (example scripts will be saved here)
path_gtsummary <- file.path(tempdir(), "gtsummary")
fs::dir_create(path_gtsummary)
unlink(path_gtsummary) # just in case it already existed with files in folder

# delete existing png example images
list.files(here::here("man", "figures")) %>%
  purrr::keep(~(stringr::str_ends(., "_ex[:digit:]+.png") | stringr::str_ends(., "_ex.png")) &
                !stringr::str_starts(., "README-")) %>%
  purrr::walk(~fs::file_delete(here::here("man", "figures", .x)))

# cycling over each help file, and saving gt images
set_gtsummary_theme(list("pkgwide-lgl:quiet" = TRUE))
for (f in gt_functions) {
  usethis::ui_done("Working on {f}")

  # run code from example
  utils::example(topic = f, package = "gtsummary", character.only = TRUE, give.lines = FALSE, echo = FALSE)

  # get list of example objects that end in "_ex###"
  example_objs <- ls()[stringr::str_ends(ls(), "_ex[:digit:]+") | stringr::str_ends(ls(), "_ex")]

  # saving an image of every gt or gtsummary example
  purrr::walk(
    example_objs,
    function(example_chr) {
      # converting string to object
      example_obj <- eval(parse(text = example_chr))
      usethis::ui_todo("Saving `{example_chr}.png`")

      # convert gtsummary object to gt
      if (inherits(example_obj, "gtsummary"))
        example_obj <- as_gt(example_obj)

      # checking object is now a gt object
      if (inherits(example_obj, "gt_tbl"))
        # saving image
        gt::gtsave(example_obj,
                   filename = here::here("man", "figures", stringr::str_glue("{example_chr}.png")))

      # saving flextable image
      if (inherits(example_obj, "flextable"))
        flextable::save_as_image(example_obj,
                                 webshot = "webshot2",
                                 path = here::here("man", "figures", stringr::str_glue("{example_chr}.png")))

      return(invisible())
    }
  )

  # removing all objects except `gt_functions`, `path_gtsummary`
  rm(list = ls()[!ls() %in% c("gt_functions", "path_gtsummary")])
}
reset_gtsummary_theme()
