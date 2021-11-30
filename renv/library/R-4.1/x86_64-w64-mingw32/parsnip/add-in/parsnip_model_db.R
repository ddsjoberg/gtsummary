# ------------------------------------------------------------------------------
# code to make the parsnip model database used by the RStudio addin

# ------------------------------------------------------------------------------

library(tidymodels)
library(usethis)

# also requires installation of:
packages <- c("parsnip", "discrim", "plsmod", "rules", "baguette", "poissonreg", "modeltime", "modeltime.gluonts")

# ------------------------------------------------------------------------------

# Detects model specifications via their print methods
print_methods <- function(x) {
  require(x, character.only  = TRUE)
  ns <- asNamespace(ns = x)
  mthds <- ls(envir = ns, pattern = "^print\\.")
  mthds <- gsub("^print\\.", "", mthds)
  purrr::map_dfr(mthds, get_engines) %>% dplyr::mutate(package = x)
}
get_engines <- function(x) {
  eng <- try(parsnip::show_engines(x), silent = TRUE)
  if (inherits(eng, "try-error")) {
    eng <- tibble::tibble(engine = NA_character_, mode = NA_character_, model = x)
  } else {
    eng$model <- x
  }
  eng
}
get_tunable_param <- function(mode, package, model, engine) {
  cl <- rlang::call2(.ns = package, .fn = model)
  obj <- rlang::eval_tidy(cl)
  obj <- parsnip::set_engine(obj, engine)
  obj <- parsnip::set_mode(obj, mode)
  res <-
    tune::tunable(obj) %>%
    dplyr::select(parameter = name)

  # ------------------------------------------------------------------------------
  # Edit some model parameters

  if (model == "rand_forest") {
    res <- res[res$parameter != "trees",]
  }
  if (model == "mars") {
    res <- res[res$parameter == "prod_degree",]
  }
  if (engine %in% c("rule_fit", "xgboost")) {
    res <- res[res$parameter != "mtry",]
  }
  if (model %in% c("bag_tree", "bag_mars")) {
    res <- res[0,]
  }
  if (engine %in% c("rpart")) {
    res <- res[res$parameter != "tree-depth",]
  }
  res

}

# ------------------------------------------------------------------------------

model_db <-
  purrr::map_dfr(packages, print_methods) %>%
  dplyr::filter(!is.na(engine)) %>%
  dplyr::mutate(label = paste0(model, " (", engine, ")")) %>%
  dplyr::arrange(model, engine, mode)

num_modes <-
  model_db %>%
  dplyr::group_by(package, model, engine) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(single_mode = n == 1) %>%
  dplyr::select(package, model, engine, single_mode)

model_db <-
  dplyr::left_join(model_db, num_modes, by = c("package", "model", "engine")) %>%
  dplyr::filter(engine != "spark") %>%
  dplyr::mutate(parameters = purrr::pmap(list(mode, package, model, engine), get_tunable_param))

usethis::use_data(model_db, overwrite = TRUE)

