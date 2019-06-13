#' #' Selectors to complement the tidyselect functions
#' #'
#' #' Set of functions to aid in selecting variables in bulk.
#' #' @rdname selectors
#' #' @export
#'
#' all_numeric <- function() {
#'   print(rlang::current_env()$.data)
#'
#'   .data %>%
#'     purrr::map_chr(~class(.x) %>% intersect("numeric")) %>%
#'     purrr::keep(~identical(., "numeric")) %>%
#'     names()
#' }
#'
#' #' @rdname selectors
#' #' @export
#' all_integer <- function() {
#'   parent.env(environment())$.data %>%
#'     purrr::map_chr(typeof) %>%
#'     purrr::keep(~identical(., "integer")) %>%
#'     names()
#' }
#'
#' #' @rdname selectors
#' #' @export
#' all_double <- function() {
#'   parent.env(environment())$.data %>%
#'     purrr::map_chr(typeof) %>%
#'     purrr::keep(~identical(., "double")) %>%
#'     names()
#' }
#'
#' #' @rdname selectors
#' #' @export
#' all_character <- function() {
#'   parent.env(environment())$.data %>%
#'     purrr::map_chr(typeof) %>%
#'     purrr::keep(~identical(., "character")) %>%
#'     names()
#' }
#'
#' #' @rdname selectors
#' #' @export
#' all_continuous <- function() {
#'   parent.env(environment())$.meta_data %>%
#'     filter(.data$type == "continuous") %>%
#'     pull(.data$variable)
#' }
#'
#' #' @rdname selectors
#' #' @export
#' all_categorical <- function() {
#'   parent.env(environment())$.meta_data %>%
#'     filter(.data$type %in% c("categorical", "dichotomous")) %>%
#'     pull(.data$variable)
#' }
