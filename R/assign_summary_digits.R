

# assign_summary_digits <- function(data, statistic, type, digits = NULL) {
#   # extract the statistics
#   statistic <- lapply(statistic, function(x) .extract_glue_elements(x) |> unlist())
#
#   lapply(
#     names(statistic),
#     function(variable) {
#       if (!is.null(digits[[variable]])){
#         return(rep_named(statistic[[variable]], digits[[variable]]))
#       }
#
#       if (type[[variable]] %in% c("cateogrical", "dichotomous")) {
#
#       }
#     }
#   )
#
# }
