set.seed(23433)
r_version <- paste0(R.Version()$major, ".", R.Version()$minor)

# function to pull estimates from tbl_regression object
# input gt object, output vector of coefficients
coefs_in_gt <- function(x) {
  x$table_body %>%
    dplyr::pull(estimate) %>%
    na.omit() %>%
    as.vector()
}
