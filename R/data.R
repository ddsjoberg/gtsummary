#' gt package git commit SHA
#'
#' This is the git commit SHA for the gt package associated with the current
#' release of the gtsummary package. Because the gt package is currently under
#' development, breaking changes are more likely to be introduced. This git
#' commit SHA references a version of the gt package compatible with the
#' current release of gtsummary. Use the SHA to install the appropriate version
#' of the gt package tested for compatibility with gtsummary.
#' @keywords internal
#' @examples
#' gtsummary::gt_sha
#'
#' # install gt version compatible with gtsummary release
#' # remotes::install_github("rstudio/gt", ref = gtsummary::gt_sha)
"gt_sha"

#' Results from a simulated study of two chemotherapy agents: Drug A and Drug B
#'
#' A dataset containing the baseline characteristics of 200 patients
#' who received Drug A or Drug B.  Dataset also contains the outcome of
#' tumor response to the treatment.
#'
#' @format A data frame with 200 rows--one row per patient
#' \describe{
#'     \item{trt}{Chemotherapy Treatment}
#'     \item{age}{Age, yrs}
#'     \item{marker}{Marker Level, ng/mL}
#'     \item{stage}{T Stage}
#'     \item{grade}{Grade}
#'     \item{response}{Tumor Response}
#'     \item{death}{Patient Died}
#'     \item{ttdeath}{Months to Death/Censor}
#' }
"trial"
