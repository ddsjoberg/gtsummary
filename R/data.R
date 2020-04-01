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
