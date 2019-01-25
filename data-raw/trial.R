# Results from a simulated trial of Placebo vs Drug

set.seed(8976)
n <- 200
trial <-
  dplyr::data_frame(
    trt = ifelse(runif(n) < 0.5, "Placebo", "Drug"),
    age = rnorm(n, mean = 50, sd = 15) %>% as.integer(),
    marker = rgamma(n, 1, 1) %>% round(digits = 3),
    stage = sample(c("T1", "T2", "T3", "T4"), size = n, replace = TRUE) %>% factor(),
    grade = sample(c("I", "II", "III"), size = n, replace = TRUE) %>% factor(),
    response_prob =
      ((trt == "Drug") - 0.2 * as.numeric(stage) - 0.1 * as.numeric(grade) + 0.1 * marker) %>% {
        1 / (1 + exp(-.))
      },
    response = runif(n) < response_prob
  ) %>%
  dplyr::mutate(
    age = ifelse(runif(n) < 0.95, age, NA_real_),
    marker = ifelse(runif(n) < 0.95, marker, NA_real_),
    response = ifelse(runif(n) < 0.95, response, NA_integer_)
  ) %>%
  dplyr::select(-dplyr::one_of("response_prob"))
summary(trial)

attr(trial$trt, "label") <- "Treatment Randomization"
attr(trial$age, "label") <- "Age, yrs"
attr(trial$marker, "label") <- "Marker Level, ng/mL"
attr(trial$stage, "label") <- "T Stage"
attr(trial$grade, "label") <- "Grade"
attr(trial$response, "label") <- "Tumor Response"

usethis::use_data(trial, overwrite = TRUE)
