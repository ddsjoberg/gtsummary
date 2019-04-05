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
<<<<<<< HEAD
    response = runif(n) < response_prob,
    ttdeath_true =
      exp(1 + 0.2 * response +
            -0.1 * as.numeric(stage) +
            -0.1 * as.numeric(grade) +
            rnorm(n, sd = 0.5)) * 12,
    death = ifelse(ttdeath_true <= 24, 1L, 0L),
    ttdeath = pmin(ttdeath_true, 24) %>% round(digits = 2)
=======
    response = runif(n) < response_prob
>>>>>>> origin/master
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
<<<<<<< HEAD
attr(trial$death, "label") <- "Patient Died"
attr(trial$ttdeath, "label") <- "Months from Randomization to Death/Censor"
=======
>>>>>>> origin/master

usethis::use_data(trial, overwrite = TRUE)
