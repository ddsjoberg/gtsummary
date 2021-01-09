pkgname <- "broom.helpers"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('broom.helpers')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("model_get_coefficients_type")
### * model_get_coefficients_type

flush(stderr()); flush(stdout())

### Name: model_get_coefficients_type
### Title: Get coefficient type
### Aliases: model_get_coefficients_type
###   model_get_coefficients_type.default model_get_coefficients_type.glm
###   model_get_coefficients_type.negbin model_get_coefficients_type.geeglm
###   model_get_coefficients_type.glmerMod
###   model_get_coefficients_type.clogit model_get_coefficients_type.polr
###   model_get_coefficients_type.multinom
###   model_get_coefficients_type.svyolr model_get_coefficients_type.clm
###   model_get_coefficients_type.clmm model_get_coefficients_type.coxph

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) %>%
  model_get_coefficients_type()

Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
  glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
  model_get_coefficients_type()



cleanEx()
nameEx("model_get_contrasts")
### * model_get_contrasts

flush(stderr()); flush(stdout())

### Name: model_get_contrasts
### Title: Get contrasts used in the model
### Aliases: model_get_contrasts model_get_contrasts.default

### ** Examples

glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) %>%
  model_get_contrasts()



cleanEx()
nameEx("model_get_model")
### * model_get_model

flush(stderr()); flush(stdout())

### Name: model_get_model
### Title: Get the model from model objects
### Aliases: model_get_model model_get_model.default model_get_model.mira

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) %>%
  model_get_model()



cleanEx()
nameEx("model_get_model_frame")
### * model_get_model_frame

flush(stderr()); flush(stdout())

### Name: model_get_model_frame
### Title: Get the model frame of a model
### Aliases: model_get_model_frame model_get_model_frame.default
###   model_get_model_frame.coxph model_get_model_frame.survreg

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) %>%
  model_get_model_frame() %>%
  head()



cleanEx()
nameEx("model_get_model_matrix")
### * model_get_model_matrix

flush(stderr()); flush(stdout())

### Name: model_get_model_matrix
### Title: Get the model matrix of a model
### Aliases: model_get_model_matrix model_get_model_matrix.default
###   model_get_model_matrix.multinom model_get_model_matrix.clm

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) %>%
  model_get_model_matrix() %>%
  head()



cleanEx()
nameEx("model_get_nlevels")
### * model_get_nlevels

flush(stderr()); flush(stdout())

### Name: model_get_nlevels
### Title: Get the number of levels for each factor used in 'xlevels'
### Aliases: model_get_nlevels model_get_nlevels.default

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) %>%
  model_get_nlevels()



cleanEx()
nameEx("model_get_xlevels")
### * model_get_xlevels

flush(stderr()); flush(stdout())

### Name: model_get_xlevels
### Title: Get xlevels used in the model
### Aliases: model_get_xlevels model_get_xlevels.default
###   model_get_xlevels.lmerMod model_get_xlevels.glmerMod

### ** Examples

lm(hp ~ mpg + factor(cyl), mtcars) %>%
  model_get_xlevels()



cleanEx()
nameEx("model_identify_variables")
### * model_identify_variables

flush(stderr()); flush(stdout())

### Name: model_identify_variables
### Title: Identify for each coefficient of a model the corresponding
###   variable
### Aliases: model_identify_variables model_identify_variables.default
###   model_identify_variables.lavaan model_identify_variables.aov
###   model_identify_variables.clm model_identify_variables.clmm

### ** Examples

Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
  glm(
    Survived ~ Class + Age * Sex,
    data = ., weights = .$n,
    family = binomial
  ) %>%
  model_identify_variables()

iris %>%
  lm(
    Sepal.Length ~ poly(Sepal.Width, 2) + Species,
    data = .,
    contrasts = list(Species = contr.sum)
  ) %>%
  model_identify_variables()



cleanEx()
nameEx("model_list_contrasts")
### * model_list_contrasts

flush(stderr()); flush(stdout())

### Name: model_list_contrasts
### Title: List contrasts used by a model
### Aliases: model_list_contrasts model_list_contrasts.default

### ** Examples

glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) %>%
  model_list_contrasts()



cleanEx()
nameEx("model_list_terms_levels")
### * model_list_terms_levels

flush(stderr()); flush(stdout())

### Name: model_list_terms_levels
### Title: List levels of categorical terms
### Aliases: model_list_terms_levels model_list_terms_levels.default

### ** Examples

glm(
  am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial,
  contrasts = list(`factor(cyl)` = contr.sum)
) %>%
  model_list_terms_levels()

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

mod <- df %>%
  glm(
    Survived ~ Class + Age + Sex,
    data = ., weights = .$n, family = binomial,
    contrasts = list(Age = contr.sum, Class = "contr.helmert")
  )
mod %>% model_list_terms_levels()
mod %>% model_list_terms_levels("{level} vs {reference_level}")
mod %>% model_list_terms_levels("{variable} [{level} - {reference_level}]")
mod %>% model_list_terms_levels(
  "{ifelse(reference, level, paste(level, '-', reference_level))}"
)



cleanEx()
nameEx("model_list_variables")
### * model_list_variables

flush(stderr()); flush(stdout())

### Name: model_list_variables
### Title: List all the variables used in a model
### Aliases: model_list_variables model_list_variables.default
###   model_list_variables.lavaan

### ** Examples

Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
  glm(
    Survived ~ Class + Age : Sex,
    data = ., weights = .$n,
    family = binomial
  ) %>%
  model_list_variables()

iris %>%
  lm(
    Sepal.Length ~ poly(Sepal.Width, 2) + Species,
    data = .,
    contrasts = list(Species = contr.sum)
  ) %>%
  model_list_variables()

if (requireNamespace("gtsummary")) {
  glm(
    response ~ poly(age, 3) + stage + grade * trt,
    na.omit(gtsummary::trial),
    family = binomial,
  ) %>%
    model_list_variables()
}



cleanEx()
nameEx("select_helpers")
### * select_helpers

flush(stderr()); flush(stdout())

### Name: select_helpers
### Title: Select helper functions
### Aliases: select_helpers all_continuous all_dichotomous all_categorical
###   all_interaction all_intercepts all_contrasts

### ** Examples

glm(response ~ age * trt + grade, gtsummary::trial, family = binomial) %>%
  tidy_plus_plus(exponentiate = TRUE, include = all_categorical())

glm(response ~ age + trt + grade + stage,
    gtsummary::trial,
    family = binomial,
    contrasts = list(trt = contr.SAS, grade = contr.sum, stage = contr.poly)) %>%
tidy_plus_plus(exponentiate = TRUE,
               include = all_contrasts(c("treatment", "sum")))



cleanEx()
nameEx("tidy_add_coefficients_type")
### * tidy_add_coefficients_type

flush(stderr()); flush(stdout())

### Name: tidy_add_coefficients_type
### Title: Add coefficients type and label as attributes
### Aliases: tidy_add_coefficients_type

### ** Examples

ex1 <- lm(hp ~ mpg + factor(cyl), mtcars) %>%
  tidy_and_attach() %>%
  tidy_add_coefficients_type()
attr(ex1, "coefficients_type")
attr(ex1, "coefficients_label")

ex2 <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
  glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
  tidy_and_attach(exponentiate = TRUE) %>%
  tidy_add_coefficients_type()
attr(ex2, "coefficients_type")
attr(ex2, "coefficients_label")



cleanEx()
nameEx("tidy_add_contrasts")
### * tidy_add_contrasts

flush(stderr()); flush(stdout())

### Name: tidy_add_contrasts
### Title: Add contrasts type for categorical variables
### Aliases: tidy_add_contrasts

### ** Examples

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

df %>%
  glm(
    Survived ~ Class + Age + Sex,
    data = ., weights = .$n, family = binomial,
    contrasts = list(Age = contr.sum, Class = "contr.helmert")
  ) %>%
  tidy_and_attach() %>%
  tidy_add_contrasts()



cleanEx()
nameEx("tidy_add_estimate_to_reference_rows")
### * tidy_add_estimate_to_reference_rows

flush(stderr()); flush(stdout())

### Name: tidy_add_estimate_to_reference_rows
### Title: Add an estimate value to references rows for categorical
###   variables
### Aliases: tidy_add_estimate_to_reference_rows

### ** Examples

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(dplyr::across(where(is.character), factor))

df %>%
  glm(
    Survived ~ Class + Age + Sex,
    data = ., weights = .$n, family = binomial,
    contrasts = list(Age = contr.sum, Class = "contr.SAS")
  ) %>%
  tidy_and_attach(exponentiate = TRUE) %>%
  tidy_add_reference_rows() %>%
  tidy_add_estimate_to_reference_rows()

if (requireNamespace("gtsummary")) {
  glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, base = 3),
      grade = contr.treatment(3, base = 2),
      trt = contr.treatment(2, base = 2)
    )
  ) %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_estimate_to_reference_rows()
}



cleanEx()
nameEx("tidy_add_header_rows")
### * tidy_add_header_rows

flush(stderr()); flush(stdout())

### Name: tidy_add_header_rows
### Title: Add header rows variables with several terms
### Aliases: tidy_add_header_rows

### ** Examples

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

res <- df %>%
  glm(
    Survived ~ Class + Age + Sex,
    data = ., weights = .$n, family = binomial,
    contrasts = list(Age = contr.sum, Class = "contr.SAS")
  ) %>%
  tidy_and_attach() %>%
  tidy_add_variable_labels(labels = list(Class = "Custom label for Class")) %>%
  tidy_add_reference_rows()
res %>% tidy_add_header_rows()
res %>% tidy_add_header_rows(show_single_row = all_dichotomous())

if (requireNamespace("gtsummary")) {
  glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, base = 3),
      grade = contr.treatment(3, base = 2),
      trt = contr.treatment(2, base = 2)
    )
  ) %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows() %>%
    tidy_add_header_rows()
}



cleanEx()
nameEx("tidy_add_reference_rows")
### * tidy_add_reference_rows

flush(stderr()); flush(stdout())

### Name: tidy_add_reference_rows
### Title: Add references rows for categorical variables
### Aliases: tidy_add_reference_rows

### ** Examples

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

res <- df %>%
  glm(
    Survived ~ Class + Age + Sex,
    data = ., weights = .$n, family = binomial,
    contrasts = list(Age = contr.sum, Class = "contr.SAS")
  ) %>%
  tidy_and_attach()
res %>% tidy_add_reference_rows()
res %>% tidy_add_reference_rows(no_reference_row = all_dichotomous())
res %>% tidy_add_reference_rows(no_reference_row = "Class")

if (requireNamespace("gtsummary")) {
  glm(
    response ~ stage + grade * trt,
    gtsummary::trial,
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, base = 3),
      grade = contr.treatment(3, base = 2),
      trt = contr.treatment(2, base = 2)
    )
  ) %>%
    tidy_and_attach() %>%
    tidy_add_reference_rows()
}



cleanEx()
nameEx("tidy_add_term_labels")
### * tidy_add_term_labels

flush(stderr()); flush(stdout())

### Name: tidy_add_term_labels
### Title: Add term labels
### Aliases: tidy_add_term_labels

### ** Examples

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Sex"
  )

mod <- df %>%
  glm(Survived ~ Class * Age * Sex, data = ., weights = .$n, family = binomial)
mod %>%
  tidy_and_attach() %>%
  tidy_add_term_labels()
mod %>%
  tidy_and_attach() %>%
  tidy_add_term_labels(
    interaction_sep = " x ",
    categorical_terms_pattern = "{level} / {reference_level}"
  )



cleanEx()
nameEx("tidy_add_variable_labels")
### * tidy_add_variable_labels

flush(stderr()); flush(stdout())

### Name: tidy_add_variable_labels
### Title: Add variable labels
### Aliases: tidy_add_variable_labels

### ** Examples

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Sex"
  )

df %>%
  glm(Survived ~ Class * Age * Sex, data = ., weights = .$n, family = binomial) %>%
  tidy_and_attach() %>%
  tidy_add_variable_labels(
    labels = list(
      "(Intercept)" = "Custom intercept",
      Sex = "Gender",
      "Class:Age" = "Custom label"
    )
  )



cleanEx()
nameEx("tidy_attach_model")
### * tidy_attach_model

flush(stderr()); flush(stdout())

### Name: tidy_attach_model
### Title: Attach a full model to the tibble of model terms
### Aliases: tidy_attach_model tidy_and_attach tidy_get_model
###   tidy_detach_model

### ** Examples

mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
tt <- mod %>%
  tidy_and_attach(conf.int = TRUE)
tt
tidy_get_model(tt)



cleanEx()
nameEx("tidy_identify_variables")
### * tidy_identify_variables

flush(stderr()); flush(stdout())

### Name: tidy_identify_variables
### Title: Identify the variable corresponding to each model coefficient
### Aliases: tidy_identify_variables

### ** Examples

Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
  glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
  tidy_and_attach() %>%
  tidy_identify_variables()

lm(
  Sepal.Length ~ poly(Sepal.Width, 2) + Species,
  data = iris,
  contrasts = list(Species = contr.sum)
) %>%
  tidy_and_attach(conf.int = TRUE) %>%
  tidy_identify_variables()



cleanEx()
nameEx("tidy_plus_plus")
### * tidy_plus_plus

flush(stderr()); flush(stdout())

### Name: tidy_plus_plus
### Title: Tidy a model and compute additional informations
### Aliases: tidy_plus_plus

### ** Examples

ex1 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris) %>%
  tidy_plus_plus()
ex1

df <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    Survived = factor(Survived, c("No", "Yes"))
  ) %>%
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Gender"
  )

ex2 <- glm(
  Survived ~ Class + Age * Sex,
  data = df, weights = df$n,
  family = binomial
) %>%
  tidy_plus_plus(
    exponentiate = TRUE,
    add_reference_rows = FALSE,
    categorical_terms_pattern = "{level} / {reference_level}"
  )
ex2

if (requireNamespace("gtsummary")) {
  ex3 <- glm(
    response ~ poly(age, 3) + stage + grade * trt,
    na.omit(gtsummary::trial),
    family = binomial,
    contrasts = list(
      stage = contr.treatment(4, base = 3),
      grade = contr.sum
    )
  ) %>%
    tidy_plus_plus(
      exponentiate = TRUE,
      variable_labels = c(age = "Age (in years)"),
      add_header_rows = TRUE,
      show_single_row = all_dichotomous(),
      term_labels = c("poly(age, 3)3" = "Cubic age"),
      keep_model = TRUE
    )
  ex3
}



cleanEx()
nameEx("tidy_remove_intercept")
### * tidy_remove_intercept

flush(stderr()); flush(stdout())

### Name: tidy_remove_intercept
### Title: Remove intercept(s)
### Aliases: tidy_remove_intercept

### ** Examples

Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived)) %>%
  glm(Survived ~ Class + Age + Sex, data = ., weights = .$n, family = binomial) %>%
  tidy_and_attach() %>%
  tidy_remove_intercept()



cleanEx()
nameEx("tidy_select_variables")
### * tidy_select_variables

flush(stderr()); flush(stdout())

### Name: tidy_select_variables
### Title: Select variables to keep/drop
### Aliases: tidy_select_variables

### ** Examples

res <- Titanic %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Survived = factor(Survived)) %>%
  glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
  tidy_and_attach() %>%
  tidy_identify_variables()

res
res %>% tidy_select_variables()
res %>% tidy_select_variables(include = "Class")
res %>% tidy_select_variables(include = -c("Age", "Sex"))
res %>% tidy_select_variables(include = starts_with("A"))
res %>% tidy_select_variables(include = all_categorical())
res %>% tidy_select_variables(include = all_dichotomous())
res %>% tidy_select_variables(include = all_interaction())
res %>% tidy_select_variables(
  include = c("Age", all_categorical(dichotomous = FALSE), all_interaction())
)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
