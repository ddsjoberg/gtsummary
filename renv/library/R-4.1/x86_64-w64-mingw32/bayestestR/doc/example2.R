## ---- include=FALSE-----------------------------------------------------------
if (!requireNamespace("see", quietly = TRUE) ||
  !requireNamespace("dplyr", quietly = TRUE) ||
  !requireNamespace("ggplot2", quietly = TRUE) ||
  !requireNamespace("performance", quietly = TRUE) ||
  !requireNamespace("BayesFactor", quietly = TRUE) ||
  !requireNamespace("rstanarm", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

data(iris)
library(knitr)
library(bayestestR)
options(knitr.kable.NA = "")
knitr::opts_chunk$set(
  comment = ">",
  message = FALSE,
  warning = FALSE,
  out.width = "100%"
)

options(digits = 2)

set.seed(333)

## -----------------------------------------------------------------------------
result <- cor.test(iris$Sepal.Width, iris$Sepal.Length)
result

## ---- results='hide'----------------------------------------------------------
library(BayesFactor)
result <- correlationBF(iris$Sepal.Width, iris$Sepal.Length)

## -----------------------------------------------------------------------------
describe_posterior(result)

## -----------------------------------------------------------------------------
bayesfactor(result)

## ----echo=FALSE, fig.cap="Wagenmakers' pizza poking analogy. From the great <www.bayesianspectacles.org> blog.", fig.align='center', out.width="80%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/LetsPokeAPizza.jpg")

## -----------------------------------------------------------------------------
library(see)

plot(bayesfactor(result)) +
  scale_fill_pizza()

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Select only two relevant species
data <- iris %>%
  filter(Species != "setosa") %>%
  droplevels()

# Visualise distributions and observations
data %>%
  ggplot(aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

## -----------------------------------------------------------------------------
result <- BayesFactor::ttestBF(formula = Sepal.Width ~ Species, data = data)
describe_posterior(result)

## -----------------------------------------------------------------------------
library(rstanarm)

model <- stan_glm(Species ~ Sepal.Width, data = data, family = "binomial", refresh = 0)

## -----------------------------------------------------------------------------
library(modelbased)

vizdata <- estimate_relation(model)

ggplot(vizdata, aes(x = Sepal.Width, y = Predicted)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.5) +
  geom_line() + 
  ylab("Probability of being virginica") +
  theme_modern()

## -----------------------------------------------------------------------------
describe_posterior(model, test = c("pd", "ROPE", "BF"))

## -----------------------------------------------------------------------------
library(performance)

model_performance(model)

## -----------------------------------------------------------------------------
library(see)

plot(rope(result))

