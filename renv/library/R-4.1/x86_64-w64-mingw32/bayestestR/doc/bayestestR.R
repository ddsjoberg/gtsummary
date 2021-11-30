## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)

knitr::opts_chunk$set(comment = ">")
options(knitr.kable.NA = "")
options(digits = 2)

if (!requireNamespace("rstanarm", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(rstanarm)
  library(bayestestR)
}

## ----echo=FALSE, fig.cap="Accurate depiction of a regular Bayesian user estimating a credible interval.", fig.align='center', out.width="50%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/bayesianMaster.jpg")

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  install.packages("remotes")
#  remotes::install_github("easystats/easystats")

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  install.packages("rstanarm")
#  library(rstanarm)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  model <- lm(Sepal.Length ~ Petal.Length, data = iris)
#  summary(model)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
model <- lm(Sepal.Length ~ Petal.Length, data = iris)
summary(model)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris)
#  posteriors <- describe_posterior(model)
#  # for a nicer table
#  print_md(posteriors, digits = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
set.seed(333)
model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris, refresh = 0)
posteriors <- describe_posterior(model)
# for a nicer table
print_md(posteriors, digits = 2)

