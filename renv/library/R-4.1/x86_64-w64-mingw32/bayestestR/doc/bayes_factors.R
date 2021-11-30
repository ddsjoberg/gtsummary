## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
options(knitr.kable.NA = "", digits = 2)
knitr::opts_chunk$set(
  echo = TRUE,
  comment = ">",
  out.width = "100%",
  message = FALSE,
  warning = FALSE,
  dpi = 150
)

pkgs <- c(
  "rstanarm", "BayesFactor", "emmeans", "logspline", "lme4", "ggplot2",
  "see", "insight", "emmeans", "knitr", "effectsize", "bayestestR"
)
if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

set.seed(4)

if (require("ggplot2") && require("see")) {
  theme_set(theme_modern())
}

## ----deathsticks_fig, echo=FALSE, fig.cap="Bayesian analysis of the Students' (1908) Sleep data set.", fig.align='center', out.width="80%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/deathsticks.jpg")

## ----sleep_boxplot, echo=FALSE------------------------------------------------
library(ggplot2)

ggplot(sleep, aes(x = group, y = extra, fill = group)) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none")

## ----rstanarm_model, eval = FALSE---------------------------------------------
#  set.seed(123)
#  library(rstanarm)
#  
#  model <- stan_glm(
#    formula = extra ~ group,
#    data = sleep,
#    prior = normal(0, 3, autoscale = FALSE)
#  )

## ---- echo=FALSE--------------------------------------------------------------
model <- stan_glm(
  formula = extra ~ group,
  data = sleep,
  prior = normal(0, 3, autoscale = FALSE),
  refresh = 0
)

## ---- echo=FALSE--------------------------------------------------------------
null <- c(-1, 1)
xrange <- c(-10, 10)

x_vals <- seq(xrange[1], xrange[2], length.out = 400)
d_vals <- dnorm(x_vals, sd = 3)
in_null <- null[1] < x_vals & x_vals < null[2]
range_groups <- rep(0, length(x_vals))
range_groups[!in_null & x_vals < 0] <- -1
range_groups[!in_null & x_vals > 0] <- 1

ggplot(mapping = aes(x_vals, d_vals, fill = in_null, group = range_groups)) +
  geom_area(color = "black", size = 1) +
  scale_fill_flat(name = "", labels = c("Alternative", "Null")) +
  labs(x = "Drug effect", y = "Density") +
  coord_cartesian(ylim = c(0, 0.45)) +
  theme_modern() +
  theme(legend.position = c(0.2, 0.8))

pnull <- diff(pnorm(null, sd = 2.5))
prior_odds <- (1 - pnull) / pnull

## ----rstanarm_fit, echo=FALSE-------------------------------------------------
library(bayestestR)
model_prior <- unupdate(model)
posterior <- insight::get_parameters(model)$group2
prior <- insight::get_parameters(model_prior)$group2

f_post <- logspline::logspline(posterior)

d_vals_post <- logspline::dlogspline(x_vals, f_post)

ggplot(mapping = aes(x_vals, d_vals_post, fill = in_null, group = range_groups)) +
  geom_area(color = "black", size = 1) +
  scale_fill_flat(name = "", labels = c("Alternative", "Null")) +
  labs(x = "Drug effect", y = "Density") +
  coord_cartesian(ylim = c(0, 0.45)) +
  theme_modern() +
  theme(legend.position = c(0.2, 0.8))

My_first_BF <- bayesfactor_parameters(model, null = c(-1, 1), prior = model_prior)

BF <- My_first_BF$BF[2]
post_odds <- prior_odds * BF

med_post <- point_estimate(posterior)$Median

## ---- eval=FALSE--------------------------------------------------------------
#  My_first_BF <- bayesfactor_parameters(model, null = c(-1, 1))
#  My_first_BF

## ---- echo=FALSE--------------------------------------------------------------
print(My_first_BF)

## -----------------------------------------------------------------------------
library(see)
plot(My_first_BF)

## -----------------------------------------------------------------------------
effectsize::interpret_bf(exp(My_first_BF$log_BF[2]), include_value = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  My_second_BF <- bayesfactor_parameters(model, null = 0)
#  My_second_BF

## ---- echo=FALSE--------------------------------------------------------------
My_second_BF <- bayesfactor_parameters(
  data.frame(group2 = posterior),
  data.frame(group2 = prior),
  null = 0
)

print(My_second_BF)

## -----------------------------------------------------------------------------
plot(My_second_BF)

## ----savagedickey_one_sided, eval=FALSE---------------------------------------
#  test_group2_right <- bayesfactor_parameters(model, direction = ">")
#  test_group2_right

## ----prior_n_post_plot_one_sided, echo=FALSE----------------------------------
test_group2_right <- bayesfactor_parameters(
  data.frame(group2 = posterior),
  data.frame(group2 = prior),
  null = 0,
  direction = ">"
)

BF <- test_group2_right$BF

print(test_group2_right)

## -----------------------------------------------------------------------------
plot(test_group2_right)

## ----inteval_div, eval=FALSE--------------------------------------------------
#  test_group2_dividing <- bayesfactor_parameters(model, null = c(-Inf, 0))
#  test_group2_dividing

## ----inteval_div2, echo=FALSE-------------------------------------------------
test_group2_dividing <- bayesfactor_parameters(
  data.frame(group2 = posterior),
  data.frame(group2 = prior),
  null = c(-Inf, 0)
)

print(test_group2_dividing)

## -----------------------------------------------------------------------------
plot(test_group2_dividing)

## -----------------------------------------------------------------------------
my_first_si <- si(
  posterior = data.frame(group2 = posterior),
  prior = data.frame(group2 = prior),
  BF = 1
)

print(my_first_si)

## -----------------------------------------------------------------------------
plot(my_first_si)

## ----brms_disp, eval = FALSE--------------------------------------------------
#  library(brms)
#  
#  # intercept only model
#  m0 <- brm(Sepal.Length ~ 1, data = iris,
#            save_pars = save_pars(all = TRUE),  refresh = 0, backend = "rstan")
#  
#  # main effects models
#  m1 <- brm(Sepal.Length ~ Petal.Length, data = iris,
#            save_pars = save_pars(all = TRUE), refresh = 0, backend = "rstan")
#  
#  m2 <- brm(Sepal.Length ~ Species, data = iris,
#            save_pars = save_pars(all = TRUE), refresh = 0, backend = "rstan")
#  
#  # additive main effects model
#  m3 <- brm(Sepal.Length ~ Species + Petal.Length, data = iris,
#            save_pars = save_pars(all = TRUE), refresh = 0, backend = "rstan")
#  
#  # full model
#  m4 <- brm(Sepal.Length ~ Species * Petal.Length, data = iris,
#            save_pars = save_pars(all = TRUE), refresh = 0, backend = "rstan")

## ----brms_models_disp, eval = FALSE-------------------------------------------
#  library(bayestestR)
#  
#  comparison <- bayesfactor_models(m1, m2, m3, m4, denominator = m0)
#  comparison

## ---- echo = FALSE------------------------------------------------------------
comparison <- structure(
  list(Model = c("Petal.Length", "Species", "Species + Petal.Length", "Species * Petal.Length", "1"), 
       log_BF = c(102.600193981361, 68.5488807613052, 128.665504913167, 128.919384882682, 0)),
  class = c("bayesfactor_models", "see_bayesfactor_models", "data.frame"), 
  row.names = c("m1", "m2", "m3", "m4", "m0"),
  denominator = 5L,
  BF_method = "marginal likelihoods (bridgesampling)",
  unsupported_models = FALSE)
print(comparison)

## ----update_models1-----------------------------------------------------------
update(comparison, reference = 3)

## ----update_models2-----------------------------------------------------------
update(comparison, reference = 2)

## -----------------------------------------------------------------------------
as.matrix(comparison)

## ----lme4_models--------------------------------------------------------------
library(lme4)

# define models with increasing complexity
m0 <- lmer(Sepal.Length ~ (1 | Species), data = iris)
m1 <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
m2 <- lmer(Sepal.Length ~ Petal.Length + (Petal.Length | Species), data = iris)
m3 <- lmer(Sepal.Length ~ Petal.Length + Petal.Width + (Petal.Length | Species), data = iris)
m4 <- lmer(Sepal.Length ~ Petal.Length * Petal.Width + (Petal.Length | Species), data = iris)

# model comparison
bayesfactor_models(m1, m2, m3, m4, denominator = m0)

## -----------------------------------------------------------------------------
iris_model <- stan_glm(Sepal.Length ~ Species + Petal.Length,
  data = iris,
  prior = normal(0, c(2, 1.2, 1.2), autoscale = FALSE),
  refresh = 0
)

## -----------------------------------------------------------------------------
botanist_hypotheses <- c(
  "Petal.Length > 0",
  "(Speciesversicolor > 0) & (Speciesvirginica > 0)"
)

## -----------------------------------------------------------------------------
model_prior <- unupdate(iris_model)

botanist_BFs <- bayesfactor_restricted(
  posterior = iris_model,
  prior = model_prior,
  hypothesis = botanist_hypotheses
)

print(botanist_BFs)

## ----plot_iris, echo=FALSE----------------------------------------------------
ggplot(iris, aes(Petal.Length, Sepal.Length, color = Species)) +
  geom_point() +
  scale_color_flat() +
  theme(legend.position = c(0.2, 0.8))

## ----inclusion_brms-----------------------------------------------------------
bayesfactor_inclusion(comparison)

## ----inclusion_brms2----------------------------------------------------------
bayesfactor_inclusion(comparison, match_models = TRUE)

## ----JASP_all-----------------------------------------------------------------
library(BayesFactor)
data(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

BF_ToothGrowth <- anovaBF(len ~ dose * supp, ToothGrowth, progress = FALSE)

bayesfactor_inclusion(BF_ToothGrowth)

## ----JASP_all_fig, echo=FALSE-------------------------------------------------
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/JASP1.PNG")

## ----JASP_matched-------------------------------------------------------------
bayesfactor_inclusion(BF_ToothGrowth, match_models = TRUE)

## ----JASP_matched_fig, echo=FALSE---------------------------------------------
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/JASP2.PNG")

## ----JASP_Nuisance------------------------------------------------------------
BF_ToothGrowth_against_dose <- BF_ToothGrowth[3:4] / BF_ToothGrowth[2] # OR:
# update(bayesfactor_models(BF_ToothGrowth),
#        subset = c(4, 5),
#        reference = 3)
BF_ToothGrowth_against_dose


bayesfactor_inclusion(BF_ToothGrowth_against_dose)

## ----JASP_Nuisance_fig, echo=FALSE--------------------------------------------
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/JASP3.PNG")

## -----------------------------------------------------------------------------
mod <- stan_glm(mpg ~ wt + am,
  data = mtcars,
  prior = normal(0, c(10, 10), autoscale = FALSE),
  diagnostic_file = file.path(tempdir(), "df1.csv"),
  refresh = 0
)

mod_carb <- stan_glm(mpg ~ wt + am + carb,
  data = mtcars,
  prior = normal(0, c(10, 10, 20), autoscale = FALSE),
  diagnostic_file = file.path(tempdir(), "df0.csv"),
  refresh = 0
)

BF_carb <- bayesfactor_models(mod_carb, denominator = mod, verbose = FALSE)
BF <- BF_carb$BF[1]
print(BF_carb)

## -----------------------------------------------------------------------------
hdi(mod_carb, ci = .95)

## -----------------------------------------------------------------------------
BMA_draws <- weighted_posteriors(mod, mod_carb)

BMA_hdi <- hdi(BMA_draws, ci = .95)
BMA_hdi

plot(BMA_hdi)

## ---- echo=FALSE--------------------------------------------------------------
set.seed(1)

## -----------------------------------------------------------------------------
library(emmeans)

groups <- emmeans(model, ~group)
group_diff <- pairs(groups)

(groups_all <- rbind(groups, group_diff))

# pass the original model via prior
bayesfactor_parameters(groups_all, prior = model)

## ---- echo=FALSE--------------------------------------------------------------
set.seed(1)

## ---- eval=FALSE--------------------------------------------------------------
#  library(modelbased)
#  
#  estimate_contrasts(model, test = "bf")

## -----------------------------------------------------------------------------
df <- iris
contrasts(df$Species)[, ] <- contr.sum(3)

fit_sum <- stan_glm(Sepal.Length ~ Species,
  data = df,
  prior = normal(0, c(1, 1), autoscale = FALSE),
  prior_PD = TRUE, # sample priors
  family = gaussian(),
  refresh = 0
)

pairs_sum <- pairs(emmeans(fit_sum, ~Species))

em_pairs_samples <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(pairs_sum, names = FALSE)))

print(pairs_sum)

ggplot(stack(em_pairs_samples), aes(x = values, fill = ind)) +
  geom_density(size = 1) +
  facet_grid(ind ~ .) +
  labs(x = "prior difference values") +
  theme(legend.position = "none")

## ---- eval=FALSE--------------------------------------------------------------
#  contrasts(df$Species) <- contr.orthonorm

## ---- eval=FALSE--------------------------------------------------------------
#  options(contrasts = c("contr.orthonorm", "contr.poly"))

## -----------------------------------------------------------------------------
contrasts(df$Species)[, ] <- contr.orthonorm(3)

fit_bayes <- stan_glm(Sepal.Length ~ Species,
  data = df,
  prior = normal(0, c(1, 1), autoscale = FALSE),
  prior_PD = TRUE, # sample priors
  family = gaussian(),
  refresh = 0
)

pairs_bayes <- pairs(emmeans(fit_bayes, ~Species))

em_pairs_samples <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(pairs_bayes, names = FALSE)))

print(pairs_bayes)

ggplot(stack(em_pairs_samples), aes(x = values, fill = ind)) +
  geom_density(size = 1) +
  facet_grid(ind ~ .) +
  labs(x = "prior difference values") +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
hyp <- c(
  # comparing 2 levels
  "setosa < versicolor",
  "setosa < virginica",
  "versicolor < virginica",

  # comparing 3 (or more) levels
  "setosa    < virginica  & virginica  < versicolor",
  "virginica < setosa     & setosa     < versicolor",
  "setosa    < versicolor & versicolor < virginica"
)

## ---- eval=FALSE--------------------------------------------------------------
#  contrasts(df$Species) <- contr.sum
#  fit_sum <- stan_glm(Sepal.Length ~ Species,
#    data = df,
#    prior = normal(0, c(1, 1), autoscale = FALSE),
#    family = gaussian()
#  )
#  
#  em_sum <- emmeans(fit_sum, ~Species) # the posterior marginal means
#  
#  bayesfactor_restricted(em_sum, fit_sum, hypothesis = hyp)

## ---- echo=FALSE--------------------------------------------------------------
contrasts(df$Species)[, ] <- contr.sum(3)
fit_sum <- stan_glm(Sepal.Length ~ Species,
  data = df,
  prior = normal(0, c(1, 1), autoscale = FALSE),
  family = gaussian(),
  refresh = 0
)

em_sum <- emmeans(fit_sum, ~Species) # the posterior marginal means

bayesfactor_restricted(em_sum, fit_sum, hypothesis = hyp)

## ---- eval=FALSE--------------------------------------------------------------
#  contrasts(df$Species) <- contr.orthonorm
#  fit_bayes <- stan_glm(Sepal.Length ~ Species,
#    data = df,
#    prior = normal(0, c(1, 1), autoscale = FALSE),
#    family = gaussian()
#  )
#  em_bayes <- emmeans(fit_sum, ~Species) # the posterior marginal means
#  bayesfactor_restricted(em_bayes, fit_sum, hypothesis = hyp)

## ---- echo=FALSE--------------------------------------------------------------
contrasts(df$Species)[, ] <- contr.orthonorm(3)
fit_bayes <- stan_glm(Sepal.Length ~ Species,
  data = df,
  prior = normal(0, c(1, 1), autoscale = FALSE),
  family = gaussian(),
  refresh = 0
)
em_bayes <- emmeans(fit_bayes, ~Species) # the posterior marginal means
bayesfactor_restricted(em_bayes, fit_bayes, hypothesis = hyp)

