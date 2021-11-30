## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

if (!requireNamespace("ggplot2", quietly = TRUE) || 
    !requireNamespace("logspline", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

set.seed(333)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(bayestestR)
library(dplyr)
library(ggplot2)

# Generate a normal distribution
posterior <- distribution_normal(1000)

# Compute HDI and ETI
ci_hdi <- ci(posterior, method = "HDI")
ci_eti <- ci(posterior, method = "ETI")

# Plot the distribution and add the limits of the two CIs
posterior %>% 
  estimate_density(extend=TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "orange") +
  theme_classic() +
  # HDI in blue
  geom_vline(xintercept = ci_hdi$CI_low, color = "royalblue", size = 3) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "royalblue", size = 3) +
  # Quantile in red
  geom_vline(xintercept = ci_eti$CI_low, color = "red", size = 1) +
  geom_vline(xintercept = ci_eti$CI_high, color = "red", size = 1)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Generate a beta distribution
posterior <- distribution_beta(1000, 6, 2)

# Compute HDI and Quantile CI
ci_hdi <- ci(posterior, method = "HDI")
ci_eti <- ci(posterior, method = "ETI")

# Plot the distribution and add the limits of the two CIs
posterior %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "orange") +
  theme_classic() +
  # HDI in blue
  geom_vline(xintercept = ci_hdi$CI_low, color = "royalblue", size = 3) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "royalblue", size = 3) +
  # ETI in red
  geom_vline(xintercept = ci_eti$CI_low, color = "red", size = 1) +
  geom_vline(xintercept = ci_eti$CI_high, color = "red", size = 1)

## ----warning=FALSE, message=FALSE---------------------------------------------
prior <- distribution_normal(1000, mean = 0, sd = 1)
posterior <- distribution_normal(1000, mean = .5, sd = .3)

si_1 <- si(posterior, prior, BF = 1)
si_3 <- si(posterior, prior, BF = 3)

ggplot(mapping = aes(x = x, y = y)) +
  theme_classic() +
  # The posterior
  geom_area(fill = "orange",
            data = estimate_density(posterior, extend = TRUE)) +
  # The prior
  geom_area(color = "black", fill = NA, size = 1, linetype = "dashed",
            data = estimate_density(prior, extend = TRUE)) +
  # BF = 1 SI in blue
  geom_vline(xintercept = si_1$CI_low, color = "royalblue", size = 1) +
  geom_vline(xintercept = si_1$CI_high, color = "royalblue", size = 1) +
  # BF = 3 SI in red
  geom_vline(xintercept = si_3$CI_low, color = "red", size = 1) +
  geom_vline(xintercept = si_3$CI_high, color = "red", size = 1)

