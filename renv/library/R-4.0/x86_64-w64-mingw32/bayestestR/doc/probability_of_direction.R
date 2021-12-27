## ----message=FALSE, warning=FALSE, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
if (!requireNamespace("see", quietly = TRUE) ||
  !requireNamespace("dplyr", quietly = TRUE) ||
  !requireNamespace("ggplot2", quietly = TRUE) ||
  !requireNamespace("tidyr", quietly = TRUE) ||
  !requireNamespace("logspline", quietly = TRUE) ||
  !requireNamespace("KernSmooth", quietly = TRUE) ||
  !requireNamespace("GGally", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

library(knitr)
options(knitr.kable.NA = "")
knitr::opts_chunk$set(comment = ">")
options(digits = 2)

set.seed(333)

## ----message=FALSE, warning=FALSE, echo=FALSE, fig.cap="Correlation between the frequentist p-value and the probability of direction (pd)", fig.align='center'----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(see)

read.csv("https://raw.github.com/easystats/easystats/master/publications/makowski_2019_bayesian/data/data.csv") %>%
  mutate(
    effect_existence = ifelse(true_effect == 1, "Presence of true effect", "Absence of true effect"),
    p_direction = p_direction * 100
  ) %>%
  ggplot(aes(x = p_direction, y = p_value, color = effect_existence)) +
  geom_point2(alpha = 0.1) +
  geom_segment(aes(x = 95, y = Inf, xend = 95, yend = 0.1), color = "black", linetype = "longdash") +
  geom_segment(aes(x = -Inf, y = 0.1, xend = 95, yend = 0.1), color = "black", linetype = "longdash") +
  geom_segment(aes(x = 97.5, y = Inf, xend = 97.5, yend = 0.05), color = "black", linetype = "dashed") +
  geom_segment(aes(x = -Inf, y = 0.05, xend = 97.5, yend = 0.05), color = "black", linetype = "dashed") +
  theme_modern() +
  scale_y_reverse(breaks = c(0.05, round(seq(0, 1, length.out = 11), digits = 2))) +
  scale_x_continuous(breaks = c(95, 97.5, round(seq(50, 100, length.out = 6)))) +
  scale_color_manual(values = c("Presence of true effect" = "green", "Absence of true effect" = "red")) +
  theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("Probability of Direction (pd)") +
  ylab("Frequentist p-value")

## ----message=FALSE, warning=FALSE, fig.align='center'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(bayestestR)
library(logspline)
library(KernSmooth)

# Compute the correlations
data <- data.frame()
for (the_mean in runif(25, 0, 4)) {
  for (the_sd in runif(25, 0.5, 4)) {
    x <- rnorm(100, the_mean, abs(the_sd))
    data <- rbind(
      data,
      data.frame(
        "direct" = pd(x),
        "kernel" = pd(x, method = "kernel"),
        "logspline" = pd(x, method = "logspline"),
        "KernSmooth" = pd(x, method = "KernSmooth")
      )
    )
  }
}
data <- as.data.frame(sapply(data, as.numeric))

# Visualize the correlations
library(ggplot2)
library(GGally)

GGally::ggpairs(data) +
  theme_classic()

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data <- data.frame()
for (i in 1:25) {
  the_mean <- runif(1, 0, 4)
  the_sd <- abs(runif(1, 0.5, 4))
  parent_distribution <- rnorm(100000, the_mean, the_sd)
  true_pd <- pd(parent_distribution)

  for (j in 1:25) {
    sample_size <- round(runif(1, 25, 5000))
    subsample <- sample(parent_distribution, sample_size)
    data <- rbind(
      data,
      data.frame(
        "sample_size" = sample_size,
        "true" = true_pd,
        "direct" = pd(subsample) - true_pd,
        "kernel" = pd(subsample, method = "kernel") - true_pd,
        "logspline" = pd(subsample, method = "logspline") - true_pd,
        "KernSmooth" = pd(subsample, method = "KernSmooth") - true_pd
      )
    )
  }
}
data <- as.data.frame(sapply(data, as.numeric))

## ----message=FALSE, warning=FALSE, fig.align='center'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)

data %>%
  tidyr::gather(Method, Distance, -sample_size, -true) %>%
  ggplot(aes(x = sample_size, y = Distance, color = Method, fill = Method)) +
  geom_point(alpha = 0.3, stroke = 0, shape = 16) +
  geom_smooth(alpha = 0.2) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  xlab("\nDistribution Size")

