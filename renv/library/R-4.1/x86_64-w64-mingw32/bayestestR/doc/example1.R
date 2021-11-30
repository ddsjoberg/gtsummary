## ---- include=FALSE-----------------------------------------------------------
library(knitr)
library(insight)
options(knitr.kable.NA = "")
knitr::opts_chunk$set(
  comment = ">",
  message = FALSE,
  warning = FALSE,
  out.width = "100%"
)

options(digits = 2)

set.seed(333)

if (!requireNamespace("rstanarm", quietly = TRUE) ||
  !requireNamespace("dplyr", quietly = TRUE) ||
  !requireNamespace("ggplot2", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

format_percent <- function(x, digits = 0, ...) {
  paste0(format_value(x * 100, digits = digits, ...), "%")
}

## -----------------------------------------------------------------------------
library(rstanarm)
library(bayestestR)
library(insight)

## -----------------------------------------------------------------------------
model <- lm(Sepal.Length ~ Petal.Length, data = iris)
summary(model)

## -----------------------------------------------------------------------------
insight::get_parameters(model)

## -----------------------------------------------------------------------------
library(ggplot2) # Load the package

# The ggplot function takes the data as argument, and then the variables
# related to aesthetic features such as the x and y axes.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point() + # This adds the points
  geom_smooth(method = "lm") # This adds a regression line

## ---- eval=FALSE--------------------------------------------------------------
#  model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris)

## ----echo=FALSE, comment=NA, results='hide'-----------------------------------
library(rstanarm)
set.seed(333)

model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris)

## ---- eval=FALSE--------------------------------------------------------------
#  posteriors <- insight::get_parameters(model)
#  
#  head(posteriors) # Show the first 6 rows

## ---- echo=FALSE--------------------------------------------------------------
posteriors <- insight::get_parameters(model)

head(posteriors) # Show the first 6 rows

## -----------------------------------------------------------------------------
nrow(posteriors) # Size (number of rows)

## ---- eval=FALSE--------------------------------------------------------------
#  model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris, chains = 2, iter = 1000, warmup = 250)
#  
#  nrow(insight::get_parameters(model)) # Size (number of rows)

## ----echo=FALSE, , comment=NA, echo=FALSE-------------------------------------
model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris, chains = 2, iter = 1000, warmup = 250, refresh = 0)
nrow(insight::get_parameters(model)) # Size (number of rows)

## -----------------------------------------------------------------------------
ggplot(posteriors, aes(x = Petal.Length)) +
  geom_density(fill = "orange")

## -----------------------------------------------------------------------------
mean(posteriors$Petal.Length)

## -----------------------------------------------------------------------------
median(posteriors$Petal.Length)

## -----------------------------------------------------------------------------
map_estimate(posteriors$Petal.Length)

## -----------------------------------------------------------------------------
ggplot(posteriors, aes(x = Petal.Length)) +
  geom_density(fill = "orange") +
  # The mean in blue
  geom_vline(xintercept = mean(posteriors$Petal.Length), color = "blue", size = 1) +
  # The median in red
  geom_vline(xintercept = median(posteriors$Petal.Length), color = "red", size = 1) +
  # The MAP in purple
  geom_vline(xintercept = map_estimate(posteriors$Petal.Length), color = "purple", size = 1)

## -----------------------------------------------------------------------------
range(posteriors$Petal.Length)

## -----------------------------------------------------------------------------
hdi(posteriors$Petal.Length, ci = 0.89)

## -----------------------------------------------------------------------------
library(dplyr)

# We keep only rows for which feed is meatmeal or sunflower
data <- filter(chickwts, feed %in% c("meatmeal", "sunflower"))

## ---- eval=FALSE--------------------------------------------------------------
#  model <- stan_glm(weight ~ feed, data = data)

## ----echo=FALSE, , comment=NA, results='hide'---------------------------------
model <- stan_glm(weight ~ feed, data = data)

## -----------------------------------------------------------------------------
posteriors <- insight::get_parameters(model)

ggplot(posteriors, aes(x = feedsunflower)) +
  geom_density(fill = "red")

## -----------------------------------------------------------------------------
median(posteriors$feedsunflower)
hdi(posteriors$feedsunflower)

## -----------------------------------------------------------------------------
rope(posteriors$feedsunflower, range = c(-20, 20), ci = 0.89)

## ----echo=FALSE, fig.cap="Prof. Sanders giving default values to define the Region of Practical Equivalence (ROPE).", fig.align='center', out.width="75%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/profsanders.png")

## -----------------------------------------------------------------------------
rope_value <- 0.1 * sd(data$weight)
rope_range <- c(-rope_value, rope_value)
rope_range

## -----------------------------------------------------------------------------
rope_value <- rope_range(model)
rope_value

## -----------------------------------------------------------------------------
rope(posteriors$feedsunflower, range = rope_range, ci = 0.89)

## -----------------------------------------------------------------------------
n_positive <- posteriors %>%
  filter(feedsunflower > 0) %>% # select only positive values
  nrow() # Get length

n_positive / nrow(posteriors) * 100

## -----------------------------------------------------------------------------
p_direction(posteriors$feedsunflower)

## ---- eval=TRUE---------------------------------------------------------------
pd <- 97.82
onesided_p <- 1 - pd / 100
twosided_p <- onesided_p * 2
twosided_p

## -----------------------------------------------------------------------------
summary(lm(weight ~ feed, data = data))

## -----------------------------------------------------------------------------
describe_posterior(model, test = c("p_direction", "rope", "bayesfactor"))

