## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)

## ----initial-plot-------------------------------------------------------------
ggplot(starwars, aes(x = hair_color)) + 
  geom_bar() + 
  coord_flip()

## ----fct-infreq-hair----------------------------------------------------------
ggplot(starwars, aes(x = fct_infreq(hair_color))) + 
  geom_bar() + 
  coord_flip()

## -----------------------------------------------------------------------------
starwars %>%
  count(skin_color, sort = TRUE)

## -----------------------------------------------------------------------------
starwars %>%
  mutate(skin_color = fct_lump(skin_color, n = 5)) %>%
  count(skin_color, sort = TRUE)

## -----------------------------------------------------------------------------
starwars %>%
  mutate(skin_color = fct_lump(skin_color, prop = .1)) %>%
  count(skin_color, sort = TRUE)

## -----------------------------------------------------------------------------
starwars %>%
  mutate(skin_color = fct_lump(skin_color, prop = .1, other_level = "extra")) %>%
  count(skin_color, sort = TRUE)

## ----fct-lump-mean------------------------------------------------------------
avg_mass_eye_color <- starwars %>%
  mutate(eye_color = fct_lump(eye_color, n = 6)) %>%
  group_by(eye_color) %>%
  summarise(mean_mass = mean(mass, na.rm = TRUE))

avg_mass_eye_color

## ----fct-reorder--------------------------------------------------------------
avg_mass_eye_color %>%
  mutate(eye_color = fct_reorder(eye_color, mean_mass)) %>%
  ggplot(aes(x = eye_color, y = mean_mass)) + 
  geom_col()

## -----------------------------------------------------------------------------
gss_cat %>%
  count(rincome)

## -----------------------------------------------------------------------------
levels(gss_cat$rincome)

## -----------------------------------------------------------------------------
reshuffled_income <- gss_cat$rincome %>%
  fct_shuffle()

levels(reshuffled_income)

## -----------------------------------------------------------------------------
fct_relevel(reshuffled_income, c("Lt $1000", "$1000 to 2999")) %>%
  levels()

## -----------------------------------------------------------------------------
fct_relevel(reshuffled_income, c("Lt $1000", "$1000 to 2999"), after = 1) %>%
  levels()

