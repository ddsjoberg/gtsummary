## ---- echo = FALSE, results = "hide", message = FALSE-------------------------
require("emmeans")
knitr::opts_chunk$set(fig.width = 4.5, class.output = "ro")

## -----------------------------------------------------------------------------
pigs.lm1 <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.rg <- ref_grid(pigs.lm1)
pigs.emm.s <- emmeans(pigs.rg, "source")

## -----------------------------------------------------------------------------
test(pigs.emm.s)

## -----------------------------------------------------------------------------
test(pigs.emm.s, null = log(40), side = ">")

## -----------------------------------------------------------------------------
confint(pigs.emm.s, calc = c(n = ~.wgt.))

## -----------------------------------------------------------------------------
test(pigs.emm.s, null = log(40), side = ">", type = "response")

## -----------------------------------------------------------------------------
confint(pigs.emm.s, side = ">", level = .90, type = "response")

## -----------------------------------------------------------------------------
confint(pigs.emm.s, adjust = "tukey")

## -----------------------------------------------------------------------------
test(pigs.emm.s, null = log(40), side = ">", adjust = "bonferroni")

## -----------------------------------------------------------------------------
confint(pigs.rg, by = "source")

## ----eval = FALSE-------------------------------------------------------------
#  emmeans(pigs.lm, ~ percent | source)     ### same results as above
#  summary(.Last.value, by = percent)       ### grouped the other way

## -----------------------------------------------------------------------------
pigsint.lm <- lm(log(conc) ~ source * factor(percent), data = pigs)
pigsint.rg <- ref_grid(pigsint.lm)
contrast(pigsint.rg, "consec", simple = "percent")

## -----------------------------------------------------------------------------
pigs.prs.s <- pairs(pigs.emm.s)
pigs.prs.s

## -----------------------------------------------------------------------------
test(pigs.prs.s, joint = TRUE)

## -----------------------------------------------------------------------------
joint_tests(pigsint.rg)

## -----------------------------------------------------------------------------
joint_tests(pigsint.rg, by = "source")

## -----------------------------------------------------------------------------
test(pigs.prs.s, delta = log(1.25), adjust = "none")

