## ---- echo = FALSE, results = "hide", message = FALSE---------------------------------------------
require("emmeans")
knitr::opts_chunk$set(fig.width = 4.5, class.output = "ro")

## -------------------------------------------------------------------------------------------------
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)

## -------------------------------------------------------------------------------------------------
pigs.emm.s <- emmeans(pigs.lm, "source")
str(pigs.emm.s)

## -------------------------------------------------------------------------------------------------
summary(pigs.emm.s, infer = TRUE, null = log(35))

## -------------------------------------------------------------------------------------------------
summary(pigs.emm.s, infer = TRUE, null = log(35), type = "response")

## -------------------------------------------------------------------------------------------------
str(regrid(pigs.emm.s))

summary(regrid(pigs.emm.s), infer = TRUE, null = 35)

## -------------------------------------------------------------------------------------------------
pigs.rg <- ref_grid(pigs.lm)
pigs.remm.s <- emmeans(regrid(pigs.rg), "source")
summary(pigs.remm.s, infer = TRUE, null = 35)

## ----eval = FALSE---------------------------------------------------------------------------------
#  pigs.remm.s <- emmeans(pigs.lm, "source", transform = "response")

## ----eval = FALSE---------------------------------------------------------------------------------
#  emmeans(pigs.lm, "source", type = "response")

## -------------------------------------------------------------------------------------------------
neuralgia.glm <- glm(Pain ~ Treatment * Sex + Age, family = binomial(), data = neuralgia)
neuralgia.emm <- emmeans(neuralgia.glm, "Treatment", type = "response")
neuralgia.emm

## -------------------------------------------------------------------------------------------------
pairs(neuralgia.emm, reverse = TRUE)

## -------------------------------------------------------------------------------------------------
emmip(neuralgia.glm, Sex ~ Treatment)

## -------------------------------------------------------------------------------------------------
warp.glm <- glm(sqrt(breaks) ~ wool*tension, family = Gamma, data = warpbreaks)
ref_grid(warp.glm)

## -------------------------------------------------------------------------------------------------
emmeans(warp.glm, ~ tension | wool, type = "response")

## -------------------------------------------------------------------------------------------------
emmeans(warp.glm, ~ tension | wool, type = "unlink")

## ----eval = FALSE---------------------------------------------------------------------------------
#  tran <- make.tran("asin.sqrt", 100)
#  my.model <- with(tran,
#      lmer(linkfun(percent) ~ treatment + (1|Block), data = mydata))

## ----eval = FALSE---------------------------------------------------------------------------------
#  mydata <- transform(mydata, logy.5 = log(yield + 0.5))
#  my.model <- lmer(logy.5 ~ treatment + (1|Block), data = mydata)

## ----eval = FALSE---------------------------------------------------------------------------------
#  my.rg <- update(ref_grid(my.model), tran = make.tran("genlog", .5))

## ----eval = FALSE---------------------------------------------------------------------------------
#  model.rg <- update(ref_grid(model), tran = "sqrt")

## -------------------------------------------------------------------------------------------------
pigroot.lm <- lm(sqrt(conc) ~ source + factor(percent), data = pigs)
piglog.emm.s <- regrid(emmeans(pigroot.lm, "source"), transform = "log")
confint(piglog.emm.s, type = "response")
pairs(piglog.emm.s, type = "response")

## ---- eval = FALSE--------------------------------------------------------------------------------
#  regrid(emm, transform = "probit")

## ---- message = FALSE-----------------------------------------------------------------------------
fiber.lm <- lm(scale(strength) ~ machine * scale(diameter), data = fiber)
emmeans(fiber.lm, "machine")   # on the standardized scale
emmeans(fiber.lm, "machine", type = "response")   # strength scale

## -------------------------------------------------------------------------------------------------
emtrends(fiber.lm, "machine", var = "diameter")

## -------------------------------------------------------------------------------------------------
emtrends(fiber.lm, "machine", var = "diameter", transform = "response")

## -------------------------------------------------------------------------------------------------
with(fiber, c(mean = mean(diameter), sd = sd(diameter)))
emtrends(fiber.lm, "machine", var = "scale(diameter, 24.133, 4.324)")

## -------------------------------------------------------------------------------------------------
coef(fiber.lm)[4:6]

## ---- eval = FALSE--------------------------------------------------------------------------------
#  mod <- some.fcn(scale(RT) ~ group + (1|subject), data = mydata)
#  emmeans(mod, "group", type = "response",
#          tran = make.tran("scale", y = mydata$RT))

## ---- eval = FALSE--------------------------------------------------------------------------------
#  mod <- with(make.tran("scale", y = mydata$RT),
#              some.fcn(linkfun(RT) ~ group + (1|subject), data = mydata))
#  emmeans(mod, "group", type = "response")

## ---- message = FALSE-----------------------------------------------------------------------------
fib.lm <- lm(strength ~ machine * diameter, data = fiber)

# On raw scale:
emmeans(fib.lm, "machine")

# On standardized scale:
tran <- make.tran("scale", y = fiber$strength)
emmeans(fib.lm, "machine", transform = tran)

## -------------------------------------------------------------------------------------------------
sigma(pigs.lm)

## -------------------------------------------------------------------------------------------------
summary(pigs.emm.s, type = "response", bias.adj = TRUE)

## -------------------------------------------------------------------------------------------------
ismod <- glm(count ~ spray, data = InsectSprays, family = poisson())
emmeans(ismod, "spray", type = "response", bias.adj = FALSE)
emmeans(ismod, "spray", type = "response", bias.adj = TRUE)

## -------------------------------------------------------------------------------------------------
with(InsectSprays, tapply(count, spray, mean))

## ---- message = FALSE-----------------------------------------------------------------------------
require(lme4)
cbpp <- transform(cbpp, unit = 1:nrow(cbpp))
cbpp.glmer <- glmer(cbind(incidence, size - incidence) ~ period + 
                          (1 | herd) +  (1 | unit),
                    family = binomial, data = cbpp)

emm <- emmeans(cbpp.glmer, "period")
summary(emm, type = "response")

## -------------------------------------------------------------------------------------------------
lme4::VarCorr(cbpp.glmer)

## -------------------------------------------------------------------------------------------------
total.SD = sqrt(0.89107^2 + 0.18396^2)

## -------------------------------------------------------------------------------------------------
summary(emm, type = "response", bias.adjust = TRUE, sigma = total.SD)

## -------------------------------------------------------------------------------------------------
cases <- with(cbpp, tapply(incidence, period, sum))
trials <- with(cbpp, tapply(size, period, sum))
cases / trials

