### R code from vignette source 'coercion.Rnw'

###################################################
### code chunk number 1: coercion.Rnw:19-22
###################################################
require( pbkrtest )
prettyVersion <- packageDescription("pbkrtest")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: coercion.Rnw:64-66
###################################################
options(prompt = "R> ", continue = "+  ", width = 80, useFancyQuotes=FALSE)
dir.create("figures")


###################################################
### code chunk number 3: coercion.Rnw:71-72
###################################################
library(pbkrtest)


###################################################
### code chunk number 4: coercion.Rnw:79-82
###################################################
mod0 <- lm(dist ~ 1, data=cars); coef(mod0)
mod1 <- update(mod0, .~. + speed); coef(mod1)
mod2 <- update(mod1, .~. + I(speed^2)); coef(mod2)


###################################################
### code chunk number 5: coercion.Rnw:88-91
###################################################
L21 <- model2remat(mod2, mod1); L21
L20 <- model2remat(mod2, mod0); L20
L10 <- model2remat(mod1, mod0); L10


###################################################
### code chunk number 6: coercion.Rnw:97-100
###################################################
new1  <- remat2model(mod2, L21); coef(new1)
new0a <- remat2model(mod2, L20); coef(new0a)
new0b <- remat2model(mod1, L10); coef(new0b)


###################################################
### code chunk number 7: coercion.Rnw:107-111
###################################################
eps <- 1e-8
max(abs(fitted(new1)  - fitted(mod1))) < eps
max(abs(fitted(new0a) - fitted(mod0))) < eps
max(abs(fitted(new0b) - fitted(mod0))) < eps


