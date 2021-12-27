### R code from vignette source 'lmtest-intro.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library(lmtest)
options(SweaveHooks=list(twofig=function() {par(mfrow=c(1,2))},
                         twofig2=function() {par(mfrow=c(2,1))},
                         onefig=function() {par(mfrow=c(1,1))}))


###################################################
### code chunk number 2: macro-jocci
###################################################
getOption("SweaveHooks")[["twofig"]]()
data(jocci)
plot(jocci[,"dy"], ylab = "jocci (log first differences)")
ar6.model <- dy ~ dy1 + dy2 + dy3 + dy4 + dy5 +dy6
jocci.fm <- lm(ar6.model, data = jocci)
plot(time(jocci), residuals(jocci.fm), xlab = "Time", ylab = "AR(6) residuals")


###################################################
### code chunk number 3: macro-corr1
###################################################
data(jocci)
dwtest(dy ~ 1, data = jocci)


###################################################
### code chunk number 4: macro-model
###################################################
ar6.model <- dy ~ dy1 + dy2 + dy3 + dy4 + dy5 +dy6


###################################################
### code chunk number 5: macro-corr2
###################################################
bgtest(ar6.model, data = jocci)


###################################################
### code chunk number 6: macro-hetsked1
###################################################
var.model <- ~ I(dy1^2) + I(dy2^2) + I(dy3^2) + I(dy4^2) + I(dy5^2) + I(dy6^2)
bptest(ar6.model, var.model, data = jocci)


###################################################
### code chunk number 7: mandible-data
###################################################
getOption("SweaveHooks")[["onefig"]]()
data(Mandible)
mandible <- log(Mandible)
attach(mandible)
plot(mandible)
fm <- lm(length ~ age)
fm2 <- lm(length ~ age + I(age^2))
lines(age, fitted(fm), col = 2)
lines(age, fitted(fm2), col = 4)


###################################################
### code chunk number 8: mandible-res
###################################################
getOption("SweaveHooks")[["twofig"]]()
plot(age, residuals(fm), ylab = "residuals (linear model)")
plot(age, residuals(fm2), ylab = "residuals (quadratic model)")
detach(mandible)


###################################################
### code chunk number 9: mandible-tests1 (eval = FALSE)
###################################################
## data(Mandible)
## mandible <- log(Mandible)
## harvtest(length ~ age, order.by = ~ age, data = mandible)
## raintest(length ~ age, order.by = ~ age, data = mandible)


###################################################
### code chunk number 10: mandible-tests2
###################################################
resettest(length ~ age, data = mandible)


###################################################
### code chunk number 11: mandible-tests3
###################################################
resettest(length ~ age, power = 2, type = "regressor", data = mandible)


###################################################
### code chunk number 12: mandible-tests4
###################################################
raintest(length ~ age + I(age^2), order.by = ~ age, data = mandible)


###################################################
### code chunk number 13: mandible-supF
###################################################
if(require(strucchange)) {
  supF.pval <- round(sctest(length ~ age + I(age^2), data = mandible, to = 0.9, type = "supF")$p.value, digits = 3)
} else {
#  warning("`strucchange' not available: p value set to NA")
  supF.pval <- NA
}


