### R code from vignette source 'clm_article.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("ordinal")
library("xtable")


###################################################
### code chunk number 2: clm_article.Rnw:792-794
###################################################
clm_args <- gsub("function ", "clm", deparse(args(clm)))
cat(paste(clm_args[-length(clm_args)], "\n"))


###################################################
### code chunk number 3: clm_article.Rnw:807-809
###################################################
cc_args <- gsub("function ", "clm.control", deparse(args(clm.control)))
cat(paste(cc_args[-length(cc_args)], "\n"))


###################################################
### code chunk number 4: clm_article.Rnw:840-850
###################################################
## data(wine)
tab <- with(wine, table(temp:contact, rating))
mat <- cbind(rep(c("cold", "warm"), each = 2),
             rep(c("no", "yes"), 2),
             tab)
colnames(mat) <- c("Temperature", "Contact",
                   paste("~~", 1:5, sep = ""))
xtab <- xtable(mat)
print(xtab, only.contents=TRUE, include.rownames=FALSE,
      sanitize.text.function = function(x) x)


###################################################
### code chunk number 5: clm_article.Rnw:878-881
###################################################
library("ordinal")
fm1 <- clm(rating ~ temp + contact, data=wine)
summary(fm1)


###################################################
### code chunk number 6: clm_article.Rnw:932-933
###################################################
anova(fm1, type="III")


###################################################
### code chunk number 7: clm_article.Rnw:937-939
###################################################
fm2 <- clm(rating ~ temp, data=wine)
anova(fm2, fm1)


###################################################
### code chunk number 8: clm_article.Rnw:945-946
###################################################
drop1(fm1, test="Chi")


###################################################
### code chunk number 9: clm_article.Rnw:951-953
###################################################
fm0 <- clm(rating ~ 1, data=wine)
add1(fm0, scope = ~ temp + contact, test="Chi")


###################################################
### code chunk number 10: clm_article.Rnw:957-958
###################################################
confint(fm1)


###################################################
### code chunk number 11: clm_article.Rnw:994-996
###################################################
fm.nom <- clm(rating ~ temp, nominal = ~ contact, data=wine)
summary(fm.nom)


###################################################
### code chunk number 12: clm_article.Rnw:1026-1027
###################################################
fm.nom$Theta


###################################################
### code chunk number 13: clm_article.Rnw:1036-1037
###################################################
anova(fm1, fm.nom)


###################################################
### code chunk number 14: clm_article.Rnw:1048-1049
###################################################
fm.nom2 <- clm(rating ~ temp + contact, nominal = ~ contact, data=wine)


###################################################
### code chunk number 15: clm_article.Rnw:1052-1053
###################################################
fm.nom2


###################################################
### code chunk number 16: clm_article.Rnw:1057-1058
###################################################
nominal_test(fm1)


###################################################
### code chunk number 17: clm_article.Rnw:1077-1079
###################################################
fm.sca <- clm(rating ~ temp + contact, scale = ~ temp, data=wine)
summary(fm.sca)


###################################################
### code chunk number 18: clm_article.Rnw:1084-1085
###################################################
scale_test(fm1)


###################################################
### code chunk number 19: clm_article.Rnw:1107-1110
###################################################
fm.equi <- clm(rating ~ temp + contact, data=wine,
               threshold="equidistant")
summary(fm.equi)


###################################################
### code chunk number 20: clm_article.Rnw:1117-1118
###################################################
drop(fm.equi$tJac %*% coef(fm.equi)[c("threshold.1", "spacing")])


###################################################
### code chunk number 21: clm_article.Rnw:1125-1126
###################################################
mean(diff(coef(fm1)[1:4]))


###################################################
### code chunk number 22: clm_article.Rnw:1132-1133
###################################################
anova(fm1, fm.equi)


###################################################
### code chunk number 23: profileLikelihood
###################################################
pr1 <- profile(fm1, alpha=1e-4)
plot(pr1)


###################################################
### code chunk number 24: prof1
###################################################
plot(pr1, which.par=1)


###################################################
### code chunk number 25: prof2
###################################################
plot(pr1, which.par=2)


###################################################
### code chunk number 26: clm_article.Rnw:1204-1207
###################################################
slice.fm1 <- slice(fm1, lambda = 5)
par(mfrow = c(2, 3))
plot(slice.fm1)


###################################################
### code chunk number 27: slice11
###################################################
plot(slice.fm1, parm = 1)


###################################################
### code chunk number 28: slice12
###################################################
plot(slice.fm1, parm = 2)


###################################################
### code chunk number 29: slice13
###################################################
plot(slice.fm1, parm = 3)


###################################################
### code chunk number 30: slice14
###################################################
plot(slice.fm1, parm = 4)


###################################################
### code chunk number 31: slice15
###################################################
plot(slice.fm1, parm = 5)


###################################################
### code chunk number 32: slice16
###################################################
plot(slice.fm1, parm = 6)


###################################################
### code chunk number 33: slice2
###################################################
slice2.fm1 <- slice(fm1, parm = 4:5, lambda = 1e-5)
par(mfrow = c(1, 2))
plot(slice2.fm1)


###################################################
### code chunk number 34: slice24
###################################################
plot(slice2.fm1, parm = 1)


###################################################
### code chunk number 35: slice25
###################################################
plot(slice2.fm1, parm = 2)


###################################################
### code chunk number 36: clm_article.Rnw:1268-1269
###################################################
convergence(fm1)


###################################################
### code chunk number 37: clm_article.Rnw:1295-1296
###################################################
head(pred <- predict(fm1, newdata = subset(wine, select = -rating))$fit)


###################################################
### code chunk number 38: clm_article.Rnw:1300-1302
###################################################
stopifnot(isTRUE(all.equal(fitted(fm1), t(pred)[t(col(pred) == wine$rating)])),
          isTRUE(all.equal(fitted(fm1), predict(fm1, newdata=wine)$fit)))


###################################################
### code chunk number 39: clm_article.Rnw:1305-1309
###################################################
newData <- expand.grid(temp    = levels(wine$temp),
                       contact = levels(wine$contact))
cbind(newData, round(predict(fm1, newdata=newData)$fit, 3),
      "class"=predict(fm1, newdata=newData, type="class")$fit)


###################################################
### code chunk number 40: clm_article.Rnw:1312-1313
###################################################
head(apply(pred, 1, function(x) round(weighted.mean(1:5, x))))


###################################################
### code chunk number 41: clm_article.Rnw:1316-1320
###################################################
p1 <- apply(predict(fm1, newdata = subset(wine, select=-rating))$fit, 1,
            function(x) round(weighted.mean(1:5, x)))
p2 <- as.numeric(as.character(predict(fm1, type="class")$fit))
stopifnot(isTRUE(all.equal(p1, p2, check.attributes = FALSE)))


###################################################
### code chunk number 42: clm_article.Rnw:1325-1327
###################################################
predictions <- predict(fm1, se.fit=TRUE, interval=TRUE)
head(do.call("cbind", predictions))


###################################################
### code chunk number 43: clm_article.Rnw:1361-1368
###################################################
wine <- within(wine, {
  rating_comb3 <- rating
  levels(rating_comb3) <- c("1", "2-4", "2-4", "2-4", "5")
})
ftable(rating_comb3 ~ temp, data=wine)
fm.comb3 <- clm(rating_comb3 ~ temp, data=wine)
summary(fm.comb3)


###################################################
### code chunk number 44: clm_article.Rnw:1373-1375
###################################################
fm.comb3_b <- clm(rating_comb3 ~ 1, data=wine)
anova(fm.comb3, fm.comb3_b)


###################################################
### code chunk number 45: clm_article.Rnw:1380-1382
###################################################
fm.nom2 <- clm(rating ~ contact, nominal = ~ temp, data=wine)
summary(fm.nom2)


###################################################
### code chunk number 46: clm_article.Rnw:1393-1395
###################################################
fm.soup <- clm(SURENESS ~ PRODID * DAY, data=soup)
summary(fm.soup)


###################################################
### code chunk number 47: clm_article.Rnw:1398-1399
###################################################
with(soup, table(DAY, PRODID))


###################################################
### code chunk number 48: clm_article.Rnw:1411-1418
###################################################
wine <- within(wine, {
  rating_comb2 <- rating
  levels(rating_comb2) <- c("1-2", "1-2", "3-5", "3-5", "3-5")
})
ftable(rating_comb2 ~ contact, data=wine)
fm.comb2 <- clm(rating_comb2 ~ contact, scale = ~ contact, data=wine)
summary(fm.comb2)


###################################################
### code chunk number 49: clm_article.Rnw:1421-1434
###################################################
## Example with unidentified parameters with 3 response categories
## not shown in paper:
wine <- within(wine, {
  rating_comb3b <- rating
  levels(rating_comb3b) <- c("1-2", "1-2", "3", "4-5", "4-5")
})
wine$rating_comb3b[1] <- "4-5" # Need to remove the zero here to avoid inf MLE
ftable(rating_comb3b ~ temp + contact, data=wine)

fm.comb3_c <- clm(rating_comb3b ~ contact * temp, scale=~contact * temp,
                  nominal=~contact, data=wine)
summary(fm.comb3_c)
convergence(fm.comb3_c)


###################################################
### code chunk number 50: clm_article.Rnw:1443-1445
###################################################
rho <- update(fm1, doFit=FALSE)
names(rho)


###################################################
### code chunk number 51: clm_article.Rnw:1448-1450
###################################################
rho$clm.nll(rho)
c(rho$clm.grad(rho))


###################################################
### code chunk number 52: clm_article.Rnw:1453-1455
###################################################
rho$clm.nll(rho, par=coef(fm1))
print(c(rho$clm.grad(rho)), digits=3)


###################################################
### code chunk number 53: clm_article.Rnw:1460-1470
###################################################
nll <- function(par, envir) {
  envir$par <- par
  envir$clm.nll(envir)
}
grad <- function(par, envir) {
  envir$par <- par
  envir$clm.nll(envir)
  envir$clm.grad(envir)
}
nlminb(rho$par, nll, grad, upper=c(rep(Inf, 4), 2, 2), envir=rho)$par


###################################################
### code chunk number 54: clm_article.Rnw:1479-1483
###################################################
artery <- data.frame(disease = factor(rep(0:4, 2), ordered = TRUE),
                     smoker  = factor(rep(c("no", "yes"), each=5)),
                     freq    = c(334, 99, 117, 159, 30, 350, 307, 345, 481, 67))
addmargins(xtabs(freq ~ smoker + disease, data = artery), margin = 2)


###################################################
### code chunk number 55: clm_article.Rnw:1487-1489
###################################################
fm <- clm(disease ~ smoker, weights=freq, data=artery)
exp(fm$beta)


###################################################
### code chunk number 56: clm_article.Rnw:1494-1497
###################################################
fm.nom <- clm(disease ~ 1, nominal = ~ smoker, weights=freq, data=artery,
              sign.nominal = "negative")
coef(fm.nom)[5:8]


###################################################
### code chunk number 57: clm_article.Rnw:1500-1501
###################################################
coef(fm.lm <- lm(I(coef(fm.nom)[5:8]) ~ I(0:3)))


###################################################
### code chunk number 58: clm_article.Rnw:1504-1511
###################################################
nll2 <- function(par, envir) {
  envir$par <- c(par[1:4], par[5] + par[6] * (0:3))
  envir$clm.nll(envir)
}
start <- unname(c(coef(fm.nom)[1:4], coef(fm.lm)))
fit <- nlminb(start, nll2, envir=update(fm.nom, doFit=FALSE))
round(fit$par[5:6], 2)


