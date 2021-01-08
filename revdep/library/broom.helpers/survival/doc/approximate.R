### R code from vignette source 'approximate.Rnw'

###################################################
### code chunk number 1: init
###################################################
options(continue="  ", width=60)
options(SweaveHooks=list(fig=function() par(mar=c(4.1, 4.1, .3, 1.1))))
pdf.options(pointsize=8) #text in graph about the same as regular text
library(survival, quietly=TRUE)


###################################################
### code chunk number 2: approx1
###################################################
getOption("SweaveHooks")[["fig"]]()
ksurv <- survfit(Surv(time, status) ~1, data=kidney)
plot(ksurv, fun="cumhaz", conf.int=FALSE, lwd=2,
     xlab="Time since catheter insertion", ylab="Cumulative Hazard")
lines(c(0, 45, 500, 560), c(0, .55, 2.7, 4), col=2, lwd=2)


###################################################
### code chunk number 3: approx2
###################################################
kdata2 <- survSplit(Surv(time, status) ~., data=kidney, cut=c(45, 500),
    episode="interval")
kfit1 <- coxph(Surv(time, status) ~ age + sex, kidney, ties='breslow')

kfit2 <- glm(status ~ age + sex + factor(interval) -1 + 
                 offset(log(time-tstart)), family=poisson, data=kdata2)

cbind(Cox= summary(kfit1)$coefficients[,c(1,3)],
      poisson = summary(kfit2)$coefficients[1:2, 1:2])


###################################################
### code chunk number 4: approx3
###################################################
utime <- sort(unique(kidney$time[kidney$status==1])) # unique deaths
kdata3 <- survSplit(Surv(time, status) ~., data=kidney, cut=utime,
    episode="interval")
kdata3 <- subset(kdata3, time == c(utime,0)[interval]) # remove partials

kfit3 <- glm(status ~ age + sex + factor(interval) -1,
                 family=poisson, data=kdata3) 
kfit4 <- glm(status ~ age + sex + factor(interval) -1,
                 family=binomial, data=kdata3) 
rbind(poisson= coef(kfit3)[1:2], binomial = coef(kfit4)[1:2])


###################################################
### code chunk number 5: approx4
###################################################
getOption("SweaveHooks")[["fig"]]()
counts <- c(table(kdata3$interval))  # subjects in each interval
xmat <- as.matrix(kdata3[,c('age', 'sex')])
centers <- rowsum(xmat, kdata3$interval) / counts
xmat2 <- xmat - centers[kdata3$interval,]

kfit4a <- glm(status ~ xmat2 + factor(interval) -1, poisson, kdata3)
temp <- coef(kfit4a)[-(1:2)]   # intercepts
phat <- with(kdata3, tapply(status, interval, sum)) /counts
matplot(1:length(counts), cbind(phat, exp(temp)), log='y',
     xlab="Interval", ylab="Simple event rate")
legend(5, .5, c("Rate", "Poisson intercept"), pch="12", col=1:2)


###################################################
### code chunk number 6: approx5
###################################################
kdata3$phat <- phat[kdata3$interval]  # add phat to the data set
logit <- function(x) log(x/(1-x))
kfit4b <- glm(status ~ xmat2 + offset(log(phat)), poisson, kdata3)
kfit4c <- glm(status ~ xmat2, poisson, kdata3)

kfit4d <- glm(status ~ xmat2 + offset(logit(phat)), binomial, kdata3,
              subset=(phat<1))
kfit4e <- glm(status ~ xmat2, binomial, kdata3,
              subset=(phat<1))

rbind(Cox= coef(kfit1), poisson=coef(kfit4a)[1:2], 
      poisson2 = coef(kfit4b)[2:3], poisson3 = coef(kfit4c)[2:3],
      binomial2 = coef(kfit4d)[2:3], binomial3 = coef(kfit4e)[2:3])



