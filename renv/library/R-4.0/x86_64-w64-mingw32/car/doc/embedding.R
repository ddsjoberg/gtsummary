### R code from vignette source 'embedding.Rnw'

###################################################
### code chunk number 1: embedding.Rnw:12-15
###################################################
library(knitr)
render_sweave()
options(width=80, digits=4, useFancyQuotes=FALSE, prompt=" ", continue=" ")


###################################################
### code chunk number 2: embedding.Rnw:36-39
###################################################
library(car)
m1 <- lm(time ~ t1 + t2, Transact)
deltaMethod(m1, "t1/(t2 + 2)")


###################################################
### code chunk number 3: embedding.Rnw:42-47
###################################################
ans <- NULL
for (z in 1:4) {
 ans <- rbind(ans, deltaMethod(m1, "t1/(t2 + z)",
     func = gsub("z", z, "t1/(t1+z)"))) }
ans


###################################################
### code chunk number 4: embedding.Rnw:52-59
###################################################
f1 <- function(mod) {
 ans <- NULL
 for (x in 1:4) {
    ans <- rbind(ans, deltaMethod(mod, "t1/(t2 + x)",
        func = gsub("x", x, "t1/(t1+x)")) )}
 ans
 }


###################################################
### code chunk number 5: embedding.Rnw:71-73
###################################################
x <- 10
f1(m1)


###################################################
### code chunk number 6: embedding.Rnw:79-87
###################################################
f2 <- function(mod) {
 ans <- NULL
 for (x in 1:4) {
    ans <- rbind(ans, deltaMethod(mod, "t1/(t2 + x)",
        func = gsub("x", x, "t1/(t1+x)"), constants=list(x=x)) )}
 ans
 }
f2(m1)


###################################################
### code chunk number 7: embedding.Rnw:94-96
###################################################
m2 <- lm(prestige ~ education, Prestige)
ncvTest(m2, ~ income)


###################################################
### code chunk number 8: embedding.Rnw:99-104 (eval = FALSE)
###################################################
## f3 <- function(meanmod, dta, varmod) {
##   m3 <- lm(meanmod, dta)
##   ncvTest(m3, varmod)
##   }
## f3(prestige ~ education, Prestige, ~ income)


###################################################
### code chunk number 9: embedding.Rnw:112-122
###################################################
f4 <- function(meanmod, dta, varmod) {
   assign(".dta", dta, envir=.GlobalEnv)
   assign(".meanmod", meanmod, envir=.GlobalEnv)
   m1 <- lm(.meanmod, .dta)
   ans <- ncvTest(m1, varmod)
   remove(".dta", envir=.GlobalEnv)
   remove(".meanmod", envir=.GlobalEnv)
   ans
   }
f4(prestige ~ education, Prestige, ~income)


###################################################
### code chunk number 10: embedding.Rnw:127-135 (eval = FALSE)
###################################################
## library(effects)
## fc <- function(dta, formula, terms) {
##  print(m1 <- lm(formula, .dta))
##  Effect(terms, m1)
##  }
## form <- prestige ~ income*type + education
## terms <- c("income", "type")
## fc(Duncan, form, terms)


###################################################
### code chunk number 11: embedding.Rnw:143-152 (eval = FALSE)
###################################################
## library(effects)
## fc.working <- function(dta, formula, terms) {
##  assign(".dta", dta, env=.GlobalEnv)
##  print(m1 <- lm(formula, .dta))
##  e1 <- Effect(terms, m1)
##  remove(".dta", envir=.GlobalEnv)
##  e1
##  }
## fc.working(Duncan, form, terms)


###################################################
### code chunk number 12: embedding.Rnw:158-161
###################################################
m1 <- lm(time ~ t1 + t2, Transact)
b1 <- Boot(m1, R=999)
summary(b1)


###################################################
### code chunk number 13: embedding.Rnw:164-165
###################################################
confint(b1)


###################################################
### code chunk number 14: embedding.Rnw:169-170 (eval = FALSE)
###################################################
## .carEnv <- new.env(parent=emptyenv())


###################################################
### code chunk number 15: embedding.Rnw:174-217 (eval = FALSE)
###################################################
## Boot.default <- function(object, f=coef, labels=names(coef(object)),
##                      R=999, method=c("case", "residual")) {
##   if(!(require(boot))) stop("The 'boot' package is missing")
##   f0 <- f(object)
##   if(length(labels) != length(f0)) labels <- paste("V", seq(length(f0)), sep="")
##   method <- match.arg(method)
##   if(method=="case") {
##      boot.f <- function(data, indices, .fn) {
##       assign(".boot.indices", indices, envir=car:::.carEnv)
##       mod <- update(object, subset=get(".boot.indices", envir=car:::.carEnv))
##       if(mod$qr$rank != object$qr$rank){
##             out <- .fn(object)
##             out <- rep(NA, length(out)) } else  {out <- .fn(mod)}
##      out
##      }
##     } else {
##     boot.f <- function(data, indices, .fn) {
##       first <- all(indices == seq(length(indices)))
##       res <- if(first) object$residuals else
##                   residuals(object, type="pearson")/sqrt(1 - hatvalues(object))
##       res <- if(!first) (res - mean(res)) else res
##       val <- fitted(object) + res[indices]
##       if (!is.null(object$na.action)){
##             pad <- object$na.action
##             attr(pad, "class") <- "exclude"
##             val <- naresid(pad, val)
##             }
##       assign(".y.boot", val, envir=car:::.carEnv)
##       mod <- update(object, get(".y.boot", envir=car:::.carEnv) ~ .)
##       if(mod$qr$rank != object$qr$rank){
##             out <- .fn(object)
##             out <- rep(NA, length(out)) } else  {out <- .fn(mod)}
##       out
##       }
##   }
##   b <- boot(data.frame(update(object, model=TRUE)$model), boot.f, R, .fn=f)
##   colnames(b$t) <- labels
##   if(exists(".y.boot", envir=car:::.carEnv))
##      remove(".y.boot", envir=car:::.carEnv)
##   if(exists(".boot.indices", envir=car:::.carEnv))
##      remove(".boot.indices", envir=car:::.carEnv)
##   b
##   }


