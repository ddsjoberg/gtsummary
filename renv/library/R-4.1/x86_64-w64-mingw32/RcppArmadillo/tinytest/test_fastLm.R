#!/usr/bin/r -t
##
##  Copyright (C) 2010 - 2019  Dirk Eddelbuettel, Romain Francois and Douglas Bates
##
##  This file is part of RcppArmadillo.
##
##  RcppArmadillo is free software: you can redistribute it and/or modify it
##  under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 2 of the License, or
##  (at your option) any later version.
##
##  RcppArmadillo is distributed in the hope that it will be useful, but
##  WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with RcppArmadillo.  If not, see <http://www.gnu.org/licenses/>.

library(RcppArmadillo)
library(datasets)

#test.fastLm <- function() {
data(trees, package="datasets")
flm <- fastLmPure(cbind(1, log(trees$Girth)), log(trees$Volume))
fit <- lm(log(Volume) ~ log(Girth), data=trees)

expect_equal(as.numeric(flm$coefficients), as.numeric(coef(fit)))#,msg="fastLm.coef")
expect_equal(as.numeric(flm$stderr), as.numeric(coef(summary(fit))[,2]))#,msg="fastLm.stderr")
expect_equal(as.numeric(flm$df.residual), as.numeric(fit$df.residual))#,msg="fastLm.df.residual")


#test.fastLm.default <- function() {
data(trees, package="datasets")
flm <- RcppArmadillo:::fastLm.default(cbind(1, log(trees$Girth)), log(trees$Volume))
fit <- lm(log(Volume) ~ log(Girth), data=trees)

expect_equal(as.numeric(flm$coefficients), as.numeric(coef(fit)))#,msg="fastLm.default.coef")
expect_equal(as.numeric(flm$stderr), as.numeric(coef(summary(fit))[,2]))#,msg="fastLm.default.stderr")
expect_equal(as.numeric(flm$df.residual), as.numeric(fit$df.residual))#,msg="fastLm.default.df.residual")
expect_equal(as.numeric(flm$residuals), as.numeric(fit$residuals))#,msg="fastLm.default.residuals")
expect_equal(as.numeric(flm$fitted.values), as.numeric(fit$fitted.values))#,msg="fastLm.default.fitted.values")

#test.summary.fastLm <- function() {
data(trees, package="datasets")
sflm <- summary(fastLm(log(Volume) ~ log(Girth), data=trees))
sfit <- summary(lm(log(Volume) ~ log(Girth), data=trees))

expect_equal(as.numeric(coef(sflm)), as.numeric(coef(sfit)))#,msg="summary.fastLm.coef")
expect_equal(sflm$r.squared, sfit$r.squared)#,msg="summary.fastLm.r.squared")
expect_equal(sflm$adj.r.squared, sfit$adj.r.squared)#,msg="summary.fastLm.r.squared")
expect_equal(sflm$sigma, sfit$sigma)#,msg="summary.fastLm.sigma")

## no intercept case
sflm <- summary(fastLm(log(Volume) ~ log(Girth) - 1, data=trees))
sfit <- summary(lm(log(Volume) ~ log(Girth) - 1, data=trees))
expect_equal(as.numeric(coef(sflm)), as.numeric(coef(sfit)))#,msg="summary.fastLm.coef.noint")
expect_equal(sflm$r.squared, sfit$r.squared)#,msg="summary.fastLm.r.squared.noint")
expect_equal(sflm$adj.r.squared, sfit$adj.r.squared)#,msg="summary.fastLm.r.squared.noint")
expect_equal(sflm$sigma, sfit$sigma)#,msg="summary.fastLm.sigma.noint")

## non-formula use
sflm <- summary(fastLm(log(trees$Girth), log(trees$Volume)))
sfit <- summary(lm(log(Volume) ~ log(Girth) - 1, data=trees))
expect_equal(as.numeric(coef(sflm)), as.numeric(coef(sfit)))#,msg="summary.fastLm.coef.nonform")
expect_equal(sflm$r.squared, sfit$r.squared)#,msg="summary.fastLm.r.squared.nonform")
expect_equal(sflm$adj.r.squared, sfit$adj.r.squared)#,msg="summary.fastLm.r.squared.nonform")
expect_equal(sflm$sigma, sfit$sigma)#,msg="summary.fastLm.sigma.nonform")


#test.fastLm.formula <- function() {
data(trees, package="datasets")
flm <- fastLm(log(Volume) ~ log(Girth), data=trees)
fit <- lm(log(Volume) ~ log(Girth), data=trees)

expect_equal(flm$coefficients, coef(fit))#, msg="fastLm.formula.coef")
expect_equal(as.numeric(flm$stderr), as.numeric(coef(summary(fit))[,2]))#,msg="fastLm.formula.stderr")
expect_equal(as.numeric(flm$df.residual), as.numeric(fit$df.residual))#,msg="fastLm.formula.df.residual")
expect_equal(as.numeric(flm$residuals), as.numeric(fit$residuals))#,msg="fastLm.formula.residuals")
expect_equal(as.numeric(flm$fitted.values), as.numeric(fit$fitted.values))#,msg="fastLm.formula.fitted.values")

## also tickle print and predict methods
expect_stdout(print(flm))
expect_stdout(print(summary(flm)))
vec <- predict(flm, newdata=data.frame(Girth=c(1,2,3), Volume=c(2,3,4)))
expect_equal(class(vec), "numeric")
expect_equal(length(vec), 3L)
vec <- predict(flm, newdata=NULL)
expect_equal(vec, fitted(flm))
