### R code from vignette source 'sandwich-CL.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library("sandwich")
library("geepack")
library("lattice")
library("lmtest")
library("multiwayvcov")
library("pcse")
library("plm")
library("pscl")

panel.xyref <- function(x, y, ...) {
  panel.abline(h = 0.95, col = "slategray")
  panel.xyplot(x, y, ...)  
}
se <- function(vcov) sapply(vcov, function(x) sqrt(diag(x)))

options(prompt = "R> ", continue = "+  ", digits = 5)

if(file.exists("sim-CL.rda")) {
  load("sim-CL.rda")
} else {
  source("sim-CL.R")
}


###################################################
### code chunk number 2: innovation-data
###################################################
data("InstInnovation", package = "sandwich")
library("pscl")
h_innov <- hurdle(
  cites ~ institutions + log(capital/employment) + log(sales),
  data = InstInnovation, dist = "negbin")


###################################################
### code chunk number 3: innovation-coeftest
###################################################
library("sandwich")
library("lmtest")
coeftest(h_innov, vcov = vcovCL, cluster = ~ company)


###################################################
### code chunk number 4: innovation-se (eval = FALSE)
###################################################
## suppressWarnings(RNGversion("3.5.0"))
## set.seed(0)
## vc <- list(
##   "standard" = vcov(h_innov),
##   "basic" = sandwich(h_innov),
##   "CL-1" = vcovCL(h_innov, cluster = ~ company),
##   "boot" = vcovBS(h_innov, cluster = ~ company)
## )
## se <- function(vcov) sapply(vcov, function(x) sqrt(diag(x)))
## se(vc)


###################################################
### code chunk number 5: innovation-se2
###################################################
se(vc_innov)


###################################################
### code chunk number 6: petersen-model
###################################################
data("PetersenCL", package = "sandwich")
p_lm <- lm(y ~ x, data = PetersenCL)


###################################################
### code chunk number 7: petersen-comparison1
###################################################
library("multiwayvcov")
se(list(
  "sandwich" = vcovCL(p_lm, cluster = ~ firm),
  "multiwayvcov" = cluster.vcov(p_lm, cluster = ~ firm)
))


###################################################
### code chunk number 8: petersen-comparison2
###################################################
library("plm")
p_plm <- plm(y ~ x, data = PetersenCL, model = "pooling",
 indexes = c("firm", "year"))

library("geepack")
vcov.geeglm <- function(object) {
  vc <- object$geese$vbeta
  rownames(vc) <- colnames(vc) <- names(coef(object))
  return(vc)
}
p_gee <- geeglm(y ~ x, data = PetersenCL, id = PetersenCL$firm,
 corstr = "independence", family = gaussian)
se(list(
  "sandwich" = vcovCL(p_lm, cluster = ~ firm,
    cadjust = FALSE, type = "HC0"),
  "plm" = vcovHC(p_plm, cluster = "group"),
  "geepack" = vcov(p_gee)
))


###################################################
### code chunk number 9: petersen-twocl
###################################################
se(list(
  "sandwich" = vcovCL(p_lm, cluster = ~ firm + year, multi0 = TRUE),
  "multiwayvcov" = cluster.vcov(p_lm, cluster = ~ firm + year)
))


###################################################
### code chunk number 10: petersen-comparison3
###################################################
se(list(
  "sandwich" = vcovPL(p_lm, cluster = ~ firm + year, adjust = FALSE),
  "plm" = vcovSCC(p_plm)
))


###################################################
### code chunk number 11: petersen-comparison4
###################################################
library("pcse")
se(list(
  "sandwich" = sandwich::vcovPC(p_lm, cluster = ~ firm + year),
  "pcse" = pcse::vcovPC(p_lm, groupN = PetersenCL$firm,
    groupT = PetersenCL$year)
))


###################################################
### code chunk number 12: petersen-unbalanced1
###################################################
PU <- subset(PetersenCL, !(firm == 1 & year == 10))
pu_lm <- lm(y ~ x, data = PU)


###################################################
### code chunk number 13: petersen-unbalanced2
###################################################
se(list(
  "sandwichT" = sandwich::vcovPC(pu_lm, cluster = ~ firm + year,
    pairwise = TRUE),
  "pcseT" = pcse::vcovPC(pu_lm, PU$firm, PU$year, pairwise = TRUE),
  "sandwichF" = sandwich::vcovPC(pu_lm, cluster = ~ firm + year,
    pairwise = FALSE),
  "pcseF" = pcse::vcovPC(pu_lm, PU$firm, PU$year, pairwise = FALSE)
))


###################################################
### code chunk number 14: sim-01-figure
###################################################
my.settings <- canonical.theme(color = TRUE)
my.settings[["strip.background"]]$col <- "gray"
my.settings[["strip.border"]]$col <- "black"
my.settings[["superpose.line"]]$lwd <- 1
s01$vcov <- factor(s01$vcov, levels(s01$vcov)[c(2,4,3,1,8,5,7,6)])
my.settings[["superpose.line"]]$col <- my.settings[["superpose.symbol"]]$col <- my.settings[["superpose.symbol"]]$col <-
  c("#377eb8", "green","#006400", "#dc75ed", "darkred", "orange", "black", "grey")
my.settings[["superpose.symbol"]]$pch <- c(19, 19, 19, 19, 17, 25, 3, 8)
xyplot(coverage ~ rho | par, groups = ~ factor(vcov),
  data = s01, subset = par != "(Intercept)",
  ylim = c(0.1, 1),
  type = "b", xlab = expression(rho), ylab = "Empirical coverage",
  auto.key = list(columns = 3),
  par.strip.text = list(col = "black"), par.settings = my.settings,
  panel = panel.xyref)


###################################################
### code chunk number 15: sim-02-figure
###################################################
my.settings <- canonical.theme(color = TRUE)
my.settings[["strip.background"]]$col <- "gray"
my.settings[["strip.border"]]$col <- "black"
my.settings[["superpose.line"]]$lwd <- 1
s02$dist <- factor(as.character(s02$dist), levels = c("gaussian", "binomial(logit)", "poisson"))
s02$vcov <- factor(s02$vcov, levels(s02$vcov)[c(2,4,3,1,8,5,7,6)])
my.settings[["superpose.line"]]$col <- my.settings[["superpose.symbol"]]$col <- my.settings[["superpose.symbol"]]$col <-
  c("#377eb8", "green","#006400", "#dc75ed", "darkred", "orange", "black", "grey")
my.settings[["superpose.symbol"]]$pch <- c(19, 19, 19, 19, 17, 25, 3, 8)
xyplot(coverage ~ rho | dist, groups = ~ factor(vcov),
  data = s02, subset = par != "(Intercept)",
  ylim = c(0.5, 1),
  type = "b", xlab = expression(rho), ylab = "Empirical coverage",
  auto.key = list(columns = 3),
  par.strip.text = list(col = "black"), par.settings = my.settings,
  panel = panel.xyref)


###################################################
### code chunk number 16: sim-03-figure
###################################################
s33 <- na.omit(s33)
my.settings <- canonical.theme(color = TRUE)
my.settings[["strip.background"]]$col <- "gray"
my.settings[["strip.border"]]$col <- "black"
my.settings[["superpose.line"]]$lwd <- 1
s33$vcov <- factor(s33$vcov, levels(s33$vcov)[c(2,1,4,3)])
my.settings[["superpose.line"]]$col <- my.settings[["superpose.symbol"]]$col <- my.settings[["superpose.symbol"]]$fill <- c("#377eb8", "#000080", "darkred", "orange")
my.settings[["superpose.symbol"]]$pch <- c(19, 19, 17, 25)
xyplot(coverage ~ rho | dist, groups = ~ factor(vcov),
  data = s33, subset = par == "x1",
  ylim = c(0.8, 1),
  type = "b", xlab = expression(rho), ylab = "Empirical coverage",
  auto.key = list(columns = 2),
  par.strip.text = list(col = "black"), par.settings = my.settings,
  panel = panel.xyref)


###################################################
### code chunk number 17: sim-04-figure
###################################################
my.settings <- canonical.theme(color = TRUE)
my.settings[["strip.background"]]$col <- "gray"
my.settings[["strip.border"]]$col <- "black"
my.settings[["superpose.line"]]$lwd <- 1
s04$dist <- factor(as.character(s04$dist), c("gaussian", "binomial(logit)", "poisson"))
my.settings[["superpose.line"]]$col <- c("#377eb8", "#00E5EE", "#e41a1c", "#4daf4a", "#dc75ed")
my.settings[["superpose.symbol"]]$col <- c("#377eb8", "#00E5EE","#e41a1c", "#4daf4a", "#dc75ed")
my.settings[["superpose.symbol"]]$pch <- 19
xyplot(coverage ~ nid | dist, groups = ~ factor(vcov, levels = c(paste0("CL-", 0:3), "BS")),
  data = na.omit(s04), subset = par != "(Intercept)",
  type = "b", xlab = "G", ylab = "Empirical coverage",
  auto.key = list(columns = 2),
  par.strip.text = list(col = "black"), par.settings = my.settings,
  panel = panel.xyref)


###################################################
### code chunk number 18: sim-0607-figure
###################################################
my.settings <- canonical.theme(color = TRUE)
my.settings[["strip.background"]]$col <- "gray"
my.settings[["strip.border"]]$col <- "black"
my.settings[["superpose.line"]]$lwd <- 1
s0607$vcov <- factor(s0607$vcov, levels(s0607$vcov)[c(1,3,2)])
my.settings[["superpose.line"]]$col <- my.settings[["superpose.symbol"]]$col <- c("#377eb8","green", "#006400")
my.settings[["superpose.symbol"]]$pch <- 19
xyplot(coverage ~ nround | factor(par) + factor(copula), groups = ~ factor(vcov),
  data = na.omit(s0607), subset = par != "(Intercept)",
  type = "b", xlab = "Observations per cluster", ylab = "Empirical coverage",
  auto.key = list(columns = 2),
  par.strip.text = list(col = "black"), par.settings = my.settings,
  panel = panel.xyref)


###################################################
### code chunk number 19: sim-08-figure
###################################################
my.settings <- canonical.theme(color = TRUE)
my.settings[["strip.background"]]$col <- "gray"
my.settings[["strip.border"]]$col <- "black"
my.settings[["superpose.line"]]$lwd <- 1
s08$vcov <- factor(s08$vcov, levels(s08$vcov)[c(1,3,2)])
my.settings[["superpose.line"]]$col <- my.settings[["superpose.symbol"]]$col <- c("#377eb8","green", "#006400")
my.settings[["superpose.symbol"]]$pch <- 19
xyplot(coverage ~ nround | factor(par) + factor(dist), groups = ~ factor(vcov),
  data = na.omit(s08), subset = par != "(Intercept)",
  type = "b", xlab = "Observations per cluster", ylab = "Empirical coverage",
  auto.key = list(columns = 2),
  par.strip.text = list(col = "black"), par.settings = my.settings,
  panel = panel.xyref)


