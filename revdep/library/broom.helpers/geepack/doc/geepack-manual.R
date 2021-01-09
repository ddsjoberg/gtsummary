### R code from vignette source 'geepack-manual.Rnw'

###################################################
### code chunk number 1: geepack-manual.Rnw:16-19
###################################################
require( geepack )
prettyVersion <- packageDescription("geepack")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: geepack-manual.Rnw:73-75
###################################################
library(geepack)
citation("geepack")


###################################################
### code chunk number 3: geepack-manual.Rnw:92-100
###################################################
library(geepack)
timeorder <- rep(1:5, 6)
tvar      <- timeorder + rnorm(length(timeorder))
idvar <- rep(1:6, each=5)
uuu   <- rep(rnorm(6), each=5)
yvar  <- 1 + 2*tvar + uuu + rnorm(length(tvar))
simdat <- data.frame(idvar, timeorder, tvar, yvar)
head(simdat,12)


###################################################
### code chunk number 4: geepack-manual.Rnw:109-111
###################################################
mod1 <- geeglm(yvar~tvar, id=idvar, data=simdat, corstr="ar1")
mod1


###################################################
### code chunk number 5: geepack-manual.Rnw:127-133
###################################################
set.seed(123)
## library(doBy)
simdatPerm <- simdat[sample(nrow(simdat)),]
## simdatPerm <- orderBy(~idvar, simdatPerm)
simdatPerm <- simdatPerm[order(simdatPerm$idvar),]
head(simdatPerm)


###################################################
### code chunk number 6: geepack-manual.Rnw:143-145
###################################################
mod2 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="ar1")
mod2


###################################################
### code chunk number 7: geepack-manual.Rnw:152-155
###################################################
## simdatPerm2 <- orderBy(~timeorder, data=simdat)
simdatPerm2 <- simdat[order(simdat$timeorder),]
geeglm(yvar~tvar, id=idvar, data=simdatPerm2, corstr="ar1")


###################################################
### code chunk number 8: geepack-manual.Rnw:162-166
###################################################
wav <- simdatPerm$timeorder
wav
mod3 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="ar1", waves=wav)
mod3


###################################################
### code chunk number 9: geepack-manual.Rnw:175-181
###################################################
cor.fixed <- matrix(c(1    , 0.5  , 0.25,  0.125, 0.125,
                      0.5  , 1    , 0.25,  0.125, 0.125,
                      0.25 , 0.25 , 1   ,  0.5  , 0.125,
                      0.125, 0.125, 0.5  , 1    , 0.125,
                      0.125, 0.125, 0.125, 0.125, 1     ), 5, 5)
cor.fixed


###################################################
### code chunk number 10: geepack-manual.Rnw:189-191
###################################################
zcor <- fixed2Zcor(cor.fixed, id=simdatPerm$idvar, waves=simdatPerm$timeorder)
zcor


###################################################
### code chunk number 11: geepack-manual.Rnw:200-202
###################################################
mod4 <- geeglm(yvar~tvar, id=idvar, data=simdatPerm, corstr="fixed", zcor=zcor)
mod4


