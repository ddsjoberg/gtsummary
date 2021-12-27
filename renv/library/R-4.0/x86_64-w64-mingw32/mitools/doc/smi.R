### R code from vignette source 'smi.Rnw'

###################################################
### code chunk number 1: smi.Rnw:9-10
###################################################
options(width=70)


###################################################
### code chunk number 2: smi.Rnw:19-30
###################################################
library(mitools)
data.dir<-system.file("dta",package="mitools")

## read in data
library(foreign)
women<-imputationList(lapply(list.files(data.dir,
				pattern="f.\\.dta",full=TRUE),
                           read.dta, warn.missing.labels=FALSE))
men<-imputationList(lapply(list.files(data.dir,
				pattern="m.\\.dta",full=TRUE),
                           read.dta, warn.missing.labels=FALSE))


###################################################
### code chunk number 3: smi.Rnw:34-41
###################################################
## add sex variable
women<-update(women,sex=0)
men<-update(men, sex=1)
## combine two sets of imputations
all<-rbind(women,men)
all
colnames(all)


###################################################
### code chunk number 4: smi.Rnw:45-46
###################################################
with(all, table(sex, drkfre))


###################################################
### code chunk number 5: smi.Rnw:49-52
###################################################
all<-update(all, drkreg=as.numeric(drkfre)>2)
## tables
with(all, table(sex, drkreg))


###################################################
### code chunk number 6: smi.Rnw:56-60
###################################################
## logistic regression model
model1<-with(all, glm(drkreg~wave*sex, family=binomial()))
MIcombine(model1)
summary(MIcombine(model1))


###################################################
### code chunk number 7: smi.Rnw:65-68
###################################################
beta<-MIextract(model1, fun=coef)
vars<-MIextract(model1, fun=vcov)
summary(MIcombine(beta,vars))


