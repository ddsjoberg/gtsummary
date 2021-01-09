## data.dir<-system.file("inst",package="inst")
data.dir<-"~/MI/inst"

## read in data
library(foreign)
women<-imputationList(lapply(list.files(data.dir,pattern="f.\\.dta",full=TRUE),
                             read.dta))
men<-imputationList(lapply(list.files(data.dir,pattern="m.\\.dta",full=TRUE),
                           read.dta))

## add sex variable
women<-update(women,sex=0)
men<-update(men, sex=1)
## combine two sets of imputations
all<-rbind(women,men)
all<-update(all, drkreg=as.numeric(drkfre)>2)

## tables
with(all, table(sex, drkfre))
with(all, table(sex, drkreg))

## logistic regression model
model1<-with(all, glm(drkreg~wave*sex, family=binomial()))
MIcombine(model1)
summary(MIcombine(model1))

## alternative version
beta<-MIextract(model1, fun=coef)
vars<-MIextract(model1, fun=vcov)
summary(MIcombine(beta, vars))

