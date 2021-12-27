### R code from vignette source 'survey.Rnw'

###################################################
### code chunk number 1: survey.Rnw:26-29
###################################################
library(survey)
data(api)
dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)


###################################################
### code chunk number 2: survey.Rnw:33-34
###################################################
summary(dclus1)


###################################################
### code chunk number 3: survey.Rnw:43-48
###################################################
svymean(~api00, dclus1)
svyquantile(~api00, dclus1, quantile=c(0.25,0.5,0.75), ci=TRUE)
svytotal(~stype, dclus1)
svytotal(~enroll, dclus1)
svyratio(~api.stu,~enroll, dclus1)


###################################################
### code chunk number 4: survey.Rnw:55-56
###################################################
svyratio(~api.stu, ~enroll, design=subset(dclus1, stype=="H"))


###################################################
### code chunk number 5: survey.Rnw:64-66
###################################################
vars<-names(apiclus1)[c(12:13,16:23,27:37)]
svymean(make.formula(vars),dclus1,na.rm=TRUE)


###################################################
### code chunk number 6: survey.Rnw:73-74
###################################################
svyby(~ell+meals, ~stype, design=dclus1, svymean)


###################################################
### code chunk number 7: survey.Rnw:79-83
###################################################
regmodel <- svyglm(api00~ell+meals,design=dclus1)
logitmodel <- svyglm(I(sch.wide=="Yes")~ell+meals, design=dclus1, family=quasibinomial())
summary(regmodel)
summary(logitmodel)


###################################################
### code chunk number 8: survey.Rnw:87-88
###################################################
gclus1 <- calibrate(dclus1, formula=~api99, population=c(6194, 3914069))


###################################################
### code chunk number 9: survey.Rnw:91-96
###################################################
svymean(~api00, gclus1)
svyquantile(~api00, gclus1, quantile=c(0.25,0.5,0.75), ci=TRUE)
svytotal(~stype, gclus1)
svytotal(~enroll, gclus1)
svyratio(~api.stu,~enroll, gclus1)


