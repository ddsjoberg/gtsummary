### R code from vignette source 'domain.Rnw'

###################################################
### code chunk number 1: domain.Rnw:29-34
###################################################
library(survey)
data(fpc)
dfpc<-svydesign(id=~psuid,strat=~stratid,weight=~weight,data=fpc,nest=TRUE)
dsub<-subset(dfpc,x>4)
svymean(~x,design=dsub)


###################################################
### code chunk number 2: domain.Rnw:41-42
###################################################
svyby(~x,~I(x>4),design=dfpc, svymean)


###################################################
### code chunk number 3: domain.Rnw:49-50
###################################################
summary(svyglm(x~I(x>4)+0,design=dfpc))


###################################################
### code chunk number 4: domain.Rnw:57-58
###################################################
svyratio(~I(x*(x>4)),~as.numeric(x>4), dfpc)


###################################################
### code chunk number 5: domain.Rnw:76-84
###################################################
data(api)
dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
pop.totals<-c(`(Intercept)`=6194, stypeH=755, stypeM=1018)
gclus1 <- calibrate(dclus1, ~stype+api99, c(pop.totals, api99=3914069))

svymean(~api00, subset(gclus1, comp.imp=="Yes"))
svyratio(~I(api00*(comp.imp=="Yes")), ~as.numeric(comp.imp=="Yes"), gclus1)
summary(svyglm(api00~comp.imp-1, gclus1))


###################################################
### code chunk number 6: domain.Rnw:88-94
###################################################
data(mu284)
dmu284<-svydesign(id=~id1+id2,fpc=~n1+n2, data=mu284)

svymean(~y1, subset(dmu284,y1>40))
svyratio(~I(y1*(y1>40)),~as.numeric(y1>40),dmu284)
summary(svyglm(y1~I(y1>40)+0,dmu284))


###################################################
### code chunk number 7: domain.Rnw:100-108
###################################################
library("survival")
data(nwtco)
nwtco$incc2<-as.logical(with(nwtco, ifelse(rel | instit==2,1,rbinom(nrow(nwtco),1,.1))))
dccs8<-twophase(id=list(~seqno,~seqno), strata=list(NULL,~interaction(rel,stage,instit)),
                data=nwtco, subset=~incc2)
svymean(~rel, subset(dccs8,age>36))
svyratio(~I(rel*as.numeric(age>36)), ~as.numeric(age>36), dccs8)
summary(svyglm(rel~I(age>36)+0, dccs8))


