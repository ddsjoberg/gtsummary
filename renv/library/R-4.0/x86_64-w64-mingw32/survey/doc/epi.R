### R code from vignette source 'epi.Rnw'

###################################################
### code chunk number 1: epi.Rnw:45-61
###################################################
library(survey)
load(system.file("doc","nwts.rda",package="survey"))
nwtsnb<-nwts
nwtsnb$case<-nwts$case-nwtsb$case
nwtsnb$control<-nwts$control-nwtsb$control

a<-rbind(nwtsb,nwtsnb)
a$in.ccs<-rep(c(TRUE,FALSE),each=16)

b<-rbind(a,a)
b$rel<-rep(c(1,0),each=32)
b$n<-ifelse(b$rel,b$case,b$control)
index<-rep(1:64,b$n)

nwt.exp<-b[index,c(1:3,6,7)]
nwt.exp$id<-1:4088


###################################################
### code chunk number 2: epi.Rnw:65-66
###################################################
glm(rel~factor(stage)*factor(histol), family=binomial, data=nwt.exp)


###################################################
### code chunk number 3: epi.Rnw:75-79
###################################################
dccs2<-twophase(id=list(~id,~id),subset=~in.ccs,
                strata=list(NULL,~interaction(instit,rel)),data=nwt.exp)

summary(svyglm(rel~factor(stage)*factor(histol),family=binomial,design=dccs2))


###################################################
### code chunk number 4: epi.Rnw:88-94
###################################################
dccs8<-twophase(id=list(~id,~id),subset=~in.ccs,
                strata=list(NULL,~interaction(instit,stage,rel)),data=nwt.exp)
gccs8<-calibrate(dccs2,phase=2,formula=~interaction(instit,stage,rel))

summary(svyglm(rel~factor(stage)*factor(histol),family=binomial,design=dccs8))
summary(svyglm(rel~factor(stage)*factor(histol),family=binomial,design=gccs8))


###################################################
### code chunk number 5: epi.Rnw:122-126
###################################################
library(survey)
library(survival)
data(nwtco)
ntwco<-subset(nwtco, !is.na(edrel))


###################################################
### code chunk number 6: epi.Rnw:130-131
###################################################
coxph(Surv(edrel, rel)~factor(stage)+factor(histol)+I(age/12),data=nwtco)


###################################################
### code chunk number 7: epi.Rnw:143-155
###################################################
(dcch<-twophase(id=list(~seqno,~seqno), strata=list(NULL,~rel),
                  subset=~I(in.subcohort | rel), data=nwtco))
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12),
                design=dcch)

subcoh <- nwtco$in.subcohort
selccoh <- with(nwtco, rel==1|subcoh==1)
ccoh.data <- nwtco[selccoh,] 
ccoh.data$subcohort <- subcoh[selccoh]
cch(Surv(edrel, rel) ~ factor(stage) + factor(histol) + I(age/12),
           data =ccoh.data, subcoh = ~subcohort, id=~seqno,
           cohort.size=4028, method="LinYing")


###################################################
### code chunk number 8: epi.Rnw:165-176
###################################################
nwtco$eventrec<-rep(0,nrow(nwtco))
nwtco.extra<-subset(nwtco, rel==1)
nwtco.extra$eventrec<-1
nwtco.expd<-rbind(subset(nwtco,in.subcohort==1),nwtco.extra)
nwtco.expd$stop<-with(nwtco.expd, 
                      ifelse(rel & !eventrec, edrel-0.001,edrel))
nwtco.expd$start<-with(nwtco.expd, 
                       ifelse(rel & eventrec, edrel-0.001, 0))
nwtco.expd$event<-with(nwtco.expd,
                       ifelse(rel & eventrec, 1, 0))
nwtco.expd$pwts<-ifelse(nwtco.expd$event, 1, 1/with(nwtco,mean(in.subcohort | rel)))


###################################################
### code chunk number 9: epi.Rnw:185-189
###################################################
(dBarlow<-svydesign(id=~seqno+eventrec, strata=~in.subcohort+rel,
                    data=nwtco.expd, weight=~pwts))
svycoxph(Surv(start,stop,event)~factor(stage)+factor(histol)+I(age/12),
                design=dBarlow)


###################################################
### code chunk number 10: epi.Rnw:194-197
###################################################
(dWacholder <- as.svrepdesign(dBarlow,type="bootstrap",replicates=500))
svycoxph(Surv(start,stop,event)~factor(stage)+factor(histol)+I(age/12),
                design=dWacholder)


###################################################
### code chunk number 11: epi.Rnw:209-217
###################################################
load(system.file("doc","nwtco-subcohort.rda",package="survey"))
nwtco$subcohort<-subcohort

d_BorganII <- twophase(id=list(~seqno,~seqno),
                       strata=list(NULL,~interaction(instit,rel)),
                       data=nwtco, subset=~I(rel |subcohort))
(b2<-svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12),
                design=d_BorganII))


###################################################
### code chunk number 12: epi.Rnw:222-225
###################################################
d_BorganIIps <- calibrate(d_BorganII, phase=2, formula=~age+interaction(instit,rel,stage))
svycoxph(Surv(edrel,rel)~factor(stage)+factor(histol)+I(age/12),
                  design=d_BorganIIps)


