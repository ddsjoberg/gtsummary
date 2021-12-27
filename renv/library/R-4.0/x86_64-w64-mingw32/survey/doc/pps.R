### R code from vignette source 'pps.Rnw'

###################################################
### code chunk number 1: pps.Rnw:57-61
###################################################
library(survey)
data(election)
summary(election$p)
summary(election_pps$p)


###################################################
### code chunk number 2: pps.Rnw:65-77
###################################################
## Hajek type
dpps_br<- svydesign(id=~1,  fpc=~p, data=election_pps, pps="brewer")
## Horvitz-Thompson type
dpps_ov<- svydesign(id=~1,  fpc=~p, data=election_pps, pps="overton")
dpps_hr<- svydesign(id=~1,  fpc=~p, data=election_pps, pps=HR(sum(election$p^2)/40))
dpps_hr1<- svydesign(id=~1, fpc=~p, data=election_pps, pps=HR())
dpps_ht<- svydesign(id=~1,  fpc=~p, data=election_pps, pps=ppsmat(election_jointprob))
## Yates-Grundy type
dpps_yg<- svydesign(id=~1,  fpc=~p, data=election_pps, pps=ppsmat(election_jointprob),variance="YG")
dpps_hryg<- svydesign(id=~1,  fpc=~p, data=election_pps, pps=HR(sum(election$p^2)/40),variance="YG")
## The with-replacement approximation
dppswr <-svydesign(id=~1, probs=~p, data=election_pps)


###################################################
### code chunk number 3: pps.Rnw:81-82
###################################################
show(image(dpps_ht))


###################################################
### code chunk number 4: pps.Rnw:84-85
###################################################
show(image(dpps_ov))


###################################################
### code chunk number 5: pps.Rnw:91-99
###################################################
svytotal(~Bush+Kerry+Nader, dpps_ht)
svytotal(~Bush+Kerry+Nader, dpps_yg)
svytotal(~Bush+Kerry+Nader, dpps_hr)
svytotal(~Bush+Kerry+Nader, dpps_hryg)
svytotal(~Bush+Kerry+Nader, dpps_hr1)
svytotal(~Bush+Kerry+Nader, dpps_br)
svytotal(~Bush+Kerry+Nader, dpps_ov)
svytotal(~Bush+Kerry+Nader, dppswr)


