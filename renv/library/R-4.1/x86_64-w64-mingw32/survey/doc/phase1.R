### R code from vignette source 'phase1.Rnw'

###################################################
### code chunk number 1: phase1.Rnw:82-105
###################################################
rei<-read.table(textConnection(
"  id   N n.a h n.ah n.h   sub  y
1   1 300  20 1   12   5  TRUE  1
2   2 300  20 1   12   5  TRUE  2
3   3 300  20 1   12   5  TRUE  3
4   4 300  20 1   12   5  TRUE  4
5   5 300  20 1   12   5  TRUE  5
6   6 300  20 1   12   5 FALSE NA
7   7 300  20 1   12   5 FALSE NA
8   8 300  20 1   12   5 FALSE NA
9   9 300  20 1   12   5 FALSE NA
10 10 300  20 1   12   5 FALSE NA
11 11 300  20 1   12   5 FALSE NA
12 12 300  20 1   12   5 FALSE NA
13 13 300  20 2    8   3  TRUE  6
14 14 300  20 2    8   3  TRUE  7
15 15 300  20 2    8   3  TRUE  8
16 16 300  20 2    8   3 FALSE NA
17 17 300  20 2    8   3 FALSE NA
18 18 300  20 2    8   3 FALSE NA
19 19 300  20 2    8   3 FALSE NA
20 20 300  20 2    8   3 FALSE NA
"), header=TRUE)


###################################################
### code chunk number 2: phase1.Rnw:109-113
###################################################
library(survey)
des.rei <- twophase(id=list(~id,~id), strata=list(NULL,~h),
                    fpc=list(~N,NULL), subset=~sub, data=rei)
tot<- svytotal(~y, des.rei)


###################################################
### code chunk number 3: phase1.Rnw:117-124
###################################################
rei$w.ah <- rei$n.ah / rei$n.a
a.rei <- aggregate(rei, by=list(rei$h), mean, na.rm=TRUE)
a.rei$S.ysh <- tapply(rei$y, rei$h, var, na.rm=TRUE)
a.rei$y.u <- sum(a.rei$w.ah * a.rei$y)
a.rei$f<-with(a.rei, n.a/N)
a.rei$delta.h<-with(a.rei, (1/n.h)*(n.a-n.ah)/(n.a-1))
Vphase1<-with(a.rei, sum(N*N*((1-f)/n.a)*( w.ah*(1-delta.h)*S.ysh+ ((n.a)/(n.a-1))*w.ah*(y-y.u)^2)))


###################################################
### code chunk number 4: phase1.Rnw:128-130
###################################################
Vphase1
attr(vcov(tot),"phases")$phase1


