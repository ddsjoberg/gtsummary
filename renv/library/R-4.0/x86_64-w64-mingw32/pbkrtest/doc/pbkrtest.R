### R code from vignette source 'pbkrtest.Rnw'

###################################################
### code chunk number 1: pbkrtest.Rnw:19-22
###################################################
require( pbkrtest )
prettyVersion <- packageDescription("pbkrtest")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: pbkrtest.Rnw:65-67
###################################################
options(prompt = "R> ", continue = "+  ", width = 80, useFancyQuotes=FALSE)
dir.create("figures")


###################################################
### code chunk number 3: pbkrtest.Rnw:72-73
###################################################
library(pbkrtest)


###################################################
### code chunk number 4: pbkrtest.Rnw:82-84
###################################################
data(shoes, package="MASS")
shoes


###################################################
### code chunk number 5: pbkrtest.Rnw:90-93
###################################################
plot(A~1, data=shoes, col="red",lwd=2, pch=1, ylab="wear", xlab="boy")
points(B~1, data=shoes, col="blue", lwd=2, pch=2)
points(I((A+B)/2)~1, data=shoes, pch="-", lwd=2)


###################################################
### code chunk number 6: pbkrtest.Rnw:101-104
###################################################
r1<-t.test(shoes$A, shoes$B, paired=T)
r2<-t.test(shoes$A-shoes$B)
r1


###################################################
### code chunk number 7: pbkrtest.Rnw:112-120
###################################################
boy <- rep(1:10,2)
boyf<- factor(letters[boy])
mat <- factor(c(rep("A", 10), rep("B",10)))
## Balanced data:
shoe.b <- data.frame(wear=unlist(shoes), boy=boy, boyf=boyf, mat=mat)
head(shoe.b)
## Imbalanced data; delete (boy=1, mat=1) and (boy=2, mat=b)
shoe.i <-  shoe.b[-c(1,12),]


###################################################
### code chunk number 8: pbkrtest.Rnw:126-130
###################################################
lmm1.b  <- lmer( wear ~ mat + (1|boyf), data=shoe.b )
lmm0.b  <- update( lmm1.b, .~. - mat)
lmm1.i  <- lmer( wear ~ mat + (1|boyf), data=shoe.i )
lmm0.i  <- update(lmm1.i, .~. - mat)


###################################################
### code chunk number 9: pbkrtest.Rnw:137-139
###################################################
anova( lmm1.b, lmm0.b, test="Chisq" ) ## Balanced data
anova( lmm1.i, lmm0.i, test="Chisq" ) ## Imbalanced data


###################################################
### code chunk number 10: pbkrtest.Rnw:150-151
###################################################
( kr.b<-KRmodcomp(lmm1.b, lmm0.b) )


###################################################
### code chunk number 11: pbkrtest.Rnw:155-156
###################################################
summary( kr.b )


###################################################
### code chunk number 12: pbkrtest.Rnw:162-163
###################################################
getKR(kr.b, "ddf")


###################################################
### code chunk number 13: pbkrtest.Rnw:168-169
###################################################
( kr.i<-KRmodcomp(lmm1.i, lmm0.i) )


###################################################
### code chunk number 14: pbkrtest.Rnw:176-178
###################################################
shoes2 <- list(A=shoes$A[-(1:2)], B=shoes$B[-(1:2)])
t.test(shoes2$A, shoes2$B, paired=T)


###################################################
### code chunk number 15: pbkrtest.Rnw:191-192
###################################################
( pb.b <- PBmodcomp(lmm1.b, lmm0.b, nsim=500, cl=2) )


###################################################
### code chunk number 16: pbkrtest.Rnw:196-197
###################################################
summary( pb.b )


###################################################
### code chunk number 17: pbkrtest.Rnw:205-206
###################################################
( pb.i<-PBmodcomp(lmm1.i, lmm0.i, nsim=500, cl=2) )


###################################################
### code chunk number 18: pbkrtest.Rnw:210-211
###################################################
summary( pb.i )


###################################################
### code chunk number 19: pbkrtest.Rnw:223-227
###################################################
shoe3 <- subset(shoe.b, boy<=5)
shoe3 <- shoe3[order(shoe3$boy), ]
lmm1  <- lmer( wear ~ mat + (1|boyf), data=shoe3 )
str( SG <- get_SigmaG( lmm1 ), max=2)


###################################################
### code chunk number 20: pbkrtest.Rnw:231-232
###################################################
round( SG$Sigma*10 )


###################################################
### code chunk number 21: pbkrtest.Rnw:236-237
###################################################
SG$G


