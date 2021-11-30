### R code from vignette source 'discrim.Rnw'

###################################################
### code chunk number 1: discrim.Rnw:39-45
###################################################
options(continue="  ", width=70)
options(SweaveHooks=list(fig=function() par(mar=c(4.1, 4.1, .3, 1.1))))
pdf.options(pointsize=10) #text in graph about the same as regular text
options(contrasts=c("contr.treatment", "contr.poly")) #ensure default
library("survival")
palette(c("#000000", "#D95F02", "#1B9E77", "#7570B3", "#E7298A", "#66A61E"))


###################################################
### code chunk number 2: discrim.Rnw:276-281
###################################################
tdata <- data.frame(id=c(1,1,1, 2, 3,3, 4, 5),
                    time1=c(0, 1, 4, 0, 0, 3, 0, 0),
                    time2=c(1, 4, 8, 2, 3, 6, 7, 8),
                    state=c(2, 3, 0, 0, 2, 0, 0, 0))
tdata


