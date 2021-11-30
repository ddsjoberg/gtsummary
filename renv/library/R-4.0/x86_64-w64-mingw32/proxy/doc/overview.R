### R code from vignette source 'overview.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
library(proxy)
x <- summary(pr_DB, "long")
FUN <- function(index) {
    for (i in which(index)) {
        writeLines(sprintf("Aliases: %s", paste(x$names[[i]], collapse = ", ")))
        writeLines(sprintf("Type   : %s", x$type[i]))
        writeLines(sprintf("Formula: %s\n", x$formula[i]))
    }
}


###################################################
### code chunk number 2: overview.Rnw:24-25
###################################################
FUN(x$distance == FALSE)


###################################################
### code chunk number 3: overview.Rnw:29-30
###################################################
FUN(x$distance == TRUE)


