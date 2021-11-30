## ----setup, include=FALSE, message=FALSE--------------------------------------
library(knitr)

## ----dwplot1,message=FALSE,warning=FALSE--------------------------------------
library("dplyr")
library("tidyr")
library("broom.mixed")
if (require("brms") && require("dotwhisker") && require("ggplot2")) {
    L <- load(system.file("extdata", "brms_example.rda", package="broom.mixed"))
    gg0 <- (tidy(brms_crossedRE)
        ## disambiguate
        %>% mutate(term=ifelse(grepl("sd__(Int",term,fixed=TRUE),
                               paste(group,term,sep="."),
                               term))
        %>% dwplot
    )
    gg0 + geom_vline(xintercept=0,lty=2)
}

## ----results="asis",echo=FALSE, message=FALSE---------------------------------
cc <- read.csv(system.file("capabilities.csv",package="broom.mixed"))
if (require("pander")) {
    pander::pander(cc,split.tables=Inf)
}

