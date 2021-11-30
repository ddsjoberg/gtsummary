# construct the LahmanData data frame from the package

ldata <- vcdExtra::datasets("Lahman")
ldata <- ldata[-which(ldata$Item=="LahmanData"),]
ldata <- ldata[-grep("Labels", ldata$Item),]
ldata

# get table dimensions
dims <- t(matrix(as.numeric(unlist(strsplit(ldata$dim, "x"))), 2, nrow(ldata)))
title <- sub(" -.*", "", ldata$Title)
ldata <- data.frame(file=ldata$Item, class=ldata$class, nobs=dims[,1], nvar=dims[,2], title=title, stringsAsFactors = FALSE )

LahmanData <- ldata


save(LahmanData, file="data/LahmanData.RData")