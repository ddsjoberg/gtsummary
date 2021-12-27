library(nlme)
if(requireNamespace("sfsmisc")) {
    print(sfsmisc::sessionInfoX(pkgs = c("Matrix", "nlme")))
} else withAutoprint({
    packageDescription("nlme")
    packageDescription("Matrix")
    sessionInfo()
})

sdir <- system.file("scripts", package="nlme")
cat("nlme/scripts directory used: ", sdir, "\n=============\n")
for(f in list.files(sdir, pattern = "^ch[0-9]*[.]R$")) {
    cat("\n",f,":\n------\n", sep='')
    source(file.path(sdir, f), echo=TRUE)
}
## runs through, taking only 54 secs now (2020-07) (in BATCH)
summary(warnings())
