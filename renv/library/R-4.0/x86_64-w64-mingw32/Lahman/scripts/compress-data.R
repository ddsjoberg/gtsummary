# compress the data sets in data/
# see: https://r-pkgs.org/data.html

tools::checkRdaFiles("data")
tools::resaveRdaFiles("data", compress="bzip2")