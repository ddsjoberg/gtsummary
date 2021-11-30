\dontrun{
# The data was generated through downloading via the API
library(pxweb)

# Get the last 15 years of data (the data always lags 1 year)
current_year <- as.integer(format(Sys.Date(), "%Y")) -1
SCB <- get_pxweb_data(
  url = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101B/BefolkningMedelAlder",
  dims = list(Region = c('00', '01', '03', '25'),
              Kon = c('1', '2'),
              ContentsCode = c('BE0101G9'),
              Tid = (current_year-14):current_year),
  clean = TRUE)

# Some cleaning was needed before use
SCB$region <- factor(substring(as.character(SCB$region), 4))
Swe_ltrs <- c("å" = "&aring;",
              "Å" = "&Aring;",
              "ä" = "&auml;",
              "Ä" = "&Auml;",
              "ö" = "&ouml;",
              "Ö" = "&Ouml;")
for (i in 1:length(Swe_ltrs)){
  levels(SCB$region) <- gsub(names(Swe_ltrs)[i],
              Swe_ltrs[i],
              levels(SCB$region))
}

save(SCB, file = "data/SCB.rda")
}
