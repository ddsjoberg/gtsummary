## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(
  comment = ">",
  message = FALSE,
  warning = FALSE,
  out.width = "100%"
)

options(digits=2)

set.seed(333)

## ----echo=FALSE, fig.cap="Yoda Bayes (896 BBY - 4 ABY).", fig.align='center', out.width="80%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/YodaBayes.jpg")

