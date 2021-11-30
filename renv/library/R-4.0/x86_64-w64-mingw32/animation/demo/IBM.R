## an example taken from the R-help mailing list (show IBM stock price)
## URL: http://tolstoy.newcastle.edu.au/R/e12/help/10/11/5831.html

library(animation)

if (require('zoo') && require('fImport')) {
  saveLatex({
    library(zoo)
    library(fImport)
    IBM = yahooSeries('IBM', from = '2000-01-01')
    IBM.Close = IBM[, 'IBM.Close']
    rng = range(time(IBM.Close))
    Syr = as.numeric(format(rng[1], '%Y'))
    Eyr = as.numeric(format(rng[2], '%Y'))
    Smth = as.numeric(format(rng[1], '%m'))
    for (yr in Syr:Eyr) {
      par(mfrow = c(4, 3), mar = c(4, 4, 1, .1))
      Temp1 = IBM.Close[which(format(time(IBM.Close), '%Y') == yr), ]
      Temp3 = tapply(Temp1[, 1], as.yearmon(time(Temp1)), FUN = mean)
      for (i in Smth:length(Temp3)) {
        i = ifelse(i < 10, paste(0, i, sep = ''), i)
        Date = paste(i, yr, sep = '-')
        Temp2 = IBM.Close[which(format(time(IBM.Close), '%m-%Y') == Date), ]
        plot(time(Temp2), Temp2, type = 'l',
             main = paste(factor(as.numeric(i), labels = month.name[as.numeric(i)]),
                          yr, sep = '-')
        )
      }
    }
  }, interval = 1, latex.filename = 'IBM-2000-2010.tex',
            documentclass = '\\documentclass{article}\n\\usepackage{a4wide}',
            ani.height = 600, ani.width = 600)
}

## the code can be embedded into Sweave without modifications
## 'latex.filename' and 'documentclass' are not necessary for Sweave
