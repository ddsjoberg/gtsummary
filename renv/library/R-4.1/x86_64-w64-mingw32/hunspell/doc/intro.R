## ---- echo = FALSE, message = FALSE-------------------------------------------
knitr::opts_chunk$set(comment = "")
NOT_CRAN = isTRUE(nchar(Sys.getenv('NOT_CRAN')) || (Sys.getenv('USER') == 'jeroen'))

## -----------------------------------------------------------------------------
library(hunspell)

# Check individual words
words <- c("beer", "wiskey", "wine")
correct <- hunspell_check(words)
print(correct)

# Find suggestions for incorrect words
hunspell_suggest(words[!correct])

## -----------------------------------------------------------------------------
bad <- hunspell("spell checkers are not neccessairy for langauge ninjas")
print(bad[[1]])
hunspell_suggest(bad[[1]])

## -----------------------------------------------------------------------------
download.file("https://arxiv.org/e-print/1406.4806v1", "1406.4806v1.tar.gz",  mode = "wb")
untar("1406.4806v1.tar.gz", "content.tex")
text <- readLines("content.tex", warn = FALSE)
bad_words <- hunspell(text, format = "latex")
sort(unique(unlist(bad_words)))

## ---- eval = require('pdftools')----------------------------------------------
text <- pdftools::pdf_text('https://www.gnu.org/licenses/quick-guide-gplv3.pdf')
bad_words <- hunspell(text)
sort(unique(unlist(bad_words)))

## ---- eval=FALSE--------------------------------------------------------------
#  spelling::spell_check_package("~/workspace/V8")

## -----------------------------------------------------------------------------
# Stemming
words <- c("love", "loving", "lovingly", "loved", "lover", "lovely")
hunspell_stem(words)

## -----------------------------------------------------------------------------
hunspell_analyze(words)

## -----------------------------------------------------------------------------
text <- readLines("content.tex", warn = FALSE)
allwords <- hunspell_parse(text, format = "latex")

# Third line (title) only
print(allwords[[3]])

## -----------------------------------------------------------------------------
allwords <- hunspell_parse(janeaustenr::prideprejudice)
stems <- unlist(hunspell_stem(unlist(allwords)))
words <- sort(table(stems), decreasing = TRUE)
print(head(words, 30))

## -----------------------------------------------------------------------------
df <- as.data.frame(words)
df$stems <- as.character(df$stems)
stops <- df$stems %in% stopwords::stopwords(source="stopwords-iso")
wcdata <- head(df[!stops,], 150)
print(wcdata, max = 40)

## ---- eval = NOT_CRAN---------------------------------------------------------
library(wordcloud2)
names(wcdata) <- c("word", "freq")
wcdata$freq <- (wcdata$freq)^(2/3)
wordcloud2(wcdata)

## -----------------------------------------------------------------------------
list_dictionaries()

## -----------------------------------------------------------------------------
dictionary("en_GB")

## ---- eval = FALSE------------------------------------------------------------
#  dutch <- dictionary("~/workspace/Dictionaries/Dutch.dic")
#  print(dutch)

## -----------------------------------------------------------------------------
hunspell("My favourite colour to visualise is grey")
hunspell("My favourite colour to visualise is grey", dict = 'en_GB')

## ---- eval = FALSE------------------------------------------------------------
#  dutch <- dictionary("~/workspace/Dictionaries/Dutch.dic")
#  hunspell("Hij heeft de klok wel horen luiden, maar weet niet waar de klepel hangt", dict = dutch)

## -----------------------------------------------------------------------------
Sys.setenv(DICPATH = "/my/custom/hunspell/dir")
hunspell:::dicpath()

## ---- echo = FALSE, message = FALSE-------------------------------------------
unlink(c("1406.4806v1.tar.gz", "content.tex"))

