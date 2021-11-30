## ---- echo = FALSE------------------------------------------------------------

suppressPackageStartupMessages(library(huxtable))
is_latex <- guess_knitr_output_format() == 'latex'

comp <- read.csv('comparison.csv', stringsAsFactors = FALSE, header = FALSE)

ch <- as_hux(comp, add_colnames = FALSE)
bold(ch)[1,] <- TRUE
bottom_border(ch)[1,] <- 1
subsections <- ch[[1]] %in% c('HTML output', 'LaTeX output', 'Other features', 'Other formats', 'Notes')
top_border(ch)[subsections, ] <- 1
bold(ch)[subsections, 1] <- TRUE
italic(ch)[subsections, 1] <- TRUE
background_color(ch)[, seq(3, ncol(ch), 2)] <- grey(.95)
background_color(ch)[, 2] <- 'lightpink'
rotation(ch)[1,] <- 270
valign(ch)[1,] <- 'middle'
align(ch)[-1, -1] <- 'center'
ch <- set_all_padding(ch, -1, everywhere, 0)

ch <- rbind(ch, rep('', ncol(ch)))
last <- nrow(ch)
ch[last, 1] <- 'A (Y) means that there is limited support for the feature. 
                    For example, multirow cells may only be supported in headers, or only horizontal
                    border lines may work.'
font_size(ch)[last, 1] <- 10
colspan(ch)[last, 1]   <- ncol(ch)
bold(ch)[last, 1]      <- FALSE
italic(ch)[last, 1]    <- FALSE
bottom_border(ch)[last, 1] <- 2
wrap(ch) <- TRUE

if (is_latex) {
  row_height(ch) <- c('20pt', rep('10pt', nrow(ch) - 1))
  col_width(ch) <- c('120pt', rep('36pt', ncol(ch) - 1))
  height(ch) <- 0.95 
  position(ch) <- 'left'
  font_size(ch) <- 10
  font_size(ch)[c(last - 1, last), 1] <- 8
  ch1 <- ch[, 1:8]
  ch2 <- ch[, c(1, 9:ncol(ch))]
  width(ch1) <- 1
  width(ch2) <- 1
  caption(ch1) <- 'Comparison table, part 1'
  caption(ch2) <- 'Comparison table, part 2'
} else {
  width(ch) <- '700pt'
  row_height(ch) <- c('80pt', rep('14pt', nrow(ch) - 1))
  col_width(ch) <- c('60pt', rep('30pt', ncol(ch) - 1))
}

if (! is_latex) ch
if (is_latex) ch1
if (is_latex) ch2


