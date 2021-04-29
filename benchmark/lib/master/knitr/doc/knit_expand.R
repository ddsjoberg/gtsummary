## -----------------------------------------------------------------------------
library(knitr)
knit_expand(text = 'The value of pi is {{pi}}.')
knit_expand(text = 'The value of a is {{a}}, so a + 1 is {{a+1}}.', a = rnorm(1))
knit_expand(text = 'The area of a circle with radius {{r}} is {{pi*r^2}}', r = 5)

## -----------------------------------------------------------------------------
knit_expand(text = 'a is {{a}} and b is {{b}}, with my own pi being {{pi}} instead of {{base::pi}}', a=1, b=2, pi=3)

## -----------------------------------------------------------------------------
knit_expand(text = 'I do not like curly braces, so use % with <> instead: a is <% a %>.', a = 8, delim = c("<%", "%>"))

## -----------------------------------------------------------------------------
knit_expand(text = 'hello $(LETTERS[24]) and $(pi)!', delim = c("$(", ")"))

## -----------------------------------------------------------------------------
knit_expand(text = 'you cannot see the value of x {{x=rnorm(1)}}but it is indeed created: x = {{x}}')
res = knit_expand(text = c(' x | x^2', '{{x=1:5;paste(sprintf("%2d | %3d", x, x^2), collapse = "\n")}}'))
cat(res)

## -----------------------------------------------------------------------------
res = knit_expand(text = c('{{i=0;h2=function(x){i<<-i+1;sprintf("<h2>%d. %s</h2>", i, x)} }}<html>',
'{{h2("First Section")}}', '{{h2("Second Section")}}', '{{h2("Conclusion")}}', '</html>'))
cat(res)

## ----lm-cyl-------------------------------------------------------------------
lm(mpg~cyl, data=mtcars)

## ----lm-disp------------------------------------------------------------------
lm(mpg~disp, data=mtcars)

## ----lm-hp--------------------------------------------------------------------
lm(mpg~hp, data=mtcars)

## ----lm-drat------------------------------------------------------------------
lm(mpg~drat, data=mtcars)

## ----lm-wt--------------------------------------------------------------------
lm(mpg~wt, data=mtcars)

## ----lm-qsec------------------------------------------------------------------
lm(mpg~qsec, data=mtcars)

## ----lm-vs--------------------------------------------------------------------
lm(mpg~vs, data=mtcars)

## ----lm-am--------------------------------------------------------------------
lm(mpg~am, data=mtcars)

## ----lm-gear------------------------------------------------------------------
lm(mpg~gear, data=mtcars)

## ----lm-carb------------------------------------------------------------------
lm(mpg~carb, data=mtcars)

## ----results='hide'-----------------------------------------------------------
src = lapply(names(mtcars)[-1], function(i) {
knit_expand(text=c("# Regression on {{i}}", '```{r lm-{{i}}}', 'lm(mpg~{{i}}, data=mtcars)', '```'))
})
# knit the source
res = knit_child(text = unlist(src))
res = paste('<pre><code>', gsub('^\\s*|\\s*$', '', res), '</code></pre>', sep = '')

