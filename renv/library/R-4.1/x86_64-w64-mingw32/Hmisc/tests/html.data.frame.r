require(Hmisc)
getHdata(pbc)
f <- '/tmp/z.html'
cat('', file=f)
html(contents(pbc), levelType='table')
d <- describe(pbc)
html(d)
