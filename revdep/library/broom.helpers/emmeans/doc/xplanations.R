## ---- echo = FALSE, results = "hide", message = FALSE---------------------------------------------
require("emmeans")
knitr::opts_chunk$set(fig.width = 4.5, fig.height = 2.0, class.output = "ro",
                      class.message = "re", class.error = "re", class.warning = "re")
###knitr::opts_chunk$set(fig.width = 4.5, fig.height = 2.0)

## ---- message = FALSE-----------------------------------------------------------------------------
m = c(6.1, 4.5, 5.4,    6.3, 5.5, 6.7)
se2 = c(.3, .4, .37,  .41, .23, .48)^2
lev = list(A = c("a1","a2","a3"), B = c("b1", "b2"))
foo = emmobj(m, diag(se2), levels = lev, linfct = diag(6))
plot(foo, CIs = FALSE, comparisons = TRUE)

## ---- message = FALSE-----------------------------------------------------------------------------
mkmat <- function(V, rho = 0, indexes = list(1:3, 4:6)) {
    sd = sqrt(diag(V))
    for (i in indexes)
        V[i,i] = (1 - rho)*diag(sd[i]^2) + rho*outer(sd[i], sd[i])
    V
}
# Intraclass correrlation = 0.3
foo3 = foo
foo3@V <- mkmat(foo3@V, 0.3)
plot(foo3, CIs = FALSE, comparisons = TRUE)

## ---- message = FALSE-----------------------------------------------------------------------------
foo6 = foo
foo6@V <- mkmat(foo6@V, 0.6)
plot(foo6, CIs = FALSE, comparisons = TRUE)

## ---- message = FALSE, error = TRUE---------------------------------------------------------------
foo8 = foo
foo8@V <- mkmat(foo8@V, 0.8)
plot(foo8, CIs = FALSE, comparisons = TRUE)

## ---- message = FALSE-----------------------------------------------------------------------------
plot(foo8, CIs = FALSE, comparisons = TRUE, by = "B")

## -------------------------------------------------------------------------------------------------
pwpp(foo6, sort = FALSE)
pwpm(foo6)

