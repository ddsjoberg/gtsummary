## ---- echo = FALSE, results = "hide", message = FALSE-------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
require(bit)
.ff.version <- try(packageVersion("ff"), silent = TRUE)
.ff.is.available <- !inherits(.ff.version, "try-error") && .ff.version >= "4.0.0" && require(ff)
# rmarkdown::render("vignettes/bit-usage.Rmd")
# devtools::build_vignettes()

## -----------------------------------------------------------------------------
logical()
bit()
bitwhich()

## -----------------------------------------------------------------------------
logical(3)
bit(3)
bitwhich(3)

## -----------------------------------------------------------------------------
bitwhich(3, TRUE)

## -----------------------------------------------------------------------------
bitwhich(3, 2)
bitwhich(3, -2)

## -----------------------------------------------------------------------------
l <- logical(3)
length(l) <- 6
l

## -----------------------------------------------------------------------------
b <- bit(3)
length(b) <- 6
b

## -----------------------------------------------------------------------------
w <- bitwhich(3,2)
length(w) <- 6
w
w <- bitwhich(3,-2)
length(w) <- 6
w

## -----------------------------------------------------------------------------
l <- logical(3); l[6]
b <- bit(3); b[6]
w <- bitwhich(3); w[6]

## -----------------------------------------------------------------------------
l[6] <- NA; l
b[6] <- NA; b
w[6] <- NA; w

## -----------------------------------------------------------------------------
l[[6]]
b[[6]]
w[[6]]

## -----------------------------------------------------------------------------
l[[9]] <- TRUE
b[[9]] <- TRUE
w[[9]] <- TRUE
l
b
w

## -----------------------------------------------------------------------------
l <- c(FALSE, TRUE, FALSE)
i <- as.integer(l)
as.logical(i)

## -----------------------------------------------------------------------------
l <- c(FALSE, TRUE, FALSE)
w <- as.which(l)
w
as.logical(w)

## -----------------------------------------------------------------------------
l <- c(FALSE, TRUE, FALSE)
w <- which(l)
w
as.logical(w)  # does not coerce back

## -----------------------------------------------------------------------------
i <- c(7,3)
w <- as.which(i, maxindex=12)
w

## -----------------------------------------------------------------------------
as.integer(w)

## -----------------------------------------------------------------------------
r <- ri(1, 2^16, 2^20) # sample(2^20, replace=TRUE, prob=c(.125,875))
all.as <- list(
  double = as.double
, integer= as.integer
, logical = as.logical
, bit = as.bit
, bitwhich = as.bitwhich
, which = as.which
, ri = function(x)x
)
all.types <- lapply(all.as, function(f)f(r))
sapply(all.types, object.size)

## -----------------------------------------------------------------------------
all.comb <- vector('list', length(all.types)^2)
all.id <- rep(NA, length(all.types)^2)
dim(all.comb)      <- dim(all.id)      <-    c(from=length(all.types),  to=length(all.types))
dimnames(all.comb) <- dimnames(all.id) <- list(from= names(all.types) , to= names(all.types))
for (i in seq_along(all.types))
  for (j in seq_along(all.as)){
    # coerce all types to all types (FROM -> TO)
    all.comb[[i,j]] <- all.as[[j]](all.types[[i]])
    # and test whether coercing back to the FROM type gives the orginal object
    all.id[i,j] <- identical(all.as[[i]](all.comb[[i,j]]),  all.types[[i]])
  }
all.id

## -----------------------------------------------------------------------------
data.frame(booltype=sapply(all.types, booltype), is.boolean=sapply(all.types, is.booltype), row.names=names(all.types))

## -----------------------------------------------------------------------------
x <- bit(1e6)
y <- x | c(FALSE, TRUE)
object.size(y) / object.size(x)

## -----------------------------------------------------------------------------
x <- bit(1e6)
y <- x | as.bit(c(FALSE, TRUE))
object.size(y) / object.size(x)

## -----------------------------------------------------------------------------
l <- logical(6)
b <- bit(6) 
c(l,b)

## -----------------------------------------------------------------------------
c(b,l)
c(l, as.logical(b))

## -----------------------------------------------------------------------------
c(as.bit(l), b)

## -----------------------------------------------------------------------------
c.booltype(l, b)

## -----------------------------------------------------------------------------
b <- as.bit(c(FALSE,TRUE))
rev(b)
rep(b, 3)
rep(b, length.out=6)

## -----------------------------------------------------------------------------
l <- c(NA,NA,FALSE,TRUE,TRUE)
b <- as.bit(l)
length(b)
anyNA(b)
any(b)
all(b)
sum(b)
min(b)
max(b)
range(b)
summary(b)

## -----------------------------------------------------------------------------
# minimum after coercion to integer
min(c(FALSE,TRUE))
# minimum  position of first TRUE
min.booltype(c(FALSE,TRUE))

## -----------------------------------------------------------------------------
b <- as.bit(sample(c(FALSE, TRUE), 1e6, TRUE))
summary(b,range=c(1,3e5))

## -----------------------------------------------------------------------------
sapply(chunk(b, by=3e5, method="seq"), function(i)summary(b, range=i))

## -----------------------------------------------------------------------------
sapply(chunk(b, by=3e5), function(i)summary(b, range=i))

## ---- eval=.ff.is.available, message=FALSE------------------------------------
x <- ff(vmode="single", length=length(b))   # create a huge ff vector
x[as.hi(b)] <- runif(sum(b))      # replace some numbers at filtered positions
summary(x[])

## ---- eval=.ff.is.available---------------------------------------------------
sapply(chunk(x, by=3e5), function(i)summary(x[i]))

## ---- eval=.ff.is.available---------------------------------------------------
sapply(chunk(x, by=3e5), function(i)summary(x[as.hi(b, range=i)]))

## -----------------------------------------------------------------------------
set.seed(1); n <- 9
x <- sample(n, replace=TRUE); x
y <- sample(n, replace=TRUE); y
x %in% y
bit_in(x,y)
bit_in(x,y, retFUN=as.logical)

## -----------------------------------------------------------------------------
x <- c(NA,NA,1L,1L,2L,3L)
duplicated(x)
bit_duplicated(x, retFUN=as.logical)
bit_duplicated(x, na.rm=NA, retFUN=as.logical)

duplicated(x, incomparables = NA)
bit_duplicated(x, na.rm=FALSE, retFUN=as.logical)

bit_duplicated(x, na.rm=TRUE, retFUN=as.logical)

## -----------------------------------------------------------------------------
x <- c(NA,NA,1L,1L,2L,3L)
unique(x)
bit_unique(x)

unique(x, incomparables = NA)
bit_unique(x, na.rm=FALSE)

bit_unique(x, na.rm=TRUE)

## -----------------------------------------------------------------------------
x <- c(NA,NA,1L,1L,3L)
y <- c(NA,NA,2L,2L,3L)
union(x,y)
bit_union(x,y)

## -----------------------------------------------------------------------------
x <- c(0L,NA,NA,1L,1L,3L)
y <- c(NA,NA,2L,2L,3L,4L)
intersect(x,y)
bit_intersect(x,y)

## -----------------------------------------------------------------------------
x <- c(0L,NA,NA,1L,1L,3L)
y <- c(NA,NA,2L,2L,3L,4L)
setdiff(x,y)
bit_setdiff(x,y)

## -----------------------------------------------------------------------------
x <- c(0L,NA,NA,1L,1L,3L)
y <- c(NA,NA,2L,2L,3L,4L)
union(setdiff(x,y),setdiff(y,x))
bit_symdiff(x,y)

## -----------------------------------------------------------------------------
x <- c(0L,NA,NA,1L,1L,3L)
y <- c(NA,NA,2L,2L,3L,4L)
setequal(y,x)
bit_setequal(x,y)

## -----------------------------------------------------------------------------
bit_rangediff(c(1L,7L), (3:5))
bit_rangediff(c(7L,1L), (3:5))
bit_rangediff(c(1L,7L), -(3:5), revy=TRUE)
bit_rangediff(c(1L,7L), -(3:5), revx=TRUE)

## -----------------------------------------------------------------------------
bit_rangediff(c(1L,7L), (1:7))
bit_rangediff(c(1L,7L), -(1:7))
bit_rangediff(c(1L,7L), (1:7), revy=TRUE)

## -----------------------------------------------------------------------------
(1:9)[-7]
bit_rangediff(c(1L,9L), -7L, revy=TRUE)

## -----------------------------------------------------------------------------
x <- c(NA,NA,1L,1L,2L,3L)
    any(duplicated(x))  # full hash work, returns FALSE or TRUE
     anyDuplicated(x)   # early termination of hash work, returns 0 or position of first duplicate
any(bit_duplicated(x))  # full bit work, returns FALSE or TRUE
 bit_anyDuplicated(x)   # early termination of bit work, returns 0 or position of first duplicate

## -----------------------------------------------------------------------------
x <- c(NA,NA,1L,1L,2L,3L)
    sum(duplicated(x))  # full hash work, returns FALSE or TRUE
sum(bit_duplicated(x))  # full bit work, returns FALSE or TRUE
 bit_sumDuplicated(x)   # early termination of bit work, returns 0 or position of first duplicated

## -----------------------------------------------------------------------------
x <- sample(9, 9, TRUE)
unique(sort(x))
sort(unique(x))
bit_sort_unique(x)

## -----------------------------------------------------------------------------
x <- sample(9, 9, TRUE)
sort(x)
bit_sort(x)

## -----------------------------------------------------------------------------
x = sample(12)
bit_sort(x)
merge_unique(bit_sort(x))
bit_sort_unique(x)

## -----------------------------------------------------------------------------
x = as.integer(c(3,4,4,5))
y = as.integer(c(3,4,5))
setequal(x,y)
merge_setequal(x,y)
merge_setequal(x,y, method="exact")

## -----------------------------------------------------------------------------
x = as.integer(c(0,1,2,2,3,3,3))
y = as.integer(c(1,2,3))
setdiff(x,y)
merge_setdiff(x,y)
merge_setdiff(x,y, method="exact")
merge_rangediff(c(0L,4L),y)
merge_rangediff(c(0L,4L),c(-3L,-2L)) # y has no effect due to different sign
merge_rangediff(c(0L,4L),c(-3L,-2L), revy=TRUE)
merge_rangediff(c(0L,4L),c(-3L,-2L), revx=TRUE)

## -----------------------------------------------------------------------------
x = -2:1
y = -1:2
setdiff(x,y)
union(setdiff(x,y),setdiff(y,x))
merge_symdiff(x,y)
merge_intersect(x,y)
merge_rangesect(c(-2L,1L),y)

## -----------------------------------------------------------------------------
x = as.integer(c(1,2,2,3,3,3))
y = 2:4
union(x,y)
merge_union(x,y, method="unique")
merge_union(x,y, method="exact")
merge_union(x,y, method="all")
sort(c(x,y))
c(x,y)

## -----------------------------------------------------------------------------
x = 2:4
y = as.integer(c(0,1,2,2,3,3,3))
match(x,y)
merge_match(x,y)

## -----------------------------------------------------------------------------
x %in% y
merge_in(x,y)
merge_notin(x,y)

## -----------------------------------------------------------------------------
x <- c(2L,4L)
merge_rangein(x,y)
merge_rangenotin(x,y)

## -----------------------------------------------------------------------------
x <- bit_sort(sample(1000,10))
merge_first(x)
merge_last(x)
merge_firstnotin(c(300L,600L), x)
merge_firstin(c(300L,600L), x)
merge_lastin(c(300L,600L), x)
merge_lastnotin(c(300L,600L), x)

