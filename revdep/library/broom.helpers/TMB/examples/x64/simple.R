require(TMB)
dyn.load(dynlib("simple"))

## Test data
set.seed(123)
y <- rep(1900:2010,each=2)
year <- factor(y)
quarter <- factor(rep(1:4,length.out=length(year)))
period <- factor((y > mean(y))+1)
## Random year+quarter effect, fixed period effect:
B <- model.matrix(~year+quarter-1)
A <- model.matrix(~period-1)
B <- as(B,"dgTMatrix")
A <- as(A,"dgTMatrix")
u <- rnorm(ncol(B)) ## logsdu=0
beta <- rnorm(ncol(A))*100
eps <- rnorm(nrow(B),sd=1) ## logsd0=0
x <- as.numeric( A %*% beta + B %*% u + eps )

## Fit model
obj <- MakeADFun(data=list(x=x, B=B, A=A),
                 parameters=list(u=u*0, beta=beta*0, logsdu=1, logsd0=1),
                 random="u",
                 DLL="simple",
                 silent=TRUE
                 )
opt <- nlminb(obj$par, obj$fn, obj$gr)
