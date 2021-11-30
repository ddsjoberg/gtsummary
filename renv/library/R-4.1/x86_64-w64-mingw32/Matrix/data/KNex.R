stopifnot(requireNamespace("Matrix" , quietly = TRUE)) # includes 'methods'

KNex <-
    local({
	load(system.file(file.path("external", "KNex_slots.rda"), package = "Matrix"))
	## -> 'L'
	r <- list(mm = methods::new("dgCMatrix"), y = L[["y"]])
        `slot<-` <- methods::`slot<-`
	for (n in c("Dim", "i","p","x")) ## needs methods::slot<-
	    slot(r$mm, n) <- L[[n]]
	r
    })
