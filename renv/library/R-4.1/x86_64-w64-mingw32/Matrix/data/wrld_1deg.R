stopifnot(requireNamespace("Matrix" , quietly = TRUE)) # includes 'methods'

wrld_1deg <-
    local({
	load(system.file(file.path("external", "wrld_1deg_slots.rda"),
                         package = "Matrix"))
	## -> 'L'
	r <- methods::new("dsCMatrix")
	for (n in c("Dim", "i","p","x"))
	    methods::slot(r, n) <- L[[n]]
	r
    })

if(FALSE) {## The reverse:
 L <- list()
 for (n in c("Dim", "i","p","x"))    L[[n]] <- slot(wrld_1deg, n)
}
