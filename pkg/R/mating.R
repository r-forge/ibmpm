`mating` <- function(x, mmm = FALSE, max = 1L) {
	limit <- max - 1L
	stopifnot(is.logical(mmm))
	mi <- x$sex == "M" & x$mcount == limit
	fi <- x$sex == "F" & x$mcount == limit
	nm <- sum(mi)
	nf <- sum(fi)
	m <- if(mmm) nf else min(nf, nm)
	if(m != 0L) {
		ffi <- vsample(which(fi), m)
		fmi <- vsample(which(mi), m, replace = mmm) # replace ??
		x[ffi, "mate"] <- x[fmi, "g"]
		x[ffi, "mcount"]  <- x[ffi, "mcount"] + 1L
		x[fmi, "mcount"]  <- x[fmi, "mcount"] + 1L
	}
	x
}
