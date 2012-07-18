`offspring` <- function(x, noffspring, k, popmod,
						mutation = NULL,
						p.mutation = 0, ...) {
	n.loci <- dim(x[['g']])[2L]
	loci <- colnames(x[, "g"])
	mothers <- which(x$sex == "F" & !is.na(x$mate[, 1L]))
	nmothers <- length(mothers)

	if(nmothers == 0L) return(x[NULL, ])
	psurv <- popmod(n = nmothers, r = noffspring, k = k, ...)
	stopifnot(is.finite(nmothers * psurv))
	off <- vsample(mothers, size = rpois(1L, nmothers * psurv), replace = TRUE)
	noff <- length(off)

	if(noff	== 0L) {
		return(data.frame(sex = factor(NULL, levels(x$sex)), mcount = integer(noff),
			   g = integer(noff), mate = integer(noff), row.names = NULL))
	}

	res <- data.frame(
		sex = gl(2L, 1L, length = noff, labels = levels(x$sex)),
		mcount = integer(noff), g = NA_integer_, mate = NA_integer_,
		 row.names = NULL)

	g <- arrayto3d(x[off, "g"])
	m <- arrayto3d(x[off, "mate"])
	a <- array(NA_integer_, dim = dim(g))
	nn <- prod(dim(g)[-2L])
	a[,1L,] <- ifelse(runif(nn) < .5, g[, 1L,], m[, 2L,])
	a[,2L,] <- ifelse(runif(nn) < .5, m[, 1L,], m[, 2L,])
	g <- arrayto2d(a)
	dimnames(g) <- list(NULL, loci)
	res[["g"]] <- g

	#res[["g"]] <- matrix(ifelse(runif(noff * n.loci) < 0.5, x[off, "g"], x[off, "mate"]),
	#	ncol = n.loci, nrow = noff, dimnames = list(NULL, loci))
	if(is.function(mutation) && p.mutation > 0) {
		mutants <- which(runif(noff) < p.mutation)
		for(i in mutants) res$g[i, ] <- mutation(res$g[i, ])
	}

	res[["mate"]] <- matrix(NA_integer_, ncol = n.loci, nrow = noff,
		dimnames = list(NULL, loci))

	return(res)
}
