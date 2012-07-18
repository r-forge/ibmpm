arrayto3d <- function(x, dim2 = 2L) {
	d <- dim(x)
	if(length(dim(x)) != 2L) stop("'x' is not 2-dimensional")
	dim(x) <- c(d[1L], dim2, d[2L] / dim2)
	x
}

arrayto2d <- function(x) {
	d <- dim(x)
	if(length(dim(x)) != 3L) stop("'x' is not 3-dimensional")
	dim(x) <- c(d[1L], d[2L] * d[3L])
	x
}
