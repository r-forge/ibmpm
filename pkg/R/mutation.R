`mutation` <-
function(x) {
	n <- length(x)
	mut.type <- sample(3L, 1L)
	mut.pos <- sample(n, 1L)
	x[mut.pos] <- switch(mut.type,
		x[mut.pos] + 1L,
		max(0L, x[mut.pos] - 1L),
		as.integer(runif(1L, 0, x[mut.pos])))
	x
}
