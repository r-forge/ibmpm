# truncated exponential growth
tr.exp.growth <- function(n, r, k, ...)  r * pmin(1, k /(n * r))

logistic.growth <- function(n, r, k, ...) 1 + ((r - 1) * ((k - n) / k))

st.logistic.growth <- function(n, r, k, r.prop = 0.9, ...)
	runif(length(n), r.prop, 1) * (1 + ((r - 1) * ((k - n) / k)))

gst.logistic.growth <- function(n, r, k, shape = .01, rate = 1, ...) {
	res <- (1 + ((r - 1) * ((k - n) / k)))
	res / (1 + rgamma(length(res), shape = shape, rate = rate))
}

ext.logistic.growth <- function(n, r, k, t,
	ext.interval = 10L,
	r.prop = .5,
	...) {
	res <- (1 + ((r - 1) * ((k - n) / k)))
	return(if(t %% ext.interval == 0L) res * r.prop else res)
}
