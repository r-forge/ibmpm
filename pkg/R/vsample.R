vsample <-
function(x, size, replace = FALSE, ...) {
	if(length(x) == 1L) {
		if(replace && !missing(size)) rep(x, length.out = size) else x
	} else sample(x, size = size, replace = replace, ...)
}
