`transfer` <- function(wdat, disp.mat, n.pat = length(wdat), who = NULL) {
	afterdat <- vector("list", n.pat)
	pseq <- seq.int(n.pat)
	if(!is.null(who)) {
		stopifnot(who %in% c("M", "F"))
		for(i in pseq) {
			j <- wdat[[i]][, "sex"] != who
			afterdat[[i]] <- wdat[[i]][j, ]
			wdat[[i]] <- wdat[[i]][!j, ]
		}
	}
	for(patch in pseq) {
		x <- wdat[[patch]]
		x <- x[runif(nrow(x)) < pmin(1, sum(disp.mat[patch, ])),]
		nind <- nrow(x)
		target <- factor(sample(n.pat, nind, prob = disp.mat[patch, ],
						 replace = TRUE), levels = pseq)
		mx <- split(x, target, drop = FALSE)
		for(i in pseq) {
			if(nrow(mx[[i]]) != 0L) afterdat[[i]] <- rbind(afterdat[[i]], mx[[i]])
		}
	}
	names(afterdat) <- pseq
	return(afterdat)
}
