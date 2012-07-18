read.structure <- function(file, suffix = c("A", "B")) {
	f <- file(file)
	dat <- readLines(f)
	close(f)
	dat <- strsplit(dat, "\\s+")
	loci <- dat[[1L]]
	dat <- do.call(rbind, dat[-1L])
	indnames <- dat[, 1L]
	dat <- dat[, -1L]
	dat <- array(as.integer(dat), dim = dim(dat))
	patches <- dat[, 1L]
	dat <- dat[, -1L]
	dat[dat <= 0] <- NA_integer_
	res <- data.frame(patch = patches)
	res <- cbind(res, dat)
	rownames(res) <- indnames
	colnames(res)[-1L] <- paste(rep(loci, each = 2L), suffix, sep = ".")
	res
}

