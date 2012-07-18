metapop <- function(disp.mat, data, sim.len, n.offspring, carr.cap,
					event.order = c("m.disperse", "f.disperse", "mate", "reproduce"),
					pop.mod = tr.exp.growth,
					output.interval = NA,
					m.mult.mating = FALSE,
					mutation = NULL,
					p.mutation = 0,
					...
					) {

	if(!missing(output.interval)) .NotYetUsed("output.interval", FALSE)

	if(!missing(event.order)) {
		formal.args <- formals(sys.function(sys.parent()))
		choices <- eval(formal.args[["event.order"]])
		event.order <- choices[pmatch(event.order, choices)]
		if(any(is.na(event.order))) stop("some items in 'event.order' cannot be matched unambiguously")
	}

	stopifnot(all(c("patch", "sex") %in% names(data)))
	if(!all(levels(data$sex) %in% c("F", "M")) || nlevels(data$sex) != 2L)
		stop("'data$sex' must be a factor with levels 'F' and 'M'")
	if(any(is.na(data$sex))) stop("NAs in data$sex are not allowed")


	dmd <- dim(disp.mat)
	if((length(dmd) != 2L) || (dmd[1L] != dmd[2L]))
		stop("'disp.mat' must be a square matrix")
	n.pat <- dmd[1L]

	for(i in ncol(data)) storage.mode(data[, i]) <- "integer"

	if(!is.numeric(data$patch)) stop("'data$patch' must be numeric")
	if(any(data$patch > n.pat || data$patch <= 0L))
		stop("'data$patch' must be in range from 1 to ", n.pat)

	orignames <- colnames(data)
	if(!is.null(data$n))
		data <- data[rep(seq.int(nrow(data)),data$n), names(data) != "n"]

	n.ind <- nrow(data)
	loci <- names(data)[!(names(data) %in% c("patch", "sex"))]
	n.loci <- length(loci)

	if(n.loci == 0L) stop("'data' must contain at least one column for allele values")


	wdat <- data.frame(sex = data$sex, mcount = 0L)
	wdat$g <- as.matrix(data[loci])
	wdat$mate <- matrix(NA_integer_, ncol = n.loci, nrow = n.ind,
		dimnames = list(NULL, loci))
	wdat <- split(wdat, factor(data[, "patch"], levels = 1L:n.pat),
				  drop = FALSE)

	n.offspring <- rep(n.offspring, length.out = n.pat)
	carr.cap <- rep(carr.cap, length.out = n.pat)
	pop.mod <- match.fun(pop.mod)

	for(time.step in seq_len(sim.len)) {
		for(event in event.order) {
			cat(event, "\n")
			wdat <- switch(event,
				   m.disperse = transfer(wdat, disp.mat, n.pat, who = "M"),
				   f.disperse = transfer(wdat, disp.mat, n.pat, who = "F"),
				   mate = sapply(wdat, mating, mmm = m.mult.mating, simplify = FALSE),
				   reproduce = {
						res <- vector("list", n.pat)
						for(i in seq.int(n.pat)) {
							res[[i]] <-
							offspring(wdat[[i]], noffspring = n.offspring[i],
								k = carr.cap[i], popmod = pop.mod,
								mutation = mutation, p.mutation = p.mutation,
								# these are passed to pop.model:
								time.step = time.step, ...)
						}
						res
				   }, NULL)
			print(sapply(wdat, nrow))
			stopifnot(length(wdat) == n.pat)
		}
		popsize <- sum(vapply(wdat, nrow, integer(1L)))
		if(popsize == 0L) break
		print(sapply(wdat, nrow))
	}
	zdat <- do.call(rbind, wdat)
	zdat$mate <- zdat$mcount <- NULL
	zdat <- cbind(data.frame(patch = rep(seq_len(n.pat), sapply(wdat, nrow)),
				  sex = zdat[, "sex"]), zdat$g)

	#return(aggregate(list(n = zdat[, 1L]), zdat, length)[, orignames])
	#return(aggregate(list(n = zdat[, 1L]), zdat, length)[, orignames])

	if("n" %in% orignames) {
		nm <- apply(zdat, 1L, paste, collapse = "-")
		rownames(zdat) <- nm
		zdat <- unique(zdat)
		zdat$n <- c(table(nm))[rownames(zdat)]
	}
	return(zdat)
}
