\name{pop.model}
\alias{pop.model}
\alias{tr.exp.growth}
\alias{logistic.growth}
\alias{ext.logistic.growth}
\alias{st.logistic.growth}
\alias{gst.logistic.growth}
\encoding{utf-8}

\title{Population models}
\description{
Population models to use with 'metapop': truncated exponential growth and
	variants of logistic growth: plain, with extinction at even time intervals,
	uniform stochastic and gamma stochastic.
}

\usage{
tr.exp.growth(n, r, k, ...)
logistic.growth(n, r, k, ...)
ext.logistic.growth (n, r, k, t, ext.interval = 10L, r.prop = .5, ...)
st.logistic.growth(n, r, k, r.prop = 0.9, ...)
gst.logistic.growth(n, r, k, shape = .01, rate = 1, ...)

}

\arguments{
  \item{n}{population size (number of reproducing females).}
  \item{r}{maximum number of offspring per female.}
  \item{k}{carrying capacity.}
  \item{t}{time step.}
  \item{shape, rate}{parameters of the gamma distribution.}
  \item{ext.interval}{interval of the extinction (or population crash) events.}
  \item{r.prop}{proportion of \code{r} to which the growth rate decreases
	during the collapse (or minimum of it)}
  \item{\dots}{other arguments (not used). The user-defined function should
	always contain the \code{\dots} argument.}
}
\details{

These function are examples of a population model function that can be
used with 'metapop' (supplied as 'pop.model' argument).

}
\value{
A vector of per-capita offspring sizes.
}

\references{
REF?
}
\author{Kamil Barto\enc{ń}{n}}

\seealso{
\code{\link{metapop}}
}
\examples{

popmods <- c("tr.exp.growth", "logistic.growth", "st.logistic.growth",
			 "gst.logistic.growth", "ext.logistic.growth")

clr <- structure(seq_along(popmods), names = popmods)

RNGkind("Mersenne")
set.seed(17)
l <- 500
k <- 100
r <- 1.05
plot(c(0, l), c(0, k), xlab = "time", ylab = "population size",
	 type = "n", ylim = c(0, k * 1.05), yaxs = "i", axes = FALSE)
axis(1)
axis(2, c(0, k / c(2,1)), labels = c(0, "K/2", "K"), las = 1)
box()
for(pm in popmods) {
	x <- numeric(l)
	x[1] <- 1
	for(i in 2:l) x[i] <- x[i - 1] * get(pm)(x[i - 1], r, k, t = i,
		r.prop = .95, ext.interval = 20, shape = .001, rate = 1)
	lines(1:l, x, col = clr[pm])
}
abline(h = c(0, k), lty= 3)
legend("bottomright", popmods, text.col = clr, cex = .75,
	   title = "Function:", title.adj = 0.1, lty = 1, col = clr,
	   bg = "white")


}

\keyword{simulation}
