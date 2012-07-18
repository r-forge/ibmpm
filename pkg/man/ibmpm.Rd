\name{metapop}
\alias{metapop}
\encoding{utf-8}
\title{
Metapopulation simulation
}
\description{
Simulate a metapopulation with genetic dynamics.
}
\usage{
metapop(disp.mat, data, sim.len, n.offspring, carr.cap,
	event.order = c("m.disperse", "f.disperse", "mate", "reproduce"),
	pop.mod = tr.exp.growth, output.interval = NA, m.mult.mating = FALSE,
	mutation = NULL, p.mutation = 0, ...)
}
\arguments{
  \item{disp.mat}{a square matrix of size \code{n.patches} x \code{n.patches}
	containing the dispersal probabilities. The source patches are given in
	rows, target patches in columns, so \code{disp.mat[i,j]} is the probability
	of transfer from patch \code{i} to patch \code{j}. If the rows sum to less
	than one, the sum gives the probability of survival during the dispersal
	event. On the diagonal (\code{disp.mat[i,i]}) are the probabilities of
	staying in the patch \emph{i}. }
  \item{data}{a \code{data.frame} with columns \sQuote{patch}, \sQuote{sex}, and
	at least one additional column for allele lengths. Each row describes one
	individual, or, if column \code{n} is present it describes \emph{n}
	individuals of the same genotype, sex, and location.}
  \item{sim.len}{numeric, the simulation length (number of time steps).}
  \item{n.offspring}{maximum number of offspring per female. This can be either
	a numeric scalar or vector of length \code{n.patches}, in the latter case
	values are per patch}
  \item{carr.cap}{numeric scalar or vector of length \code{n.patches}, carrying
	capacity of the patches.}
  \item{event.order}{a character vector giving the sequence of events in a
	single time step. Accepted values are: "m.disperse", "f.disperse", "mate"
	and "reproduce". The names may be abbreviated. Note that there may
	be multiple events of the same type. }
  \item{pop.mod}{function implementing the population model. See
	\code{\link{pop.model}}. }
  \item{output.interval}{interval at which the population state is returned
	(not yet inplemented). }
  \item{m.mult.mating}{logical, is multiple mating of males allowed?}
  \item{mutation}{function to carry out mutations in the 'genome' of one
	individual. It has to accept a numeric vector as the only argument, and
	return a numeric vector of the same length. }
  \item{p.mutation}{numeric scalar, the mutation probability}
  \item{\dots}{further arguments passed to \code{pop.mod}}
}

\details{
In the \code{data}, each two homologous alleles must occur in columns next to
each other, e.g. \sQuote{allele1A}, \sQuote{allele1B}, \sQuote{allele2A},
\sQuote{allele2B}, etc.
}

\value{
A \code{data.frame} with the same column types as the input data provided
as \code{data} argument, where rows represent individuals (or
genotypes/sex/location and their counts if column \code{n} was present) at
\code{time = sim.len}.
\itemize{
\item \code{patch} numeric giving the patch number, referring to the row and column of
	\code{disp.mat}.
\item \code{sex} factor with two levels, \code{"F"} and \code{"M"};
\item \code{n} optional, number of individuals of that kind;
\item \code{\dots} remaining columns contain allele values.
}
}
%\references{}
\author{Kamil Barto\enc{ń}{n}}

\seealso{
\code{\link{pop.model}}
}
\examples{
# TODO

}
\keyword{simulation}
