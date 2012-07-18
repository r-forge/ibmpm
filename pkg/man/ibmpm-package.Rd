\name{ibmpm-package}
\alias{ibmpm-package}
\encoding{utf-8}
\docType{package}
\title{Individual-based metapopulation simulation including genetics}
\description{This package provides an individual-based metapopulation model,
including genetic dynamics.}
\details{

The package 'ibmpm' includes a main function 'metapop', which implements a
simulation model of the metapopulation dynamics. The model is individual-based,
and each individuals carry a suite of neutral genes. Genes are diploid
and are represented as integer numbers. Individuals reproduce sexually,
offspring inherits each allele randomly from one of the parents.

Each time step is a sequence of following events: male dispersal, female
dispersal, mating, reproduction. The order and number of these events can be
specified. Each mated female produces a number of offspring being a function of
maximum number of offspring, carrying capacity. This function (the population
growth model) is defined by user. Several population models are provided,
including logistic (and its stochastic variations) and truncated exponential
growth.

Mutations alter the alleles according to the provided mutation function.

For more details see help page for the function \code{\link{metapop}}.


}
\author{Kamil Barto\enc{ń}{n}}

\keyword{ package }

%\examples{
%
%}
