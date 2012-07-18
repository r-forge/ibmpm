\name{read.structure}
\alias{read.structure}
\encoding{utf-8}
\title{
Read 'Structure' data file
}
\description{
Read a data file formatted as input for \sQuote{Structure}
}
\usage{
read.structure(file, suffix = c("A", "B"))
}
\arguments{
  \item{file}{file name. }
  \item{suffix}{suffix to be attached to column names containing homologous alleles.}
}


\value{
A \code{data.frame}, where rows represent individuals. The first column
\code{patch} gives the patch number, and the remaining columns contain integer
allele values.
}

%\references{}

\author{Kamil Barto\enc{ń}{n}}

\examples{


#read example file:
dat <- read.structure(system.file(package = "ibmpm", "examples", "structure.txt"))

# infer sex from rownames
dat$sex <- factor(gsub("^.*([MF])[0-9]+$", "\\\\1", rownames(dat), perl = TRUE),
		levels = c("F", "M"))

head(dat)

n.patch <- max(dat$patch)
# random dispersal matrix
disp.mat <- array(runif(n.patch^2), dim = c(n.patch, n.patch))

dat <- metapop(disp.mat, dat, sim.len = 20, n.offspring = 5, carr.cap = 100)


}
\keyword{data}
