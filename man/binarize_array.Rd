\name{binarize.array}
\alias{binarize.array}
\title{Fast Adaptive Binarization}
\description{Performs fast adaptive binarization of numeric arrays, providing options for filtering rows with insufficient variation}
\usage{binarize.array(x,min.filter=NA,var.filter=0,fc.filter=0,log.base=NA,use.gap=FALSE)}
\arguments{
	\item{x}{Numeric data input array used to generate binary output array. Each row of the array represents a different variable.}
	\item{min.filter}{Minimum-value filter: rows of \code{x} with no value greater than \code{min.filter} will have all values set to \code{0}.}
	\item{var.filter}{Variation filter: the proportion of lowest-variance rows of \code{x} to have all values set to \code{0}.}
	\item{fc.filter}{Fold-change filter: rows of \code{x} with maximum fold-change less than \code{fc.filter} will have all values set to \code{0}.}
	\item{log.base}{Base of logarithm to use for calculating fold-changes in rows of \code{x}. Unless \code{log.base=NA} input data \code{x} is assumed to be log-transformed.}
	\item{use.gap}{Boolean indicating whether to use gap statistic to identify rows insufficiently converted to binary representation. If \code{TRUE}, execution will be _much_ slower.}
}
\details{Implementation of an adaptive method for binarizing gene expression data on a per-probe basis and demonstrate the superior effectiveness of our method when compared with other, commonly used approaches. This adaptive binarization method can be applied to DNA methylation microarray data, which has implications for cross-platform integration, and can reduce batch effects in the data.}
\value{Binarized representation of \code{x}. That is, a numeric array of same dimensions as input \code{x}, containing values \code{0} (representing a 'low' value of corresponding variable) and \code{1} (respresenting a 'high' value of the corresponding variable).}
\author{Ed Curry \email{e.curry@imperial.ac.uk}}
\examples{
## create a numeric array
x.cont <- array(runif(60),dim=c(10,6))
\dontrun{x.cont}

## find binary representation of array
x.bin <- binarize.array(x.cont)
\dontrun{x.bin}

## use gap statistic to filter insufficiently variable rows
x.gap <- binarize.array(x.cont,use.gap=TRUE)
\dontrun{x.gap}
}
