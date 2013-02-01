\name{clusterDisc}
\alias{clusterDisc}
\title{Fast Adaptive Binarization - internal}
\description{Performs fast adaptive binarization of numeric arrays}
\usage{clusterDisc(x,use.gap)}
\arguments{
	\item{x}{Numeric data input vector used to generate binary output}
	\item{use.gap}{Boolean indicating whether to use gap statistic to infer whether or not the data can be sufficiently converted to a binary representation.}
}
\details{Function called by \code{binarize.array}}
\value{Binarized representation of \code{x}. That is, a numeric vector of the same length as input \code{x}, containing values \code{0} (representing a 'low' value) and \code{1} (respresenting a 'high' value).}
\author{Ed Curry \email{e.curry@imperial.ac.uk}}
