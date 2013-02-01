\name{mskmeans}
\alias{mskmeans}
\title{Maximally-Separated K-Means}
\description{Performs k-means clustering with initialization of centroids to partition data points around the data points with greatest magnitude difference}
\usage{mskmeans(data,k=2)}
\arguments{
	\item{data}{Numeric data input vector used to generate binary output}
	\item{k}{Number of clusters}
}
\details{Function called by \code{binarize.array}. Calculates k-means (default k=2 gives binarization) classification around maximally-separated data points}
\value{Discretized representation of \code{data}. For k=2, that is a numeric vector of the same length as input \code{data}, containing values \code{0} (representing a 'low' value) and \code{1} (respresenting a 'high' value).}
\author{Ed Curry \email{e.curry@imperial.ac.uk}}
