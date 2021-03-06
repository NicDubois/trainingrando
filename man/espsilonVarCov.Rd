% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CovarianceStructure.R
\name{espsilonVarCov}
\alias{espsilonVarCov}
\title{espsilonVarCov}
\usage{
espsilonVarCov(VarCovMatr, nTimePoints, RandoDataFrame, replicates = 1)
}
\arguments{
\item{VarCovMatr}{a VarCovMatr object which is a squared matrix defining the covariance structure}

\item{nTimePoints}{the number of timepoints}

\item{RandoDataFrame}{a TrainRandoSubj object with the randomization data}

\item{replicates}{the number of timepoints}
}
\value{
epsilon
}
\description{
espsilonVarCov
}
\examples{
myespilon <- espsilonVarCov(VarCovMatr=myVarCovMatr,RandoDataFrame=myRandoDataFrame, nTimePoints=6, replicates=1)
}
