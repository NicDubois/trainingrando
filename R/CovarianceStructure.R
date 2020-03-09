
# SAS SAT stat 14.3 User guide Linear Model page 117
 # Y = X beta + Z gamma + epsilon
 # whith Gamma ~ N(0,G) and epsilon ~ N(0,R)

#' UniformCorrelation
#'
#' @param rho the pearson correlation parameter (should be between -1 and 1)
#' @param sigma2 the residual variance
#' @param nTimePoints the number of timepoints
#' @return VarCovMatr a VarCovMatr object which is a squared matrix defining the covariance structure.
#' @examples
#' VarCovMatr <- UniformCorrelation(rho=0.5,sigma2=2,nTimePoints=6)
#' @import matrixcalc
#' @export
UniformCorrelation <- function(rho,sigma2,nTimePoints)
{
  if(abs(rho)>1){
    print(rho)
    stop(paste("rho should be between -1 and 1 (Here rho=",rho,")",sep=''))
  }
  if(sigma2<0){
    print(sigma2)
    stop(paste("sigma2 cannot be negative",sigma2,")"))
  }
  UC <- sigma2*(matrix(rho,nrow=nTimePoints, ncol=nTimePoints) - rho*diag(nTimePoints) + diag(nTimePoints))
  if(!matrixcalc::is.positive.semi.definite(UC)){
    print(UC)
    stop('The variance covariance matrix is not semi positive definite')
    }
  return(UC)
}

#' espsilonVarCov
#'
#' @param VarCovMatr a VarCovMatr object which is a squared matrix defining the covariance structure
#' @param nTimePoints the number of timepoints
#' @param replicates the number of timepoints
#' @inheritParams AddAResponse
#' @return epsilon
#' @examples myespilon <- espsilonVarCov(VarCovMatr=myVarCovMatr,RandoDataFrame=myRandoDataFrame, nTimePoints=6, replicates=1)
#' @import mvtnorm tidyr
#' @export
espsilonVarCov <- function(VarCovMatr, nTimePoints,RandoDataFrame,replicates=1){
  # Creation of the epsilon vector
  Exp <- rep(x=0,times=nTimePoints)
  nrmv <- dim(myRandoDataFrame)[1] * replicates
  rmvnormMatrix <- mvtnorm::rmvnorm(n=nrmv, mean=Exp, sigma=VarCovMatr)
  dimnames(rmvnormMatrix)[[2]] <- paste("TimePoint_",seq(1,nTimePoints),sep='')

  rmvnormTibbles<- tibble::as_tibble(rmvnormMatrix)
  rmvnormTibbles$usubjid <- RandoDataFrame$usubjid

   epsilon <-rmvnormTibbles %>% tidyr::pivot_longer(-usubjid,names_to = "TimePoint", values_to = "Residuals")

  return(epsilon)
}

#' RandomEffect
#'
#' @description Creation of a vector of the random effect
#' @param randomeffect an independent variable which is modeled as a random effect.
#' @param VARRandoEffect The variance of the random effect
#' @return ZGamma1
#' @examples myZGamma <- RandomEffect(randomeffect=myRandoDataFrame$V2, VARRandoEffect=10)
#' @export
RandomEffect <- function(randomeffect,VARRandoEffect)
{
  nlevelRando <- length(unique(randomeffect))
  EffectRando1 <- rnorm(n=nlevelRando,mean=0,sd=sqrt(VARRandoEffect))
  dum <- dummies::dummy(randomeffect)

  ZGamma <- dum %*% EffectRando1
  return(ZGamma)
}
