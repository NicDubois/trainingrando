#' AddAResponse
#'
#' @param RandoDataFrame a TrainRandoSubj object with the randomization data
#' @param nobs the number of observations in the dataset by default
#'  the number of rows in RandoDataFrame
#' @param ResponseUnit a character string with the unit of the response
#' @param RespValues can be a fixed value intercept which will be repeated nobs times
#' or a numerical vector of the same lenght than nobs.
#' @return response TrainRandoResp object with a response variable
#'  (dependent variable)
#' @examples
#' data("myArms","myRandoDataFrame","myprogfact")
#' myResponse <- AddAResponse(RandoDataFrame=myRandoDataFrame, ResponseUnit='Weight (in kg)',RespValues=70)
#' @export
#'
AddAResponse <- function(RandoDataFrame, nobs=NA, ResponseUnit='', RespValues)
{
  if(is.na(nobs)){
      nobs <- dim(RandoDataFrame)[1]
  }
  if(length(RespValues)==1){
    responseValues <- rep(RespValues,times=nobs)
  }
  else if(length(RespValues)==nobs){
    responseValues <- RespValues
  }
  else {
    stop(paste("RespValues should have a length of either 1 or equal to nobs (Here length(RespValue)=",length(RespValue),")",sep=''))
  }

response <- list(Values=responseValues,ResponseUnit=ResponseUnit,DistributionOfError='',Model='Y = \n mu0',ModelWith='With \n')
class(response) <- c("TrainRandoResp","list")
return(response)
}

#' RespResidVar
#'
#' @param Response TrainRandoResp object with a response variable (dependent variable)
#' @param SDReplicates a character string with the unit of the response
#' @return response TrainRandoResp object with a response variable (dependent variable)
#' @examples
#' data("myResponse")
#' myResponse <- RespResidVar(Response=myResponse,SDReplicates=10)
#' @export
#' @import stats
RespResidVar <- function(Response, SDReplicates)
{
  nobs <- length(Response$Values)
  Residuals <- stats::rnorm(n=nobs,mean=0,sd=SDReplicates)
  Response$Values <- Response$Values + Residuals
  Response$DistributionOfError='Normal'
  Response$Model <- paste(Response$Model,' + epsilon')
  Response$ModelWith <- paste(Response$ModelWith,' epsilon ~ N(0,',SDReplicates,') \n',sep='')
  return(Response)
}

#' RespAddFixedEffect
#'
#' @param BetaVector a vector of the beta effect
#' @param ColID index of the column of the RandoDataFrame which will be used as independant variable
#' @inheritParams AddAResponse
#' @inheritParams RespResidVar
#' @return response TrainRandoResp object with a response variable (dependent variable)
#' @examples
#' data("myResponse","myRandoDataFrame")
#' myResponse <- RespAddFixedEffect(Response=trainingrando::myResponse,RandoDataFrame=trainingrando::myRandoDataFrame,ColID=2, BetaVector=c(-1,0,0,1))
#' @export
RespAddFixedEffect <- function(Response, RandoDataFrame,ColID, BetaVector)
{
  XVector<-RandoDataFrame[,ColID]
  xV1 <-DicotomizeVector(XVector)
  Response$Values <- Response$Values + xV1 %*% BetaVector

  a <-  attr(RandoDataFrame,"variable.labels")[ColID]
  b <- stringr::str_replace_all(a,"[:blank:]|[:punct:]","")

  Response$Model <- paste(Response$Model, '\n ',' + ', b,' * beta_',b,sep='')
  return(Response)
}
