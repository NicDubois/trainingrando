#' CorrBetweenIndVar
#'
#' @param RandoDataFrame a TrainRandoSubj object witht the randomization data
#' @return CorrVector , a vector with the correlation between the treatment arm and the other independent variables.
#' @examples
#' CorrVector <- CorrBetweenIndVar(RandoDataFrame=myRandoDataFrame)
#' @export
#' @import stats
CorrBetweenIndVar<-function(RandoDataFrame)
{
ncol <- dim(RandoDataFrame)[2]
CorrMatrix <- stats::cor(x=RandoDataFrame[,2:ncol])
CorrVector <- CorrMatrix[,ncol-1]
return(CorrVector)
}
