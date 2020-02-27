#' Object specifying the arms of a study.
#'
#' A a dataframe and TrainRandoArm with LowerBound and Upperbound for the random allocation.
#'
#' @format A data frame with 4 rows and 5 variables:
#' \describe{
#'   \item{Name}{The name of the treatment arms (Factor)}
#'   \item{Prob}{The probability of each treatment arms (num)}
#'   \item{CumProb}{The cumulative probability (num)}
#'   \item{LowerBound}{ (num)}
#'   \item{UpperBound}{ (num)}
#' }
"myArms"


#' Object specifying the prognostic factor of a disease.
#'
#' A a list and TrainRandoProgFact object defining the progostic factor of a disease.
#'
#' @format A list in which each element corresponds to a prognostic factor.
#' \describe{
#' }
"myprogfact"

#' Object TrainRandoSubj object with the randomization data.
#'
#' A a data.frame and TrainRandoSubj object defining the progostic factor of a disease.
#'
#' @format A list in which each element corresponds to a prognostic factor.
#' \describe{
#' \item{usubjid}{the unique subject id (char)}
#' \item{V2}{the age group (num with factor)}
#' \item{V3}{the type of tumor (num with factor)}
#' \item{V4}{the genotype (num with factor)}
#' \item{Arm}{the arms (factor)}
#' }
"myRandoDataFrame"

#' Object TrainRandoResp object with the response (dependent variable).
#'
#' A list and TrainRandoResp object defining the progostic factor of a disease.
#'
#' @format A list with the following elements:
#' \describe{
#' \item{usubjid}{the unique subject id (char)}
#' \item{V2}{the age group (num with factor)}
#' \item{V3}{the type of tumor (num with factor)}
#' \item{V4}{the genotype (num with factor)}
#' \item{Arm}{the arms (factor)}
#' }
"myResponse"

#' A variance covariance matrix.
#'
#' A variance covariance matrix for a linear model with repeated measures.
#'
#' @format A square positive defined matrix
#' \describe{
#' }
"myVarCovMatr"

#' Epsilon object with the random errors and the two keys id variables (usubjidd, TimePoint).
#'
#' A data.frame, tbl and tbl_df object defining with the random error and the keys id variables.
#'
#' @format A list with the following elements:
#' \describe{
#' \item{usubjid}{the unique subject id (char)}
#' \item{TimePoint}{the time points (char)}
#' \item{Residuals}{the residuals i.e random error (num)}
#' }
"myepsilon"

