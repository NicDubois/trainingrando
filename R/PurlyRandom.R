#' PurlyRandom
#'
#' @param Arms A list with the level of the prognostic factors
#' @param RandoDataFrame a TrainRandoSubj object witht the randomization data
#' @param usubjid the identifier of the subject (that is the id of the patient)
#' @return RandoDataFrame , a TrainRandoSubj object witht the randomization
#'  data where the patient given in usubjid has a new treatment allocation
#' according to independant uniform distribution.
#' @examples
#' myRandoDataFrame02 <- PurlyRandom(Arms=myArms,RandoDataFrame=myRandoDataFrame,usubjid="Pat-0002")
#' @export
PurlyRandom <- function(Arms,RandoDataFrame,usubjid=NULL){
# Simplest approach is to use a randomization procedure whereby
# each patient has an equal probability 1/N of receiving any one of the N treatments

  # Count the number of subject already randomized
  PatCurrNbr <- dim(RandoDataFrame)[1]
  # Enroll the first subject
  if (is.null(r <- get0("usubjid"))) {
    usubjid <- paste('Pat-',sprintf("%04d", PatCurrNbr),sep='')
  }


Dice = runif(n=1,min=0,max=1)
if(Dice ==1){Dice <- 0.99999999}
ArmIndex <- Arms$LowerBound <= Dice & (Dice < Arms$UpperBound)
names(ArmIndex) <-as.vector(Arms$Name)
RandoDataFrame$Arm[RandoDataFrame$usubjid==usubjid]=Arms$Name[ArmIndex]
return(RandoDataFrame)
}

