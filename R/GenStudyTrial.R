#' GenStudyTrial
#'
#' @param npat the number of patients that will be simulated in the trials
#' @param progfact A TrainRandoProgFact object defining the prognostic factors
#' @param Arms A TrainRandoArm object defining the treatment arms
#' @return RandoDataFrame TrainRandoSubj object witht the randomization data
#' @examples
#' data("myArms","myprogfact")
#' RandoDataFrame <- GenStudyTrial(npat = 25, progfact = myprogfact, Arms=myArms)
#' @export
GenStudyTrial<-function(npat = 25, progfact, Arms)
{
  # First patient
  myRandoDataFrame <- AddFirstSubject(progfact=progfact,Arms=Arms)
  # Generate the npat-1 other patients
  for (i in c(2:npat)){
    myRandoDataFrame <- AddNextSubject(RandoDataFrame=myRandoDataFrame, progfact=progfact)
    myRandoDataFrame <- PurlyRandom(Arms=Arms,RandoDataFrame=myRandoDataFrame)
  }

  return(myRandoDataFrame)

}
