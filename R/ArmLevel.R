
#' ArmLevelProg.
#'
#' @param ArmLevel A dataframe with ArmLevel$Name = c('TreatmentA','TreatmentB')
#' @return ArmLevel a dataframe with LowerBound and Upperbound for the random allocation
#' @examples
#' myArms <- ArmLevelProg(data.frame("Name" = c('Placebo', 'Treat1','Treat2','Treat3')))
#' @export
ArmLevelProg <- function(ArmLevel){
  Arm <- ArmLevel
  nArms <- dim(Arm)[1]
  if(length(Arm$Prob)==0){
    Arm$Prob <- rep(1/nArms,nArms)
  }
  Arm$CumProb <- rep(NaN,nArms)
  for (i in c(1:nArms)){
    if(i>1){
      PreviousCumProb <- Arm$CumProb[i-1]
    }
    else {
      PreviousCumProb <- 0
    }
    Arm$CumProb[i] <- Arm$Prob[i]+PreviousCumProb
    Arm$LowerBound[i] <- PreviousCumProb
    Arm$UpperBound[i] <- Arm$CumProb[i]
  }
  attr(Arm, "variable.labels") <- c('Treatment name','Planned proportion of subjects','Cummulative proportion of subjects','Lower bound of the cummulative distribution','Upper bound of the cummulative distribution')
  class(Arm) <- c("TrainRandoArm","data.frame")
  return(Arm)
}
#' print a TrainRandoArm object
#' @inheritParams base::print
#' @export
print.TrainRandoArm <-function(...){
  cat("Treatment arms and their corresponding planned proportion of subjects: \n")
  x2 <-x[,1:2]
  print(x2)
}
