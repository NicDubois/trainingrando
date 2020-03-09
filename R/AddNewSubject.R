#' AddFirstSubject
#'
#' @param progfact A TrainRandoProgFact object defining the prognostic factors
#' @param Arms A TrainRandoArm object defining the treatment arms
#' @param usubjid the identifier of the subject (that is the id of the patient)
#' @return RandoDataFrame a TrainRandoSubj object witht the randomization data
#' @examples
#' data("myArms","myprogfact")
#' myRandoDataFrame <- AddFirstSubject(progfact=trainingrando::myprogfact,Arms=trainingrando::myArms)
#' @export
AddFirstSubject <- function(progfact,Arms,usubjid='Pat-0001'){
  # Enroll the first subject
  pat01 <- AddNewSubject(progfact,usubjid)

  # Introduce the patient in the dataframe
  RandoDataFrame <- pat01

  # Randomly assign a treatment to the 1st patient
  Arm = round(stats::runif(n=1, min = 0.5, max = length(Arms$Name) +0.5))
  levels(Arm) <- Arms$Name
  RandoDataFrame$Arm[1]=Arm
  levels(RandoDataFrame$Arm) <- Arms$Name
  attr(RandoDataFrame, "variable.labels") <- attr(pat01,"variable.labels")
  return(RandoDataFrame)
}


#' AddNewSubject.
#'
#' @description Add a new subject based on the prognostic factor and the id of the subject.
#'   This function is often used inside the function AddNextSubject.
#' @inheritParams AddFirstSubject
#' @return NewSubj , A new patient with the prognostic level randomly assigned according to
#' according to independant uniform distribution.
#' @examples
#' myNewSubj <- AddNewSubject(progfact=myprogfact,usubjid='Pat001')
#' @export
AddNewSubject <- function(progfact,usubjid) {

  # Count the number of prognostic factors
  nC <- length(progfact)

  # Create an empty data frame for the output
  NewSubj <- as.data.frame(matrix(nrow = 1, ncol = nC+2))
  colnames(NewSubj)[1] <- "usubjid"
  colnames(NewSubj)[nC+2] <- "Arm"
  NewSubj[1,1] <- usubjid

  for (i in c(1:nC))
  {
    # Generate 1 random variable following a uniform distribution which will be used as index to pick up randomly a level of a prognostic factor
    Ci <- round(stats::runif(n=1, min = 0.5, max = length(progfact[[i]]) +0.5))
    NewSubj[,i+1] <- Ci
    levels(NewSubj[,i+1]) <- progfact[[i]]
    rm(Ci)
  }
  attr(NewSubj, "variable.labels") <- c("Unique subject id",attr(progfact,"variable.labels"),"Treatment arm")

  class(NewSubj) <- c("TrainRandoSubj","data.frame")
  return(NewSubj)
}

#' Print a TrainRandoSubj object
#'
#' @inheritParams base::print
#' @export
print.TrainRandoSubj <-function(x,...){
  ncol <- dim(x)[2]
  nProgFactArm <- ncol-1
  y <- x

  for (i in c(1:nProgFactArm)){
      y[,ncol+i] <- levels(x[,i+1])[x[,i+1]]
  }
  # Add the variable names
  a <-  attr(x,"variable.labels")
  b <- stringr::str_replace_all(a,"[:blank:]|[:punct:]","")
  ncol <- dim(y)[2]
  z<-y[,c(1,seq(from=(nProgFactArm+2),to=ncol))]
  dimnames(z)[[2]]<-b[1:dim(z)[2]]
  cat("Randomization data: \n")
  zz <- as.data.frame(as.matrix(z))
  print(zz)
}


#' AddNextSubject
#'
#' @param RandoDataFrame a TrainRandoSubj object witht the randomization data
#' @inheritParams AddFirstSubject
#' @return RandoDataFrame a TrainRandoSubj object witht the randomization data
#' @examples
#' data("myArms","myRandoDataFrame","myprogfact")
#' myRandoDataFrame <- AddNextSubject(RandoDataFrame=trainingrando::myRandoDataFrame, progfact=trainingrando::myprogfact)
#' @export
AddNextSubject <- function(RandoDataFrame,progfact){
  # Count the number of subject already randomized
  PatCurrNbr <- dim(RandoDataFrame)[1]
  PatNbr <- PatCurrNbr+1
  # Enroll the first subject
  if (is.null(r <- get0("usubjid"))) {
    usubjid <- paste('Pat-',sprintf("%04d", PatNbr),sep='')
  }

  patNew <- AddNewSubject(progfact,usubjid=usubjid)

  # Introduce the patient as a new row in the dataframe
  RandoDataFrame[PatNbr,] <- patNew[1,]
  return(RandoDataFrame)
}
