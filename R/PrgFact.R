
#' Create a new TrainRandoProgFact object.
#'
#' @description This create a new list with a first prognostic factor
#' @param progfactLabel a character string with the name of the prognostic factor.
#' @param progfactLevels a vector of character strings with the levels of the prognostic factor.
#' @return Une liste de classe SAPUL4012Moy2EchPair.
#' @seealso AddNewPrgFact
#' @examples
#' myprogfact <- CreatePrgFact(progfactLabel = "Age group",progfactLevels=c("0-18 years", "19-35 years","36-60 years","> 60 years"))
#' @export
CreatePrgFact <- function(progfactLabel='Prognostic factor 1', progfactLevels=c("A","B","C"))
{
  progfact<-list(progfactLevels)
  attr(progfact, "variable.labels") <- progfactLabel
  class(progfact) <- "TrainRandoProgFact"
  return(progfact)
}

#' Print a TrainRandoProgFact object
#'
#' @param x Un a object TrainRandoProgFact
#' @export
print.TrainRandoProgFact <-function(x,...){
  cat("Prognostic factor(s): \n")
  for (i in c(1:length(x))){
  cat("   ", attr(x,"variable.labels")[i]," - ", length(x[[i]]),"levels: \n")
    for (j in c(1:length(x[[i]]))){
  cat("       ",x[[i]][j],"\n")
  }
}
}

#' Add a new training factor in an existing TrainRandoProgFact object.
#'
#' @description This function adds a new prognostic factor in an existing TrainRandoProgFact object.
#' @param X une matrice avec les valeurs des deux échantillons.
#' @return Une liste de classe SAPUL4012Moy2EchPair.
#' @seealso CreatePrgFact
#' @examples
#' myprogfact2 <- AddNewPrgFact(currentProgfact=myprogfact,progfactLabel = "Type of tumor",progfactLevels=c("A", "B","C"))
#' @export
AddNewPrgFact <- function(currentProgfact, progfactLabel='Prognostic factor 2', progfactLevels=c("A","B","C"))
{
  n <- length(currentProgfact)
  n1 <- n+1
  NewProgfact <-currentProgfact
  NewProgfact[[n1]] <- progfactLevels
  attr(NewProgfact, "variable.labels") <- c(attr(currentProgfact,"variable.labels"),progfactLabel)
  class(NewProgfact) <- "TrainRandoProgFact"
  return(NewProgfact)
}
