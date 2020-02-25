#' DicotomizeVector
#'
#' @description This create a n x c boolean matrix from a vector of c factor levels
#' @param v1 a vector of c factor levels
#' @return a n x c boolean matrix
DicotomizeVector<-function(v1){
  mynrow <-length(v1)
  mynCol <- nlevels(v1)
  if(mynrow > 0 & mynCol==0){
    warning("The input is not a factor")
  }
  else{
    mylevels <- levels(v1)

    xv1 <- matrix(ncol=mynCol,nrow=mynrow)
    dimnames(xv1)[[2]] <- mylevels
    for(i in c(1:mynCol))
    {
      xv1[,i] <- as.character(levels(v1)[v1])==mylevels[i]
    }
    return(xv1)
  }
}
