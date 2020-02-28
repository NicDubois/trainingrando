# Link function XBeta = g(µ) and mean function µ = g.inv(XBeta)

logit <- function(p)
{
  if(p>1 | p<0){
    print(p)
    stop(paste("p should be between 0 and 1 (Here p=",p,")",sep=''))
  }
  XBeta <- log(p/(1-p))
  return(XBeta)
}

InvLogit <- function(XBeta)
  {
  Mu <- 1/(1+exp(-XBeta))
  return(Mu)
}


