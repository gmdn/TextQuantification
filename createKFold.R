createKFold <- function(nObs, k = 5) {
    
  ## find the dimension of each k-fold
  dimKFold <- nObs %/% k

  ## generate random sample of observations
  smpl <- sample(1:nObs)
  
  ## build list of kFolds
  kFolds <- list()
  
  for(i in 1:(k - 1)) {
    kFolds[[i]] <- smpl[(((i - 1) * dimKFold) + 1):(i * dimKFold)]
  }
  kFolds[[k]] <- smpl[(((k - 1) * dimKFold) + 1):nObs]
  
  return(kFolds)
}