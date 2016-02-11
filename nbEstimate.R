#source("findNegativeInterpolation.R")

nbEstimate <- function(training, trainingLabels, prior=c(1, 1)) {
 
  ## take positive and negative examples
  pos <- training[trainingLabels == 1, ]
  neg <- training[trainingLabels == 0, ]
  
  ## beta prior (hyper-)parameters
  a <- prior[1]
  b <- prior[2]
  
  ####### positive class #######
  ## compute freq
  freq <- colSums(pos)
  numOfPositiveDocs <- dim(pos)[1]
    
  # add smoothing parameters (to numerator and denominator)
  num <- freq + a
  den <- numOfPositiveDocs + a + b
  
  ## estimate parameters
  posParam <- num / den
  
  ####### negative class #######  
  freq <- colSums(neg)
  numOfNegativeDocs <- dim(neg)[1]
  
  ## smoothing
  num <- freq + a
  den <- numOfNegativeDocs + a + b
  
  ## estimate parameters
  negParam <- num / den
  
  ####### estimate probabilities #######
  ## precompute the sum over features of log(1 - param)
  sumPositive  <- sum(log(1 - posParam))
  sumNegative  <- sum(log(1 - negParam))
  
  ## compute log of parameters
  posLogParam <- log(posParam)
  posLogParamDiff <- log(1 - posParam)
  negLogParam <- log(negParam)
  negLogParamDiff <- log(1 - negParam)
  
  ## compute category prior
  posPrior <- log(dim(pos)[1]/(dim(pos)[1] + dim(neg))[1])
  negPrior <- log(1 - dim(pos)[1]/(dim(pos)[1] + dim(neg))[1])
  
  
  ####### return estimates and line parameters #######
  estimates <- list(sumPositive, posLogParam, posLogParamDiff, posPrior,
                    sumNegative, negLogParam, negLogParamDiff, negPrior)
  names(estimates) <- c("sumPositive", "posLogParam", "posLogParamDiff",
                        "posPrior", "sumNegative", "negLogParam", "negLogParamDiff", "negPrior")
    
  return(estimates)
   
}