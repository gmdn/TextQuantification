nbClassify <- function(dataset, labels, estimates, findInterp = FALSE) {
  
  ## take positive and negative examples
  pos <- which(labels == 1)
  neg <- which(labels == 0)
    
  x <- (dataset %*% (estimates$posLogParam - estimates$posLogParamDiff)) + estimates$sumPositive + estimates$posPrior
  y <- (dataset %*% (estimates$negLogParam - estimates$negLogParamDiff)) + estimates$sumNegative + estimates$negPrior
  
  ######## find interpolation ########
  lineParam <- c(0, 1)
  
  if(findInterp) {
    
    lineParam <- findNegativeInterpolation(positive = data.frame(x[pos], y[pos]),
                                           negative = data.frame(x[neg], y[neg]))
  }
  
  ## compute confusion matric
  truePositive <- sum(x[pos, ] * lineParam[2] + lineParam[1] > y[pos, ])
  falseNegative <- length(pos) - truePositive
  falsePositive <- sum(x[neg, ] * lineParam[2] + lineParam[1] > y[neg, ])
  trueNegative <- length(neg) - falsePositive
  
  ## compute recall
  recall       <- truePositive / length(pos)
  ## compute precision
  if(truePositive == 0) {
    precision <- 1
  } else {
    precision <- truePositive / (truePositive + falsePositive)  
  }
  
  ## compute F1
  F1 <- 2 * recall * precision / (recall + precision)
  
  ## compute KLD
  lambdaTePos    <- length(pos) / (length(pos) + length(neg))
  lambdaHatTePos <- (truePositive + falsePositive) / (length(pos) + length(neg))
  lambdaTeNeg    <- length(neg) / (length(pos) + length(neg))
  lambdaHatTeNeg <- (trueNegative + falseNegative) / (length(pos) + length(neg))
  
  if(lambdaTePos == 0) {
    KLD <- 0
  }
  else if(lambdaHatTeNeg == 0) {
    KLD <- Inf
  }
  else {
    KLD <- (lambdaTePos * log(lambdaTePos / lambdaHatTePos)) + (lambdaTeNeg * log(lambdaTeNeg / lambdaHatTeNeg))
  }
  
  return(list(matrix(c(x[, 1], y[, 1]), nrow=dim(x)[1], ncol=2, byrow = FALSE),
              lineParam,
              c(recall, precision, F1, KLD, lambdaHatTePos))
         )

}