
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
#library(FSelector)
source("nbEstimate.R")
source("nbClassify.R")
source("loadData.R")
source("createKFold.R")

options(shiny.trace = FALSE)

shinyServer(function(input, output, session) {
  
  # compute document frequencies
  docFreq <- colSums(dataset)
  
  # Get the index of the most frequent terms in order
  idxFeatures <- order(docFreq, decreasing = T)
  
  # Set the number of the fold for validation
  validationFold <- 0
  
  # Vector of max performances (F1 on training)
  # F1, num of k-fold, k-fold, features, alpha, beta, ang. coeff, intercept
  maxTraining <- c(0, 5, 1, 35000, 1, 1, 1, 0)
  
  # Vector of max performances (F1 on validation)
  # F1, num of k-fold, k-fold, features, alpha, beta, ang. coeff, intercept
  maxValidation <- c(0, 5, 1, 35000, 1, 1, 1, 0)
  
  ## Vector of combinations (q, m) explored together with evaluation KLD
  explored <- data.frame(q = numeric(0), m = numeric(0), KLD = numeric(0))
  
  # Return the requested labels
  labelsInput <- reactive({
    switch(input$category,
           "acq"      = reut21578Labels$acq,
           "corn"     = reut21578Labels$corn,
           "crude"    = reut21578Labels$crude,
           "earn"     = reut21578Labels$earn,
           "grain"    = reut21578Labels$grain,
           "interest" = reut21578Labels$interest,
           "money-fx" = reut21578Labels$money,
           "ship"     = reut21578Labels$ship,
           "trade"    = reut21578Labels$trade,
           "wheat"    = reut21578Labels$wheat)
  })

  # Return the requested labels
  labelsInputTest <- reactive({
    switch(input$category,
           "acq"      = reut21578LabelsTest$acq,
           "corn"     = reut21578LabelsTest$corn,
           "crude"    = reut21578LabelsTest$crude,
           "earn"     = reut21578LabelsTest$earn,
           "grain"    = reut21578LabelsTest$grain,
           "interest" = reut21578LabelsTest$interest,
           "money-fx" = reut21578LabelsTest$money,
           "ship"     = reut21578LabelsTest$ship,
           "trade"    = reut21578LabelsTest$trade,
           "wheat"    = reut21578LabelsTest$wheat)
  })
  
  # Create list of kfolds based on slider input kFolds
  kFoldInput <- reactive({
    
    ## restore max values
    maxTraining <<- c(0, 5, 1, 35000, 1, 1, 1, 0)
    maxValidation <<- c(0, 5, 1, 35000, 1, 1, 1, 0)
    
    ## restore vector of combinations (q, m, KLD) explored
    explored <<- data.frame(q = numeric(0), m = numeric(0), KLD = numeric(0))
    
    ## Observe actions on resample button
    input$resample
    
    ## Get the number of folds
    kf <- input$kFolds
    
    ## Set the validation fold
    validationFold <<- 0
    
    ## build K folds (and return them)
    createKFold(nObs = dim(dataset)[1], k = kf)
    
  })
  
  # Return the indexes of the selected features
  featuresInput <- reactive({
    
    ## restore max values
    maxTraining <<- c(0, 5, 1, 35000, 1, 1, 1, 0)
    maxValidation <<- c(0, 5, 1, 35000, 1, 1, 1, 0)
    
    ## restore vector of combinations (q, m, KLD) explored
    explored <<- data.frame(q = numeric(0), m = numeric(0), KLD = numeric(0))
    
    #numOfFeat <- input$features
    input$features
    
  })
  
  # Get the fold to validate on
  switchKFold <- reactive({
    input$switch
    explored <<- data.frame(q = numeric(0), m = numeric(0), KLD = numeric(0))
    validationFold <<- validationFold %% input$kFolds + 1
  })
  
  ## Estimate the probabilities based on:
  ## category, kfolds, features, alpha and beta
  estimates <- reactive({
    
    # Get the labels
    catLabels <- labelsInput()
    
    # Get the features
    features <- featuresInput()
    
    # Get the kfolds
    kFolds <- kFoldInput()
    
    # Get switch button value (and add 1)
    kFoldNum <- switchKFold()
    
    # Build training indexes (kFoldNum is the validation)
    idxTrain <- unlist(kFolds[-kFoldNum])
    
    # Subset dataset and labels
    trainingKFold <- dataset[idxTrain, idxFeatures[1:features]]
    trainingLabels <- catLabels[idxTrain]

    validationKFold <- dataset[-idxTrain, idxFeatures[1:features]]
    validationLabels <- catLabels[-idxTrain]
    
    # Estimate probabilities
    probs <- nbEstimate(training = trainingKFold,
                        trainingLabels = trainingLabels,
                        prior = c(1, 1))
                        #prior = c(input$alpha, input$beta))
    
    # Return dataset, labels, and probabilities
    return(list(trainingKFold, trainingLabels,
                validationKFold, validationLabels,
                probs))
    
  })  
  
#   # Compute coordinates of training set
#   coordinatesTrain <- reactive({
#             
#     # Compute the estimates
#     probs <- estimates()
#     
#     # Compute coordinates
#     coords <- nbClassify(dataset = probs[[1]],
#                          labels = probs[[2]],
#                          estimates = probs[[5]])
#     
#     # return coordinates with labels and the other output of nbClassify
#     return(list(dataset = data.frame(coord = coords[[1]], labels = probs[[2]]),
#                 lineParam = coords[[2]],
#                 measures = coords[[3]]))
#     
#   })
  

  # Compute coordinates of validation set
  coordinatesValid <- reactive({
    
    # Compute the estimates
    probs <- estimates()
    
    # Compute coordinates
    coords <- nbClassify(dataset = probs[[3]],
                         labels = probs[[4]],
                         estimates = probs[[5]])
    
    # return coordinates with labels and the other output of nbClassify
    return(list(dataset = data.frame(coord = coords[[1]], labels = probs[[4]]),
                lineParam = coords[[2]],
                measures = coords[[3]]))
    
  })

  # Compute coordinates of test set
  coordinatesTest <- reactive({
    
    # Compute the estimates
    probs <- estimates()
    
    # Get the features
    features <- featuresInput()
    
    ## get labels
    labelsTest <- labelsInputTest()
    
    # Compute coordinates
    coords <- nbClassify(dataset = datasetTest[, idxFeatures[1:features]],
                         labels = labelsTest,
                         estimates = probs[[5]])
    
    # return coordinates with labels and the other output of nbClassify
    return(list(dataset = data.frame(coord = coords[[1]], labels = labelsTest),
                lineParam = coords[[2]],
                measures = coords[[3]]))
    
  })
  
  # Compute measures 
  measures <- reactive({
    
    # Get coordinates
    coords <- coordinatesValid()[[1]]
    
    ## measure performance
    ## compute recall
    truePositive <- sum(coords[coords[, 3] == 1, 1] * input$m + input$q > coords[coords[, 3] == 1, 2])
    recallValid  <- truePositive / sum(coords[, 3] == 1)
    
    ## compute precision
    falsePositive  <- sum(coords[coords[, 3] == 0, 1] * input$m + input$q > coords[coords[, 3] == 0, 2])
    if(truePositive == 0) {
      precisionValid <- 1
    } else {
      precisionValid <- truePositive / (truePositive + falsePositive)  
    }
    
    ## compute F1
    F1Valid <- 2 * recallValid * precisionValid / (recallValid + precisionValid)
    
    ## compute KLD
    trueNegative   <- sum(coords[, 3] == 0) - falsePositive
    falseNegative  <- sum(coords[, 3] == 1) - truePositive
    
    lambdaTePos    <- sum(coords[, 3] == 1) / (sum(coords[, 3] == 1) + sum(coords[, 3] == 0))
    lambdaHatTePos <- (truePositive + falsePositive) / (sum(coords[, 3] == 1) + sum(coords[, 3] == 0))
    lambdaTeNeg    <- 1 - lambdaTePos
    lambdaHatTeNeg <- (trueNegative + falseNegative) / (sum(coords[, 3] == 1) + sum(coords[, 3] == 0))
    
    KLD <- 0
    
    if(lambdaTePos == 0) {
      KLD <- 0
    }
    else if(lambdaHatTeNeg == 0) {
      KLD <- Inf
    }
    else {
      KLD <- (lambdaTePos * log(lambdaTePos / lambdaHatTePos)) + (lambdaTeNeg * log(lambdaTeNeg / lambdaHatTeNeg))
    }
    
    ## Return list of measures 
    return(list(train = c(), #c(recallTrain, precisionTrain, F1Train),
                valid = c(recallValid, precisionValid, F1Valid, KLD, lambdaHatTePos)))
    
  })
  
  # Compute measures 
  measuresTest <- reactive({
    
    # Get coordinates
    coords <- coordinatesTest()[[1]]
    
    ## measure performance
    ## compute recall
    truePositive <- sum(coords[coords[, 3] == 1, 1] * input$m + input$q > coords[coords[, 3] == 1, 2])
    recallTest  <- truePositive / sum(coords[, 3] == 1)
    
    ## compute precision
    falsePositive  <- sum(coords[coords[, 3] == 0, 1] * input$m + input$q > coords[coords[, 3] == 0, 2])
    if(truePositive == 0) {
      precisionTest <- 1
    } else {
      precisionTest <- truePositive / (truePositive + falsePositive)  
    }
    
    ## compute F1
    F1Test <- 2 * recallTest * precisionTest / (recallTest + precisionTest)
    
    ## compute KLD
    trueNegative   <- sum(coords[, 3] == 0) - falsePositive
    falseNegative  <- sum(coords[, 3] == 1) - truePositive
    
    lambdaTePos    <- sum(coords[, 3] == 1) / (sum(coords[, 3] == 1) + sum(coords[, 3] == 0))
    lambdaHatTePos <- (truePositive + falsePositive) / (sum(coords[, 3] == 1) + sum(coords[, 3] == 0))
    lambdaTeNeg    <- 1 - lambdaTePos
    lambdaHatTeNeg <- (trueNegative + falseNegative) / (sum(coords[, 3] == 1) + sum(coords[, 3] == 0))
    
    KLD <- 0
    
    if(lambdaTePos == 0) {
      KLD <- 0
    }
    else if(lambdaHatTeNeg == 0) {
      KLD <- Inf
    }
    else {
      KLD <- (lambdaTePos * log(lambdaTePos / lambdaHatTePos)) + (lambdaTeNeg * log(lambdaTeNeg / lambdaHatTeNeg))
    }
    
    ## Return list of measures 
    return(c(recallTest, precisionTest, F1Test, KLD, lambdaHatTePos))
    
  })
  
  
  
  output$validObjects <- renderText({
    coords <- coordinatesValid()
    paste("Validation, k-fold =", switchKFold(),
          "\nNum of objects:", dim(coords[[1]])[1],
          "\nNum of positive examples:", sum(coords[[1]]$labels),
          "\nTrue prevalence of class:", format(sum(coords[[1]]$labels)/length(coords[[1]]$labels), digits = 2)
    )
  })

  output$testObjects <- renderText({
    labelsTest <- labelsInputTest()
    paste("Test",
          "\nNum of objects:", length(labelsTest),
          "\nNum of positive examples:", sum(labelsTest),
          "\nTrue prevalence of class:", format(sum(labelsTest)/length(labelsTest), digits = 2)
    )
  })

  
  
  output$validMeasures <- renderTable({
    
    coords <- coordinatesValid()
    meas <- measures()
    
    # update max measures validation
    if(!is.na(meas[[2]][3]) & meas[[2]][3] > maxValidation[1]) {
      maxValidation <<- c(meas[[2]][3],
                        isolate(input$kFolds),
                        validationFold,
                        isolate(input$features),
                        isolate(input$alpha),
                        isolate(input$beta),
                        isolate(input$m),
                        isolate(input$q))
      #print(paste("Validation", maxValidation))
    }
    
    ## update vector of combinations explored
    df_names <- colnames(explored)
    explored <<- rbind(explored, c(input$q, input$m, meas[[2]][4]))
    colnames(explored) <<- df_names
    #print(explored)
    
    data.frame("Rec" = c(coords[[3]][1], meas[[2]][1]),
               "Pre" = c(coords[[3]][2], meas[[2]][2]),
               "F1"  = c(coords[[3]][3], meas[[2]][3]),
               "KLD" = format(c(coords[[3]][4], meas[[2]][4]), digits = 3, scientific = TRUE),
               "CC"  = c(coords[[3]][5], meas[[2]][5]),
               row.names = c("0-1 loss", "alt. loss"))
  },
  include.rownames = TRUE
  #digits = 4
  )
  
  output$testMeasures <- renderTable({
    
    coords <- coordinatesTest()
    meas <- measuresTest()
    
    data.frame("Rec" = c(coords[[3]][1], meas[1]),
               "Pre" = c(coords[[3]][2], meas[2]),
               "F1"  = c(coords[[3]][3], meas[3]),
               "KLD" = format(c(coords[[3]][4], meas[4]), digits = 3, scientific = TRUE),
               "CC"  = c(coords[[3]][5], meas[5]),
               row.names = c("0-1 loss", "alt. loss"))
  },
  include.rownames = TRUE
  #digits = 4
  )
  

  # Generate plot of the coordinates
  output$plotValid <- renderPlot({
    
    # Get coordinates
    coords <- coordinatesValid()[[1]]
    coords[, 3] <- as.factor(coords[, 3])
    colnames(coords) <- c("x", "y", "classes")
    
    # Get line parameters
    params <- c(input$q, input$m)
    
    # Build plot
    ggplot(coords, aes(x = x, y = y, colour = classes)) +
      geom_point(alpha = 0.2) + 
      #geom_point(aes(colour = factor(labels)), alpha = 0.2) + 
      geom_abline(slope = 1, intercept = 0, colour = "green") +
      geom_abline(slope = params[2], intercept = params[1], colour = "blue")
    
  })

  output$plotExplored <- renderPlot({
    
    # Get coordinates
    coords <- coordinatesValid()[[1]]
    meas <- measures()
    
    explored[, 3] <- cut(explored[, 3], breaks = c(0, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, Inf))
    
    # Build plot
    ggplot(explored, aes(x = m, y = q, colour = KLD, size = KLD)) +
      geom_point() + 
      #geom_point(aes(colour = explored$KLD)) + #, size = explored$KLD)) +
      #geom_point(aes(colour = cut(explored$KLD, breaks = c(0, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1)))) + #, size = explored$KLD)) +
      coord_cartesian(xlim = c(0.9, 1.3), ylim = c(-100, 250))
      
    
  })
  
  ## observe action button to reset slider input values
  observe({
    x <- input$reset
    validationFold <- 1
    explored <<- data.frame(q = numeric(0), m = numeric(0), KLD = numeric(0))
    updateSliderInput(session, inputId = "kFolds", value = 5)
    updateSliderInput(session, inputId = "features", value = 35000)
    updateSliderInput(session, inputId = "alpha", value = 1.0)
    updateSliderInput(session, inputId = "beta", value = 1.0)
    updateSliderInput(session, inputId = "m", value = 1.0)
    updateSliderInput(session, inputId = "q", value = 0.0)
  })

  ## observe action button to reset slider input values to best training values
  observe({
    x <- input$bestTraining
    updateSliderInput(session, inputId = "kFolds", value = maxTraining[2])
    validationFold <- maxTraining[3]
    updateSliderInput(session, inputId = "features", value = maxTraining[4])
    updateSliderInput(session, inputId = "alpha", value = maxTraining[5])
    updateSliderInput(session, inputId = "beta", value = maxTraining[6])
    updateSliderInput(session, inputId = "m", value = maxTraining[7])
    updateSliderInput(session, inputId = "q", value = maxTraining[8])
  })
  
  ## observe action button to reset slider input values to best validation values
  observe({
    x <- input$bestValidation
    updateSliderInput(session, inputId = "kFolds", value = maxValidation[2])
    validationFold <- maxValidation[3]
    updateSliderInput(session, inputId = "features", value = maxValidation[4])
    #updateSliderInput(session, inputId = "alpha", value = maxValidation[5])
    #updateSliderInput(session, inputId = "beta", value = maxValidation[6])
    updateSliderInput(session, inputId = "m", value = maxValidation[7])
    updateSliderInput(session, inputId = "q", value = maxValidation[8])
  })
  
  
  

  ## observe changes in action button to resample folds
  observe({
    labelsInput()
    
    ## restore max values
    maxTraining <<- c(0, 5, 35000, 1, 1, 1, 0)
    maxValidation <<- c(0, 5, 1, 35000, 1, 1, 1, 0)
    
    explored <<- data.frame(q = numeric(0), m = numeric(0), KLD = numeric(0))
    
    ## restore sliders values
    updateSliderInput(session, inputId = "kFolds", value = 5)
    #updateSliderInput(session, inputId = "alpha", value = 1.0)
    #updateSliderInput(session, inputId = "beta", value = 1.0)
    updateSliderInput(session, inputId = "m", value = 1.0)
    updateSliderInput(session, inputId = "q", value = 0.0)
  })
  
  
})
