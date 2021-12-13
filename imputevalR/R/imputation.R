#' Run Imputation Procedure and Generate Imputed Datasets
#' 
#' Takes in a data.frame with missing values, and runs the imputation algorithm 
#' to return 5 imputed data sets (stored as a list) with missing values replaced 
#' by imputed values. 
#' 
#' @param df A data.frame for which imputed data sets should be generated
#' @param nchains The number of chains (ie number of imputed datasets) to generate
#' @param niter The number of iterations to complete in each chain (default is 100 for convergence)
#' 
#' @return A list (\code{returnSets}) that contains 5 imputed data.frames with missing values 
#' that were originally in df with imputed values based on the predicted imputed values
#' 
#' @examples
#' complete_data <- read.csv("data/imputed_nhanes.csv")
#' miss_data <- imputevalR::makeNA(complete_data, proportionNA = 0.2)
#' imputed <- imputer(miss_data, nchains = 1, niter = 5)
#' 
#' @importFrom Rdpack reprompt
#' 
#' @export
imputer <- function(df, nchains = 5, niter = 100) {
  
  colnum <- length(df[1,])
  rownum <- length(df[,1])
  
  misRcd <- is.na(df)
  
  ### determine variable type: 1 continuous 2 binary
  datType <- numeric()
  
  for (i in 1 : colnum) {
    if (length(unique(df[,i])) <= 3) {
      datType[i] <- 2
    } else {
      datType[i] <- 1
    }
  }
  
  ### get the filled dataset
  dsets <- list()
  
  ############### will return 5 imputed dataset
  for (j in 1:nchains) {
    dfNew <- df

    for (i in 1 : colnum) {
      misInd <- which(is.na(dfNew[,i]) == TRUE)
      pct <- runif(length(misInd), min = 0, max = 1)
      impVal <- quantile(dfNew[,i], pct, na.rm = TRUE)
      dfNew[which(is.na(dfNew[,i]) == TRUE), i] <- impVal

      if (datType[i] == 2) {
        dfNew[,i] <- round(dfNew[,i])
      }
    }
    
    dsets[[j]] <- dfNew
  }
  
  returnSets <- list()
  
  #################################################
  ### 5 chains for now
  for (chain in 1 : length(dsets)) {
    
    startDt <- dsets[[chain]]
    
    ### number of iterations
    for (iter in 1 : niter) {
      print(iter)
      for (var in 1 : colnum) {
        
        ## create the formula
        y <- colnames(startDt)[var]
        x <- colnames(startDt)[-var]
        form <- paste0(y, " ~ ")
        
        for (p in 1 : (colnum - 1)) {
          
          if (p < (colnum - 1)) {
            form <- paste0(form, x[p], " + ")
          } else {
            form <-  paste0(form, x[p])
          }
        }
        
        misList <- which(misRcd[,var] == TRUE)
        
        ### continuous variable
        if (datType[var] == 1) {
          
          model1 <- lm(formula = form, data = startDt)
          intervals <- predict(model1, newdata = startDt[misList,], interval = "prediction")
          #startDt[misList, var] <- intervals[,1]
          for (k in 1 : length(misList)) {
            
            ### sample from the predicted distribution (i think this is the conditional distribution)
            estVal <- rnorm(1, mean = intervals[k,1], sd = (intervals[k,1] - intervals[k,2])/1.96)
            startDt[misList[k], var] <- estVal
          }
          
          if (var == 1) {
            bmiMean[iter] <- mean(startDt[misList, var])
            #bmiMean[iter] <- startDt[misList[1], var]
          }
        } else {
          ### binary variables
          model2 <- glm(formula = form, data = startDt, family = binomial(link = "logit"))
          p1 <- predict(model2, newdata = startDt[misList,], type = "response")
          
          for (k in 1 : length(misList)) {
            
            estVal <- rbinom(n = 1, size = 1, prob = p1[k])
            startDt[misList[k], var] <- estVal
          }
        } 
      }
    }

    returnSets[[chain]] <- startDt
  }

  return(returnSets)
}
