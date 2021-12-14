###############################################################################
#
# Main Imputation algorithm for missing data
#
###############################################################################


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
#' data(nhanes)
#' miss_data <- imputevalR::makeNA(nhanes, proportionNA = 0.2)
#' imputed <- imputer(miss_data, nchains = 1, niter = 5)
#' 
#' @importFrom Rdpack reprompt
#' @import stats
#' 
#' @export
imputer <- function(df, nchains = 5, niter = 100) {
  
  colnum <- length(df[1,]) # length of variables
  rownum <- length(df[,1]) # length of rows (ie length of one variable)
  misRcd <- is.na(df)      # matrix of booleans, TRUE for missing values
  
  ### Determine variable type: 1 = continuous, 2 = binary
  datType <- numeric()

  for (i in 1 : colnum) {
    if (length(unique(df[,i])) <= 3) {
      datType[i] <- 2 # less than 3 unique values --> binary
    } else {
      datType[i] <- 1 # 3 or more unique values --> continuous
    }
  }
  
  # Impute the datasets ----------------------------------------------
  
  ### Starting values to initialize sampler
  dsets <- list() # initialize empty list
  
  # Loop through number of chains to fill missing values with
  # starting values to begin the sampling
  for (j in 1:nchains) {
    dfNew <- df

    for (i in 1 : colnum) {
      misInd <- which(is.na(dfNew[,i]) == TRUE)
      pct    <- runif(length(misInd), min = 0, max = 1)

      # starting values are the pct pertentile of the observed
      # distribution of non-missing values, a good starting guess
      impVal <- quantile(dfNew[,i], pct, na.rm = TRUE)

      dfNew[which(is.na(dfNew[,i]) == TRUE), i] <- impVal

      # if binary variable, round the value to 0 or 1
      if (datType[i] == 2) {
        dfNew[,i] <- round(dfNew[,i])
      }
    }
    
    dsets[[j]] <- dfNew
  }
  
  returnSets <- list()
  
  ### Sampler to draw the missing values for each imputed dataset
  for (chain in 1 : length(dsets)) {
    
    startDt <- dsets[[chain]]
    
    ### Loop through number of iterations within chain
    for (iter in 1 : niter) {

      # Simple progress bar
      if (iter %% 10 == 0) {
        cat(paste0("CHAIN: ", chain, ", ITERATION ", iter, "/", niter, "\n"))  
      }

      # Loop through variables
      for (var in 1 : colnum) {
        
        ## Create the formula, which is
        ## missing_variable ~ all other variables
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
        
        ### Continuous variable --> linear regression, normal predictive distr. draw
        if (datType[var] == 1) {
          
          model1 <- lm(formula = form, data = startDt)
          intervals <- predict(model1, newdata = startDt[misList,], interval = "prediction")
          
          #startDt[misList, var] <- intervals[,1]
          for (k in 1 : length(misList)) {
            
            ### Sample from the predictive distribution 
            estVal <- rnorm(1, mean = intervals[k,1], sd = (intervals[k,1] - intervals[k,2])/qnorm(0.975))
            startDt[misList[k], var] <- estVal
          }
        } else {
          ### Binary variables --> logistic regression
          model2 <- glm(formula = form, data = startDt, family = binomial(link = "logit"))
          p1 <- predict(model2, newdata = startDt[misList,], type = "response")
          
          for (k in 1 : length(misList)) {
            estVal <- rbinom(n = 1, size = 1, prob = p1[k])
            startDt[misList[k], var] <- estVal
          }
        } 
      }
    }

    # Save imputed dataset into returnSets list 
    returnSets[[chain]] <- startDt
  }

  # Return the list of imputed data frames
  return(returnSets)
}



