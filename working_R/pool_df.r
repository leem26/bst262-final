pool_df <- function(dfList){
  
  numDF <- length(dfList)
  
  rows <- dim(dfList[[1]])[1]
  
  columns <- dim(dfList[[1]])[2]
  
  finalDF <- data.frame(matrix(nrow = rows, ncol = columns))
  
  
  for (r in 1:rows){
    
    for (c in 1:columns){
      
      cell_values <- NULL
      
      for (l in 1:numDF){
        
        cell_values <- c(cell_values, dfList[[l]][r,c])
        
      }
      
      avg_value <- mean(cell_values)
      
      finalDF[r,c] <- avg_value
      
    }
    
    
  }
  
  colnames(finalDF) <- colnames(dfList[[1]])
  
  rownames(finalDF) <- rownames(dfList[[1]])
  
  return(finalDF)
  
}