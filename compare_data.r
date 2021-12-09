
# Calculates the mean for each column for df1 and df2, then calculates the squared difference
compare_by_column <- function(df1, df2){
  
  mean1 <- apply(df1, 2, mean)
  mean2 <- apply(df2, 2, mean)
  
  squarediff <- (mean1-mean2)**2
  
  return(squarediff)
  
}

# Calculates the squared difference of each cell in df1 against df2
compare_by_cell <- function(df1, df2){
  
  compared_df <- (df1 - df2)**2
  summed_diff <- apply(compared_df, 2, sum)
  
  return(summed_diff)
  
}