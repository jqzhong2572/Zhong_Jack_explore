explore <- function(data, plotswitch = "off", threshold = 0, bins = NULL) {
  
  freq_table <- freq_table (data)
  num_summary <- num_summary (data)
  r_squared <- r_squared (data)
  pearson_corr <- pearson_corr (data, threshold)
  plots <- plots (data, plotswitch, bins)
    
}

freq_table <- function(data) {
  Num_Or_Log <- c(data[sapply(data, is.factor)], data[sapply(data, is.logical)])
  return (lapply(Num_Or_Log, table))
}

num_summary <- function(data) {
  Num <- data[sapply(data, is.numeric)]
  return (summary(Num))
}

r_squared <- function(data) {
  data <- data[sapply(data, is.numeric)] # only the numeric
  pairwise_names <- c() # initiate pair of variables
  pairwise_r_squared <- c() # initiate the r_squared
  for (i in 1:(length(colname)-1)) {
    for (j in (i+1):length(colname)) {
      temp <- summary(lm(data[,i]~data[,j]))$r.squared
        # do linear model on the paired columns and get the r.squared number
      pairwise_names <- c(pairwise_names, paste(colname[i], colname[j], sep="-"))
        # add the new pairnames to the old list
      pairwise_r_squared <- c(pairwise_r_squared, temp)
        # add teh new r_squared number to the old list
    }
  }
  new_data <- data.frame(pairwise_names, pairwise_r_squared)
  colnames(new_data) <- c("Variable Pairs", "R-squared")
  return (new_data)
}  
  

pearson_corr <- function(data, threshold) {
  Num <- data[sapply(data, is.numeric)] # creates a variable that only has the numeric
  b <- combn(colnames(Num), 2) # finds all combinations of the name pairs
  pairs <- paste(b[1,], b[2, ], sep = "-") 
     # the column names are seperated by - using paste function to paste the columns
  c <- cor(Num, method = "pearson")
     # finds the pearson correlation using the cor function and creates of matrix of the values
  correlation <- c[which(lower.tri(c))]  
     # gets the correclation values of the lower triangular matrix since those match the column pairs 
  newdf <- data.frame(pairs, correlation) # create a new data frame with our pairs
  newdf <- subset(newdf, correlation > threshold) # apply the threshold
  return(newdf)
}




