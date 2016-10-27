explore <- function(data, plotswitch = "off", threshold = 0, bins = NULL) {
  library (ggplot2)
  freq_table <- freq_table (data)
  num_summary <- num_summary (data)
  r_squared <- r_squared (data)
  pearson_corr <- pearson_corr (data, threshold)
  plotsBlue <- plotsNum (data, plotswitch, bins)
  plotsGray <- plots_cat_or_bi (data, plotswitch)
  
  return (c(freq_table, num_summary, r_squared, pearson_corr, plotsBlue, plotsGray))
  
}

freq_table <- function(data) {
  Cat_Or_Log <- c(data[sapply(data, is.factor)], data[sapply(data, is.logical)])
  return (lapply(Cat_Or_Log, table))
}

num_summary <- function(data) {
  Num <- data[sapply(data, is.numeric)]
  return (summary(Num))
}

r_squared <- function(data) {
  data <- data[sapply(data, is.numeric)] # only the numeric
  colname <- colnames(data) # extract column names
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
  

pearson_corr <- function(data, threshold = 0) {
  Num <- data[sapply(data, is.numeric)] # creates a variable that only has the numeric
  b <- combn(colnames(Num), 2) # finds all combinations of the name pairs
  pairs <- paste(b[1,], b[2, ], sep = "-") 
     # the column names are seperated by - using paste function
  c <- cor(Num, method = "pearson")
     # finds the pearson correlation using the cor function
  correlation <- c[which(lower.tri(c))]  
     # lower triangular matrix gets rid of dupilicates and 1's on the diagonal
  newdf <- data.frame(pairs, correlation) # create a new data frame with our pairs
  newdf <- subset(newdf, correlation > threshold) # apply the threshold
  return(newdf)
}


plotsNum <- function(data, plotswitch, bins=NULL) {
  data_frame <- data[sapply(data, is.numeric)]
  for(j in 1:length(bins)){ # perform function for each bin size
    if (plotswitch == "on") {
       for(i in 1:ncol(data_frame)){
          A <- ggplot(data_frame,aes(x=data_frame[,i],..density..))+
                 geom_histogram(bins=bins[j],fill="blue")+
                 geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                 xlab(colnames(data_frame)[i])
          # plot a density histogram for each bin size
      
          B <- ggplot(data_frame,aes(x=data_frame[,i]))+
                geom_histogram(bins=bins,fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame[i]))
          # plot a count histogram for each bin size
          print (A)
          print (B)
       }
    }
    else if (plotswitch == "grid") {
        
    }
    else {
    }
  }
}


plots_cat_or_bi <- function(data, plotswitch = "off") {
  Cat_Or_Bi <- data[,sapply(data,is.factor)|sapply(data,is.logical)]
  if(plotswitch=="on"|plotswitch=="grid"){
    for(i in 1:ncol(Cat_Or_Bi)){
      p <- ggplot(Cat_Or_Bi,aes(x=data[,i]),colour="gray")+
        geom_bar()+ xlab(colnames(Cat_Or_Bi[i]))
      print(p)
    }
  }
}


