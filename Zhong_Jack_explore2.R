# Jack Zhong
# Nov. 2016 

library(ggplot2)
library(grid)
# run the subfunctions first
# for the defensive coding, just make sure things that passes to the subfunctions are correct


#1-------------------------------------------------------------------------
freq_table <- function(data) {
  # this function gives a frequency table for categorical and logical variable
  # Parameter: a dataframe
  # Returns: a frequency table

  tb1 <- lapply(data.frame(data[,sapply(data,is.logical)]),table) #draw the table for logical variables
  tb2 <- lapply(data.frame(data[,sapply(data,is.factor)]),table) #draw the table for factor variables
  return(list(tb1,tb2))
}


#2---------------------------------------------------------------------------
num_summary <- function(data) {
  # this function gives a summary for numerical variable
  # Parameter: a dataframe
  # Returns: a statistics table
  Num <- data[sapply(data, is.numeric)]
  return (summary(Num))
}


r_squared <- function(data) {
  # this function calculate r-sqaure value and returns dataframe
  # Parameter: a dataframe
  # Returns: a new dataframe that contains each pair of column names and 
  # corresponding r-square value

  data <- na.omit(data) # omit any na
  data <- data[sapply(data, is.numeric)] # only the numeric
  colname <- colnames(data) # extract column names
  pairwise_names <- c() # initiate pair of variables
  pairwise_r_squared <- c() # initiate the r_squared

  if (ncol(data) <= 1){
    new_data ='sorry we cannot count pairwise R spuare because of the number of numeric variables'
  }
  else{

  for (i in 1:(length(colname)-1)) {
    for (j in (i+1):length(colname)) {
      ##Prof G - changed the following line
      temp <- summary(lm(data[[i]]~data[[j]]))$r.squared
      # do linear model on the paired columns and get the r.squared number
      pairwise_names <- c(pairwise_names, paste(colname[i], colname[j], sep="-"))
      # add the new pairnames to the old list
      pairwise_r_squared <- c(pairwise_r_squared, temp)
      # add teh new r_squared number to the old list
    }
  }
  new_data <- data.frame(pairwise_names, pairwise_r_squared)
  colnames(new_data) <- c("Variable Pairs", "R-squared")

  }
  return (new_data)
}  


pearson_corr <- function(data, threshold = 0) {
  # this function caculate corr and returns a dataframe
  #Parameter: a dataframe
  #Returns: a new dataframe that contains each pair of column names and corresponding 
  #pearson correlation coefficient

  data <- na.omit(data) # omit any na
  Num <- data[,sapply(data, is.numeric)] # creates a variable that only has the numeric
  
  b <- combn(colnames(Num), 2) # finds all combinations of the name pairs
  pairs <- paste(b[1,], b[2, ], sep = "-") 
  # the column names are seperated by - using paste function
  c <- cor(Num, method = "pearson")
  # finds the pearson correlation using the cor function
  correlation <- c[which(lower.tri(c))]  
  # lower triangular matrix gets rid of dupilicates and 1's on the diagonal
  newdf <- data.frame(pairs, correlation) # create a new data frame with our pairs
  newdf <- subset(newdf, correlation > threshold) # apply the threshold
  colnames(newdf) <- c("Variable Pairs", "Pearson Exceeds Threshold")
  return(newdf)
}




#3------------------------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # this function draws multiple graphs in one page.
  # reference: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  plots <- c(list(...), plotlist)
  # create a list 'plots' using ... and plotlist
  numPlots = length(plots)
  if (is.null(layout)) {
    # if layout is NULL, then use 'cols' to determine layout
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
    # ncol=number of columns in plots
    # nrow=number of rows needed, calculated from # of cols
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # plot each in the correct location
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      # the position that contain this subplot
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


plotsNum <- function(data,plotswitch='off',bins=NULL){
  # this function plots a pair of blue histograms with a vertical red line at the 
  # mean (counts and density) for every numerical variable at 
  # each number of bins integer specified in the bin vector parameter.
  
  # Parameter:
  #   a dataframe
  #   plotswitch: a character decide whether to plot
  #   vector: bin numbers of historgram (use the default bin size if vector is not provided)
  
  #Returns: histogranms
  num=data[sapply(data,is.numeric)]
  if(plotswitch == "on"){
    if(!is.null(vector)){ # if the user do not specify the binsize
      for(j in 1:length(bins)){ 
        for(i in 1:ncol(num)){
          mean <- mean(num[,i]) 
          # caculate the mean of each numeric column
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+ 
            geom_histogram(fill="blue",bins=bins[j])+
            ggtitle(paste(colnames(num[i]),bins[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") 
          # draw the histogram of count
          # add red line interpreating mean
          
          p2 <- ggplot(num,aes(x=num[i],..density..))+
            geom_histogram(fill="blue",bins=bins[j])+
            ggtitle(paste(colnames(num[i]),bins[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") 
          #draw the density histogram
          
          grid.newpage()
          #new page
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          title <- paste(colnames(num[i]),bins[j],sep=" bin=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
          # print p1 and p2 histograms in pairs
          
        }
      }
    }else{ #if bins isn't NULL
      for(i in 1:ncol(num)){
        mean <- mean(num[,i]) 
        # caculate the mean of each numeric column
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+  
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        # draw the histogram of count
        
        p2 <- ggplot(num,aes(x=num[i],..density..))+
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        # draw the density histogram
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(num[i]),"default bins",sep=" bins=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))#print p1 and p2 two histograms
        
      }
      
    }
    
  }else{
    if(plotswitch == "grid"){#  plotswitch is "grid"
      for(j in 1:length(bins)){
        grid.newpage()
        his_count <-list()   
        his_density <- list()  
        #create two empty list
        for(i in 1:ncol(num)){
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(fill="blue", bins = bins[j])+ 
            labs(title= paste(bins[j], "bins")) 
          # draw histograms for counts and add them to the list
        }
        multiplot(plotlist = his_count, cols = 2)  
        #draw all histogram with same bins in one page
        for(i in 1:ncol(num)){
          his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue", bins = bins[j])+ 
            labs(title= paste(bins[j], "bins")) 
          #draw histograms for density and add them to the list
        }
        multiplot(plotlist = his_density, cols = 2)  
        #similar to above, draw all histogram of density with same bins in one page
      }
    }
  }
}




#4-----------------------------------------------------------------
is.binary <- function(v) {
  # this function  tell whether the vector is a binary vector
  # Parameter: a vector
  # Returns: TRUE if the vector is binary, FALSE else
  x <- unique(v)                    
  #x contains all unique values in v
  length(x) - sum(is.na(x)) == 2L         
  #check to see if x only contains 2 distinct values
}

##Prof G - Error being generated in the plot.
##Prof G - Same problem as in generating the LM
# Don't know how to automatically pick scale for object of type tbl_df/tbl/data.frame. Defaulting to continuous.
# Don't know how to automatically pick scale for object of type tbl_df/tbl/data.frame. Defaulting to continuous.
# Hide Traceback
# 
# Rerun with Debug
# Error: Discrete value supplied to continuous scale 
# 18 stop("Discrete value supplied to continuous scale", call. = FALSE) 
# 17 scales::train_continuous(x, self$range) 
# 16 f(..., self = self) 
# 15 self$range$train(x) 
# 14 f(..., self = self) 
# 13 scales[[i]][[method]](data[[var]][scale_index[[i]]]) 
# 12 FUN(X[[i]], ...) 
# 11 lapply(seq_along(scales), function(i) {
#   scales[[i]][[method]](data[[var]][scale_index[[i]]])
# }) 
# 10 FUN(X[[i]], ...) 
# 9 lapply(vars, function(var) {
#   pieces <- lapply(seq_along(scales), function(i) {
#     scales[[i]][[method]](data[[var]][scale_index[[i]]])
#   }) ... 
#   8 scale_apply(layer_data, x_vars, "train", SCALE_X, panel$x_scales) 
#   7 train_position(panel, data, scale_x(), scale_y()) 
#   6 ggplot_build(x) 
#   5 print.ggplot(p) 
#   4 print(p) at Zhong_Jack_explore2.R#249
#   3 plots_cat_or_bi(data, plotswitch) at Zhong_Jack_explore2.R#272
#   2 explore(data, plotswitch, threshold, bins) at Zhong_Jack_explore2.R#323
#   1 explore2(diamonds, "grid", 0.3, c(10, 25, 50)) 

plots_cat_or_bi <- function(data, plotswitch = "off") {
  data1 <- data[,sapply(data,is.factor)]
  data2 <- data[,sapply(data,is.logical)]
  data3 <- data[,sapply(data,is.binary)]
  Cat_Or_Bi <- data.frame(data1,data2,data3)
  # pick out categorical and binary columns
  if(plotswitch=="on"|plotswitch=="grid"){
    for(i in 1:ncol(Cat_Or_Bi)){
      ##Prof G - changed the following line
      p <- ggplot(Cat_Or_Bi,aes(x=data[[i]]))+
        geom_bar(fill='gray')+ xlab(colnames(Cat_Or_Bi[i]))
      # for each column, print a bar plot
      print(p)
    }
  }
}

explore <- function(data, plotswitch = "off", threshold = 0, bins = NULL) {
  # this is the main function
  # execute the subfunctions first 
  # parameters: 
  #   a dataframe, a string indicating plotswitch
  #   number between 0 to 1 as threshhold, number of binsize
  # return:
  #   frequency table for categorical and logical variable
  #   summary for numerical variable
  #   r-sqaure value for numerical variables and returns dataframe
  #   caculate corr between numerical variables and returns a dataframe
  # 2 plots with required elements
  
  freq_table <- freq_table (data)
  num_summary <- num_summary (data)
  r_squared <- r_squared (data)
  pearson_corr <- pearson_corr (data, threshold)
  plotsBlue <- plotsNum (data, plotswitch, bins)
  plotsGray <- plots_cat_or_bi (data, plotswitch)
  
  return (list(freq_table, num_summary, r_squared, pearson_corr, plotsBlue, plotsGray))
  
}

explore2 <- function(data,plotswitch="off", threshold=0.5, bins=NULL){
  # explore with defensive coding
  # take care of N/As
  data <- na.omit(data)
  
  # if the first parameter is not a dataframe, change it into a dataframe
  if(!is.data.frame(data)){                 
    data <- as.data.frame(data)
  }
  
  # if the second parameter is not what we required, ask users to reinput it
  while(plotswitch != "off" && plotswitch != "on" && plotswitch != "grid"){  
    print("invalid input for plotswitch")
    plotswitch <- readline(prompt="Choose one of these for plotswitch: off / on / grid ")  
  }
  
  # correlation should be between 0 and 1
  while(!is.numeric(threshold) || threshold < 0 || threshold >1 ){    # check threshold
    print("correlation threshold must be numeric within range [0,1]")
    threshold <- as.numeric(readline(prompt="Enter your correlation threshold: "))   #re-enter the input
  }
  
  # check if bin vector all numeric not less than 0
  if(!is.null(bins)){
    if(!is.numeric(bins)||(is.numeric(bins) && (TRUE %in% (bins <= 0)))){ 
      print("the bins vector must be numeric larger than 0, enter new bins one by one and press 'return'...")
      bins <- c()
      bin <- 1
      #input "return"  to finish loop
      while(bin != ""){ 
        #re-enter the bin vector
        bin <- readline(prompt="Enter the number of bins: ")->bin1
        bin1 <- as.numeric(bin1)
        bins <- c(bins, bin1)
      }
      bins <- na.omit(bins) # get rid of the N/As
    }
    
    # round decimals

    if (!is.integer(bins)) {        
      bins <- round(bins)
    }
  }
  # return explore function results with the correct inputs
  return(explore(data,plotswitch,threshold,bins))
}

# test

#explore2(diamonds,"abcd",1.5)



