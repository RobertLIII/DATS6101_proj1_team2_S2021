


# loads the data for training get param=T otherwise, testing set
load_data <- function(train=T){
  data.full <- read.csv("melb_data.csv")
  
  
  data.full$Price.cuts <- as.character(cut(data.full$Price, 10))
  data.full$Price.cuts[data.full$Price > 3650000] <- "(3.65e+06,9.01e+06]"
  
  data.full$Price.cuts <- as.factor(data.full$Price.cuts)
  
  
  # create train and test data
  samps <- read.csv("sample.txt", sep=" ")
  
  data.train <- data.full[(samps$x),]
  data.test <- data.full[(-samps$x),]
  
  
  if (train==T){
    return(data.train)
  }
  else{
    return(data.test)
  }
}



r2 <- function(y.predict, y.actual=y.test){
  
  TSS <- sum((y.actual - mean(y.actual))^2)
  RSS <- sum((y.predict - y.actual)^2)
  
  rSq <- 1 - RSS/TSS
  
  return(rSq)
}


