


# loads the data for training get param=T otherwise, testing set
load_data <- function(train=T){
  data.full <- read.csv("melb_data.csv")
  
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



