



# loads the data for training set param=T otherwise, testing set
load_data <- function(train=T){
  data.full <- read.csv("melb_data.csv")
  
  # create categories
  data.full$Price.cuts <- as.character(cut(data.full$Price, 10))
  data.full$Price.cuts[data.full$Price > 3650000] <- "(3.65e+06,9.01e+06]"
  
  # convert to factors
  data.full[sapply(data.full, is.character)] <- lapply(data.full[sapply(data.full, is.character)], as.factor)
  
  data.full$Postcode <- factor(data.full$Postcode)
  
  # address is unique therefor not a helpful predictor
  data.full <- subset(data.full, select=-c(Address))
  
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
  
  TSS <- sum((y.actual - mean(y.actual))^2, na.rm=T)
  RSS <- sum((y.predict - y.actual)^2, na.rm=T)
  
  rSq <- 1 - RSS/TSS
  
  return(rSq)
}





# Trees and Forests
loadPkg("randomForest")
loadPkg("tree")
trees <- function(){
  
  train <- load_data()
  test <- load_data(F)
  
  # factors < 32 cats
  model <-tree(Price.cuts~., data=subset(train, select=-c(Price, SellerG, Suburb, Date, Postcode, CouncilArea)), na.action=na.roughfix)
  
  p <- predict(model, test, type="class")
  
  sum(p == test$Price.cuts)
}



forest.reg <- function(){
  
  train <- load_data()
  test <- load_data(F)
  
  # factors < 53 cats
  model <- randomForest(Price~., data=subset(train, select=-c(Price.cuts, SellerG, Suburb, Date, Postcode)), importance=T, na.action=na.roughfix, ntree=100)
  
  p <- predict(model, test)
  
  r2(p, test$Price)
}




forest.cat <- function(){
  
  train <- load_data()
  test <- load_data(F)
  
  model <- randomForest(Price.cuts~., data=subset(train, select=-c(Price)), importance=T, na.action=na.omit, ntree=100)
  
  p <- predict(model, test, na.action=na.roughfix)
  
  
}

unloadPkg(randomForest)
unloadPkg(tree)