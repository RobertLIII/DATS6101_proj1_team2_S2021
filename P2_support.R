



# loads the data for training set param=T otherwise, testing set
load_data <- function(train=T){
  data.full <- read.csv("melb_data.csv")
  
  # replace NA with mean for suburb
  data.full <- interpolate(data.full)
  
  # if suburb not possible, use column mean
  data.full$YearBuilt[is.na(data.full$YearBuilt)] <- mean(data.full$YearBuilt, na.rm=TRUE)
  data.full$BuildingArea[is.na(data.full$BuildingArea)] <- mean(data.full$BuildingArea, na.rm=TRUE)
  
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


interpolate <- function(data){
  
  for (i in 1:nrow(data)){
    
    if (is.na(data[i, "Car"])){
      
      suburb <- data[i, "Suburb"]
      
      data[i, "Car"] <- mean(subset(data, Suburb==suburb)$Car, na.rm=T)
    }
    
    
    if (is.na(data[i, "BuildingArea"])){
      
      suburb <- data[i, "Suburb"]
      
      data[i, "BuildingArea"] <- mean(subset(data, Suburb==suburb)$BuildingArea, na.rm=T)
    }
    
    if (is.na(data[i, "YearBuilt"])){
      
      suburb <- data[i, "Suburb"]
      
      data[i, "YearBuilt"] <- mean(subset(data, Suburb==suburb)$YearBuilt, na.rm=T)
    }
    
  }
  
  return(data)
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

trees <- function(train, test){
  
  # factors < 32 cats
  model <-tree(Price.cuts~., data=subset(train, select=-c(Price, SellerG, Suburb, Date, Postcode, CouncilArea)), na.action=na.pass)
  
  p <- predict(model, test, type="class")
  
  acc <- sum(p == test$Price.cuts)/length(p)
  
  return(acc)
}



forest.reg <- function(train, test){

  # factors < 53 cats
  model <- randomForest(Price~., data=subset(train, select=-c(Price.cuts, SellerG, Suburb, Date, Postcode)), importance=T, na.action=na.roughfix, ntree=200)
  
  p <- predict(model, test, na.action=na.roughfix)
  
  r.2 <- r2(p, test$Price)
  
  return(r.2)
}


forest.cat <- function(train, test){
  
  # factors < 53 cats
  model <- randomForest(Price.cuts~., data=subset(train, select=-c(Price, SellerG, Suburb, Date, Postcode)), importance=T, na.action=na.roughfix, ntree=200)
  
  p <- predict(model, test, na.action=na.roughfix)
  
  acc <- sum(p == test$Price.cuts, na.rm=T)/length(p)
  
  return(acc)
}


unloadPkg(randomForest)
unloadPkg(tree)