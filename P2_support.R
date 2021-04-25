
# use this function to conveniently load libraries and work smoothly with knitting
# can add quietly=T option to the require() function
# the loadPkg function essentially replaced/substituted two functions install.packages() and library() in one step.
loadPkg = function(x) { if (!require(x,character.only=T, quietly =T)) { install.packages(x,dep=T,repos="http://cran.us.r-project.org"); if(!require(x,character.only=T)) stop("Package not found") } }

# unload/detact package when done using it
unloadPkg = function(pkg, character.only = FALSE) { 
  if(!character.only) { pkg <- as.character(substitute(pkg)) } 
  search_item <- paste("package", pkg,sep = ":") 
  while(search_item %in% search()) { detach(search_item, unload = TRUE, character.only = TRUE) } 
}



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
  data.full$Price.cuts[data.full$Price > 7.61e+04 & data.full$Price <= 9.76e+05] <- 1
  data.full$Price.cuts[data.full$Price > 9.76e+05 & data.full$Price <= 1.87e+06] <- 2
  data.full$Price.cuts[data.full$Price > 1.87e+06 & data.full$Price <= 2.76e+06] <- 3
  data.full$Price.cuts[data.full$Price > 2.76e+06 & data.full$Price <= 3.65e+06] <- 4
  data.full$Price.cuts[data.full$Price > 3.65e+06] <- 5
  
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


