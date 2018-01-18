setwd("~/Documents/R")

#Load raw data
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Add a "Survived" variable to the test set to allow for combinig data sets
test.survived <- data.frame(survived = rep("None",nrow(test)),test[,])

#Swapping columns for consistency
test.survived <- test.survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]
#Combine data sets
data.combined <- rbind(train,test.survived)

#A bit about R data types
str(data.combined) #str = structure, gives info about our datatype

#Changing some values to factors, because it better represents the type of column
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Take a look at gross survival rates
table(data.combined$Survived)

# Distribution across classes (1=upper class, 2 = mid, 3= lower)
table(data.combined$Pclass)

library(ggplot2)

#Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x=Pclass, fill = factor(Survived))) +
  stat_count(width=0.5) + 
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill = "Survived")

#Examine the first few names in the training data set
head(as.character(train$Name)) #head returns some of the ojbects, not all (just for viewing)

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

#Two duplicates, take a closer look
#First, get the duplicate names and store them as a vector

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#Next, take a look at the records in the combined data set
#This goes through all names in data.combined, and if they are in
#dup.names, then extract them (which). the "," goes through every row.
data.combined[which(data.combined$Name %in% dup.names),]

# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

#Any correlation with other variables (eg., sibsp (nr of sibling or spouses))
#Select every name with "Miss." and store in variable misses
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

#Expand upon the relationship between 'Survived' and 'Pclas' by adding the new
#"Title" variable to the data set and explore a potential
#3-dimensional relationship

#Create a utility function to help with title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss",Name)) > 0){
    return("Miss.")
  } else if (length(grep("Master.",Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.",Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.",Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)

#Since we only have survived labels for the train set, only use
#the first 891 rows
ggplot(data.combined[1:891,], aes(x = title,fill=Survived)) +
  stat_count(width = 0.5) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") + 
  xlab("Title") +
  ylab("Total count") + 
  labs(fill="Survived")
#Facetwrap på Pclass är kolumnerna "1,2,3" med vita streck emellan

#The graphs basically show that women and kids were prioritized