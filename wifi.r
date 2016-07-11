library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)
library(e1071)
library(kknn)
library (caret)
library(cluster)
library(nnet)
library(neuralnet)



set.seed.alpha <- function(x) {
  require("digest")
  hexval <- paste0("0x",digest(x,"crc32"))
  intval <- type.convert(hexval) %% .Machine$integer.max
  set.seed(intval)
}






#UCI DATASET PREPARATION
#setwd("~/Documents/machinelearning_R/datasets")
dataset <- read.csv("trainingData.csv",header = TRUE,sep=",")
#datasetV <- read.csv("validationData.csv",header = TRUE,sep=",")
fdataset<-dplyr::filter(dataset,SPACEID%in%c(110,111))
names(fdataset)[525] <- "idZ"
tidyData <- dplyr::select(fdataset,WAP001:WAP520,idZ)
tidyData$idZ <- as.factor(tidyData$idZ)
#




#GET DATA FROM OUR SERVER
#
#
#
#
#Name of facility
facility <- "lira house"
#user id
user <- 2

#rawData1 <- getMLdata(facility,user)

suppressWarnings( tidyData1 <- prepareData(rawData1) )

#zones id is a factor, not a number!
tidyData1$idZ <- as.factor(tidyData1$idZ)




#
#    SEPARATE DATASET IN TRAIN AND TEST SETS
#    THIS WILL BE DIFFERENT FOR EACH DATASET
#
#
#
#####################################################################################################


#the following code removes entries (columns of the same AP) with 90% of > 100 measures, i.e. the signal was to weak to me measured reliably

bol <- tidyData == 100
discard <- NULL
for (col in  1:ncol(bol)){
 
  if( mean( bol[,col]) > .95){
    discard <- cbind(discard,col)
  }
  
}

#remove all entries from discard list
tidyData <- tidyData[,-discard] 


# Scaling data

preProc  <- caret::preProcess(tidyData)
scaled <- predict(preProc, tidyData)


# Train-test random splitting for machine learning
# 30% for tests and the rest for training


set.seed.alpha("3")
set.seed(989899)

index <- sample(1:nrow(tidyData),round(0.7*nrow(tidyData)))
#Train and test UNESCALED
train <- tidyData[index,]
test <- tidyData[-index,]
#Train and test SCALED
train_s <- scaled[index,]
test_s <- scaled[-index,]


##########################################################################################
#
#
#  Prepare test dataset to be tested
#  We need to make sure that it has the same columns of the train dataset
#  We take out any RSSIs not present in train dataset and complete the test dataset with RSSIs (i.e. columns) present only in train dataset
#
#
attach(train_s)
train_s[idZ==12,]$idZ <- 7
train_s$idZ <- factor(train_s$idZ)
detach(train_s)
attach(test_s)
id12 <- which(test_s$idZ==12)
test_s[id12,]$idZ <- c(7)
test_s$idZ <- factor(test_s$idZ)
detach(test_s)


######################################################################################
#
#
#
#
#CROSS VALIDATION
#
#
#
#
#
#
#
listValues <- NULL
for (i in 7:nrow(train)){
    listValues<- rbind(listValues,invisible(tests(train[1:i,],test)))
}



erroFinal <- rbind(erroFinal,listValues[nrow(listValues),][c(3,5)])


#
#
#
#PLOT ERRORS
#
#
#
#
#
#


x <- seq(1,nrow(listValues[5:nrow(listValues),]),1)
y <- 1:30

#data to be plotted and fited
data <- as.integer(listValues[5:nrow(listValues),2])

#model as n degree polynomial
model <- lm(data~poly(x,3))



plot(data,col="blue",ylab = "Erro percentual",xlab="# de Pontos de Treino")


xx <- seq(0,13, length.out=250)
lines(xx,predict(model,data.frame(x=xx)))
points(data)


View(listValues)