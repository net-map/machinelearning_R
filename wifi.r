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
#
#
#
#
#
#setwd("~/Documents/machinelearning_R/datasets")
dataset <- read.csv("trainingData.csv",header = TRUE,sep=",")

datasetV <- read.csv("validationData.csv",header = TRUE,sep=",")


#TESTE 2 zonas
#fdataset<-dplyr::filter(dataset,SPACEID%in%c(110,111))
#TESTE 4 ZONAS
#fdataset<-dplyr::filter(dataset,SPACEID%in%c(226,227,228,229),RELATIVEPOSITION ==1)
#TESTE 6 ZONAS
fdataset<-dplyr::filter(dataset,SPACEID%in%c(101,102,103,104,105,106),RELATIVEPOSITION ==1)
fdatasetV<-dplyr::filter(datasetV,SPACEID%in%c(101,102,103,104,105,106),RELATIVEPOSITION ==1)



names(fdataset)[525] <- "idZ"
tidyData <- dplyr::select(fdataset,WAP001:WAP520,idZ)
tidyData$idZ <- as.factor(tidyData$idZ)





#GET DATA FROM OUR SERVER
#
#
#
#
#Name of facility
facility <- "lira house"
#user id
user <- 2

rawData1 <- getMLdata(facility,user)

suppressWarnings( tidyData <- prepareData(rawData1) )

#zones id is a factor, not a number!
tidyData$idZ <- as.factor(tidyData$idZ)




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
id13<- which(train_s$idZ==12)
train_s[id13,]$idZ <- 7
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



#K-FOLD CROSS-VALIDATION NEURALNET
NNerrorList <- NULL
kNumber <- 10
flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)

#flds[[1]] gets first fold indexes, etc

neuronList <- c(50,80,100,120,150,200,300,350,400,450)

for( i in 1:kNumber){
  NNerrorList <- rbind(NNerrorList,crossValidateNN(scaled[-flds[[i]],],scaled[flds[[i]],],neuronList[i]))
}


plot(neuronList,NNerrorList[,2],pch="Δ",ylab = "Erro de Validação",xlab="# neuronios na HL",main="Cross-Validation 10-Fold para Rede Neural")




#K-FOLD KNN CROSS VALIDATION 
#KNN error list
KNNerrorList<-NULL
kNumber <- 20
flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)

vizinhosList <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)


for (i in 1:kNumber){
  KNNerrorList <- rbind(KNNerrorList,crossValidateKNN(scaled[-flds[[i]],],scaled[flds[[i]],],vizinhosList[i]))
}



plot(vizinhosList,KNNerrorList[,2],pch="Δ",ylab = "Erro de Validação",xlab="K-Value para KNN",main="Cross-Validation 20-Fold para KNN")





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
#PLOT ERROR CURVE OF N-th model of the set
#
#
  plotError <- function(nModel,title){
    
  x <- seq(6,nrow(train_s),2)
  xx <- 1:nrow(listValues)
  #data to be plotted and fited
  data <- listValues[,nModel]
  
  #model as n degree polynomial
  model <- lm(data~poly(xx,3))
  
  #plot scatter and tendence line
  plot(x=x,data,col="red",ylab = "Erro percentual",xlab="# de Pontos de Treino",pch="v",main=title)
  lines(x,predict(model,data.frame(x=x)))
  
  }
  
  

  
plotError(7, "Vote Error")  
plotError(6, "NeuralNet Error")
plotError(4, "SVM Error")
plotError(2, "KNN Error")

backupSK <- listValues


for (i in 1:nrow(test_s))
  print(singleTest(NNmodel,SVM,KNN,dplyr::select(test_s[i,],-idZ)))




