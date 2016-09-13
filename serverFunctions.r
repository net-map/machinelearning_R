library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)
library(e1071)
library(kknn)
library(cluster)
library(caret)
library(nnet)
library(neuralnet)
library(gridExtra)
library(rpart)
library(RWeka)




#prepare UCI dataset, first by findind it in "path"
#then, take only measures specified by floor and building
#justInside is a boolean that specifies if the data points should be the ones measured INSIDE the rooms
prepareUCIdata2 <- function (path,building,floor,zones=NULL,justInside=FALSE){
  
  #building: 0, 1 , 2
  #floor : 0,1,2,3,4
  
  filePath <- file.path(path,"trainingData.csv")
  dataset <- read.csv(filePath,header = TRUE,sep=",")
  
  if(is.null(zones) && justInside==FALSE ){  fdataset<-dplyr::filter(dataset,FLOOR==floor, BUILDINGID == building)}
  else  if  (!is.null(zones) && justInside==FALSE)  {  fdataset<-dplyr::filter(dataset,FLOOR==floor, BUILDINGID == building,SPACEID%in%zones) }
  else if (!is.null(zones) && justInside==TRUE){fdataset<-dplyr::filter(dataset,FLOOR==floor, BUILDINGID == building,SPACEID%in%zones,RELATIVEPOSITION ==1)}
  else if (is.null(zones) && justInside==TRUE){fdataset<-dplyr::filter(dataset,FLOOR==floor, BUILDINGID == building,RELATIVEPOSITION ==1)}
  
  
  names(fdataset)[525] <- "idZ"
  tidyData <- dplyr::select(fdataset,WAP001:WAP520,idZ)
  tidyData$idZ <- as.factor(tidyData$idZ)
  
  
  #Eliminate useless features
  bol <- tidyData == 100
  
  tidyData[bol] = -120
  
  discard <- NULL
  for (col in  1:ncol(bol)){
    if( mean( bol[,col]) == 1){
      discard <- cbind(discard,col)
    }
    
  }
  
  #remove all entries from discard list
  tidyData <- tidyData[,-discard] 
  
  
  # Scaling data
  
  idZ <- tidyData$idZ
  
  
  tidyData <- dplyr::select(tidyData,-idZ) + 120
  
  tidyData <- cbind(idZ,tidyData)
  
  
  #PCA .95 threshold
  PCA  <- caret::preProcess(dplyr::select(tidyData,-idZ),method=c("center","scale","pca"))
  #save PCA parameters for future conversion
  assign("PCA",PCA,.GlobalEnv)
  
  saveRDS(PCA,"pca.rds")
  
  #project data into PCA space
  scaledPCA <- predict(PCA, dplyr::select(tidyData,-idZ))
  
  #IDZ IS NOW ON INDEX 1! REMEMBER THAT FOR GOD'S SAKE!
  scaledPCA <- cbind(idZ,scaledPCA)
  
  #NON PCA SCALING
  preProc  <- caret::preProcess(tidyData)
  #saveRDS(preProc,"trainedModels/scale.rds")
  scaled <- predict(preProc, tidyData)
  assign("preProc",preProc,.GlobalEnv)
  
  
  #IDZ IS NOW ON INDEX 1! REMEMBER THAT FOR GOD'S SAKE!
  scaled <- cbind(idZ,dplyr::select(scaled,-idZ))
  
  assign("scaled",scaled,.GlobalEnv)
  
  
  
  #set.seed(31415)
  
  
  #TRAIN AND TEST SET SPLIT
  
  index <- sample(1:nrow(tidyData),round(0.8*nrow(tidyData)))
  #Train and test UNESCALED
  train <- tidyData[index,]
  test <- tidyData[-index,]
  assign("train",train,.GlobalEnv)
  assign("test",test,.GlobalEnv)
  
  
  
  #Train and test SCALED
  train_s <- scaled[index,]
  test_s <- scaled[-index,]
  
  assign("train_s",train_s,.GlobalEnv)
  assign("test_s",test_s,.GlobalEnv)
  
  
  #Train and test in PCA space
  train_pca <- scaledPCA[index,]
  test_pca <- scaledPCA[-index,]
  
  assign("train_pca",train_pca,.GlobalEnv)
  assign("test_pca",test_pca,.GlobalEnv)
  
  
  
  dataList <- list("train" = train, "train_s" = train_s,"train_pca" = train_pca,"test" =test, "test_s" = test_s,"test_pca" = test_pca)
  
  return(dataList)
  
  
  
}





#prepare UCI dataset, first by findind it in "path"
#take from data entries with ids in list listZones
prepareUCIdata <- function (path,listZones){
  
  filePath <- file.path(path,"trainingData.csv")
  
  dataset <- read.csv(filePath,header = TRUE,sep=",")
  

  
  #TESTE 2 zonas
  #fdataset<-dplyr::filter(dataset,SPACEID%in%c(110,111))
  #TESTE 4 ZONAS
  #fdataset<-dplyr::filter(dataset,SPACEID%in%c(116,117,118,119,120,121,122,123,124),RELATIVEPOSITION ==1)
  #TESTE 6 ZONAS
  #fdataset<-dplyr::filter(dataset,SPACEID%in%c(101,102,103,104,105,106),RELATIVEPOSITION ==1)
  
  #testeGenerico
  fdataset<-dplyr::filter(dataset,SPACEID%in%listZones,RELATIVEPOSITION ==1)
  
  
   
  names(fdataset)[525] <- "idZ"
  tidyData <- dplyr::select(fdataset,WAP001:WAP520,idZ)
  tidyData$idZ <- as.factor(tidyData$idZ)
  
  
  
  
  #Eliminate useless features
  bol <- tidyData == 100
  
  tidyData[bol] = -120
  
  discard <- NULL
  for (col in  1:ncol(bol)){
    if( mean( bol[,col]) == 1){
      discard <- cbind(discard,col)
    }
    
  }
  
  #remove all entries from discard list
  tidyData <- tidyData[,-discard] 
  
  
  # Scaling data
  
  idZ <- tidyData$idZ
  
  
  tidyData <- dplyr::select(tidyData,-idZ) + 120
  
  tidyData <- cbind(idZ,tidyData)
  
  
  #PCA .95 threshold
  PCA  <- caret::preProcess(dplyr::select(tidyData,-idZ),method=c("center","scale","pca"))
  #save PCA parameters for future conversion
  assign("PCA",PCA,.GlobalEnv)
  
  saveRDS(PCA,"pca.rds")
  
  #project data into PCA space
  scaledPCA <- predict(PCA, dplyr::select(tidyData,-idZ))
  
  #IDZ IS NOW ON INDEX 1! REMEMBER THAT FOR GOD'S SAKE!
  scaledPCA <- cbind(idZ,scaledPCA)
  
  #NON PCA SCALING
  preProc  <- caret::preProcess(tidyData)
  assign("scale",preProc,.GlobalEnv)
  #saveRDS(preProc,"trainedModels/scale.rds")
  scaled <- predict(preProc, tidyData)
  
  
  #IDZ IS NOW ON INDEX 1! REMEMBER THAT FOR GOD'S SAKE!
  scaled <- cbind(idZ,dplyr::select(scaled,-idZ))
  
  assign("scaled",scaled,.GlobalEnv)
  
  
  
  #set.seed(31415)
  
  
  #TRAIN AND TEST SET SPLIT
  
  index <- sample(1:nrow(tidyData),round(0.8*nrow(tidyData)))
  #Train and test UNESCALED
  train <- tidyData[index,]
  test <- tidyData[-index,]
  assign("train",train,.GlobalEnv)
  assign("test",test,.GlobalEnv)
  
  
  
  #Train and test SCALED
  train_s <- scaled[index,]
  test_s <- scaled[-index,]
  
  assign("train_s",train_s,.GlobalEnv)
  assign("test_s",test_s,.GlobalEnv)
  
  
  #Train and test in PCA space
  train_pca <- scaledPCA[index,]
  test_pca <- scaledPCA[-index,]
  
  assign("train_pca",train_pca,.GlobalEnv)
  assign("test_pca",test_pca,.GlobalEnv)
  
  
  
  dataList <- list("train" = train, "train_s" = train_s,"train_pca" = train_pca,"test" =test, "test_s" = test_s,"test_pca" = test_pca)
  
  return(dataList)
  
}



#
#
#TRAIN MODELS WITH TRAIN SET IN FORMAT SPECIFIED IN ANOTHER FUNCTIONS
#
#
#
trainModels <- function(train,trainPCA,test){
  
  #DECISION TREE

  
  tree <- J48(idZ~.,data=train)
  
  
  #serialize java object object
  rJava::.jcache(tree$classifier)
   
  
  #tree <- rpart(idZ~.,data=train,method="class")
  #prune to avoid overfitting
  #tree <-   prune(tree,tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  
  #assign("Tree",tree,.GlobalEnv)
  #saveRDS(tree,"tree.rds")
  
  
  
  #NEURALNETWORK
  
  #transforms factors in binary dummy vectors
  #ASSUMING IDZ IS IN COLUMN 1!!!!!!!!!
  nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train[,1]))
  
  addq <- function(x) paste0("`", x, "`")
  #adds `x` to every name in data
  names(nnData) <- addq(names(nnData))
  
  n <- names(nnData)
  #gets indexes of dummy id columns 
  indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)
  
  lhseq <- paste(names(nnData[,indexId]),collapse="+")
  
  rhseq <- paste(names(nnData[,-indexId]),collapse="+")
  
  #creates formula
  f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))
  
  #for some reason, remove quotes and it works
  nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train[,1]))
  
  #TRAIN neuralnet!
  
  neuron <- 210
  
  nn <- neuralnet::neuralnet(f,data=nnData,hidden=c(neuron),linear.output=FALSE) 
  #assign("NeuralNet",nn,.GlobalEnv)
  #saveRDS(nn,"NeuralNet.rds")
  
  #SUPPORT VECTOR MACHINE
  
  #We must separate data into X matrix for the features and Y for the response vector with the classes
  #suppressWarnings(attach(train_s))
  #detach(train_s)
  #xi<- dplyr::select(train,-idZ)
  #yi <- train$idZ
  
  
  kernelType <- "radial" 
  #mylogit1 <-svm(x=xi,y=yi,kernel = kernelType,scale=FALSE,probability = TRUE)
  
  TESTE <- train
  assign("TESTE",train,.GlobalEnv)
  
  mylogit1 <- svm(idZ~.,data=train,probability=TRUE,scale=FALSE)
  assign("SVM",mylogit1,.GlobalEnv)
  #saveRDS(mylogit1,"SVM.rds")
  
  
  
  #
  #
  #K NEAREST NEIGHBOURS
  #
  #
  
  #knnTrain<-train.kknn(idZ ~. , kmax=3,scale=FALSE,kernel = c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal"),distance=1, data=train)
  #knnTrain<-kknn(formula = idZ ~. , k=3,scale=FALSE,kernel = "optimal",distance=1, train=train,test=test)
  
  #assign("KNN",knnTrain,.GlobalEnv)
  #saveRDS(knnTrain,"KNN.rds")
  
  
  
  modelList <- list("NeuralNet" = nn,"SVM" = mylogit1,"Tree" = tree)
  
  return (modelList)

}


#Use trained models to provide a single classification answer from testVector
#REMOVE ZONE ID FROM VECTOR
#
#WEIGHTED BAYESIAN VOTE
#
#
#RSSID1 RSSID2 RSSID3 ... 
# -30     -39    -29         
#
#
#
singleTestBayesianVote <- function (testVector,NNmodel,SVMmodel,TreeModel,train){
  
  #TO DO
  
}

#Use trained models to provide a list of classification answers from test matrix
#REMOVE ZONE ID FROM VECTOR
#
#WEIGHTED BAYESIAN VOTE
#
#
#RSSID1 RSSID2 RSSID3 ... 
# -30     -39    -29         
#
#
#
MatrixTestBayesianVote <- function (test,NNmodel,SVMmodel,TreeModel,train){
  
  
  
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  
  
  
  #NEURALNET PREDICTION
  

  nnProb<-neuralnet::compute(NNmodel,test)$net.result
  
  nnPrediction <-apply(nnProb,1,function(x) which.max(x))
  
  
  #get idz computed
  idZNN <- as.numeric(as.character(factors[nnPrediction]))
  
  
  #SVM PREDICTION
  #svmPrediction <- as.numeric(predict(SVMmodel,test))
  

  svmProb <- attr(predict(SVMmodel,test,probability=TRUE),"probabilities")  
  

  #get idz computed
  #idZSVM <- as.numeric(as.character(factors[svmPrediction]))
  
  
  #KNN PREDICTION
  knnTrain<-kknn(formula=idZ ~. , k=7,distance=1, train=train,test=test,kernel="optimal")
  knnProb <- knnTrain$prob
  knnPrediction <- as.numeric(as.character(knnTrain$fitted.values))
  knnPrediction <- factors[knnPrediction]
  idZKNN <- knnPrediction
  
  #DECISION TREE PREDICTION
  predictionTree <- predict(TreeModel,test,type="probability")
  
  idZtree <-  as.numeric(as.character(factors[apply (predictionTree,1,function(x) which.max(x))]))
  treeProb <-  predictionTree

  #+treeProb

  
  sumProb <-  knnProb + nnProb +svmProb +treeProb
  #get class with maximum summed probability
  idZWeightedProb <-  as.numeric(as.character(factors[apply (sumProb,1,function(x) which.max(x))]))
  
 
  assign("probs",sumProb,.GlobalEnv)

  #finally return calculated idZ
  return (idZWeightedProb)
 
}


#Use trained models to provide a single classification answer from testVector
#REMOVE ZONE ID FROM VECTOR
#
#SIMPLE VOTE
#
#
#RSSID1 RSSID2 RSSID3 ... 
# -30     -39    -29         
#
#
#
singleTest <- function (testVector,NNmodel,SVMmodel,KNNmodel,Treemodel){
  
 
  
  #get names of columns used to train classifiers
  names <- colnames(SVMmodel$SV)
  
  
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  
  
  #creates dummy vector with BSSIDs used to train the classifier
  dummyVector <- t(as.data.frame(x=rep(-120,length(names)),names))
  
  #merge testVector with dummyVector in a way that if there is a BSSID missing in the testVector, it is created with -120
  commomNames <- intersect(names,names(testVector))
  #get values that are present in testVector
  testVector <- merge(dummyVector,testVector,all.y=TRUE)
  
  testVector[is.na(testVector)] <- -120
  
 
  
  #compute projection of test vector into PCA data used by the Neural Network
  #NOT USING THIS RIGHT NOW
  #PCAvector <- predict(PCA,testVector)
  
  
  
  #NEURALNET PREDICTION
  nnPrediction <-apply(neuralnet::compute(NNmodel,testVector)$net.result,1,function(x) which.max(x))
  #get idz computed
  idZNN <- factors[nnPrediction]
  
  
  #SVM PREDICTION
  svmPrediction <- as.numeric(predict(SVMmodel,testVector))
  
  #get idz computed
  idZSVM <- factors[svmPrediction]
  
  
  #KNN PREDICTION
  print(testVector)
  knnPrediction <- as.numeric(predict(KNNmodel,testVector))
  
  #get idz computed
  idZKNN <- factors[knnPrediction]
  
  #DECISION TREE PREDICTION
  predictionTree <- predict(Treemodel,testVector)
  
  idZtree <-  factors[apply (predictionTree,1,function(x) which.max(x))]
  
  
  
  
  
  
  
  results <- cbind(idZKNN,idZSVM,idZNN,idZtree)
  
  
  #get most recurring result
  idZVote <-  as.numeric(names(sort(table(results),decreasing = TRUE)[1])) 
  
  #finally return calculated idZ
  return (idZVote)
  
}

singleTest2 <- function (testVector,trainset,scaleModel,NNmodel,SVMmodel,Treemodel){
  
  
  
  #get names of columns used to train classifiers
  names <- colnames(SVMmodel$SV)
  
  
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  
  
  #creates dummy vector with BSSIDs used to train the classifier
  dummyVector <- t(as.data.frame(x=rep(-120,length(names)),names))
  
  #merge testVector with dummyVector in a way that if there is a BSSID missing in the testVector, it is created with -120
  commomNames <- intersect(names,names(testVector))
  #get values that are present in testVector
  testVector <- merge(dummyVector,testVector,all.y=TRUE)
  
  testVector[is.na(testVector)] <- -120
  
 
  #scale new data!
  testVector <- predict(scaleModel,testVector)
  

  #compute projection of test vector into PCA data used by the Neural Network
  #NOT USING THIS RIGHT NOW
  #PCAvector <- predict(PCA,testVector)
  
  
  
  #NEURALNET PREDICTION
  nnProb <- neuralnet::compute(NNmodel,testVector)$net.result
  nnPrediction <-apply(nnProb,1,function(x) which.max(x))
  #get idz computed
  idZNN <- factors[nnPrediction]
  
  
  #SVM PREDICTION
  svmPrediction <- as.numeric(predict(SVMmodel,testVector))
  
  svmProb <- attr(predict(SVMmodel,testVector,probability=TRUE),"probabilities")  
  
  
  #get idz computed
  idZSVM <- factors[svmPrediction]
  
  
  #KNN PREDICTION
  knnTrain<-kknn(formula=idZ ~. , k=7,distance=1, train=train,test=testVector,kernel="optimal")
  knnProb <- knnTrain$prob
  #get idz computed
  idZKNN <- as.numeric(as.character(knnTrain$fitted.values))
   
  
  #DECISION TREE PREDICTION
  predictionTree <- predict(Treemodel,testVector,type="probability")
  
  treeProb <- predictionTree
  
  idZtree <-  as.numeric(as.character(factors[apply (predictionTree,1,function(x) which.max(x))]))
  
  
  
  
  
  
  results <- cbind(idZKNN,idZSVM,idZNN,idZtree)
  
  bayesianSum <- knnProb+treeProb+svmProb+nnProb
  
  #get factor with more summed probability
  idZBayas <- as.numeric(as.character(factors[apply (bayesianSum,1,function(x) which.max(x))]))
  
  #get most recurring result
  idZVote <-  as.numeric(names(sort(table(results),decreasing = TRUE)[1])) 

  
  
  #finally return calculated idZ
  return (idZVote)
  
}






















#
#
#
#performs a voting test on a MATRIX without the ZoneID
singleTestMatrix <- function (test,NNmodel,SVMmodel,Treemodel,train){
  
  
  
 
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  
  
  
  #NEURALNET PREDICTION
  nnPrediction <-apply(neuralnet::compute(NNmodel,test)$net.result,1,function(x) which.max(x))
  nnProb<-apply(neuralnet::compute(NNmodel,test)$net.result,1,function(x) max(x))
  
  
  #get idz computed
  idZNN <- as.numeric(as.character(factors[nnPrediction]))
  
  
  #SVM PREDICTION
  svmPrediction <- as.numeric(predict(SVMmodel,test))
  
  #get idz computed
  idZSVM <- as.numeric(as.character(factors[svmPrediction]))
  
  
  #KNN PREDICTION
  knnTrain<-kknn(formula=idZ ~. , k=7,distance=1, train=train,test=test,kernel="optimal")
  knnPrediction <- as.numeric(as.character(knnTrain$fitted.values))
  
  
  #get idz computed
  idZKNN <- knnPrediction
  
  #DECISION TREE PREDICTION
  predictionTree <- predict(Treemodel,test)
  
  #idZtree <-  as.numeric(as.character(factors[apply (predictionTree,1,function(x) which.max(x))]))
  idZtree <- as.numeric(as.character(factors[predictionTree]))
  
  
  
  
  
  
  results <- cbind(idZKNN,idZSVM,idZNN,idZtree)
  
  assign("results",results,.GlobalEnv)
  #get most recurring result
  idZVote <- apply(results,1,function (x) as.numeric(names(sort(table(x),decreasing = TRUE)[1])))
  #idZVote <-  as.numeric(names(sort(table(results),decreasing = TRUE)[1])) 
  
  #finally return calculated idZ
  return (idZVote)
  
}







singleTestNN <- function (test,NNmodel,SVMmodel,Treemodel){

  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)

  
  #NEURALNET PREDICTION
  nnPrediction <-apply(neuralnet::compute(NNmodel,test)$net.result,1,function(x) which.max(x))
  
  #get idz computed
  idZNN <- factors[nnPrediction]
  
  return (as.numeric(as.character(idZNN)))
  
}


singleTestSVM <- function (test,NNmodel,SVMmodel,Treemodel){
  
  
  
  
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  
  
  #SVM PREDICTION
  svmPrediction <- as.numeric(predict(SVMmodel,test))
  
  #get idz computed
  idZSVM <- factors[svmPrediction]
  
  return(as.numeric(as.character(idZSVM)))
   
}



singleTestTree <- function (test,NNmodel,SVMmodel,Treemodel){

  
  
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  
 
  #DECISION TREE PREDICTION
  predictionTree <- predict(Treemodel,test)
  
  #idZtree <-  factors[apply (predictionTree,1,function(x) which.max(x))]
  
  return(as.numeric(as.character(factors[predictionTree])))
}



singleTestKNN <- function (test,NNmodel,train){
  

  
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  

  
  

  #KNN PREDICTION
  knnTrain<-kknn(formula=idZ ~. , k=7,distance=1, train=train,test=test,kernel="optimal")
  knnPrediction <- as.numeric(as.character(knnTrain$fitted.values))

  
 
  return(knnPrediction)
  
}



crossValidateKNN <- function (trainingSet,validationSet,k,dist){
  
  knnTrain<-kknn(formula=idZ ~. , k=k,distance=dist, train=trainingSet,test=validationSet,kernel="optimal")
  
  
  
  #trainRes <-apply(neuralnet::compute(nn,nnData[,-indexId])$net.result,1,function(x) which.max(x))
  
  
  testError <- 100*(1-mean(validationSet$idZ == knnTrain$fitted.values))
  
  
  knnTrain
  
  return (testError)
  
  
}




crossValidateSVM <- function (trainingSet,validationSet,kernelType){
  
  #SUPPORT VECTOR MACHINE
  
  #We must separate data into X matrix for the features and Y for the response vector with the classes
  #suppressWarnings(attach(train_s))
  #detach(train_s)
  xi<- subset(trainingSet,select= - idZ)
  yi <- trainingSet$idZ
  mylogit <-svm(xi,yi,kernel = kernelType)
  
  #print("Com SVM, com os dados de treino, temos erro de:")
  trainError <- 100*(1-mean(trainingSet$idZ == predict(mylogit)))
  
  #print("Com SVM, com os dados de teste, temos erro de:")
  testError <- 100*(1-mean(validationSet$idZ == predict(mylogit,dplyr::select(validationSet,-idZ))))
  
  
  return(c(trainError,testError))
}



crossValidateNN <- function (trainset,validateset,neuron){
  
  
  idZ <- trainset$idZ
  idZtest <- validateset$idZ
  
  
  #transforms factors in binary dummy vectors
  #ASSUMING IDZ IS IN COLUMN 1
  nnData <- cbind(dplyr::select(trainset,-idZ),nnet::class.ind(trainset[,1]))
  nnDatatest <- cbind(dplyr::select(validateset,-idZ),nnet::class.ind(validateset[,1]))
  
  addq <- function(x) paste0("`", x, "`")
  #adds `x` to every name in data
  names(nnData) <- addq(names(nnData))
  names(nnDatatest) <- addq(names(nnDatatest))
  
  n <- names(nnData)
  nTest <- names(nnDatatest)
  #gets indexes of dummy id columns 
  indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)
  indexIdTest <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",nTest)
  
  lhseq <- paste(names(nnData[,indexId]),collapse="+")
  
  rhseq <- paste(names(nnData[,-indexId]),collapse="+")
  
  #creates formula
  f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))
  
  #for some reason, remove quotes and it works
  nnData <- cbind(dplyr::select(trainset,-idZ),nnet::class.ind(trainset[,1]))
  nnDatatest <- cbind(dplyr::select(validateset,-idZ),nnet::class.ind(validateset[,1]))
  
  #TRAIN neuralnet!
  nnErrorList <- NULL
  
  
  
  nn <- neuralnet::neuralnet(f,data=nnData,hidden=neuron,linear.output=FALSE)
  
  
  trainRes <-apply(neuralnet::compute(nn,nnData[,-indexId])$net.result,1,function(x) which.max(x))
  testRes <-apply(neuralnet::compute(nn,nnDatatest[,-indexIdTest])$net.result,1,function(x) which.max(x))
  
  
  #remount idZ for train and test datasets
  #idZtest <- factor(apply(nnDatatest[,indexIdTest], 1, function(x) which(x == 1)), labels= colnames(nnDatatest[,indexIdTest])) 
  #idZ <- factor(apply(nnData[,indexId], 1, function(x) which(x == 1)), labels = colnames(nnData[,indexId])) 
  
  #create response factors from computed data
  
  factors <- nn$model.list$response
  factors <- gsub("`",'',factors)
  
  
  
  idZtestres <- factors[testRes]
  idZres <- factors[trainRes]
  
  
  
  resultsNN <- idZtestres
  
  
  trainError <- 100*(1-mean(idZ == idZres))
  testError <- 100*(1-mean(idZtest == idZtestres))
  
  print (c("O erro com ",neuron,"neuronios na HL teve resultado de ",testError))
  
  return (c(trainError,testError))
  
  
  
  
}







crossValidateTree <- function (trainset,testset){
  
  #Decision Tree
  
  idz <- levels(trainset$idZ)
  
  tree <- rpart(idZ~.,data=trainset,method="class")
  
  #prune to avoid overfitting
  tree <-   prune(tree,tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

  
  predictionTree <- predict(tree,dplyr::select(testset,-idZ))
  
  factorNumber <- apply (predictionTree,1,function(x) which.max(x))
  
  errorTree <- idz[factorNumber] == testset$idZ
  
  
  return(1-mean(errorTree))
  
  
  
  
  
}


#x is a list 
montaLista<- function(x,zoneID,acquiID){
  return (list(BSSID=x[1],RSSI=x[2],idZ=zoneID,acquiID=acquiID))
}
