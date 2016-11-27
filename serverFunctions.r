

library(rmongodb)
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


#
#Function to get a acquisition set from a localization test in mongo, which is stored there by some cellphone using the system
#
#
#
#Args:
#     queueID: ID from objet with RSSIs and BSSIDs stored by the cellphone on mongo
#
#Output:
#     list with measure in the format specified by the SingleTest funcion
#
#
#
getMeasureFromMongo <- function(queueID){
  
  mongo <- mongo.create(host="localhost:27017",username="netmap",password = "brocoliéumvegetal")
  
  if(mongo.is.connected(mongo) == TRUE){
    
    db <- "server_api_production"
    
    queue <- paste(db,"queued_classifications",sep = ".")
    
    #retrive queue that match ID
    results<-mongo.find.one(mongo,queue,query=list('_id'=mongo.oid.from.string(queueID)))
    
    #transform in to an R list
    results <- mongo.bson.to.list(results)
    
    dataVector <- results$access_points
    
    
    BSSIDlist <- unlist(lapply(dataVector,function(x) return( x$BSSID )))
    
    RSSIlist <-  unlist(lapply(dataVector,function(x) return( x$RSSI )))
    
    
    #Reshape measure to the format to be worked by the single test function
    transposedData <- matrix(nrow=1,ncol=length(BSSIDlist))
    
    transposedData <- data.frame(transposedData)
    
    
    names(transposedData) <- BSSIDlist
    
    transposedData[1,] <- RSSIlist
    
    return(transposedData)
    
  } else{
    
    print("Could not connect to mongo!")
    
    return("ERROR")
}

  
  
  
}



#
#Function to to be called locally to test our trained models with data
#
#
#
#Args:
#     facilityName: Name of facility to be tested
#
#Output:
#     Error Rate 
#
#
#
#
#
testRealModels<-function(facilityName){
  
  
  mongo <- mongo.create(host="52.67.171.39:27017",username="netmap",password = "brocoliéumvegetal")
  
  if(mongo.is.connected(mongo) == TRUE){
  
    db <- "server_api_production"
    
    
    facilities <- paste(db,"facilities",sep = ".")
    
    #retrive list of facilities that match query and get 1st element which is the ID
    results<-mongo.find.all(mongo,facilities,list(name=facilityName))
    
    if(length(results)==0){
      print("Could not find that facility name")
      return()
    }else{
      facilityID <- results[[1]][[1]]
    }
    
    
    pathData <- paste("prepared-data/",facilityID,".rds",sep="")
    
  
    
    
    
    #get datasets so we can use the train set in the KNN prediction
    data <- readRDS(pathData)
    
    
    idZ <- data$idZ
    
    data <- dplyr::select(data,-idZ)
    
    
    #NON PCA SCALING
    preProc  <- caret::preProcess(data)
    
    scaled <- predict(preProc, data)
    
    scaled <- cbind(idZ,scaled)
  
    
    
    #get possible idZs
    factors <- levels(scaled$idZ)
    
    #names of features used for training
    names <- names(data)[-1]
    
    
    #split train and test sets
    index <- sample(1:nrow(scaled),round(0.8*nrow(scaled)))
    
    
    
    #Train and test SCALED
    train_s <- scaled[index,]
    test_s <- scaled[-index,]
    
    #train SMO
    SMOmodel <- SMO(idZ~.,data=train_s)
    
    #train Adaboost + tree
    treeAdamodel <- AdaBoostM1(idZ~. , data = train_s ,control = Weka_control(W = list(J48, M=5)))
    
    
    #KNN TRAIN
    knnModel<-kknn(formula=idZ ~. , k=4,distance=1, train=train_s,test=test_s,kernel="optimal")
    
    
    
    
    listaModelos <- list("SMO"=SMOmodel,"KNN"=knnModel,"treeAda"=treeAdamodel)
    
    #initialize summed support vector
    sumProb <- vector(mode="numeric",length = length(factors))
    algoRates <- NULL
    for (model in listaModelos){
      
      temp <- predictionWrapper(model,test_s,probabilities=TRUE)
      
      
      resultsIDZt <- factors[apply (temp,1,function(x) which.max(x))]
      
      rateSuccesst <- 100*mean(resultsIDZt==test_s$idZ)
      
      algoRates <- cbind(algoRates,rateSuccesst)
      #WEIGHTED VOTING RULE
      sumProb <- sumProb + temp
      
    }
    
 
    resultsIDZ <- factors[apply (sumProb,1,function(x) which.max(x))]
    
    rateSuccess <- 100*mean(resultsIDZ==test_s$idZ)
    
    algoRates <- cbind(algoRates,rateSuccess)
    
    par(mar=c(5,8,4,2)) # increase y-axis margin.
    
    par(las=2) # make label text perpendicular to axis
    barplot(algoRates,main = "Taxa de acerto de teste do sistema e algoritmos",horiz = TRUE,names.arg=c("SMO", "KNN", "AdaBoost","net.map"))
    
    return(rateSuccess)
    
    
  }else{
    return("error: Could not connect to mongo")
  }
  
}



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
  
  
  zonas <-  unique(fdataset$SPACEID)
  assign("zonas",zonas,.GlobalEnv)
  
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





#
#REMOVE ZONE ID FROM VECTOR
#
#WEIGHTED  VOTE
#
#TEST VECTOR FORMAT:
#RSSID1 RSSID2 RSSID3 ... 
# -30     -39    -29         
#
#
#

#
#Use trained models to provide a single classification answer from testVector
#
#
#
#Args:
#     testVector: The format must be as specified below:
#     
#                     RSSID1 RSSID2 RSSID3 ... 
#                       -30     -39    -29         
#
#     train:      dataset that was used to train the models
#
#     modelsList:     List with the trained models
#Output:
#     predicted zone ID 
#
#
#
#
#


prediction.from.models <- function(testVector,train,modelsList){
  
  #get possible idZs
  factors <- levels(train$idZ)
  
  #names of features used for training
  names <- names(train)[-1]
  
  
  
  #creates dummy vector with BSSIDs used to train the classifier
  dummyVector <- t(as.data.frame(x=rep(NA,length(names)),names))
  
  #merge testVector with dummyVector in a way that if there is a BSSID missing in the testVector, it is created with -120
  #commonNames <- intersect(names,names(testVector))
  #diff <- outersect(commonNames,names(testVector))
  #newAPs <- !(names(testVector) %in% commonNames)
  #get values that are present in testVector
  #print("Dummy Vector:")
  #print(dummyVector)
  #print("Test Vector:")
  #print(testVector)
  #print("-------------")

  mergedVector <- merge(dummyVector,testVector,all.y=TRUE)
  
  mergedVector[is.na(mergedVector)] <- -120
  
  
  #scale new data!
  scaledVector <- predict(modelsList$preProc,mergedVector)
  
  print(setdiff(names(scaledVector),names(train)))
  
  print("-------------")
  print(scaledVector)
  print("-------------")
  #KNN TRAIN
  knnModel<-kknn(formula=idZ ~. , k=4,distance=1, train=train,test=scaledVector,kernel="optimal")
  
  
  
  
  listaModelos <- list("SMO"=modelsList$SMO,"KNN"=knnModel,"treeAda"=modelsList$Tree)
  
  #initialize summed support vector
  sumProb <- vector(mode="numeric",length = length(factors))
  
  for (model in listaModelos){
    
    temp <- predictionWrapper(model,scaledVector,probabilities=TRUE)
    
    #WEIGHTED VOTING RULE
    sumProb <- sumProb + temp
    
  }
  
  #results
  print(which.max(sumProb))
  print(factors[which.max(sumProb)])
  confValue <- max(sumProb)
  idZBayas <- factors[which.max(sumProb)]
  
  
  
  
  print(idZBayas)
  #return output zone_id
  
  if(!is.na(idZBayas)){
    zoneName <- aws.getNamefromID(idZBayas)
    output <- list("ZonaName"=zoneName,"Confidence"=confValue)
    jsonOut <- jsonlite::toJSON(output)
    return(jsonOut)
  }
  
  return (idZBayas)
  
  
  
}

#
# Wrapper function to unify call to predition functions from the models used
#
#
#
#Args:
#         model:   model object in which the prediction is being made
#         test:    dataframe in which the test is going to be made
#                       can be just a single line or multiple ones  
#
#         probabilities:  boolean to specify if the output is the class probabilities (TRUE) or
#                         just the class names (FALSE)
#
#
#
#Output:
#         Prediction in the form of probabilities or class name
#
predictionWrapper<- function(model,test,probabilities=TRUE,...) {

  
if(grepl("SMO",model$call) || grepl("Ada",model$call) ||  grepl("J48",model$call)){
  
  if(!probabilities){
    pred <- predict(model,test)
  }
  else{
  
    pred <- predict(model,test,type="probability")  
    
  }
    
} else if(grepl("neuralnet",model$call,fixed = TRUE)){ 
  
  
  factors<- model$model.list$response
  factors <- gsub("`",'',factors)
  
  
  nnProb<-neuralnet::compute(model,test)$net.result
  
  if(!probabilities){
      
      nnPrediction <-apply(nnProb,1,function(x) which.max(x))
      pred <- as.factor(as.numeric(as.character(factors[nnPrediction])))
      levels(pred)<-factors
  }
  else{
    pred<-nnProb
  }

  
} else if(grepl("kknn",model$call,fixed = TRUE)){
  
  if(!probabilities){
    pred <- model$fitted.values
  }else{
    pred <- model$prob
    
  }
  
} else if(grepl("svm",model$call,fixed=TRUE)){
  
  if(!probabilities){
    pred <- predict(model,test)
  }else{
  
    pred <- attr(predict(model,test,probability=TRUE),"probabilities")  
  
  }
  
}

  
  
  
  
  
  
 return(pred) 
  
}


#Get ID from a facility name in mongo
#
#Input: facility name
#
#
#Output: facility ID
#
#
aws.getIDfromName <- function(name){
  
  print("Trying to connect to mongo...")
  
  mongo <- mongo.create(host="localhost:27017",username="netmap",password = "brocoliéumvegetal")
  
  if (mongo.is.connected(mongo) == TRUE) {
    
    print("Succesfully connected to mongo")
    
    db <- "server_api_production"
    
    
    facilities <- paste(db,"facilities",sep = ".")
    
    #retrive list of facilities that match query and get 1st element which is the ID
    facilityID<-mongo.find.all(mongo,facilities,list(name=name))[[1]][[1]]
  
    return (facilityID)
  }
  else{
    
    print("Name not found on db!")
    
    return()
  }
}

#Get ID from a facility name in mongo
#
#Input: zone ID
#
#
#Output: zone name
#
#
aws.getNamefromID <- function(zoneID){
  
  print("Trying to connect to mongo...")
  
  mongo <- mongo.create(host="52.67.171.39:27017",username="netmap",password = "brocoliéumvegetal")
  
  if (mongo.is.connected(mongo) == TRUE) {
    
    print("Succesfully connected to mongo")
    
    db <- "server_api_production"
    
    
    zones <- paste(db,"zones",sep = ".")
    
    
    #retrive all zones from that facility
    zoneName <- mongo.find.all(mongo,zones,list('_id'=mongo.oid.from.string(zoneID)))[[1]][[2]]
    
    
    
    
    return (zoneName)
  }
  else{
    
    print("Name not found on db!")
    
    return()
  }
}












#
#
#Input: facilityID
#
#
#Output: Get dataset from mongo and prepare it for training
#
#
#* @get /prepare
aws.PrepareData <- function (facilityID){
  #ec2-52-67-171-39.sa-east-1.compute.amazonaws.com
  #aws.PrepareData("580264eabde5c6211d18821b")
  print("Trying to connect to mongo...")
  
  mongo <- mongo.create(host="localhost:27017",username="netmap",password = "brocoliéumvegetal")
  
  if (mongo.is.connected(mongo) == TRUE) {
    
    print("Succesfully connected to mongo")
    
    db <- "server_api_production"
    
    zones <- paste(db,"zones",sep = ".")
    
    
    #retrive all zones from that facility
    listZones <- mongo.find.all(mongo,zones,list(facility_id=facilityID))
    
    rawData <- NULL
    
    #for each zone found
    for (z in 1:length(listZones)){
      
      #get ID of that zone from list of lists
      zoneID <- listZones[[z]][[1]]
      
      acquisition <- paste(db,"acquisitions",sep = ".")
      
      listAcquisitions <- mongo.find.all(mongo,acquisition,list(zone_id=zoneID))
      
      lista <- NULL
      
      for (a in 1:length(listAcquisitions)){
        listAP <- listAcquisitions[[a]]$access_points
        acquiID <- listAcquisitions[[a]]$`_id`
        #get relevant informantion from list
        temp <- lapply(listAP,function(x) return (c(x$BSSID,x$RSSI)))
        
        #reshape list and add relevant IDs
        temp2<- lapply(temp,montaLista,zoneID=zoneID,acquiID=acquiID)
        
        #create list in desirable format
        
        #unlist lists 
        for (l in 1:length(temp2)){
          lista <- rbind(lista,unlist(temp2[[l]]))
        }
        
        
        
      }
      
      rawData <- rbind(rawData,lista)
      
    }
    
    
    #we DONT want strings to be turned to factors!
    rawDataDF<-data.frame(rawData,stringsAsFactors = FALSE)
    
    names(rawDataDF)<- c("BSSID","RSSI","idZ","acquiID")
    
    rawDataDF$RSSI <- as.numeric(rawDataDF$RSSI)
    
    
    molten <- melt(rawDataDF,id.vars=c("idZ","acquiID","BSSID"), value.name = "RSSI",measure.vars = c("RSSI"))
    
    molten$variable <- NULL
    
    #gambiarra
    #molten <- molten[-14,]
    
    attach(rawDataDF)
    tidyData <- reshape2::dcast(molten,   acquiID+ idZ  ~ BSSID, value.var = 'RSSI')
    detach(rawDataDF)
    
    
  }else{
    print("Could not connect to Mongo! DAMN IT LIRA")
    return()
  }
  
  
  
  #convert to numbers
  #zones <- as.numeric(args)
  
  #source("serverFunctions.r")
  #setwd("~/Documents/machinelearning_R")
  
  dataPath <- "prepared-data"
  #remove Aquisition ID, as we don't really need it from now on
  tidyData<- tidyData[,-1]
  
  
  #transform idZ into factor!
  tidyData[,1] <- as.factor(tidyData[,1])
  
  #REMOVE NAs
  tidyData[is.na(tidyData)] <- -120
  
  print (paste(dataPath,"/",facilityID,".rds",sep=""))                      
                       
  #save file with facilityID as name
  saveRDS(tidyData,paste(dataPath,"/",facilityID,".rds",sep=""))
  rm(list = setdiff(ls(), lsf.str()))
  return("ok")
  
}


#
#Make a zone prediction based on input data from cellphone
#
#Args: 
#       jsonMeasure: object with measures and facility from which the trained models will be used
#
#Output: 
#       ID of zone 
#
#
#* @post /singleTest
aws.SingleTest <- function (facility_id,access_points){

    dataVector <- access_points
  
    
    facilityID <- facility_id
   
  
    
    BSSIDlist <- dataVector$BSSID
    RSSIlist <- dataVector$RSSI
    
    #row vector
    transposedData <- matrix(nrow=1,ncol=length(BSSIDlist))
    
    transposedData <- data.frame(transposedData)
    
    
    names(transposedData) <- BSSIDlist
    
    transposedData[1,] <- RSSIlist
  

   
     #get queued measure from mongo
     #transposedData <- getMeasureFromMongo(queueID)
    

  
  pathModels <- paste("trained-models/",facilityID,".rds",sep="")
  
  #get trained models
  trainedModels <- readRDS(pathModels)
  print("Got models!")
  #deserialize Java J48 and SMO objects
  

  
  invisible(rJava::.jstrVal(trainedModels$Tree$classifier))
  
  invisible(rJava::.jstrVal(trainedModels$SMO$classifier))
  

  pathData <- paste("prepared-data/",facilityID,".rds",sep="")

  #get datasets so we can use the train set in the KNN prediction
  dataset <- readRDS(pathData)
  print("Got data!")
  

  
  return(prediction.from.models(transposedData,dataset,trainedModels))
}

#* @get /train
aws.trainModels <- function (facilityID){
  
  #setwd("~/Documents/machinelearning_R")
  
  pathData <- paste("prepared-data/",facilityID,".rds",sep="")
  
  
  tidyData <- readRDS(pathData)
  
  
  #SCALE DATA
  
  idZ <- tidyData$idZ
  
  
  tidyData <- dplyr::select(tidyData,-idZ)
  
  
  #NON PCA SCALING
  preProc  <- caret::preProcess(tidyData)
  
  scaled <- predict(preProc, tidyData)
  
  scaled <- cbind(idZ,scaled)
  #now, we train the models with data already scaled
  trainedModels <- trainModels(scaled)
  
  #save scaling object with models!
  trainedModels<- c(trainedModels,"preProc"=list(preProc))
  
  pathModels <- paste("trained-models/",facilityID,".rds",sep="")
  
  
  saveRDS(trainedModels,pathModels)
  rm(list = setdiff(ls(), lsf.str()))
  return("ok")
  
}

#
#
#TRAIN MODELS WITH TRAIN SET IN FORMAT SPECIFIED IN ANOTHER FUNCTIONS
#
#
#
trainModels <- function(train){
  
  #DECISION TREE
  
  
  #tree <- J48(idZ~.,data=train)
  treeAda <- AdaBoostM1(idZ~. , data = train ,control = Weka_control(W = list(J48, M=5)))

  #serialize java object object
  rJava::.jcache(treeAda$classifier)
  
  
  
  #NEURALNETWORK
  
  #transforms factors in binary dummy vectors
  #ASSUMING IDZ IS IN COLUMN 1!!!!!!!!!
  
  usingNN=FALSE
  
  if(usingNN==TRUE){
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
  }
  
  #assign("NeuralNet",nn,.GlobalEnv)
  #saveRDS(nn,"NeuralNet.rds")
  
  #SUPPORT VECTOR MACHINE
 
  
  
  SMO <- SMO(idZ~.,data=train)
  assign("SMO",SMO,.GlobalEnv)
  
  rJava::.jcache(SMO$classifier)
  
  #saveRDS(mylogit1,"SVM.rds")
  
  
  if(usingNN){
    modelList <- list("NeuralNet" = nn,"SMO" = SMO,"Tree" = treeAda)
  }
  else{
    modelList <- list("SMO" = SMO,"Tree" = treeAda)
  }
  return (modelList)
  
}



#Function to be used as FUN argument in lapply
montaLista<- function(x,zoneID,acquiID){		
  return (list(BSSID=x[1],RSSI=x[2],idZ=zoneID,acquiID=acquiID))		
}

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}


