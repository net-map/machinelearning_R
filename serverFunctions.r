

invisible(library(rmongodb))
invisible(library(ISLR))
invisible(library(pracma))
invisible(library(dplyr))
invisible(library(jsonlite))
invisible(library(reshape2))
invisible(library(e1071))
invisible(library(kknn))
invisible(library(cluster))
invisible(library(caret))
invisible(library(nnet))
invisible(library(neuralnet))
invisible(library(gridExtra))
invisible(library(rpart))
invisible(library(RWeka))
invisible(library(Rserve))




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






#Use trained models to provide a single classification answer from testVector
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
prediction.from.models <- function(testVector,train,modelsList){
  
  #get possible idZs
  factors <- levels(train$idZ)
  
  #names of features used for training
  names <- names(train)[-1]
  
  
  #creates dummy vector with BSSIDs used to train the classifier
  dummyVector <- t(as.data.frame(x=rep(-120,length(names)),names))
  
  #merge testVector with dummyVector in a way that if there is a BSSID missing in the testVector, it is created with -120
  commomNames <- intersect(names,names(testVector))
  #get values that are present in testVector
  testVector <- merge(dummyVector,testVector,all.y=TRUE)
  
  testVector[is.na(testVector)] <- -120
  
  
  #scale new data!
  testVector <- predict(modelsList$preProc,testVector)
  
  
  #KNN TRAIN
  knnModel<-kknn(formula=idZ ~. , k=4,distance=1, train=train,test=testVector,kernel="optimal")
  
  
  
  
  listaModelos <- list("SMO"=modelsList$SMO,"KNN"=knnModel,"treeAda"=modelsList$Tree)
  
  #initialize summed support vector
  sumProb <- vector(mode="numeric",length = length(factors))
  
  for (model in listaModelos){
    
    temp <- predictionWrapper(model,testVector,probabilities=TRUE)
    
    #WEIGHTED VOTING RULE
    sumProb <- sumProb + temp
    
  }
  
  #results
  idZBayas <- as.numeric(factors[which.max(sumProb)])
  
  
  
  
  
  #return output zone_id
  return (idZBayas)
  
  
  
}


#wrapper for predict methods for ML models
#return predictions as FACTORS or support matrix
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


#
#
#Input: facilityID
#
#
#Output: Get dataset from mongo and prepare it for training
#
#
aws.PrepareData <- function (facilityID){
  
  mongo <- mongo.create(host="localhost:27017",username="net.map",password = "p4gic0tb9f2m2yj37iav")
  
  if (mongo.is.connected(mongo) == TRUE) {
 
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
  
  print(getwd())
  print (paste(dataPath,"/",facilityID,".rds",sep=""))                      
                       
  #save file with facilityID as name
  saveRDS(tidyData,paste(dataPath,"/",facilityID,".rds",sep=""))
  
}



#
#
#
#Input: json object with measures and facility from which the trained models will be used
#
#Output: ID of zone 
#
#
aws.SingleTest <- function (jsonMeasure,facilityID){
  
  #setwd("~/Documents/machinelearning_R")
  #getData
  dataVector <- jsonlite::fromJSON(jsonMeasure)$acquisition$access_points
  
  print(dataVector)
  
  BSSIDlist <- dataVector$BSSID
  RSSIlist <- dataVector$RSSI
  
  #row vector
  transposedData <- matrix(nrow=1,ncol=length(BSSIDlist))
  
  transposedData <- data.frame(transposedData)
  
  
  names(transposedData) <- BSSIDlist
  
  transposedData[1,] <- RSSIlist
  
  
  print(transposedData)
  
  
  pathModels <- paste("trained-models/",facilityID,".rds",sep="")
  
  #get trained models
  trainedModels <- readRDS(pathModels)
  
  #deserialize Java J48 and SMO objects
  rJava::.jstrVal(trainedModels$Tree$classifier)
  rJava::.jstrVal(trainedModels$SMO$classifier)
  
  
  
  pathData <- paste("prepared-data/",facilityID,".rds",sep="")
  
  
  #get datasets so we can use the train set in the KNN prediction
  dataset <- readRDS(pathData)
  
  
  
  
  
  return(prediction.from.models(transposedData,dataset,trainedModels))
}



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

#Rserve(debug=T,)
#Rserve (TRUE)

