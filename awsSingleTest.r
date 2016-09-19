

setwd("../machinelearning_R")

path<-paste(getwd(),"serverFunctions.r",sep="/")

invisible(source(path))



#recieve input data vector path from command line and facility name
#Rscript (JSONurl | JSONPath | JSONtext) facilityName 
args = commandArgs(trailingOnly=TRUE)
#default location and name
if(length(args)==0){
  #dataPath <- "prepared-data/UCIdata.rds"
   dataPath<-      "aquisition.json"
   facilityName <- "Brocolândia"
}


dataPath <- args[1]
facilityName <- args[2]




#getData
dataVector <- jsonlite::fromJSON(dataPath)$access_points


BSSIDlist <- dataVector$BSSID
RSSIlist <- dataVector$RSSI

#row vector
transposedData <- matrix(nrow=1,ncol=length(BSSIDlist))

transposedData <- data.frame(transposedData)


names(transposedData) <- BSSIDlist

transposedData[1,] <- RSSIlist


print(transposedData)


pathModels <- paste("trainedModels/",facilityName,".rds",sep="")

#get trained models
trainedModels <- readRDS(pathModels)

#deserialize Java J48 and SMO objects
rJava::.jstrVal(trainedModels$Tree$classifier)
rJava::.jstrVal(trainedModels$SMO$classifier)



preProc <- readRDS("trainedModels/scale.rds")



pathData <- paste("prepared-data/",facilityName,".rds",sep="")


#get datasets so we can use the train set in the KNN prediction
datasets <- readRDS(pathData)


#names of features used for training
names <- names(datasets)[-1]






#print(singleTest2(transposedData,datasets,preProc,trainedModels$NeuralNet,trainedModels$SMO,trainedModels$Tree) )
#print(singleTest2(dplyr::select(test_s,-idZ),datasets$train_s,preProc,trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) )
