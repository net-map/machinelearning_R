library(jsonlite)
library(reshape2)
invisible(source("serverFunctions.r"))



#recieve input data vector path from command line
args = commandArgs(trailingOnly=TRUE)
#default location and name
if(length(args)==0){
  #dataPath <- "prepared-data/UCIdata.rds"
   dataPath<- "aquisition.json"
}




#getData
dataVector <- jsonlite::fromJSON(dataPath)$acquisition$access_points


BSSIDlist <- dataVector$BSSID
RSSIlist <- dataVector$RSSI

#row vector
transposedData <- matrix(nrow=1,ncol=length(BSSIDlist))

transposedData <- data.frame(transposedData)


names(transposedData) <- BSSIDlist

transposedData[1,] <- RSSIlist


#get trained models
trainedModels <- readRDS("trainedModels/UCImodels.rds")

#deserialize Java J48 object
rJava::.jstrVal(Tree$classifier)



preProc <- readRDS("trainedModels/scale.rds")


#get datasets so we can use the train set in the KNN prediction
datasets <- readRDS("prepared-data/UCIdata.rds")


print(singleTest2(transposedData,datasets$train_s,preProc,trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) )
