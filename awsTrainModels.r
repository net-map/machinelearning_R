
source("serverFunctions.r")
#recieve input facility name from command line
#Rscript awsTrainModels.r facilityName 
args = commandArgs(trailingOnly=TRUE)
#default location
if(length(args)==0){
  #dataPath <- "prepared-data/UCIdata.rds"
  facilityName <- "Brocolândia"
}


facilityName <- args[1]



pathData <- paste("prepared-data/",facilityName,".rds",sep="")


dataset <- readRDS(pathData)



trainedModels <- trainModels(dataset)


pathModels <- paste("trainedModels/",facilityName,".rds",sep="")


saveRDS(trainedModels,pathModels)

