setwd("../machinelearning_R")
path<-paste(getwd(),"serverFunctions.r",sep="/")

invisible(source(path))

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


tidyData <- readRDS(pathData)


#SCALE DATA

idZ <- tidyData$idZ


tidyData <- dplyr::select(tidyData,-idZ)


#NON PCA SCALING
preProc  <- caret::preProcess(tidyData)

scaled <- predict(preProc, tidyData)


#now, we train the models with data already scaled
trainedModels <- trainModels(scaled)

#save scaling object with models!
trainedModels<- c(trainedModels,"preProc"=list(preProc))

pathModels <- paste("trainedModels/",facilityName,".rds",sep="")


saveRDS(trainedModels,pathModels)

