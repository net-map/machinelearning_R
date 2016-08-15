
source("serverFunctions.r")
#recieve input data path from command line
args = commandArgs(trailingOnly=TRUE)
#default location
if(length(args)==0){
  dataPath <- "prepared-data/UCIdata.rds"
}



datasets <- readRDS(dataPath)


trainedModels <- trainModels(datasets$train_s,datasets$train_pca,datasets$test_s)


saveRDS(trainedModels,"trainedModels/UCImodels.rds")

