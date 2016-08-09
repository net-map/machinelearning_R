#recieve input data path from command line
args = commandArgs(trailingOnly=TRUE)
#default location
if(length(args)==0){
  dataPath <- "prepared-data/UCIdata.rds"
}

dirPath <- ("/home/ec2-user/machinelearning_R")


path <- file.path(dirPath,dataPath)


datasets <- readRDS(path)


models <- trainModels(datasets$train_s,datasets$train_pca,datasets$test_s)

saveRDS(models,"trainedModels/UCImodels.rds")

