
#recieve input zones from command line
args = commandArgs(trailingOnly=TRUE)
#default values
if (length(args)==0){
  args <- c("112","113","114")
}
#convert to numbers
zones <- as.numeric(args)

source("serverFunctions.r")

dirPath <- ("/home/ec2-user/machinelearning_R")

dataPath <- "raw-data"

trainPath <- file.path(dirPath,dataPath)

datasets <- prepareUCIdata(trainPath,zones)

setwd("/home/ec2-user/machinelearning_R")

saveRDS(datasets,"prepared-data/UCIdata.rds")
