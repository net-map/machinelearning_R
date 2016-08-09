
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

<<<<<<< HEAD
saveRDS(datasets,"prepared-data/data1.rds")
=======
setwd("/home/ec2-user/machinelearning_R")

saveRDS(datasets,"prepared-data/UCIdata.rds")
>>>>>>> 4c73679062ae8400aca4ef9a19f4448a0b21f34b
