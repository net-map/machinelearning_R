source("serverFunctions.r")


dirPath <- ("/home/ec2-user/machinelearning_R")



dataPath <- "raw-data"


trainPath <- file.path(dirPath,dataPath)


zones <- c(116,117,118,119,120,121,122,123,124,125,126,127,128,129)

datasets <- prepareUCIdata(trainPath,zones)


setwd("/home/ec2-user/machinelearning_R")

saveRDS(datasets,"prepared-data/data1.rds")
