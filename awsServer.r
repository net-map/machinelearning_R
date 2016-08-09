zones <- c(116,117,118,119,120,121,122,123,124,125,126,127,128,129)

setwd("/home/ec2-user/machinelearning_R")

source("serverFunctions.r")

path <- "raw-data/trainingData.csv"

datasets <- prepareUCIdata(path,zones)


setwd("/home/ec2-user/machinelearning_R")

saveRDS(datasets,"prepared-data/data1.rds")
