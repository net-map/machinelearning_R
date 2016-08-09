zones <- c(116,117,118,119,120,121,122,123,124,125,126,127,128,129)
path <- "raw-data/trainingData.csv"

datasets <- prepareUCIdata(path,zones)


saveRDS(datasets,"prepared-data/data1.rds")
