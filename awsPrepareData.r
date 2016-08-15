
#recieve input building and floor from command line
args = commandArgs(trailingOnly=TRUE)
#default values
if (length(args)==0){
  args <- c(0,1)
}

#convert to numbers
zones <- as.numeric(args)

source("serverFunctions.r")


dataPath <- "raw-data"


datasets <- prepareUCIdata2(dataPath,args[1],args[2])


saveRDS(datasets,"prepared-data/UCIdata.rds")

