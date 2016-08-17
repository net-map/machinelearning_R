

#0.8 train-test split
dataPath <- "raw-data"
datasets <- prepareUCIdata2(dataPath,0,1)

train <- datasets$train_s
test <- datasets$test_s
data_points <- NULL
allResults <- NULL
#Iterate over trainset
for (i in seq(11,length(train),5)){
  
      
    trainedModels <- trainModels(train[10:i,],datasets$train_pca,test)
    
    simpleVote <- singleTestMatrix(dplyr::select(test,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,train[1:i,])
    weightedVote <- MatrixTestBayesianVote(dplyr::select(datasets$test,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,train[1:i,])
    
    NN <-  singleTestNN(dplyr::select(test,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
    SVM <- singleTestSVM(dplyr::select(test,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
    KNN <- singleTestKNN(dplyr::select(test,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
    Tree <-  singleTestTree(dplyr::select(test,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
    correct <- as.numeric(dplyr::select(test,-idZ)[[1]])
    factors<- trainedModels$NeuralNet$model.list$response
    factors <- gsub("`",'',factors)
    correct <- as.numeric(factors[correct])
    
    
    
    
    allResults <- rbind(allResults,cbind(simpleVote,NN,SVM,KNN,Tree,correct,weightedVote))
    
    
    
    
    rateVote <- mean(allResults[,1]==allResults[,6])
    rateNN <- mean(allResults[,2]==allResults[,6])
    rateSVM <- mean(allResults[,3]==allResults[,6])
    rateKNN <- mean(allResults[,4]==allResults[,6])
    rateTree <- mean(allResults[,5]==allResults[,6])
    rateWeight <- mean(allResults[,7]==allResults[,6])
  
    print(allResults)
    
    data_points <- rbind(data_points,allResults)
    allResults <- NULL
      

}
