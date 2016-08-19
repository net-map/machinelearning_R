

#0.8 train-test split
dataPath <- "raw-data"
datasets <- prepareUCIdata2(dataPath,0,1)

test <- datasets$test_s
data_points <- NULL
allResults <- NULL
#Iterate over trainset
for (i in seq(10,length(train),5)){
  
      
    trainedModels <- trainModels(train=datasets$train_s[1:i,],datasets$train_pca,test)
    
    simpleVote <- singleTestMatrix(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,datasets$train_s[1:i,])
    weightedVote <- MatrixTestBayesianVote(test=dplyr::select(datasets$test_s,-idZ),NNmodel=trainedModels$NeuralNet,SVMmodel=trainedModels$SVM,TreeModel=trainedModels$Tree,train=datasets$train_s[1:i,])
    
    NN <-  singleTestNN(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
    SVM <- singleTestSVM(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
    KNN <- singleTestKNN(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,train=train[1:i,]) 
    Tree <-  singleTestTree(dplyr::select(test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree) 
    correct <- as.numeric(test_s[[1]])
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
  
    rates <- c(rateVote,rateWeight,rateKNN,rateTree,rateSVM,rateNN,i)    
    data_points <- rbind(data_points,rates)
    print(rates)
    allResults <- NULL
      

}
