
zones <- c(116,117,118,119,120,121,122,123,124,125,126,127,128,129)
bigResults <- NULL
allResults <- NULL
for ( zNumber in 3:14) {
  
  
  
  datasets <-prepareUCIdata("~/Documents/netmap/datasets",sample(zones, zNumber, replace = FALSE, prob = NULL))
  
  trainedModels <- trainModels(datasets$train_s,datasets$train_pca)
  
  
  
  
  results <- NULL
  for (i in 1:nrow(datasets$test_s)){
    vote <- singleTest(dplyr::select(datasets$test_s[i,],-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree)
    NN <-  singleTestNN(dplyr::select(datasets$test_s[i,],-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
    SVM <- singleTestSVM(dplyr::select(datasets$test_s[i,],-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
    KNN <- singleTestKNN(dplyr::select(datasets$test_s[i,],-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
    Tree <-  singleTestTree(dplyr::select(datasets$test_s[i,],-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
    correct <- as.numeric(as.character(dplyr::select(datasets$test_s[i,],idZ)))
    factors<- trainedModels$NeuralNet$model.list$response
    factors <- gsub("`",'',factors)
    correct <- as.numeric(factors[correct])
    allResults <- rbind(allResults,c(vote,NN,SVM,KNN,Tree,correct))
    
  }
  print(allResults)
  rateVote <- mean(allResults[,1]==allResults[,6])
  rateNN <- mean(allResults[,2]==allResults[,6])
  rateSVM <- mean(allResults[,3]==allResults[,6])
  rateKNN <- mean(allResults[,4]==allResults[,6])
  rateTree <- mean(allResults[,5]==allResults[,6])
  
  bigResults <- rbind(bigResults,c(rateVote,rateNN,rateSVM,rateKNN,rateTree,zNumber))
  print( c(rateVote,rateNN,rateSVM,rateKNN,rateTree,zNumber))
  allResults <- NULL
  
  
}




plot(bigResults[,6],bigResults[,1])
lines(bigResults[,6],bigResults[,2])
lines(bigResults[,6],bigResults[,3])
lines(bigResults[,6],bigResults[,4])
lines(bigResults[,6],bigResults[,5],color="red")





