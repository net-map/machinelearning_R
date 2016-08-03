
zones <- c(101, 102 ,103 ,104, 105, 106, 107, 108, 109, 110, 111, 112)
bigResults <- NULL
allResults <- NULL
evenBiggerResults <- NULL

for (nTests in 1:20){
  for ( zNumber in 3:12) {
    
    
    
      datasets <-prepareUCIdata(path,sample(zones, zNumber, replace = FALSE, prob = NULL))
    
      trainedModels <- trainModels(datasets$train_s,datasets$train_pca)
    
  
      vote <- singleTestMatrix(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree)
      NN <-  singleTestNN(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
      SVM <- singleTestSVM(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
      KNN <- singleTestKNN(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
      Tree <-  singleTestTree(dplyr::select(datasets$test_s,-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) 
      correct <- as.numeric(dplyr::select(datasets$test_s,idZ)[[1]])
      factors<- trainedModels$NeuralNet$model.list$response
      factors <- gsub("`",'',factors)
      correct <- as.numeric(factors[correct])
      

      
      
      allResults <- rbind(allResults,cbind(vote,NN,SVM,KNN,Tree,correct))
      
    
   
    
    rateVote <- mean(allResults[,1]==allResults[,6])
    rateNN <- mean(allResults[,2]==allResults[,6])
    rateSVM <- mean(allResults[,3]==allResults[,6])
    rateKNN <- mean(allResults[,4]==allResults[,6])
    rateTree <- mean(allResults[,5]==allResults[,6])
    
    bigResults <- rbind(bigResults,c(rateVote,rateNN,rateSVM,rateKNN,rateTree,zNumber))
    print( c(rateVote,rateNN,rateSVM,rateKNN,rateTree,zNumber))
    allResults <- NULL
    
    
  }

  evenBiggerResults <- rbind(evenBiggerResults,bigResults)
  bigResults <- NULL
}


resultFrame <- as.data.frame(evenBiggerResults)


saveRDS(resultFrame,"resultFrame.rds")

#get means for each zone
for (i in 3:12){
  
  zoneResults <- filter(resultFrame,V6==i)
  
  
  print(c(i,apply(zoneResults[,-6],2,mean)))
  
}




