prepareUCIdata()



trainModels(train_s,train_pca)


testVector <- NULL

for (i in 1:nrow(test_s)){
 print(singleTest(NeuralNet,SVM,KNN,dplyr::select(test_s[i,],-idZ)) == test_s[i,]$idZ)
}










#K-FOLD CROSS-VALIDATION NEURALNET
NNerrorList <- NULL
kNumber <- 10
scaledPCA <- train_pca
flds <- createFolds(scaledPCA$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)

#flds[[1]] gets first fold indexes, etc
#200 neuronios parace ser bom
neuronList <- c(50,80,100,120,150,200,300,350,400,450)

#1 neuron HL
for( i in 1:kNumber){
  NNerrorList <- rbind(NNerrorList,crossValidateNN(scaledPCA[-flds[[i]],],scaledPCA[flds[[i]],],neuronList[i]))
}
