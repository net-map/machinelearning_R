

prepareUCIdata("~/Documents/machinelearning_R/datasets")



trainModels(train_s,train_pca)


testVector <- NULL

for (i in 1:nrow(test_s)){
 print(singleTest(dplyr::select(test_s[i,],-idZ),NeuralNet,SVM,KNN) == test_s[i,]$idZ)
}


#sapply(as.matrix(test_s),singleTest,NNmodel=NeuralNet,SVMmodel=SVM,KNNmodel=KNN)








#K-FOLD CROSS-VALIDATION NEURALNET
NNerrorList <- NULL
kNumber <- 10

flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)

#flds[[1]] gets first fold indexes, etc
#200 neuronios parace ser bom
#neuronList <- c(50,80,100,120,150,200,300,350,400,450)
#entre 200 e 250 eh loco
neuronList <- c(200,210,220,230,240,250,260,270,280,290)


#1 neuron HL
for( i in 1:kNumber){
  NNerrorList <- rbind(NNerrorList,crossValidateNN(scaled[-flds[[i]],],scaled[flds[[i]],],neuronList[i]))
}

plot(neuronList,NNerrorList[,2],pch="Δ",ylab = "Erro de Validação",xlab="# neuronios na HL",main="Cross-Validation 10-Fold para Rede Neural")














#K-FOLD KNN CROSS VALIDATION 
#KNN error list
KNNerrorList<-NULL
kNumber <- 10
flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)

vizinhosList <- c(1,2,3,4,5,6,7,8,9,10)
kernelList <- c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal")
# 2~4 vizinhos parece ser top
#distancia manhattan eh certamente a melhor
for (i in 1:kNumber){
  KNNerrorList <- rbind(KNNerrorList,crossValidateKNN(scaled[-flds[[i]],],scaled[flds[[i]],],vizinhosList[i],distance=1))
}


plot(vizinhosList,KNNerrorList[,2],pch="Δ",ylab = "Erro de Validação",xlab="K-Value para KNN",main="Cross-Validation 10-Fold para KNN\n Distancia Euclidiana")








