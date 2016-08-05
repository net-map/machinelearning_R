

path <- "~/Documents/machinelearning_R/datasets"
path <- "~/Documents/netmap/datasets"



#feed the location of the dataset to the function
datasets <- prepareUCIdata(path,zones)



trainedModels<-trainModels(datasets$train_s,datasets$train_pca,datasets$test_s)


temp<-NULL
for (i in 1:nrow(test_s)){
 temp<-rbind(temp,singleTest(dplyr::select(test_s[i,],-idZ),NeuralNet,SVM,KNN,Tree) == test_s[i,]$idZ)
}


print (mean(temp))

#apply(dplyr::select(test_s,-idZ),1,singleTest,NNmodel=NeuralNet,SVMmodel=SVM,KNNmodel=KNN)








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


kernelList <- c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal")
# 2~4 vizinhos parece ser top
#distancia manhattan eh certamente a melhor
for (i in 1:kNumber){
  KNNerrorList <- rbind(KNNerrorList,crossValidateKNN(scaled[-flds[[i]],],scaled[flds[[i]],]))
}


plot(vizinhosList,KNNerrorList[,2],pch="Δ",ylab = "Erro de Validação",xlab="K-Value para KNN",main="Cross-Validation 10-Fold para KNN\n Distancia Euclidiana")






#SVM CROSS-VALIDATION 
SVMerrorList <- NULL
kNumber <- 4

flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)


kernelList <- c("linear","polynomial","radial","sigmoid")

errorMean <- NULL


for (i in 1:100){
  flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)
  SVMerrorList <- NULL
  for( i in 1:kNumber){
    SVMerrorList <- rbind(SVMerrorList,crossValidateSVM(scaled[-flds[[i]],],scaled[flds[[i]],],kernelList[i]))
  }
  
  errorMean <- cbind(errorMean,SVMerrorList[,2])
}

print(SVMerrorList)




meanList <- apply(errorMean,1,mean)
varList <- apply(errorMean,1,var)
grid.table(rbind(kernelList,meanE,meanvar),rows <- c("Kernel","Média do Erro","Variância do Erro"))













#test with incremental number of Zones



zones <- c(116,117,118,119,120,121,122,123,124,125,126,127,128,129)



bigTest <- NULL

for ( zNumber in 3:14) {
  
  
  
  datasets <-prepareUCIdata("~/Documents/netmap/datasets",sample(zones, zNumber, replace = FALSE, prob = NULL))
  
  trainedModels <- trainModels(datasets$train_s,datasets$train_pca)
  
  
  
  testVector <- NULL
  results <- NULL
  for (i in 1:nrow(datasets$test_s)){
    results <- rbind (results,singleTest(dplyr::select(datasets$test_s[i,],-idZ),trainedModels$NeuralNet,trainedModels$SVM,trainedModels$KNN,trainedModels$Tree) == datasets$test_s[i,]$idZ)
  }
  
  
  print ( c("teste para ",zNumber, " zonas :" ,mean(results)  ))
  bigTest <- rbind(bigTest,c(zNumber,1-mean(results)))
  
  
}





# CROSS VALIDATE DECISION TREE




datasets <-prepareUCIdata("~/Documents/netmap/datasets",c(110,111))



crossValidateTree(datasets$train_s,datasets$test_s)







test <- dplyr::select(datasets$test_s,-idZ)


vote <- MatrixTestBayesianVote(test,trainedModels$NeuralNet,trainedModels$SVM,trainedModels$Tree,datasets$train_s)



error <- mean(vote == dplyr::select(test_s,idZ))







