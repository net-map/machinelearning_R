#K-FOLD KNN CROSS VALIDATION 
#KNN error list

datasets <- prepareUCIdata2(path,1,0,justInside = TRUE)

KNNerrorList<-NULL
testsList <- NULL
kNumber <- 10
flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)



#distList <- c(1,2)
vizinhosList <- c(1,2,3,4,5,6,7,8,9,10)
#kernelList <- c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal")
# 2~4 vizinhos parece ser top
#distancia manhattan eh certamente a melhor
for (j in 1:20){
  flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:kNumber){
    
    KNNerrorList <- rbind(KNNerrorList,crossValidateKNN(scaled[-flds[[i]],],scaled[flds[[i]],],vizinhosList[i],1))
  }
  testsList <- cbind(testsList,KNNerrorList)
  KNNerrorList <- NULL
}


jpeg("~/Documents/momonografia/imagens/CrossValKNN25zonasvalorK.jpeg", width = 1800, height = 1500, units = 'px', res = 300,quality = 100)


plot(vizinhosList,apply(testsList,1,mean),pch="Δ",ylab = "Erro de Classificação (%)",xlab="Valor de K para KNN",main="Média de 20 testes de Cross-Validation 10-Fold para KNN\n 25 Zonas Usadas")
#barplot(apply(testsList,1,mean),names.arg = c("Distância Manhattan","Distância Euclidiana"),ylab = "Erro de Validação",main = "Diferença do Erro de Validação para Diferentes Métricas de Distância\n KNN")
dev.off()
