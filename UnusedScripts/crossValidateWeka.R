



#path <- "~/Documents/machinelearning_R/datasets"
path <- "~/Documents/netmap/raw-data"

#zones <- c(101, 102 ,103 ,104, 105, 106, 107, 108, 109, 110, 111, 112,113,114,115,116,117,118,119,120,121)
#feed the location of the dataset to the function
datasets <- prepareUCIdata2(path,1,0,justInside = TRUE)
#datasets<-prepareUCIdata2(path,0,2)
#kNumber = 40
#flds <- createFolds(scaled$idZ, k = kNumber, list = TRUE, returnTrain = FALSE)


#for (i in 1:kNumber){
#      train<- scaled[-flds[[i]],]
#      test <- scaled[flds[[i]],]
#      
#      tree <- J48(idZ~.,data=train)
#      prediction <- predict(tree,test)
#      error <-1- ( mean(prediction== scaled[flds[[i]],]$idZ))
#      print(error)
#}

train_s <- datasets$train_s
test_s <- datasets$test_s

results<-NULL
resultsAda<-NULL
for (i in 10:nrow(datasets$train_s)){
  tree <- J48(idZ~.,data=train_s[1:i,])
  prediction <- predict(tree,test_s)
  testError <-1- ( mean(prediction== test_s$idZ))
  results <- rbind(results,c(testError))
  
  treeAda <- AdaBoostM1(idZ~. , data = train_s[1:i,] ,control = Weka_control(W = list(J48, M=5)))
  predictionAda <- predict(treeAda,test_s)
  testErrorAda <-1- ( mean(predictionAda== test_s$idZ))
  resultsAda <- rbind(resultsAda,c(testErrorAda))
  
  
}


print(results)
#lines(10:length(datasets$train_s),results[,1])
#lines(10:length(datasets$train_s),results[,2])

jpeg("~/Documents/momonografia/imagens/J48xADAValidacaopredio1andar0.jpeg", width = 2000, height = 1700, units = 'px', res = 300)


x <- seq(10,nrow(datasets$train_s),1)
#data to be plotted and fited
data <- results[,1]

#model as n degree polynomial
model <- lm(data~poly(x,5))




plot(x=x,results[,1]*100,pch="o",col=alpha("green2", 0.3),ylab = "Erro de Classificação (%)",xlab="# de pontos de treino",main="Erro de Validação para J48 (C4.5)\n com e sem o algoritmo AdaBoost")
library(scales)
#plot scatter and tendence line
lines(x,y=100*predict(model,data.frame(x=x)),type='l',col="green3")
lines(resultsAda*100,type="p",col=alpha("purple", 0.3),pch="x")
modelAda <- lm(resultsAda~poly(x,5))
lines(x,y=100*predict(modelAda,data.frame(x=x)),type='l',col="purple3")
legend("topright",fill=c("green", "purple" ) , legend = c("Sem AdaBoost","Com AdaBoost"))


dev.off()

