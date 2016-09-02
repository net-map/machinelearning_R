



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

results<-NULL

for (i in 10:nrow(datasets$train_s)){
  tree <- J48(idZ~.,data=train_s[1:i,])
  prediction <- predict(tree,test_s)
  prediction2 <- predict(tree,train_s)
  testError <-1- ( mean(prediction== test_s$idZ))
  trainError <- 1- ( mean(prediction2== train_s$idZ))
  results <- rbind(results,c(testError,trainError))
}


print(results)
#lines(10:length(datasets$train_s),results[,1])
#lines(10:length(datasets$train_s),results[,2])

jpeg("~/Documents/momonografia/imagens/J48Validacaopredio1andar0.jpeg", width = 4.5, height = 4, units = 'in', res = 300)


x <- seq(10,nrow(datasets$train_s),1)
#data to be plotted and fited
data <- results[,1]

#model as n degree polynomial
model <- lm(data~poly(x,3))

plot(x=x,results[,1]*100,pch="o",ylab = "Erro de Validação",xlab="# de pontos de treino",main="Erro de Validação para J48 (C4.5)")

#plot scatter and tendence line
lines(x,y=100*predict(model,data.frame(x=x)),type='l')


dev.off()

