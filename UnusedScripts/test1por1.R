


#so pra pegar as zonas
datasets<-prepareUCIdata2(path,0,1)
zones <- zonas


#perform tests on random subsets of a increasing number of zones

results <- NULL
for ( zNumber in 3:length(zones)) {
  
  datasets <-prepareUCIdata2(path,0,1,sample(zones, zNumber, replace = FALSE, prob = NULL))
  
  models  <-trainModels(datasets$train_s,datasets$train_pca,datasets$test_s)
  
  rates   <-allTests(datasets$train_s,datasets$test_s,models$NeuralNet,models$SMO,models$Tree,models$TreeAda)
  
  results <- rbind(results,rates) 
  
}


par(mfrow=c(2,2))

x<-seq(3,length(zones))


plot(unlist(results[,1]),main="Rede Neural",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,1])~poly(x,3))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

plot(unlist(results[,4]),main="SMO",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,4])~poly(x,3))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

plot(unlist(results[,5]),main="KNN",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,5])~poly(x,3))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

plot(unlist(results[,6]),main="J48",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,6])~poly(x,3))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")



par(mfrow=c(2,1))

plot(unlist(results[,2]),main="Voto Simples",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
plot(unlist(results[,3]),main="Voto com Peso",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")

