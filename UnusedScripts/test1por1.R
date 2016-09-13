


#so pra pegar as zonas
datasets<-prepareUCIdata2(path,1,0)
zones <- zonas


#perform tests on random subsets of a increasing number of zones

results <- NULL
for ( zNumber in 3:length(zones)) {
  
  datasets <-prepareUCIdata2(path,0,1,sample(zones, zNumber, replace = FALSE, prob = NULL))
  
  models  <-trainModels(datasets$train_s,datasets$train_pca,datasets$test_s)
  
  rates   <-allTests(datasets$train_s,datasets$test_s,models$NeuralNet,models$SMO,models$Tree,TreeModelAda =models$TreeAda)
  
  results <- rbind(results,rates) 
  
}




#par(mfrow=c(2,2))



x<-seq(3,length(zones))



jpeg("~/Documents/momonografia/imagens/errorporzonaNN.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
plot(unlist(results[,1])*100,main="Rede Neural",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,1])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")
dev.off()

jpeg("~/Documents/momonografia/imagens/errorporzonaSMO.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
plot(unlist(results[,4])*100,main="SMO",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,4])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")
dev.off()

jpeg("~/Documents/momonografia/imagens/errorporzonaKNN.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
plot(unlist(results[,5])*100,main="KNN",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,5])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")
dev.off()

jpeg("~/Documents/momonografia/imagens/errorporzonaJ48+Ada.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
par(mfrow=c(1,2))
plot(unlist(results[,6])*100,main="J48",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,6])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

plot(unlist(results[,7])*100,main="J48 + AdaBoost",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,7])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

dev.off()


jpeg("~/Documents/momonografia/imagens/errorporzonaVotos.jpeg", width = 2000, height = 1700, units = 'px', res = 300)


par(mfrow=c(1,2))

plot(unlist(results[,2])*100,main="Voto Simples",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,2])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

plot(unlist(results[,3])*100,main="Voto com Peso",xlab = "# de Zonas",ylab = "Erro de Classificação (%)")
model <- lm(unlist(results[,3])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

dev.off()

