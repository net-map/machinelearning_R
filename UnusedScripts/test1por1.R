trainModels <- function(train){
  
  #DECISION TREE
  
  
  #tree <- J48(idZ~.,data=train)
  treeAda <- AdaBoostM1(idZ~. , data = train ,control = Weka_control(W = list(J48, M=5)))
  
  
  tree <- J48(idZ~. , data = train)
  
  
  #serialize java object object
  rJava::.jcache(treeAda$classifier)
  
  
  
  #NEURALNETWORK
  
  #transforms factors in binary dummy vectors
  #ASSUMING IDZ IS IN COLUMN 1!!!!!!!!!
  

  
  
    nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train[,1]))
    
    addq <- function(x) paste0("`", x, "`")
    #adds `x` to every name in data
    names(nnData) <- addq(names(nnData))
    
    n <- names(nnData)
    #gets indexes of dummy id columns 
    indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)
    
    lhseq <- paste(names(nnData[,indexId]),collapse="+")
    
    rhseq <- paste(names(nnData[,-indexId]),collapse="+")
    
    #creates formula
    f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))
    
    #for some reason, remove quotes and it works
    nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train[,1]))
    
    #TRAIN neuralnet!
    
    neuron <- 210
    
    nn <- neuralnet::neuralnet(f,data=nnData,hidden=c(neuron),linear.output=FALSE) 
  
  
  #assign("NeuralNet",nn,.GlobalEnv)
  #saveRDS(nn,"NeuralNet.rds")
  
  #SUPPORT VECTOR MACHINE
  
  
  
  SMO <- SMO(idZ~.,data=train)
  assign("SMO",SMO,.GlobalEnv)
  
  rJava::.jcache(SMO$classifier)
  
  #saveRDS(mylogit1,"SVM.rds")
  
  
  
  
  
  
  modelList <- list("NeuralNet" = nn,"SMO" = SMO,"TreeAda" = treeAda,"Tree"=tree)
  

  return (modelList)
  
}


allTests <- function (train,test,NNmodel,SMOmodel,TreeModel,TreeModelAda){
  
  factors<- NNmodel$model.list$response
  factors <- gsub("`",'',factors)
  
  idZ <- dplyr::select(test,idZ)
  test<- dplyr::select(test,-idZ)
  
  
  
  #NEURALNET PREDICTION
  nnProb<-neuralnet::compute(NNmodel,test)$net.result
  nnPrediction <-apply(nnProb,1,function(x) which.max(x))
  #get idz computed
  idZNN <- as.numeric(as.character(factors[nnPrediction]))  
  
  
  #SMO PREDICTION
  idZSMO <- predict(SMOmodel,test)
  smoProb <- predict(SMOmodel,test,type="probability")
  
  
  
  #DECISION TREE PREDICTION
  predictionTree <- predict(TreeModel,test,type="probability")
  idZtree <-  as.numeric(as.character(factors[apply (predictionTree,1,function(x) which.max(x))]))
  treeProb <-  predictionTree
  
  #DECISION TREE PREDICTION
  predictionTreeAda <- predict(TreeModelAda,test,type="probability")
  idZtreeAda <-  as.numeric(as.character(factors[apply (predictionTreeAda,1,function(x) which.max(x))]))
  treeAdaProb <-  predictionTreeAda
  
  
  
  
  #KNN PREDICTION
  knnTrain<-kknn(formula=idZ ~. , k=4,distance=1, train=train,test=test,kernel="optimal")
  knnProb <- knnTrain$prob
  knnPrediction <- as.numeric(as.character(knnTrain$fitted.values))
  idZKNN <- knnPrediction
  
  #correct prediction
  correct <- as.numeric(idZ[[1]])
  testIDZ <- as.numeric(factors[correct])
  
  
  sumProb <-  knnProb + nnProb +smoProb +treeProb
  #get class with maximum summed probability
  idZBayas <-  as.numeric(as.character(factors[apply (sumProb,1,function(x) which.max(x))]))
  
  
  results <- cbind(idZKNN,idZSMO,idZNN,idZtree)
  
  #get most recurring result
  idZVote <- apply(results,1,function (x) as.numeric(names(sort(table(x),decreasing = TRUE)[1])))
  
  
  
  #COMPUTE CLASSIFICATION ~ERROR~ RATES
  
  rateNN <- 1- mean(idZNN==testIDZ)
  rateKNN <- 1- mean(idZKNN==testIDZ)
  rateTree <- 1- mean(idZtree==testIDZ)
  rateSMO <- 1- mean(idZSMO==testIDZ)
  
  rateTreeAda <- 1- mean(idZtreeAda==testIDZ)
  rateVote <- 1-mean(idZVote==testIDZ)
  rateBayas <- 1-mean(idZBayas==testIDZ)
  
  
  
  return(list("NN"=rateNN,"Simple Vote"=rateNN,"Weight Vote"=rateBayas,"SMO"=rateSMO,"KNN"=rateKNN,"J48"=rateTree,"Ada"=rateTreeAda))
  
}



#END FUNCTIONS#
##############################################################################################################################


path <- "raw-data"
#so pra pegar as zonas
datasets<-prepareUCIdata2(path,1,0)
#get from global vars 
zones <- zonas


#perform tests on random subsets of a increasing number of zones

results <- NULL
for ( zNumber in 3:length(zones)) {
  
  datasets <-prepareUCIdata2(path,0,1,sample(zones, zNumber, replace = FALSE, prob = NULL))
  
  models  <-trainModels(datasets$train_s)
  
  rates   <-allTests(datasets$train_s,datasets$test_s,models$NeuralNet,models$SMO,models$Tree,TreeModelAda =models$TreeAda)
  print("done round")
  results <- rbind(results,rates) 
  
}




#par(mfrow=c(2,2))



x<-seq(3,length(zones))



jpeg("~/Documents/momonografia/imagens/errorporzonaNN.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
plot(unlist(results[,1])*100,main="Rede Neural",xlab = "# de Zonas",ylab = "Erro de Classificação (%)",ylim=c(0, 45))
model <- lm(unlist(results[,1])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")
dev.off()

jpeg("~/Documents/momonografia/imagens/errorporzonaSMO.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
plot(unlist(results[,4])*100,main="SMO",xlab = "# de Zonas",ylab = "Erro de Classificação (%)",ylim=c(0, 45))
model <- lm(unlist(results[,4])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")
dev.off()

jpeg("~/Documents/momonografia/imagens/errorporzonaKNN.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
plot(unlist(results[,5])*100,main="KNN",xlab = "# de Zonas",ylab = "Erro de Classificação (%)",ylim=c(0, 45))
model <- lm(unlist(results[,5])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")
dev.off()

jpeg("~/Documents/momonografia/imagens/errorporzonaJ48+Ada.jpeg", width = 2000, height = 1700, units = 'px', res = 300)
par(mfrow=c(1,2))
plot(unlist(results[,6])*100,main="J48",xlab = "# de Zonas",ylab = "Erro de Classificação (%)",ylim=c(0, 45))
model <- lm(unlist(results[,6])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

plot(unlist(results[,7])*100,main="J48 + AdaBoost",xlab = "# de Zonas",ylab = "Erro de Classificação (%)",ylim=c(0, 45))
model <- lm(unlist(results[,7])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

dev.off()


jpeg("~/Documents/momonografia/imagens/errorporzonaVotos.jpeg", width = 2000, height = 1700, units = 'px', res = 300)


par(mfrow=c(1,2))

plot(unlist(results[,2])*100,main="Voto Simples",xlab = "# de Zonas",ylab = "Erro de Classificação (%)",ylim=c(0, 45))
model <- lm(unlist(results[,2])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

plot(unlist(results[,3])*100,main="Voto com Peso",xlab = "# de Zonas",ylab = "Erro de Classificação (%)",ylim=c(0, 45))
model <- lm(unlist(results[,3])*100~poly(x,2))
lines(x,y=predict(model,data.frame(x=x)),type='l',col="green3")

dev.off()

