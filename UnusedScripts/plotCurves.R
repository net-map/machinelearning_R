path <- "~/Documents/machinelearning_R/datasets"
datasets <- prepareUCIdata2(path,1,0,justInside = TRUE)





results<-NULL









for (i in 10:nrow(datasets$train_s)){
  tree <- SMO(idZ~.,data=train_s[1:i,])
  prediction <- predict(tree,test_s)
  prediction2 <- predict(tree,train_s)
  testError <-1- ( mean(prediction== test_s$idZ))
  trainError <- 1- ( mean(prediction2== train_s$idZ))
  results <- rbind(results,c(testError,trainError))
}
