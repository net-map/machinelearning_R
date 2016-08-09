#PCA TESTS

bol <- scaled == 100
scaled[bol] = -120


pca <- prcomp(scaledPCA[,-1])
response <- scaledPCA[,1]


std_dev <- pca$sdev
pr_var <- std_dev^2
#proportion of variance 
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")




#K-FOLD KNN CROSS VALIDATION 
#KNN error list
KNNerrorList<-NULL
nnErrorList <- NULL
for (i in seq(10,79,4)){
  scaledPCA <- as.data.frame(cbind(pca$x[,1:i],as.factor(response)))
  names(scaledPCA)[i+1] <- "idZ"
  index <- sample(1:nrow(scaledPCA),round(0.7*nrow(scaledPCA)))
  train_s <- scaledPCA[index,]
  test_s <- scaledPCA[-index,]
  KNNerrorList <- rbind(KNNerrorList,crossValidateKNN(train_s,test_s,3))
  NNerrorList <- rbind(NNerrorList,crossValidateNN(train_s,test_s,10))
}

#project data into PCA space
scale(test_s[2,], pca$center, pca$scale) %*% pca$rotation 




train_s <- scaled[index,]
test_s <- scaled[-index,]
print(crossValidateKNN(train_s,test_s,3))
print(crossValidateNN(train_s,test_s,10))




