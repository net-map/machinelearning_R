# Set a seed
set.seed(500)


newNames <- replicate(length(train_s), paste(sample(LETTERS, 10, replace=TRUE), collapse=""))
train_sn <- train_s
test_sn <- test_s

names(train_sn) <- newNames
names(test_sn) <- newNames


#names(train_s) <- paste("(",names(train_s),")",sep="")
#names(train_s)  <-  gsub(":","",names(train_s))

colnames(train_sn)[length(train_sn)] <- "idZ"
n <- names(train_sn)
f <- as.formula(paste("idZ ~", paste(n[!n %in% "idZ"], collapse = " + ")))

train_sn<- train_sn + 1
test_sn<- test_sn + 1
#nn <- neuralnet(f,train_sn,hidden=10,linear.output=FALSE,threshold=0.01)

str(xi)
str(yi)

library(MASS)

data <- college


# Check that no data is missing
apply(data,2,function(x) sum(is.na(x)))

# Train-test random splitting for linear model
index <- sample(1:nrow(data),round(0.5*nrow(data)))
train <- data[index,]
test <- data[-index,]

# Fitting linear model
lm.fit <- glm(Private~., data=train)
summary(lm.fit)

# Predicted data from lm
pr.lm <- predict(lm.fit,test)

# Test MSE
MSE.lm <- sum((pr.lm - test$Private)^2)/nrow(test)

#-------------------------------------------------------------------------------
# Neural net fitting

# Scaling data for the NN
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

# Train-test split
train_ <- scaled[index,]
test_ <- scaled[-index,]

# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(7,5),linear.output=FALSE)

# Visual plot of the model
plot(nn)


#aderencia ao proprio teste
tr.nn <- compute(nn,train_[,2:18])
names(train_)
trainError <- mean(as.numeric(round(tr.nn$net.result,digits = 0) == train_$Private))
print( c("Erro de treino eh ", trainError))
# Predict
pr.nn <- compute(nn,test_[,2:18])
testError <- mean(as.numeric(round(pr.nn$net.result,digits = 0) == test_$Private))
print( c("Erro de teste eh ", testError))
# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(data$quality)-min(data$quality))+min(data$quality)
test.r <- (test_$Occupancy)*(max(data$quality)-min(data$quality))+min(data$quality)

# Calculating MSE
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# Compare the two MSEs
print(paste(MSE.lm,MSE.nn))
