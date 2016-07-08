#NEURAL NET CROSS VALIDATION


#transforms factors in binary dummy vectors
nnData <- cbind(dplyr::select(train_s,-idZ),nnet::class.ind(train_s$idZ))
nnDatatest <- cbind(dplyr::select(test_s,-idZ),nnet::class.ind(test_s$idZ))

addq <- function(x) paste0("`", x, "`")
#adds `x` to every name in data
names(nnData) <- addq(names(nnData))
names(nnDatatest) <- addq(names(nnDatatest))

n <- names(nnData)
nTest <- names(nnDatatest)
#gets indexes of dummy id columns 
indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)
indexIdTest <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",nTest)

lhseq <- paste(names(nnData[,indexId]),collapse="+")

rhseq <- paste(names(nnData[,-indexId]),collapse="+")

#creates formula
f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))

#for some reason, remove quotes and it works
nnData <- cbind(dplyr::select(train_s,-idZ),nnet::class.ind(train_s$idZ))
nnDatatest <- cbind(dplyr::select(test_s,-idZ),nnet::class.ind(test_s$idZ))


listError <- NULL

for (neuron in 10:20){
  
 
        
        #TRAIN neuralnet!
        nn <- neuralnet(f,data=nnData,hidden=c(neuron,neuron),linear.output=FALSE) 
        
        nnDataResponse <- nnData[,indexId]
        nnDatatestResponse <- nnDatatest[,indexIdTest]
        
        
        trainRes <-apply(neuralnet::compute(nn,nnData[,-indexId])$net.result,1,function(x) which.max(x))
        testRes <-apply(neuralnet::compute(nn,nnDatatest[,-indexIdTest])$net.result,1,function(x) which.max(x))
        
        
        #remount idZ for train and test datasets
        idZtest <- factor(apply(nnDatatest[,indexIdTest], 1, function(x) which(x == 1)), labels = colnames(nnDatatest[,indexIdTest])) 
        idZ <- factor(apply(nnData[,indexId], 1, function(x) which(x == 1)), labels = colnames(nnData[,indexId])) 
        
        #create response factors from computed data
        idZtestres <- factor(testRes, labels = colnames(nnDatatest[,indexIdTest])) 
        idZres <- factor(trainRes, labels = colnames(nnData[,indexId])) 
        
        

        
        trainError <- 100*(1-mean(idZ == idZres))
        testError <- 100*(1-mean(idZtest == idZtestres))
        
        nnError <- c(trainError,testError)
        
        
        listError <- rbind(listError,nnError)
}
