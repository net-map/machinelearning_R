library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)
library(e1071)
library(kknn)






#Name of facility
facility <- "lira house"
#user id
user <- 2

rawData1 <- getMLdata(facility,user)

suppressWarnings( tidyData1 <- prepareData(rawData1))

#zones id is a factor, not a number!
tidyData1$idZ <- as.factor(tidyData1$idZ)


#rawData2 <- getMLdata("scopusadria",1)
#tidyData3 <- prepareData(rawData2)
#tidyData3$idZ <- c(9,9,9,9)



#
#
#
#    SEPARATE DATASET IN TRAIN AND TEST SETS
#    THIS WILL BE DIFFERENT FOR EACH DATASET
#
#
#
#####################################################################################################

#the following code removes entries with 90% of -120 measures, i.e. the signal was to weak to me measured
bol <- tidyData1 == -120
for (col in  2:ncol(bol)){
 
  if( mean( bol[,col]) > .90){
    #tidyData1[[col]]<-NULL
    
  }
  
}


#tidyData2 <- merge(tidyData1,tidyData3,all=TRUE)

#tidyData2[is.na(tidyData2)] <- -120

tidyData <- tidyData1


# Scaling data
maxs <- apply(dplyr::select(tidyData,-idZ), 2, max) 
mins <- apply(dplyr::select(tidyData,-idZ), 2, min)
scaled <- as.data.frame(scale(dplyr::select(tidyData,-idZ), center = mins, scale = maxs - mins))
#concatenate with response vector again 
scaled$idZ <- tidyData$idZ

# Train-test random splitting for machine learning
# 30% for tests and the rest for training

#set.seed(18687685)
index <- sample(1:nrow(tidyData),round(0.6*nrow(tidyData)))
#Train and test UNESCALED
train <- tidyData[index,]
test <- tidyData[-index,]
#Train and test SCALED
train_s <- scaled[index,]
test_s <- scaled[-index,]



##########################################################################################
#
#
#  Prepare test dataset to be tested
#  We need to make sure that it has the same columns of the train dataset
#  We take out any RSSIs not present in train dataset and complete the test dataset with RSSIs (i.e. columns) present only in train dataset
#
#


train_s <- filter(scaled,idZ != 12)
test_s <- filter(scaled,idZ == 12)
test_s$idZ <- c(7,7,7,7,7)

names <- intersect(colnames(train),colnames(test))

test_s2<-merge(train_s[1,],test_s,by=names,all.y=TRUE)

#index1 <- grep(".x",names(test_s2))

#index2 <- grep(".y",names(test_s2))

setdiff(names(train_s),names(test_s2))

#test_s2 <- test_s2[,-index1]
#colnames(test_s2)[index2] <- sub(".y","",colnames(test_s2)[index2])

#test_s2 <- test_s2[,colSums(is.na(test_s2))<nrow(test_s2)]

######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#tests galore!
listValues <- NULL
for (i in 10:nrow(train_s)){
    listValues<- rbind(listValues,invisible(tests(train_s[1:i,],test_s2)))
}







