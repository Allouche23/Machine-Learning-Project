#Final Project the md file 

#preparing
library(ggplot2)
library(caret)
library(randomForest)

#downloading data
trainingdata <- read.csv("pml-training.csv", header = TRUE)
testdata <- read.csv("pml-testing.csv",header = TRUE)

#Remove collumes with NA
trainingdata <- trainingdata[,colSums(is.na(trainingdata))=="0"]
testdata <- testdata[,colSums(is.na(testdata))=="0"]

#Remove Columes with no variability
nzv <- nearZeroVar(trainingdata) 
trainingdata <- trainingdata[,-nzv]
nzv <- nearZeroVar(testdata)
testdata <- testdata[,-nzv]

#Remove id columes
trainingdata <- trainingdata[,-c(1:6)]
testdata <- testdata[,-c(1:6)]
dim(trainingdata)

#slicing the data
set.seed(534)
intrain <- createDataPartition(trainingdata$classe, p = 0.7, list = FALSE)
traindata <- trainingdata[intrain,]
validdata <- trainingdata[-intrain,]

#Cross validation code
crva <- trainControl(method = "cv", number=5)
library(rattle)
suppressMessages(library(rattle))

#Classification Tree
model1 <- train(classe~.,data = traindata, method="rpart", trControl=crva)
fancyRpartPlot(model1$finalModel)
p1 <- predict(model1, newdata = validdata)
confm1 <- confusionMatrix(validdata$classe,p1)
confm1$overall[1]

#Random Forest
model2 <- randomForest(classe~.,data = traindata, method="rf", trControl=crva, verbose=FALSE)
p2 <- predict(model2, newdata = validdata)
confm2 <- confusionMatrix(validdata$classe,p2)
confm2

#predictive model
predict(model2,newdata = testdata)

