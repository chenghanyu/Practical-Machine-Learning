# Quiz 2
# Question 1

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list = FALSE)
#Remember what we only put is y in this function, it will become a indicator 
training = adData[trainIndex, ]
testing = adData[-trainIndex, ]
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

par(mfrow = c(2, 1), mar = c(4,2,2,2))
hist(training$Superplasticizer, main = "", ylab = "")
hist(log(training$Superplasticizer + 1), main = "", ylab = "")



library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

head(training)
predName <- names(training)
ILpredictor <- predName[substr(predName, 1, 2) == "IL"]
preProcess(training[, ILpredictor], method = "pca", thresh = .9)

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[, c(ILpredictor, "diagnosis")]
testingIL <- testing[, c(ILpredictor, "diagnosis")]
ModelAll <- train(diagnosis ~ ., data = trainingIL, method = "glm")
confusionMatrix(testingIL$diagnosis, predict(ModelAll, testingIL))

# PCA
preProc <- preProcess(training[, ILpredictor], method = "pca", thresh = .8)
trainPC <- predict(preProc, training[, ILpredictor])
ModelPCA <- train(trainingIL$diagnosis ~ ., method = "glm", data = trainPC)
testPC <- predict(preProc, testing[, ILpredictor])
confusionMatrix(testingIL$diagnosis, predict(ModelPCA, testPC))


