library(caret)
library(rpart)

training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
#cleaning
NZV <- nearZeroVar(training, saveMetrics=TRUE)
training <- training[, !NZV$nzv]; testing <- testing[,!NZV$nzv]
training <- training[,6: length(training)]; testing <- testing[,6: length(testing)]
cond <- (colSums(is.na(training)) == 0)
training <- training[, cond]
testing <- testing[, cond]
#partition
inTrain <- createDataPartition(training$classe, p = 0.70, list = FALSE)
training <- training[inTrain, ]
validation <- training[-inTrain, ]

#treemodeling
modFit <- train(classe~., method = "rpart", data = training)
#plot
plot(modFit$finalModel, uniform = TRUE, main = "the tree graph")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex=.8)
#predicting
pred <- predict(modFit,newdata=validation)
#checking the prediction
cfMatx <-confusionMatrix(factor(validation$classe), pred)
#randomforests
modRF <- train(classe ~ ., data = training, method = "rf", trControl = trainControl(method = "cv", 5), ntree = 250)
predRF <- predict(modRF, validation)
cfMatxRF <- confusionMatrix(factor(validation$classe), predRF)
#predict testing with RF
pred_testing <- predict(modRF, newdata = testing[, -length(names(testing))])