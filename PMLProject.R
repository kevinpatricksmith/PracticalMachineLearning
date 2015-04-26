set.seed(42)
training <- read.csv('pml-training.csv', na.strings=c("NA","#DIV/0!",""))
testing <- read.csv('pml-testing.csv', na.strings=c("NA","#DIV/0!",""))

dim(training)
str(training)
summary(training)

## first seven cols not useful
# X                       : int  1 2 3 4 5 6 7 8 9 10 ...
# user_name               : Factor w/ 6 levels "adelmo","carlitos",..: 2 2 2 2 2 2 2 2 2 2 ...
# raw_timestamp_part_1    : int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
# raw_timestamp_part_2    : int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
# cvtd_timestamp          : Factor w/ 20 levels "02/12/2011 13:32",..: 9 9 9 9 9 9 9 9 9 9 ...
# new_window              : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
# num_window         

training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]

## Find columns with near zero Var and ignore them
library("caret")
nzv <- nearZeroVar(training, saveMetrics=TRUE)
nzvList <- rownames(nzv)[nzv$nzv==TRUE]

training   <- training  [!names(training) %in% nzvList]
testing    <- testing   [!names(testing) %in% nzvList]

## Find columns with any NAs and ignore them
colsWithNa <- sapply(training, function (x) any(is.na(x) | x == ""))
focusCols <- names(colsWithNa)[!colsWithNa]
focusCols <- focusCols[-53]
    
training   <- training  [, c("classe", focusCols)]
testing    <- testing   [, c("problem_id", focusCols)]

names(training)
names(testing)

str(training)
str(testing)

## Modeling

inTrain = createDataPartition(y=training$classe, p=0.65, list=FALSE)
trainingPart   = training[inTrain,]
validationPart = training[-inTrain,]
dim(trainingPart)
dim(validationPart)

## Train RF Model
library(randomForest)
rfModel = train(classe~., method="rf", data=trainingPart)

saveRDS(rfModel, "rfModel.RDS")
rfModel = readRDS("rfModel.RDS")
rfModel

## Check on training partition which was used for training
predTrain <- predict(rfModel, trainingPart)
confusionMatrix(predTrain, trainingPart[, "classe"])

## Check on Validation partition which was not used for training
predValid <- predict(rfModel, trainingPart)
confusionMatrix(predValid, trainingPart[, "classe"])

## Predictions on course project testing set
predTest <- predict(rfModel, testing)
testing <- cbind(hatTrain , testing)


#```{r}
#DTestCS <- predict(preProc, DTest[, predCandidates, with=FALSE])
hatTrain <- predict(model, testing)
testing <- cbind(hatTrain , testing)
subset(DTest, select=names(DTest)[grep("belt|[^(fore)]arm|dumbbell|forearm", names(DTest), invert=TRUE)])
```

