# Load libraries
library(randomForest)
library(caret)

# load the data
filename="C:\\data\\diabetes_data.csv"
datasetRaw = read.csv(filename)
print(head(datasetRaw))

# clean the data
numColumns = dim(datasetRaw)[2]
vector_NAs = rep(0, numColumns)               
for (i in 1:numColumns) {
  vector_NAs[i] = sum(is.na(datasetRaw[,i]))
  
}
print("The missing values in each column:")
print(vector_NAs)

# delete columns 15 and 16 due to many missing values
# delete column 1 (id), column 7 (location) because they contain no useful information
dataset = datasetRaw[,-c(1,7,15,16)]
print(dim(dataset))

# remove the row with missing values
row.has.na <- apply(dataset, 1, function(x){any(is.na(x))})
dataset = dataset[!row.has.na,]
print(dim(dataset))
print(head(dataset))

# encode the class label (column 5): Glycosolated hemoglobin  > 7.0 is taken as a positive diagnosis of diabetes.
dataset[,5] = ifelse(dataset[,5] >= 7.0, 1, 0)
dataset[,5] = factor(dataset[,5]) # class label must be factor type

# encode the categorical data (column-7 gender)
dataset[,7] = ifelse(dataset[,7] == "female", 0, 1)
dataset[,7] = factor(dataset[,7])
# encode the categorical data (column-10 frame)
dataset[,10] = ifelse(dataset[,10] == "small", 0, ifelse(dataset[,10] == "medium", 1,2) )
dataset[,10] = factor(dataset[,10])

# split the data into training and validation sets
set.seed(7)
validation_index = createDataPartition(dataset$glyhb, p=0.90, list=FALSE)
validationData = dataset[-validation_index,]
trainingData = dataset[validation_index,]

# comparison among different classifiers
#1. Logistic Regression
set.seed(7)
control.glm = trainControl(method = "cv", number = 5)
fit.glm = train(glyhb~., data = trainingData, method = "glm", preProc = c("center","scale"), trControl = control.glm)
print(fit.glm$results) # accuracy = 0.9172205

#2. Support Vector Machine
set.seed(7)
control.svmRadial = trainControl(method="cv", number=5)
fit.svmRadial <- train(glyhb~., data=trainingData, method="svmRadial", metric="Accuracy", preProc=c("center","scale"), trControl=control.svmRadial)
# summarize fit
print(fit.svmRadial$results) #accuracy = 0.9231731

#3. random forest
control.rf = trainControl(method="cv", number=5)
set.seed(7)
metric = "Accuracy"
mtry = 7 # mtry=7 (number of variables to try)
tunegrid <- expand.grid(.mtry=mtry)
fit.rf_default <- train(glyhb~., data=trainingData, method="rf", metric=metric, tuneGrid=tunegrid, preProc=c("center","scale"), trControl=control)
print(fit.rf_default$results) # accuracy = 0.9114924

#4. parameter tunning via grid search for random forest
control.rf_search <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:15))
fit.rf_gridsearch <- train(glyhb~., data=trainingData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control.rf_search, ntree=1000)
print(fit.rf_gridsearch) # accuracy = 0.9204355 when mtry = 12
print(fit.rf_gridsearch$finalModel)
plot(fit.rf_gridsearch) 

# make predictions on the validation set
set.seed(7)
predictions = predict(fit.rf_gridsearch, newdata=validationData)
confusionMatrix = confusionMatrix(predictions, validationData$glyhb)
# confusion matrix
print(confusionMatrix$table)

# save the final classifier model into disk
saveRDS(fit.rf_gridsearch, "C:\\data\\diabetes_classification")
  
# load the model from the disk
final_model <- readRDS("C:\\data\\diabetes_classification")
print(final_model)
# make predictions using the loaded model
set.seed(7)
predictions = predict(final_model, newdata=validationData)


