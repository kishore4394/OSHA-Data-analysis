cat("\014") #clear the console
rm(list=ls()) #dclearing the environment variables

setwd("~/Documents/Fall 2017/Data mining")

library(e1071) #for using svm model
library(caret)
library(randomForest)
library(xgboost)
library(doSNOW)
library(plyr)
library(party)
#devtools::install_github('topepo/caret/pkg/caret')

dataset = read.csv("preprocessed_data.csv")
prediction_dataset = subset(dataset,select = c(City,City_Code,State,State_Code,Zip,Latitude,Longitude,Nature,Hospitalized,Part.of.Body.Title,Primary.NAICS,Industrycodes,Event,Amputation))
Amputation_dataset = subset(prediction_dataset, prediction_dataset$Amputation == "1" | prediction_dataset$Amputation =="2" | prediction_dataset$Amputation =="3" | prediction_dataset$Amputation =="4" | prediction_dataset$Amputation == "9")
Amputation_dataset$Amputation = as.numeric(Amputation_dataset$Amputation)
Amputation_dataset$Part.of.Body.Title = as.numeric(Amputation_dataset$Part.of.Body.Title)
#x = Amputation_dataset[,c("City_Code","State_Code","Latitude","Longitude","Zip","Industrycodes","Event","Primary.NAICS","Nature","Part.of.Body.Title","Hospitalized")]
#y = Amputation_dataset[,c("Amputation")]
#cor(x,y,use = "complete.obs",method = "kendall")

# creating validation dataset
# 10% of data for validation and 90% data fro training and testing dataset

# Index = createDataPartition(Amputation_dataset$Amputation, p = 0.9, list = FALSE)
# validation_dataset = Amputation_dataset[-index,]
# Amputation_dataset = Amputation_dataset[index,]
# 
# sapply(Amputation_dataset, class)

Amputation_dataset$Zip = as.numeric(Amputation_dataset$Zip)
Amputation_dataset$Primary.NAICS = as.numeric(Amputation_dataset$Primary.NAICS)
Amputation_dataset$Industrycodes = as.numeric(Amputation_dataset$Industrycodes)

Amputation_dataset$Event = as.numeric(Amputation_dataset$Event)
Amputation_dataset$Amputation = as.numeric(Amputation_dataset$Amputation)
Amputation_dataset$City = as.factor(Amputation_dataset$City)
Amputation_dataset$State = as.factor(Amputation_dataset$State)
Amputation_dataset$Amputation = as.factor(Amputation_dataset$Amputation)

percentage_of_distribution = prop.table(table(Amputation_dataset$Amputation)) * 100
summary(Amputation_dataset)

#split_index = round(0.6*nrow(Amputation_dataset))
split_index = createDataPartition(y = Amputation_dataset$Amputation, p = 0.7, list = FALSE)
train_data = Amputation_dataset[split_index,]
test_data = Amputation_dataset[-split_index,]

train_Variables = train_data[,c("City_Code","State_Code","Zip","Nature","Industrycodes","Event","Amputation")]
test_Variables = test_data[,c("City_Code","State_Code","Zip","Nature","Industrycodes","Event","Amputation")]

anyNA(train_Variables)
anyNA(test_Variables)
#plot(train_Variables,train_labels,pch=16)

set.seed(54321)

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3033)

#http://topepo.github.io/caret/train-models-by-tag.html#Support_Vector_Machines
#the above link has the type of machine learning algorithm to be used 
#model_svm = caret::train(Amputation ~ Industrycodes, data = train_Variables, method = "svmLinear", trControl = trctrl, preProcess = c("center","scale"), tuneLength = 10)
model_svm = caret::train(Amputation ~ ., data = train_Variables, method = "svmLinear", trControl = trctrl, preProcess = c("center","scale"), tuneLength = 10)
#model_svm
testing_prediction = predict(model_svm, newdata = test_Variables)
#testting_prediction
confusionMatrix(testing_prediction, test_Variables$Amputation)

#######################################################################################

split_index = createDataPartition(y = dataset$Amputation, p = 0.7, list = FALSE)
train_data = dataset[split_index,]
test_data = dataset[-split_index,]

train_Variables = train_data[,c("City_Code","State_Code","Zip","Nature","Industrycodes","Event","Amputation")]
test_Variables = test_data[,c("City_Code","State_Code","Zip","Nature","Industrycodes","Event","Amputation")]

anyNA(train_Variables)
anyNA(test_Variables)
#plot(train_Variables,train_labels,pch=16)

set.seed(54321)

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3033)

#http://topepo.github.io/caret/train-models-by-tag.html#Support_Vector_Machines
#the above link has the type of machine learning algorithm to be used 
model_svm = caret::train(Amputation ~ Industrycodes, data = train_Variables, method = "svmLinear", trControl = trctrl, preProcess = c("center","scale"), tuneLength = 10)
#model_svm = caret::train(Amputation ~ ., data = train_Variables, method = "svmLinear", trControl = trctrl, preProcess = c("center","scale"), tuneLength = 10)
#model_svm
testing_prediction = trunc(predict(model_svm, newdata = test_Variables))
#testting_prediction
confusionMatrix(testing_prediction, test_Variables$Amputation)


#####################################################################################

# tune.grid <- expand.grid(eta = 0.3,
#                          nrounds = 100,
#                          max_depth = 6:8,
#                          min_child_weight = 1,
#                          colsample_bytree = c(0.5,0.75,0.9),
#                          gamma = 0,
#                          subsample = 1)
# #View(tune.grid)
# cl <- makeCluster(10, type = "SOCK")
# registerDoSNOW(cl)
#model_caret = caret::train(Amputation ~ Industrycodes, data = train_Variables, method = "M5",trControl = trctrl, tuneLength = 10)
model_caret = caret::train(Amputation ~ ., data = train_Variables, method = "M5",trControl = trctrl, tuneLength = 10)
testing_prediction = round(predict(model_caret, newdata = test_Variables))
testing_prediction[testing_prediction==-1] = 0
confusionMatrix(testing_prediction, test_Variables$Amputation)
























