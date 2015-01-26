---
title: "Practical Machine Learning Coursera-JHU"
author: "Pere Gadea"
date: "Sunday, January 25, 2015"
output: html_document
---

### Executive Summary
Human Activity Recognition can be systematized through specific data collection devices. 
In this paper, we have data from monitoring devices (more info <http://groupware.les.inf.puc-rio.br/har>), and the aim is get a model that identifies the kind of activity according to the values of the variables.
A random forest model with an accuracy rate of 99.64% is finally selected.

### Introduction & Strategy

First, analyze the variables of the database in order to reduce their number by various techniques.
Subsequently, we will analyze the data according to different models, and finally, select the best performing model and reach conclusions.


### Exploration

First, load the dataset
```{r load, results='hide', eval=TRUE, echo=TRUE, cache=TRUE}
data <- read.csv("pml-training.csv", header=T)
```
We take a look at the variables of the dataset, and determine if any changes are needed.
```{r explore_data, results='hide',echo=TRUE}
str(data)
summary(data)
```   

First, we analyze the existence of NA values, and if we find that for some variables there are plenty of NA values, discard the variable.   
```{r NA, eval=TRUE, results='hide'}
na_validation = sapply(data, function(x) {sum(is.na(x))})
table(na_validation)                            
delete_NA = names(na_validation[na_validation==19216])
data = data[, !names(data) %in% delete_NA]
```   
We removed 67 features.

It is also interesting to delete columns that do not provide information to the accuracy of the model, such as those relating to the identification of subjects, so, we must delete the first 7 variables.
```{r id, eval=TRUE, results='hide'}
data <- data[-c(1:7)]
```

I decide to check whether the variability of other features is significant, and if not, remove the dataset variables. 
```{r zeroVar, eval=TRUE, results='hide', cache=TRUE}
library(caret); suppressMessages(library(caret))
trainZero <- nearZeroVar(data)            
data <- data[-trainZero]
```
33 variables are deleted.


### Model selection   

We split the data into two subsets: a database for model (train) and one for validation (test).
```{r partition, eval=TRUE, results='hide', echo=TRUE, cache=TRUE}
set.seed(0)
inTrain = createDataPartition(y=data$classe, p=0.7, list=FALSE)
train = data[inTrain,]
test = data[-inTrain,]
```

Considering the large number of variables, first check the linear discriminant analysis. We try find a linear combination of features which characterizes classes. 
```{r lda, cache=TRUE, eval=TRUE, echo=TRUE}
library(MASS)
modLda <- train(classe ~ ., data=train, method="lda")
preLda <- predict(modLda, test)
modLda
```
An 69% accuracy is achieved with this model. It is not an acceptable outcome.

Second, we try a random forest model, which usually performs a better fit.
```{r randomforest, cache=TRUE, eval=TRUE, echo=TRUE}
library(randomForest)
modForest <- randomForest(classe ~ ., data=train)
predForest <- predict(modForest, test)

confusionMatrix(predForest, test$classe)
```
In this model, the accuracy is 99.64%, clearly a good fit.

We get teh features with more impact in the model, and plot some graphics.
```{r visualization, eval=TRUE}
library(caret)
principals <- order(varImp(modForest), decreasing=T)
names(test[principals])[1:4]
qplot(test$roll_belt, test$yaw_belt, colour= predForest == test$classe, data=test)
qplot(test$pitch_forearm, test$magnet_dumbbell_z, colour= predForest == test$classe, data=test)
```

### Conclusions
Finally, we choose the Random Forest model over the Linear Discriminant Analysis, because it achieves better results.
When the model considers the 20 blind records of Coursera-Control, get a 100% accuracy. The model has a CI with 95% [0.9946, 0.9978]) in terms of results, with a 0.5% Error Rate, and a Kappa of 99.55%.

### Cite
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13). Stuttgart, Germany: ACM SIGCHI, 2013.

Read more: <http://groupware.les.inf.puc-rio.br/har#ixzz3PrbGnPw5>

### Annex
To validate the model with control data and refer to Coursera.

``` {r annex, results='hide', eval=FALSE}
submissionTesting <- read.csv("pml-testing.csv", header=T)

answers <- predict(modForest, submissionTesting)

pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}

pml_write_files(answers)
```
