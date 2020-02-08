<<loadlibrary, message=F>>=
library(tidyverse) #data manipulation
library(dplyr)    #data summary
library(ggpubr)   #data visualisation
@

Importing mushroom dataset from Kaggle

<<DataLoad>>=
df <- read.csv("mushrooms.csv")
@

<<Data_Summary>>=
str(df)
summary(df)
@
Findings:
\begin{itemize}
\item There are 23 variables and 8124 observations.
\item "Veil.type" variable is a constant.
\item 4208 edible and 3916 poisonous which mean this is a balanced classification dataset.
\item Missing value are mark as "?" instead of N/A
\end{itemize}

Because veil-type variable is constant and does not influence the accuracy of classification model, it will be dropped.
<<RemoveVeilType>>=
df <- subset(df, select = -c(veil.type))
@
Convert "?" to NA and drop
<<DropNA>>=
df[ df == "?" ] <- NA
df <- na.omit(df)
@
<<CheckNA>>=
sum(is.na(df)) # return number of missing data 
@

Dropping unused levels from factor
<<DropUnused,message=F>>=
library(gdata)
df <- drop.levels(df)
@

<<showDF>>=
glimpse(df)
@

<<Splitdataset>>=
# Split the data in training and testing  with ratio 70:30
set.seed(100)
inx = sample(nrow(df), round(nrow(df) * 0.7)) 
train = df[inx,] 
test = df[-inx,]
@

<<RatioTrain>>=
# Training Data
table(train$class)/nrow(train)  
@
<<RatioTest>>=
# Testing Data
table(test$class)/nrow(test)  
@

<<RandomForest100>>=
library("randomForest")
library("caret")
start.time <- Sys.time() #runtime
RFensemble100 <- randomForest(class ~ ., data=train, ntree=100)
end.time <- Sys.time()
time.taken <- end.time - start.time

prediction_for_table <- predict(RFensemble100,test[,-1])
confusionMatrix(test$class, prediction_for_table)
@

<<>>=
RFensemble100
plot(RFensemble100)
@
<<>>=
varImpPlot(RFensemble100)
time.taken
@
As can be seen, after ntree = 20, prediction error has come to 0. On the next test, a tuned ntree parameter are performed.
<<>>=
start.time <- Sys.time() #runtime
RFensemble20 <- randomForest(class ~ ., data=train, ntree=20)
end.time <- Sys.time()
time.taken <- end.time - start.time


prediction_for_table <- predict(RFensemble20,test[,-1])
confusionMatrix(test$class, prediction_for_table)
@
<<>>=
time.taken
@

<<DecisionTree>>=
library(rpart)
dt <- rpart(class ~ ., method = "class", control = rpart.control(minsplit = 1), data = df)
library(rpart.plot)

prp(dt, main="Decision Tree", box.palette = "auto", faclen = 0)
library(FSelector)
information.gain(class~., df)
@

#Naive-Bayes
library("klaR") 
  
# Train the model 
start.time <- Sys.time() #runtime
model<- NaiveBayes(class~., train, laplace = 1)
end.time <- Sys.time()

#Apply train model on test set
prd<-predict(model, test[,-1])

time.taken <- end.time - start.time

#Confusion Matrix
confusionMatrix(test$class, prd$class)

#Time taken
time.taken
@

<<KNN>>=
#KNN
library(knncat)
start.time <- Sys.time() #start time
knn_test <- knncat(train) #Train model
end.time <- Sys.time()  #end time
time.taken <- end.time - start.time
synpred <- predict (knn_test, train , test)
#Confustion matrix
confusionMatrix(synpred, test$class)
#Time taken
time.taken
@
