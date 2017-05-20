train_test<-read.csv("DATA/High_D_numeric.csv",header = T)
View(head(train_test))
train_test<-train_test[,-c(1)]
View(head(train_test))

set.seed(999)
#Splitting data into training and testing
train<-sample(1:66761,46733,replace = F)
test<--train

training_data<-train_test[train,]
testing_data<-train_test[test,]


View(head(training_data))
#===================================================================
################################## Random Forest

# Tuning takes factors as target variables
bestmtry <- tuneRF(training_data[,-c(7)], as.factor(training_data[,7]), 
                   ntreeTry=100, stepFactor=1.5, improve=0.01,
                   trace=TRUE, plot=TRUE, dobest=FALSE) 


rf.fit <- randomForest(Dispense~ ., data=training_data, 
                       mtry=3, ntree=100, keep.forest=TRUE, 
                       importance=TRUE,fold=10) 

varImpPlot(rf.fit)

rm<-rmse(testing_data[,7],predict(rf.fit,testing_data[,-7]))
print(rm)