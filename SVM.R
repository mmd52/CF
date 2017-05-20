# SVM Trial on Normal data

ndat<-read.csv("DATA/High Dispensing ATMs.csv",header = T)
View(head(ndat))
ndat<-ndat[,-1]
View(head(ndat))

set.seed(999)
#Splitting data into training and testing
train<-sample(1:66761,46733,replace = F)
test<--train

training_data<-ndat[train,]
testing_data<-ndat[test,]

mod<-lm(Dispense ~ ., data = training_data)
rmse(testing_data[,7],predict(mod, testing_data[,-7]))

##======================SIMPLE SVM
model1.svm <- svm(Dispense ~ ., data = training_data)
preds = predict(model1.svm, testing_data[,-7])
caret::confusionMatrix(testing_data[,7], preds, mode = "prec_recall")
#==85.8% Accuracy

auc<-roc(as.numeric(testing_data[,7]),as.numeric(predict(model1.svm, testing_data[,-7])))
print(auc)
plot(auc,print.auc=T)

rmse(testing_data[,7],predict(model1.svm, testing_data[,-7]))
plot(testing_data[,7],type="l")
plot(preds,type="l")

#=======================================================================================================================


train_test<-read.csv("DATA/High_D_numeric.csv",header = T)
View(head(ndat))
ndat<-ndat[,-1]
View(head(ndat))

set.seed(999)
#Splitting data into training and testing
train<-sample(1:66761,46733,replace = F)
test<--train

training_data<-train_test[train,]
testing_data<-train_test[test,]


##======================SIMPLE SVM
model1.svm <- svm(Dispense ~ ., data = training_data)
preds = predict(model1.svm, testing_data[,-7])

auc<-rmse(as.numeric(testing_data[,7]),as.numeric(predict(model1.svm, testing_data[,-7])))
print(auc)
plot(auc,print.auc=T)
rmse(testing_data[,7],predict(model1.svm, testing_data[,-7]))
