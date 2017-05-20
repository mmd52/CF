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


#====================================================================
######################## Preparing for xgboost

dtrain = xgb.DMatrix(as.matrix(training_data[,-7]), 
                     label=training_data[,7])
dtest = xgb.DMatrix(as.matrix(testing_data[,-7]))

xgb_param_adult = list(
  nrounds = c(700),
  eta = 0.057,#eta between(0.01-0.2)
  max_depth = 4, #values between(3-10)
  subsample = 0.7,#values between(0.5-1)
  colsample_bytree = 0.7,#values between(0.5-1)
  num_parallel_tree=1,
  objective='reg:linear',
  min_child_weight = 1,
  booster='gbtree'
)

res = xgb.cv(xgb_param_adult,
             dtrain,
             nrounds=700,   # changed
             nfold=10,           # changed
             early_stopping_rounds=50,
             print_every_n = 10,
             verbose= 1)

xgb.fit = xgb.train(xgb_param_adult, dtrain, 500)



rm<-rmse(testing_data[,7],predict(xgb.fit,dtest))
print(rm)
