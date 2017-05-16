#Preprocessing

ndat<-read.csv("DATA/High Dispensing ATMs.csv",header = T)
View(head(ndat))
summary(ndat)

train_test<-ndat
features = names(train_test)
for (f in features) {
  if (class(train_test[[f]])=="factor") {
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.numeric(as.integer(factor(train_test[[f]], levels=levels)))
  }
}

View(head(train_test))
train_test<-train_test[,-2]
View(head(train_test))

write.csv(train_test,"DATA/High_D_numeric.csv")