#Random Forest 
library(rpart.plot)
library(rpart)
library(party)
library(dplyr)
library(rsample)
#Using the COVID-19 pandemic dataset 
 check2 
#check2 contains the dataset

set.seed(123)
split<-initial_split(check2,prop = 0.7)
check_train<-training(split)
check_test<-training(split)
                     
#Decision Tree
tree<-rpart(formula=Deaths~.,method="anova",data=check_train,control = list(minsplit = 10, maxdepth = 12, xval = 10))
rpart.plot(tree)
plotcp(tree)

tree2<-ctree(formula=Deaths~Confirmed+Recovered+Active,data=check_train)
plot(tree2)

rpart.plot(tree)

#RandomForest

#Building Model using RandomForest
rf.model<-randomForest(Deaths~.,data=check2)
rf.model$ntree
rf.model$mse
summary(rf.model)


#Predicted V/S Actual
AvP<-cbind(rf.model$predicted,check2$Deaths)
colnames(AvP)<-c("Predicted","Actual")

print(head(AvP))

importance(rf.model)
