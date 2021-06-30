abnb<- read.csv("D:\\dataset\\DMML\\ABNB1.csv",header = TRUE, stringsAsFactors = TRUE)
str(abnb)

#preprocessing the data
colSums(is.na(abnb))
abnb<-abnb[-1]

#preparing the data - to many categorical variables
abnb$name<-NULL
abnb$host_id<-NULL
abnb$neighbourhood<-NULL
abnb$reviews_per_month<-NULL
abnb$availability_365<-NULL
abnb$last_review<-NULL
abnb$longitude<-NULL
abnb$latitude<-NULL

str(abnb)

#Standardizing the dependent vector
abnb$price<-log10(abnb$price)

#visualizing prize based on neighborhood groups
library(dplyr)
library(ggplot2)
avg_price<-abnb %>% group_by(neighbourhood_group) %>% summarise(mean_price = mean(price))
ggplot(avg_price, aes(x = reorder(neighbourhood_group, -mean_price), y = mean_price)) + 
  geom_bar(stat="identity",colour="black", fill = "darkblue") + 
  labs(title="Average Price of Rooms in each Neighbourhood Group") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5)) + xlab("") + ylab("Mean Price")

#install.packages("biganalytics")
#library (bigmemory)
#library (biganalytics)    


#Data Partition for sampling, dividing the data for testing and training. Using 60% data for training.
set.seed(321)
sample1=sample(2,nrow(abnb),replace = TRUE,prob = c(0.8,0.2))
train=abnb[sample1==1,]
test=abnb[sample1==2,]

#on full data
X<-model.matrix(price~.,abnb)[,-1]
Y<-abnb$price

#training the data
X1<-model.matrix(price~.,train)[,-1]
Y1<-train$price

#test data
X2<-model.matrix(price~.,test)[,-1]
Y2<-test$price

#memory.size(max = FALSE)
#memory.limit(size = 860000)



#install.packages("e1071")
library(e1071)
#radial kernel
mod1<-svm(price~., data=train , type="nu-regression",kernel="radial",shrinking =TRUE)
mypred1<-predict(mod1, newdata = test)
#mypred1
#RMSE for radial SVM
#install.packages("Metrics")
library(caret)
library(Metrics)
accuracy(test$price,mypred1)
rmse(test$price,mypred1)


#Linear
mod2<-svm(price~.,data=train,type="nu-regression",kernel="linear",shrinking=TRUE)
mypred2<-predict(mod2, newdata=test)
#mypred2
#RMSE for linear SVM
rmse(test$price,mypred2)

#Polynomial
mod3<-svm(price~.,data=train,type="nu-regression",kernel="polynomial",shrinking=TRUE)
mypred3<-predict(mod3, newdata=test)
#mypred3
#RMSE for Polynomial SVM
rmse(test$price,mypred3)

#SigMod
mod4<-svm(price~.,data=train,type="nu-regression",kernel="sigmoid",shrinking=TRUE)
mypred4<-predict(mod4, newdata=test)
#mypred4
#RMSE for sigmoid SVM
rmse(test$price,mypred4)




#separating the neighbhourood groups since MSE is very high.
Manhattan=abnb[which(abnb$neighbourhood_group=="Manhattan"),]
Brooklyn=abnb[which(abnb$neighbourhood_group=="Brooklyn"),]
Bronx=abnb[which(abnb$neighbourhood_group=="Bronx"),]
Queens=abnb[which(abnb$neighbourhood_group=="Queens"),]
Staten_Island=abnb[which(abnb$neighbourhood_group=="Staten Island"),]


#################################### Manhattan data ################################################
train_man<-train[which(train$neighbourhood_group=="Manhattan"),]
test_man<-test[which(test$neighbourhood_group=="Manhattan"),]
#training data for Mahattan
x1=model.matrix(price~.,train_man)[,-1]
y1=train_man$price
#test data for Manhattan
x2=model.matrix(price~.,test_man)[,-1]
y2=test_man$price


man1=svm(price~., data =train_man , type="nu-regression",kernel="radial", shrinking = T )
pred1=predict(man1, newdata = test_man)
#RMSE for radial SVM
value1=rmse(test_man$price,pred1)
print(c("SVM-radial",value1))

man2=svm(price~., data =train_man , type="nu-regression",kernel="linear", shrinking = T )
pred2=predict(man2, newdata = test_man)
#MSE for linear SVM
value1=rmse(test_man$price,pred2)
print(c("SVM-linear",value1))

man3=svm(price~., data =train_man , type="nu-regression",kernel="polynomial", shrinking = T )
pred3=predict(man3, newdata = test_man)
#MSE for polynomial SVM
value1=rmse(test_man$price,pred3)
print(c("SVM-polynomial",value1))

man4=svm(price~., data =train_man , type="nu-regression",kernel="sigmoid", shrinking = T )
pred4=predict(man4, newdata = test_man)
#MSE for sigmoid SVM
value1=rmse(test_man$price,pred4)
print(c("SVM-sigmoid",value1))


best.pred.man = as.matrix(pred1)
best.pred.man
########################################################################################################

################################### Brooklyn ###########################################################

train_brook = train[which(train$neighbourhood_group=="Brooklyn"),]
test_brook = test[which(test$neighbourhood_group=="Brooklyn"),]
#training data
x1=model.matrix(price~.,train_brook)[,-1]
y1=train_brook$price
#test data
x2=model.matrix(price~.,test_brook)[,-1]
y2=test_brook$price


brok1=svm(price~., data =train_brook , type="nu-regression",kernel="radial", shrinking = T )
pred1=predict(brok1, newdata = test_brook)
#MSE for radial SVM
g=rmse(test_brook$price,pred1)
print(c("SVM-radial",g))
  
brok2=svm(price~., data =train_brook , type="nu-regression",kernel="linear", shrinking = T )
pred2=predict(brok2, newdata = test_brook)
#MSE for linear SVM
g=rmse(test_brook$price,pred2)
print(c("SVM-linear",g))
  
brok3=svm(price~., data =train_brook , type="nu-regression",kernel="polynomial", shrinking = T )
pred3=predict(brok3, newdata = test_brook)
#MSE for polynomial SVM
g=rmse(test_brook$price,pred3)
print(c("SVM-polynomial",g))
#sigmoid kernel
brok4=svm(price~., data =train_brook , type="nu-regression",kernel="sigmoid", shrinking = T )
pred4=predict(brok4, newdata = test_brook)
#MSE for sigmoid SVM
g=rmse(test_brrok$price,pred4)
print(c("SVM-sigmoid",g))

best.pred.brook = as.matrix(pred1)
best.pred.brook
##########################################################################################################

############################################Bronx########################################################
train_brx = train[which(train$neighbourhood_group=="Bronx"),]
test_brx = test[which(test$neighbourhood_group=="Bronx"),]
#training data
x1=model.matrix(price~.,train_brx)[,-1]
y1=train_brx$price
#test data
x2=model.matrix(price~.,test_brx)[,-1]
y2=test_brx$price


brx1=svm(price~., data =train_brx , type="nu-regression",kernel="radial", shrinking = T )
pred1=predict(brx1, newdata = test_brx)
#MSE for radial SVM
g=rmse(test_brx$price,pred1)
print(c("SVM-radial",g))
#linear kernel
brx2=svm(price~., data =train_brx , type="nu-regression",kernel="linear", shrinking = T )
pred2=rmse(test_brx1$price,pred2)
#MSE for linear SVM
g=rmse(test_brx$price,pred1)
print(c("SVM-linear",g))
#polynomial kernel
brx3=svm(price~., data =train_brx , type="nu-regression",kernel="polynomial", shrinking = T )
pred3=predict(brx3, newdata = test_brx)
#MSE for polynomial SVM
g=rmse(test_brx$price,pred3)
print(c("SVM-polynomial",g))
#sigmoid kernel
brx4=svm(price~., data =train_brx , type="nu-regression",kernel="sigmoid", shrinking = T )
pred4=predict(brx4, newdata = test_brx)
#MSE for sigmoid SVM
g=rmse(test_brx$price,pred4)
print(c("SVM-sigmoid",g))

best.pred.brx= as.matrix(pred2)
best.pred.brx

######################################################################################################
##################################### Queens #########################################################

train_queen = train[which(train$neighbourhood_group=="Queens"),]
test_queen = test[which(test$neighbourhood_group=="Queens"),]
#training data
x1=model.matrix(price~.,train_queen)[,-1]
y1=train_queen$price
#test data
x2=model.matrix(price~.,test_queen)[,-1]
y2=test_queen$price



qu1=svm(price~., data =train_queen , type="nu-regression",kernel="radial", shrinking = T )
pred1=predict(qu1, newdata = test_queen)
#RMSE for radial SVM
g=rmse(test_queen$price,pred1)
print(c("SVM-radial",g))

qu2=svm(price~., data =train_queen , type="nu-regression",kernel="linear", shrinking = T )
pred2=predict(qu2, newdata = test_queen)
#RMSE for linear SVM
g=rmse(test_queen$price,pred2)
print(c("SVM-linear",g))

qu3=svm(price~., data =train_queen , type="nu-regression",kernel="polynomial", shrinking = T )
pred3=predict(qu3, newdata = test_queen)
#RMSE for polynomial SVM
g=rmse(test_queen$price,pred3)
print(c("SVM-polynomial",g))

qu4=svm(price~., data =train_queen , type="nu-regression",kernel="sigmoid", shrinking = T )
pred4=predict(qu4, newdata = test_queen)
#RMSE for sigmoid SVM
g=rmse(test_quuen$price,pred4)
print(c("SVM-sigmoid",g))

best.pred.queen = as.matrix(pred1)
best.pred.queen

#####################################################################################################################################
############################################## Staten_Island ########################################################################

train_SI = train[which(train$neighbourhood_group=="Staten Island"),]
test_SI = test[which(test$neighbourhood_group=="Staten Island"),]
#training data
x1=model.matrix(price~.,train_SI)[,-1]
y1=train_SI$price
#test data
x2=model.matrix(price~.,test_SI)[,-1]
y2=test_SI$price


SI1=svm(price~., data =train_SI , type="nu-regression",kernel="radial", shrinking = T )
pred1=predict(SI1, newdata = test_SI)
#RMSE for radial SVM
g=rmse(test_SI$price,pred1)
print(c("SVM-radial",g))

SI2=svm(price~., data =train_SI , type="nu-regression",kernel="linear", shrinking = T )
pred2=predict(SI2, newdata = test_SI)
#RMSE for linear SVM
g=rmse(test_SI$price,pred2)
print(c("SVM-linear",g))

SI3=svm(price~., data =train_SI , type="nu-regression",kernel="polynomial", shrinking = T )
pred3=predict(SI3, newdata = test_SI)
#RMSE for polynomial SVM
g=rmse(test_SI$price,pred3)
print(c("SVM-polynomial",g))

SI4=svm(price~., data =train_SI , type="nu-regression",kernel="sigmoid", shrinking = T )
pred4=predict(SI4, newdata = test_SI)
#RMSE for sigmoid SVM
g=rmse(test_SI$price,pred4)
print(c("SVM-sigmoid",g))

best.pred.SI = as.matrix(pred2)
best.pred.SI

#############################################################################################################

#new set of predictions considering all the pred
new_pred = rbind(best.pred.man,best.pred.brook,best.pred.brx,best.pred.queen,best.pred.SI)
#new test set
new_test = rbind(test_man,test_brook,test_brx,test_queen,test_SI)
new_mse = rmse(new_test$price,new_pred)
new_mse


###############################################################################################################

###############optimized Linear regression using Lasso Regression####################
#install.packages("glmnet")

library(glmnet)
library(Metrics)
lasso_model<-glmnet(train,test)
plot(lasso_model,xvar = "norm")

set.seed(321)
cv_fit <- cv.glmnet(X1,Y1, alpha = 1)
plot(cv_fit)


#lambda.min
cv_fit$lambda.min
fit.min <- glmnet(X1, Y1, alpha = 1, lambda=cv_fit$lambda.min , shrinking = T)
coef(fit.min)

#lambda.1se
cv_fit$lambda.1se
fit.1se <- glmnet(X1, Y1, alpha = 1, lambda=cv_fit$lambda.1se, shrinking = T)
coef(fit.1se)


#RMSE for Lasso
lasso_pred<-predict(fit.min,s=cv_fit$lambda.1se,newx=X2)
rmse(Y2,lasso_pred)

library(caret)
table(test$price,lasso_pred)

