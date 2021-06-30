#loading required libraries
#libraries for data cleaning
library(dplyr)
library(tidyverse)

#libraries for Visualization
library(ggplot2)
library(ggthemes)
library(corrplot)

#library for modelling and evaluation
library(caTools)
library(Metrics)
library(car)
library(caret)

#read the file - collecting the data
abnb<-read.csv("D:\\dataset\\DMML\\AB_NYC_2019-1.csv",header = TRUE)
head(abnb)
str(abnb)
summary(abnb)
dim(abnb)  # 38843 rows and 16 columns


#data-preprocessing 
#data Cleaning
colSums(is.na(abnb))
#remove columns which are not required for analysis
abnb<- abnb [,!(names(abnb) %in% c("id","name", "host_id", "host_name", "neighbourhood", "latitude", "longitude", "last_review", "name"))]

summary(abnb)#10052 NA's observed in reiews per month


#Cleaning NA's by the adding the median value
bnb<-abnb# retaining the orginal data
bnb$reviews_per_month<-ifelse(is.na(bnb$reviews_per_month),mean(bnb$reviews_per_month, na.rm = T),bnb$reviews_per_month)

summary(bnb)
str(bnb)

#Determining Correlation
x<- bnb[,!(names(bnb) %in% c("room_type","neighbourhood_group"))]
str(x)
x$price<-as.numeric(x$price)
#x$neighbourhood_group<-as.numeric(x$neighbourhood_group)
#x$room_type<-as.numeric(x$room_type)
x$minimum_nights<-as.numeric(x$minimum_nights)
x$number_of_reviews<-as.numeric(x$number_of_reviews)
x$reviews_per_month<-as.numeric(x$reviews_per_month)
x$calculated_host_listings_count<-as.numeric(x$calculated_host_listings_count)
x$availability_365<-as.numeric(x$availability_365)

s<-cor(x)
corrplot(s, method = "square" ,order="hclust" )


#some visudalization of avg price based on neighbourhood
avg_price<-abnb %>% group_by(neighbourhood_group) %>% summarise(mean_price = mean(price))
ggplot(avg_price, aes(x = reorder(neighbourhood_group, -mean_price), y = mean_price)) + 
  geom_bar(stat="identity",colour="black", fill = "darkblue") + 
  labs(title="Average Price of Rooms in each Neighbourhood Group") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8, 0.5)) + xlab("") + ylab("Mean Price")


#visualization is made on price based on room_type
ggplot(abnb, aes(x = room_type, y = price, col = room_type)) + geom_jitter(alpha = 0.5) + 
geom_hline(yintercept = mean(abnb$price), size = 1, alpha = 0.8) +
  geom_segment(aes(x = 1.325, y = 5000, xend = 1.3, yend = mean(price) + 100, colour = 'segment'), data = abnb,
               arrow = arrow(length = unit(0.03, 'npc')), size = 1.25) +
  annotate('text', x = 1.3, y = 5250, label = 'Mean price = $152') 
# > we understand that almost most price point are well below 1250 

#Boxplot for outliers 
boxplot(abnb$price, abnb$minimum_nights, abnb$number_of_reviews,abnb$availability_365)
#price is heavily skewed with high variance 
var(abnb$price)

#data Modelling
# MULTIPLE LINEAR REGRESSION

set.seed(1023)
#taking care of outliers
quantile(bnb$price) # Get q1 and q3 values 
price_IQR <- 106 # Inter quartile range / median
low_outliers <- 69 - (1.5*price_IQR) # Set lower bound
higher_outliers <- 175 + (1.5*price_IQR) # Set upper bound

no_outliers <-bnb[bnb$price > low_outliers & bnb$price < higher_outliers, ] # Exclude outliers in data set

msample <- sample.split(no_outliers$price, SplitRatio = 0.75)# considering 75% data for training rest 25% will used for testing
train_data<- subset(no_outliers,msample==T)
test_data<-subset(no_outliers,msample==F)
nrow(train_data)
nrow(test_data)

#Applying linear model

Mmodel<-lm(price~., data = train_data)
mypred<-predict(Mmodel, newdata=test_data, type="response")
summary(Mmodel)

#evaluation of the model by RMSE
rmse(test_data$price,mypred)

vif(Mmodel)
durbinWatsonTest(Mmodel)
par(mfrow=c(2,2))
plot(Mmodel)


########################################################################################################################
########################################################################################################################
########################################### ~ RANDOM FOREST ~ ####################################################

library(randomForest)
library(caret)
library(e1071)

RF_bnb <- randomForest(price~., data=train_data,mtry = 2, ntree=500,method="cv", importance=T,)#condier the train and test from the above
RF_bnb
varImpPlot(RF_bnb)
no_outliers_varimpt <- importance(RF_bnb)

#predict and evaluate the model
RF_predict <- predict(RF_bnb, newdata = test_data)
#RMSE to evualte the model
rmse(test_data$price,RF_predict)


#Both models suggest that the type of room, neighbourhood are the best predictors of the model.

#out of the two model Random forest provided a low variance , which depicts to be the best model. 
#Still, in the real world, such price extremities, although rare, should not be ignored as it is possible 
#for airbnb owners to deliberately list the accommodation at an extravagant price

