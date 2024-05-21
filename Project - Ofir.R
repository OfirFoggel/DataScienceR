# installing all packages and loading relevant libraries

install.packages('forecast')
install.packages('caret') # this is the relevant ml package
install.packages('e1071', dependencies=TRUE) 
install.packages("rpart.plot")
library(caret)
library('e1071')
library(forecast)
library(rpart)
library(rpart.plot)



# reading our training and validation data

setwd("C:/Users/ofogg/OneDrive/שולחן העבודה/R Course")
cross.raw.data <-  read.csv("cross_2022C.csv")
holdout.data <-  read.csv("holdout_2022C.csv")

#some variable learning and data processing

head(cross.raw.data)

# the frequency is monotonically decreasing with the age

table(cross.raw.data$age) 
hist(cross.raw.data$age)

# the income is ordinal categorical variable

table(cross.raw.data$income)
test.income.df <- aggregate(donation ~ income, data = cross.raw.data, FUN = "mean")
plot(test.income.df[,2] ~ test.income.df[,1], data = test.income.df)
cross.raw.data$income <- as.factor(cross.raw.data$income)

# We need to convert past donations from a character to a factor variable

table(cross.raw.data$pastDonations)
cross.raw.data[,'pastDonations'] <- factor(cross.raw.data$pastDonations)

# We need to decide whether the number of kids is numeric variable or not (It is numeric variable!)

table(cross.raw.data$numberOfKids)
test.numberofkids.df <- aggregate(donation ~ numberOfKids, data = cross.raw.data, FUN = "mean")
plot(test.numberofkids.df[,2] ~ test.numberofkids.df[,1], data = test.numberofkids.df)

# We need to convert the married variable to factor variable 

table(cross.raw.data$married)
cross.raw.data$married <- as.factor(ifelse(cross.raw.data$married == 1, 'True', 'False'))

# zip code is nominal variable and thus we shall covert him to factor 

table(cross.raw.data$zipCode)
cross.raw.data[,'zipCode'] <- factor(as.character(cross.raw.data$zipCode))

# I think we should remove the month from the data set.. do you think that either?

table(cross.raw.data$month)
test.month.df <- aggregate(donation ~ month, data = cross.raw.data, FUN = "mean")
plot(test.month.df[,2] ~ test.month.df[,1], data = test.month.df)

# checking if our target variable is normal distributed (and it's not) so linear regression is off the table 

table(cross.raw.data$donation)
dummy.df <- cross.raw.data[cross.raw.data$donation != '0',]
hist(dummy.df$donation)


# feature selection using Information gain 
#####################################
# Training classification tree  
###################################

cross.raw.data[,'Donatedornot'] <- factor(ifelse(cross.raw.data$donation == 0,
                                                 '0',ifelse(cross.raw.data$donation >= 15,'2','1')))
set.IG.df <- cross.raw.data[,c(-7,-8,-9)]
set.IG.df2 <- cross.raw.data[,c(-7,-8,-10)]
partition <- createDataPartition(set.IG.df[['Donatedornot']], p = 0.8, list=FALSE ) # returns the indexes of the train data set.    
# creating the train data set
training <- set.IG.df[partition,] 
validation <- set.IG.df[-partition,]
training2 <- set.IG.df2[partition,]
validation2 <- set.IG.df2[-partition,]
trctrl <- trainControl(method = "repeatedcv", number = 7,repeats = 7 )
rpartFit2 <- train(donation ~ age +income + numberOfKids ,
                  data = training2, 
                  method="knn", preProcess=c("scale","center"),
                  tuneGrid   = expand.grid(k = c(5:15)))
rpartFit2
rpartFit <- train(Donatedornot ~ age +income + pastDonations+ numberOfKids,
                  data = training, 
                  method='rpart',
                  parms = list(split = "information"),
                  trControl=trctrl,
                  tuneLength = 4)
rpart.plot(rpartFit$finalModel, extra = 101)
rpartPred.train<-predict(rpartFit)
rpartPred <- predict(rpartFit, newdata = validation)

#knn

rpartPred.train2<-predict(rpartFit2)
rpartPred2 <- predict(rpartFit2, newdata = validation2)
rpartPred2

# evaluation and testing over-fitting by comparing train to validation

confusionMatrix(rpartPred.train, training$Donatedornot)
confusionMatrix(rpartPred, validation$Donatedornot)

#collecting the predicted rows

holdout.data <- holdout.data[,c(-7,-8)]
holdout.data$married <- as.factor(ifelse(holdout.data$married == 1, 'True', 'False'))
holdout.data$income <- as.factor(holdout.data$income)
holdout.data$pastDonations <- as.factor(holdout.data$pastDonations)
rholdoutPred <- predict(rpartFit, newdata = holdout.data)
rholdoutPred2 <- predict(rpartFit2, newdata = holdout.data)
holdout.data[,'answers'] <- rholdoutPred
holdout.data[,'answers2'] <- rholdoutPred2
holdout.data<- holdout.data[holdout.data$answers %in% c("1", "2"), ] 
holdout.data<- holdout.data[order(holdout.data$answers), ] 
holdout.data<- holdout.data[order(holdout.data$answers2), ] 
holdout.data<- tail(holdout.data,90) 
colnames(holdout.data)[1]<- "recommendation" 
holdout.data <- holdout.data[,1]
holdout.data



