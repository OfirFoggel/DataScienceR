################################################################################
################# Setting WD and seed- do not modify this part! ################
################################################################################

# We will define the Working directory for you to the folder in which the script is located
# Therefore, don't define it manually using set.wd()

#(We check whether a package we want is installed, if it isn't we install it for you)
if(!"rstudioapi" %in% installed.packages()) { 
    install.packages("rstudioapi")}

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() ) # You can make sure that the WD is correct. If it is not - send Or an E-mail

set.seed(123) ###  We set the seed to 123, do not change it.
rm(current_path)

################################################################################
#################################### Intro #####################################
################################################################################

# Hi! We want to get to know you better!
# please assign your name/id to these variables:
name1<- "Ofir Foggel"


# If you use any package that was not used in the course materials
# Please add its name to the vector in row 36
# otherwise you can ignore the next line
my.packages<-c("ggplot2","data.table") 

################################################################################
######################### The actual work starts here: ######################### 
################################################################################

library(caret)
library('e1071')
library(forecast)
library(rpart)
library(rpart.plot)

#### Read files  ####
df<-read.csv("cross_2022C.csv")           #"Please read cross_2022C.csv file and store it as df"
holdout<-read.csv("holdout_2022C.csv")      #"Please read holdout_2022C.csv file and store it as holdout"




#### Pre-processing  ####
# Please use this part of the to do all the data cleaning, feature engineering and so on
#some variable learning and data processing

head(df)

# the frequency is monotonically decreasing with the age

table(df$age) 
hist(df$age)

# the income is ordinal categorical variable

table(df$income)
test.income.df <- aggregate(donation ~ income, data = df, FUN = "mean")
plot(test.income.df[,2] ~ test.income.df[,1], data = test.income.df)
df$income <- as.factor(df$income)

# We need to convert past donations from a character to a factor variable

table(df$pastDonations)
df[,'pastDonations'] <- factor(df$pastDonations)

# We need to decide whether the number of kids is numeric variable or not (It is numeric variable!)

table(df$numberOfKids)
test.numberofkids.df <- aggregate(donation ~ numberOfKids, data = df, FUN = "mean")
plot(test.numberofkids.df[,2] ~ test.numberofkids.df[,1], data = test.numberofkids.df)

# We need to convert the married variable to factor variable 

table(df$married)
df$married <- as.factor(ifelse(df$married == 1, 'True', 'False'))

# zip code is nominal variable and thus we shall covert him to factor 

table(df$zipCode)
df[,'zipCode'] <- factor(as.character(df$zipCode))

# month is ordinal categorical variable. we will remove it from the data

table(df$month)
test.month.df <- aggregate(donation ~ month, data = df, FUN = "mean")
plot(test.month.df[,2] ~ test.month.df[,1], data = test.month.df)

# checking if our target variable is normal distributed 

table(df$donation)
test.df <- df[df$donation != '0',]
hist(test.df$donation)

#### Models ####

#######################################################
# Training classification tree using information gain # 
#######################################################

df[,'Donatedornot'] <- factor(ifelse(df$donation == 0,
                                                 '0',ifelse(df$donation >= 15,'2','1')))
set.IG.df <- df[,c(-7,-8,-9)]
set.knn <- df[,c(-7,-8,-10)]
partition <- createDataPartition(set.IG.df[['Donatedornot']], p = 0.8, list=FALSE ) # returns the indexes of the train data set.    

# creating the train data set

training <- set.IG.df[partition,] 
validation <- set.IG.df[-partition,]
trctrl <- trainControl(method = "repeatedcv", number = 7,repeats = 7 )
rpartFit <- train(Donatedornot ~ age +income + pastDonations+ numberOfKids,
                  data = training, 
                  method='rpart',
                  parms = list(split = "information"),
                  trControl=trctrl,
                  tuneLength = 4)
rpart.plot(rpartFit$finalModel, extra = 101)
rpartPred.train<-predict(rpartFit)
rpartPred <- predict(rpartFit, newdata = validation)

######################
# Training knn model # 
######################

training_knn <- set.knn[partition,]
validation_knn <- set.knn[-partition,]
Fit_knn <- train(donation ~ age +income + numberOfKids ,
                      data = training_knn, 
                      method="knn", preProcess=c("scale","center"),
                      tuneGrid   = expand.grid(k = c(5:15)))

Pred.train_knn<-predict(Fit_knn)
Pred_knn <- predict(Fit_knn, newdata = validation_knn)


# evaluation and testing over-fitting by comparing train to validation

confusionMatrix(rpartPred.train, training$Donatedornot)
confusionMatrix(rpartPred, validation$Donatedornot)


#### Predictions  ####

holdout <- holdout[,c(-7,-8)]
holdout$married <- as.factor(ifelse(holdout$married == 1, 'True', 'False'))
holdout$income <- as.factor(holdout$income)
holdout$pastDonations <- as.factor(holdout$pastDonations)

# Prediction using tree

rholdoutPred <- predict(rpartFit, newdata = holdout)

# Prediction using knn

rholdoutPred_knn <- predict(Fit_knn, newdata = holdout)

# Generate to best vector we could according to the models we implemented

holdout[,'answers'] <- rholdoutPred
holdout[,'answers_knn'] <- rholdoutPred_knn
holdout<- holdout[holdout$answers %in% c("1", "2"), ] 
holdout<- holdout[order(holdout$answers), ] 
holdout<- holdout[order(holdout$answers_knn), ] 
holdout<- tail(holdout,90) 
colnames(holdout)[1]<- "recommendation" 

final_prediction<-holdout[,1]


#### Let's save your prediction into a csv file
filename<-paste0(ID1,"_",ID2,"_",ID3,".csv") # Don't modify this line!
write.csv(x=data.frame(recommendation=final_prediction),file = filename,row.names = FALSE) # Don't modify this line!


