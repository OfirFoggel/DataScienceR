# textual variables
rm(list = ls())
txt <- "Hi there"
txt
class(txt)

# numeric variables
x <- 8
y <- 9
result <- x+y
class(result)

# vector
vec <- c(3, 4, 9) 
vec
vec2 <- c(1,2,3)
vec*2

vec[2]
vec[4]
vec+vec2
 
vec3 <- c("r", "s", "m")
vec4<- c(1,"s","m")


#######


# conversions
var_c <- "1"
class(var_c)
var_n <- as.numeric(var_c)
class(var_n)


# from text to factor (nominal variable)
vec <- c("c", "a", "b", "a", "c")
vec
class(vec)
fct <- as.factor(vec)
#fct <- factor(vec)
fct
class(fct)

# ifelse statements 
a <- 5
if (a<7) {
  a <- a*2
} else { # optional
  a <- a/2
}

# shorter:
a <- ifelse(a<7, a*2, a/2)


# reading files. 
# set working directory
setwd("C:/projects/R course")
setwd("C:\\projects\\R course")

# read file 
file.df <- read.csv("file.csv")		

# alternative (no need to set working dir)
file.name = 'C:\\projects\\R course\\file.csv'

file.df <- read.csv(file.name)

# print file to screen
file.df
head(file.df) 
tail(file.df)
head(file.df,4) 
tail(file.df,3)

# variable names
names(file.df)

# column selection
file.df[,1]
file.df[,1:2]
file.df[,c(1,3)]


vec = c(1,3)
vec
file.df[,vec]

file.df$X2

# row selection
file.df[2:4,]
file.df[c(1,3),]
file.df[2,3]

# row selection - more advanced
file.df[file.df$X3 == "Good",]
file.df$X3 == "Good"
file.df[file.df$X3 == "Good",]

file.df[vec,]  
file.df[-vec,]

vec
file.df[,vec]  
file.df[,-vec]
file.df[vec,]  
file.df[-vec,]

########################
# install a package (only need to do once)
install.packages("car")

# load the package
library(car)

# use package?s functions
Boxplot(file.df$X2)

# using help
?t.test

################
#Data processing
################

## read the data
imdb.movies <- read.csv("IMDB_movies.csv",stringsAsFactors=TRUE)
head(imdb.movies)

players.df <- read.csv("IMDB_players.csv",stringsAsFactors=TRUE)
head(players.df)

dim(imdb.movies)

## summary

summary(imdb.movies) 


# what to do with nulls - several option and the decision depends
# on your problem and the amount of data you have. You need to be careful not to bias 
# the data, 

## 1. remove rows with missing values
is.na(imdb.movies$total.gross)

imdb.movies.nomissing <- 
  imdb.movies[is.na(imdb.movies$total.gross) == FALSE, ]

summary(imdb.movies.nomissing$total.gross)
summary(imdb.movies$total.gross)

## 2. data imputation - instead of removing an entire row (observation) we replace the missing value. in our case, we replace total.gross.
# it can be replaced with mean/median/other depending on the needs of the problem

mean(imdb.movies$total.gross, na.rm = TRUE) 
mean.total.gross=mean(imdb.movies$total.gross, na.rm = TRUE) 

imdb.movies$total.gross[is.na(imdb.movies$total.gross)] <- mean.total.gross
summary(imdb.movies$total.gross)
length(imdb.movies$total.gross)
length(imdb.movies.nomissing$total.gross)

# budget class 
summary(imdb.movies$budget)
class(imdb.movies$budget)
as.numeric(imdb.movies$budget)
as.character(imdb.movies$budget)
imdb.movies$budget <- as.numeric(as.character(imdb.movies$budget))
summary(imdb.movies$budget)

## create new variable
imdb.movies$budgetAvil <- is.na(imdb.movies$budget)

####################################
# summary and descriptive statistics for exploring the data set and learning about connections

## aggregation
table(imdb.movies$genre)

aggregate(total.gross ~ genre, data = imdb.movies, FUN = "mean")


#####################
#Basic visualizations
#####################
rm(list = ls())

ebay.df <- read.csv("Cross Auctions.csv")
head(ebay.df) 

## histogram
hist(ebay.df$ClosePrice)
summary(ebay.df$ClosePrice)

## nicer plot - changing headline and X axis
hist(ebay.df$ClosePrice,  main = "Histogram", 
     xlab = "Close Price")

## change number of bins
hist(ebay.df$ClosePrice, br = 100,  main = "Histogram with 100 Breaks", 
     xlab = "Close Price")

hist(ebay.df$ClosePrice, br = 1000,  main = "Histogram with 1000 Breaks", 
     xlab = "Close Price")

## zoom
hist(ebay.df$ClosePrice[ebay.df$ClosePrice < 200],  
     main = "Histogram", xlab = "Close Price")


## scatter plot
plot(ClosePrice ~ BuyItNow, data = ebay.df)

# choosing color using col.
# choosing the data symbol graphics using pch (1-circle, 2- triangle etc)
plot(ClosePrice ~ BuyItNow, data = ebay.df, 
     col = ifelse(ebay.df$HasReservePrice=="FALSE", "blue", "red"), 
     pch = 1)


#legend 
legend("topright", 
       legend = c("No reserve prices", "With reserved price"), 
       col = c("blue", "red"), 
       pch = 1)



## barplot
table(ebay.df$Duration)
meanClosePrice <- aggregate(ClosePrice ~ Duration, 
                            data = ebay.df, 
                            FUN = "mean")

meanClosePrice
barplot(height = meanClosePrice[,2], names.arg = meanClosePrice[,1], 
        xlab = "duration", ylab = "mean close price")



