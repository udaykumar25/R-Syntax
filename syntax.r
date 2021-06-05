## Data preprocessing and Exploratory Analysis ##

library(readr)
#import data
df <- read.csv("path\\swimming_pools.csv", stringsAsFactors = TRUE)
attach(df) #this command is used to attach dataframe so that there need not to use $ for every operation further.
df<-na.omit(df)
# Remove categorical variables
df1 <- subset(df, select = -c(CASENUM,LOSS,ATTORNEY))
colnames(df)<-c("MAPE","VALUES") #RENAME COLUMNS
df1$crim <- as.factor(df1$crim) #factor data

##### exploratory data analysis ######
str(df) #to see strcture of data
summary(speed) # gets the summary of data like mean, median, max, min, etc..,
View(df) # to view the data in new tab
sapply(df1, var) # variance of numeric dataframe
var() #to know variance for each column of dataframe
sapply(df1,sd)# standarad deviation of numeric dataframe
sd() #to know standarad deviation for each column of dataframe

library("UsingR") #library to plot histogram
library("moments") # libraryto plot densityplot
hist(speed) # histogram plot
densityplot(speed) #densityplot
skewness(speed)  #skewness( it is an negative skewness)
kurtosis(speed) #kurtosis (it is an negative kurtosis wider peak and thinner tail)

########  Data processing  ######

#####MissingValues#####
summary(df)
sum(is.na(df$))
library(mice)
immput<-mice(misseamcet2,m=5,maxit = 50,method = "pmm",seed = 500)
immput$imp$CUTOFFG
immput1<-mice(misseamcet,m=5,maxit = 50,method = "pmm",seed = 500)
immput1$imp$CUTOFFG

####Duplication_Typecasting#####
#Identify duplicates records in the data
dup <- duplicated(df)
length(which(dup==TRUE))#duplicate values
#Removing Duplicates
data_new <- df[!duplicated(df), ]
data_new
#####Zero Variance#####
apply(df,2,var)

####Outlier_Treatment#######
library("UsingR") 
boxplot(crim) #outliers are found

####################### 1. windrolization ############################
qunt1<-quantile(df$crim,probs = c(0.25,0.75)) #q1 and q3 of box
caps1<-quantile(df$crim,probs = c(0.05,0.892)) #5% and 89.2% of data
h1<-1.5*IQR(df$crim) #iqr
a1<-(qunt1[1]-h1)
b1<-(qunt1[2]+h1)
df$crim[df$crim<a1]<-caps1[1] #lower outliers assign to minimum
df$crim[df$crim>b1]<-caps1[2] #higher outliers assign to maximum
boxplot(df$crim)
#here we see no outliers
####################### 2.Replace ############################
# Now let's replace the outliers by the maximum and minimum limit
#in new dataset.
df1$crim[df1$crim<a1]<-a1 #lower outliers assign to minimum
df1$crim[df1$crim>b1]<-b1 #higher outliers assign to maximum
boxplot(df1$crim)

#####  scale or standardisation  ####
df1<-as.data.frame(scale(df))

#now this functions can apply to particular column or total data set or to any data set.
#NORMALIZATION
norm=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
df1<-df[,c(7,11,13,15,17)]
df2<-as.data.frame(lapply(df1,norm))
#scale or standardisation
df3<-as.data.frame(scale(df1))
##custom normalization
common<-function(i){
  x=(i-min(i))/(max(i)-min(i))
  return(x)
}
df2=common(df)
#df=common(df2$Area)
sapply(df1, var) #variance=1
sapply(df1,sd)

#####Dummy Variables#######
# Create dummy variables on categorcal columns
library(fastDummies)
dummy_df <- dummy_cols(df,remove_first_dummy = TRUE,remove_most_frequent_dummy =FALSE,remove_selected_columns = TRUE)

#label encoder converts into factor
library(CatEncoders)
#label factor to column and we transform it to column and cbind into dataframe 
lb_1<-LabelEncoder.fit(df1$YEAR.OF.ESTB)

#using cut function
iris$Petal.Length<-cut(Petal.Length,breaks = 3,labels = c("small","medium","large"))
View(iris)
#using dicerete function
library(arules)
irisdisc<-discretizeDF(iris,methods = list(Petal.Length=list(method="frequency",breaks=3,labels=c("sh","me","lo"),default=list(method="none"))))
irisdisc1<-discretizeDF(iris,default = list(method="interval",breaks=2,labels=c("small","large")))
#binning,grouping,binrization
library(OneR)
bin(iris)
bin(iris,nbins = 3)
bin(iris,nbins = 3,labels=c("small","medium","large"))

#type casting
as.integer(df$YEAR.OF.ESTB)
str(df)
is.numeric(df$YEAR.OF.ESTB)
#convert categorical into factor
df$DIST<-as.factor(df$DIST)

#splitting of data
library(caTools)
set.seed(0)
split <- sample.split(movies$Start_Tech_Oscar, SplitRatio = 0.8)
movies_train <- subset(movies, split == TRUE)
movies_test <- subset(movies, split == FALSE)

#cbind
newdata<-cbind(df,year)