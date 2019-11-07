############## cab fare predictions ###########

#clear all objects from R
rm(list = ls())

#set current working directory
setwd("C:/Users/nikhi/Desktop/cab fare prediction")

#cross-check working directory
getwd()

#loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "unbalanced", "e1071",
      "DataCombine", "inTrees", "rpart","MASS", "stats")

#load Packages
lapply(x, require, character.only = TRUE)
rm(x)

#loading train dataset
train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
str(train)
summary(train)
head(train,10)

#loading test dataset
test = read.csv("test.csv")
str(test)
summary(test)
head(test,10)

###############  Data pre-processing   #######################

############### 1.Exploratory data analysis #################

#1. fare_amount variable

#Change from factor to numeric
train$fare_amount = as.numeric(as.character(train$fare_amount))
str(train$fare_amount)

#Remove all 0 and negative values
train[which(train$fare_amount < 1 ),]
nrow(train[which(train$fare_amount < 1 ),])   #5 values
train = train[-which(train$fare_amount < 1 ),]

#2.Passenger_count variable

#It shouldn't exceed more than 6 passengers
train[which(train$passenger_count > 6 ),]
nrow(train[which(train$passenger_count > 6 ),])   #20 values
train = train[-which(train$passenger_count > 6),]

#It shouldn't be less than 1 passenger
train[which(train$passenger_count < 1 ),]
nrow(train[which(train$passenger_count < 1 ),])   #58 values
train = train[-which(train$passenger_count < 1),]

#3.pickup_latitude variable
#latitude range = +90 to -90
print(paste('pickup_latitude above 90=',nrow(train[which(train$pickup_latitude > 90 ),])))   #1 value
train = train[-which(train$pickup_latitude > 90),]

print(paste('pickup_latitude below -90=',nrow(train[which(train$pickup_latitude < -90 ),])))   #0 value

print(paste('pickup_latitude equal to 0=',nrow(train[which(train$pickup_latitude == 0 ),])))   #311 values
train = train[-which(train$pickup_latitude == 0),]

#4.dropoff_latitude variable
print(paste('dropoff_latitude above 90=',nrow(train[which(train$dropoff_latitude > 90 ),])))   #0 value

print(paste('dropoff_latitude below -90=',nrow(train[which(train$dropoff_latitude < -90 ),])))   #0 value

print(paste('dropoff_latitude equal to 0=',nrow(train[which(train$dropoff_latitude == 0 ),])))   #9 values
train = train[-which(train$dropoff_latitude == 0),]

#5.pickup_longitude variable
#(longitude range = +180 to -180)
print(paste('pickup_longitude above 180=',nrow(train[which(train$pickup_longitude >180 ),])))   #0 value

print(paste('pickup_longitude below -180=',nrow(train[which(train$pickup_longitude < -180 ),])))   #0 value

print(paste('pickup_longitude equal to 0=',nrow(train[which(train$pickup_longitude == 0 ),])))   #0 value

#6.dropoff_longitude variable
print(paste('dropoff_longitude above 180=',nrow(train[which(train$dropoff_longitude > 180 ),])))   #0 value

print(paste('dropoff_longitude below -180=',nrow(train[which(train$dropoff_longitude < -180 ),])))   #0 value

print(paste('dropoff_longitude equal to 0=',nrow(train[which(train$dropoff_longitude == 0 ),])))   #2 values
train = train[-which(train$dropoff_longitude == 0),]

# Make a copy
df=train

###############  2.Missing Value Analysis  ###############

#creating a dataframe with sum of missing values
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))  
#fare_amount: 22 values
#passenger_count: 55 values

#Converting row index into column.
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL

#Rename the column as missing_percentage.
names(missing_val)[1] =  "Missing_percentage"

#Calculating in terms of percentage
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train)) * 100
#fare_amount: 0.14%
#passenger_count: 0.35%

#Arranging in descending order.
missing_val = missing_val[order(-missing_val$Missing_percentage),]

#Rearranging the columns
missing_val = missing_val[,c(2,1)]

#imputation
#1.passenger_count variable
train[1887, 7] 
#actual value = 1

# Replacing 1 with NA
train[1887, 7] = NA 
train[1887, 7]

#mean
train$passenger_count[is.na(train$passenger_count)] = mean(train$passenger_count, na.rm = T)
#mean = 1.65

#KNN
train = knnImputation(train, k =789)
sum(is.na(train))
#KNN = 1.63

#conclusion: We will choose the KNN method here with k = 789 

#2.fare_amount variable
train [6879, 1]
#actual value = 6.5

train [6879, 1] = NA
train [6879, 1]

#mean
train$fare_amount[is.na(train$fare_amount)] = mean(train$fare_amount, na.rm = T)
#mean = 15.11

#median
train$fare_amount[is.na(train$fare_amount)] = median(train$fare_amount, na.rm = T)
#median = 8.5

#KNN
train = knnImputation(train, k =789)
sum(is.na(train))
#KNN =7.73

#Conclusion: We will Choose KNN method here because it imputes value closest to actual value with k = 789.

#make a copy
df1=train

##################### 3. Outlier Analysis  ##################

#1.fare_amount variable
pl1 = ggplot(train,aes(x = factor(passenger_count),y = fare_amount))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

# Replace all outliers with NA
vals = train[,"fare_amount"] %in% boxplot.stats(train[,"fare_amount"])$out
train[which(vals),"fare_amount"] = NA

#sum of NA's
sum(is.na(train$fare_amount))   #1361 values

#Imputing with KNN
train = knnImputation(train,k=3)

# lets check the missing values
sum(is.na(train$fare_amount))   # 0 value

#make a copy
df2=train
dim(train)
################## 4.Feature Engineering  #######################

#train data
#1.pickup_date variable: To obtain only date from pickup_datetime
train$pickup_date = as.Date(as.character(train$pickup_datetime))

#pickup_weekday variable: contains only week from pickup_date.
train$pickup_weekday = as.factor(format(train$pickup_date,"%u"))   # ( 1 to 7)

#pickup_mnth variable: contains only months from pickup_date. 
train$pickup_mnth = as.factor(format(train$pickup_date,"%m"))      # (1 to 12)

#pickup_yr: contains only years from pickup_date.
train$pickup_yr = as.factor(format(train$pickup_date,"%Y"))        # (2009 to 2015)

#pickup_hour: contains only hours from pickup_datetime
pickup_time = strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$pickup_hour = as.factor(format(pickup_time,"%H"))             # ( 0 to 23)

#test data
test$pickup_date = as.Date(as.character(test$pickup_datetime))

test$pickup_weekday = as.factor(format(test$pickup_date,"%u"))

test$pickup_mnth = as.factor(format(test$pickup_date,"%m"))

test$pickup_yr = as.factor(format(test$pickup_date,"%Y"))

pickup_time = strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test$pickup_hour = as.factor(format(pickup_time,"%H"))

#Removing pickup_datetime and pickup_date variable from train and test dataset.
train = subset(train,select = -c(pickup_datetime,pickup_date))
test = subset(test,select = -c(pickup_datetime,pickup_date))

#check for missing value
sum(is.na(train))    #4 values

#Omit the na values
train = na.omit(train) 

# 2.to calculate the distance travelled using longitude and latitude

#to convert degree into rad: 1Deg × ??/180 
deg_to_rad = function(deg){
  (deg * pi) / 180
}

#haversine function
haversine = function(long1,lat1,long2,lat2){
  
  phi1 = deg_to_rad(lat1)
  
  phi2 = deg_to_rad(lat2)
  
  delphi = deg_to_rad(lat2 - lat1)
  
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3/1000       #in Km
  R * c
}

#Creating a new variable called dist to store all the continuous values 
train$dist = haversine(train$pickup_longitude,train$pickup_latitude,train$dropoff_longitude,train$dropoff_latitude)
test$dist = haversine(test$pickup_longitude,test$pickup_latitude,test$dropoff_longitude,test$dropoff_latitude)

# We will remove the variables which were used to feature engineer new variables
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

#checking for outliers
#2. dist variable
pl1 = ggplot(train,aes(x = factor(passenger_count),y = dist))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

# Replace all outliers with NA
vals = train[,"dist"] %in% boxplot.stats(train[,"dist"])$out
train[which(vals),"dist"] = NA

#sum of NA's
sum(is.na(train$dist))   #1348 values

#Imputing with KNN
train = knnImputation(train,k=3)

# lets check the missing values
sum(is.na(train$fare_amount))   # 0 value

################   5. Feature selection   ###################

#converting passenger_count from numeric to factor
train$passenger_count=round(train$passenger_count)

train[,'passenger_count'] = factor(train[,'passenger_count'], labels=(1:6))

test[,'passenger_count'] = factor(test[,'passenger_count'], labels=(1:6))


#Correlation analysis for continous variables
numeric_index = sapply(train,is.numeric) 
numeric_data = train[,numeric_index]

cnames = colnames(numeric_data)

corrgram(train[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

#we cannot carryout chi-square test as target variable (fare_amount) is continous in nature

#ANOVA for categorical variables
aov_results = aov(fare_amount ~ passenger_count + pickup_hour + pickup_weekday + pickup_mnth + pickup_yr,data = train)
summary(aov_results)


#pickup_weekday has p value greater than 0.05 therefore we remove from train and test dataset 
train = subset(train,select=-pickup_weekday)
test = subset(test,select=-pickup_weekday)

######################## 6. Feature Scaling  ###################

#Normality check
qqnorm(train$fare_amount)
histogram(train$fare_amount)   #right skewed

qqnorm(train$dist)
histogram(train$dist)      #right skewed

#Normalisation
print('dist')
train[,'dist'] = (train[,'dist'] - min(train[,'dist']))/
  (max(train[,'dist'] - min(train[,'dist'])))

################## machine learning algorithm ################

set.seed(1000)
train.index = createDataPartition(train$fare_amount,p=0.75,list = FALSE)
train_data = train[train.index,]
test_data = train[-train.index,]

rmExcept(c("test","train","df",'df1','df2','test_data','train_data'))

###################  1. Linear regression   #################

#check multicollearity
library(usdm)
vif(train[,-1])

warnings()

#check for normal distribution of data
qqnorm(train_data$fare_amount)
histogram(train_data$fare_amount)    #right skewed

#check for linear relationship between dependent variable vs independent variable by scatter plot
#passenger_count
ggplot(train_data, aes_string(x = train_data$fare_amount, y = train_data$dist))+ geom_point(aes_string(colour = train_data$passenger_count), size = 4) +theme_bw() + ylab("dist") + xlab("fare_amount") + ggtitle("scatter plot analysis for passenger_count") + scale_color_discrete(name = "passenger_count")

#pickup_yr
ggplot(train_data, aes_string(x = train_data$fare_amount, y = train_data$dist))+ geom_point(aes_string(colour = train_data$pickup_yr), size = 4) +theme_bw() + ylab("dist") + xlab("fare_amount") + ggtitle("scatter plot analysis for pickup_yr") + scale_color_discrete(name = "pickup_yr")

#pickup_mnth
ggplot(train_data, aes_string(x = train_data$fare_amount, y = train_data$dist))+ geom_point(aes_string(colour = train_data$pickup_mnth), size = 4) +theme_bw() + ylab("dist") + xlab("fare_amount") + ggtitle("scatter plot analysis for pickup_mnth") + scale_color_discrete(name = "pickup_mnth")

#pickup_hour
ggplot(train_data, aes_string(x = train_data$fare_amount, y = train_data$dist))+ geom_point(aes_string(colour = train_data$pickup_hour), size = 4) +theme_bw() + ylab("dist") + xlab("fare_amount") + ggtitle("scatter plot analysis for pickup_hour") + scale_color_discrete(name = "pickup_hour")

#apply model on train_data
lm_model = lm(fare_amount ~.,data=train_data)
summary(lm_model)

#Predict on test_data
lm_predictions = predict(lm_model,test_data[,2:6])
summary(lm_predictions)
#error matrix
regr.eval(test_data[,1],lm_predictions)

#RMSE is considered because it is best suited for time series problem
#RMSE :  2.33


####################  2.Decision Tree  #####################

#apply model on train_data
Dt_model = rpart(fare_amount ~ ., data = train_data, method = "anova")
summary(Dt_model)

#Predict for new test cases
Dt_predictions = predict(Dt_model, test_data[,2:6])

#error matrix
regr.eval(test_data[,1], Dt_predictions)

#RMSE: 2.59


################   3. Random forest   #####################

#apply model on train_data
rf_model = randomForest(fare_amount ~.,data=train_data)
summary(rf_model)

#Predict for new test cases
rf_predictions = predict(rf_model,test_data[,2:6])

#error matrix
regr.eval(test_data[,1],rf_predictions)

#RMSE : 2.46




