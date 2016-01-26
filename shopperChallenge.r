library(caret)
library(data.table)
library(plyr)

shoppers <- data.frame(fread("shoppers.csv")) 
test <- data.frame(fread("test_trips.csv")) 
train <- data.frame(fread("train_trips.csv")) 
items <- data.frame(fread("trip_items.csv")) 
#Align Variables
train$store_id <- as.factor(train$store_id)
test$store_id <- as.factor(test$store_id)

train$shopper_id <- as.factor(train$shopper_id)
test$shopper_id <- as.factor(test$shopper_id)
shoppers$shopper_id <- as.factor(shoppers$shopper_id)
train <- merge(train, shoppers, by = c("shopper_id"))
test <- merge(test, shoppers, by = c("shopper_id"))
rm(shoppers)

#train$trip_id <- as.factor(train$trip_id)
#test$trip_id <- as.factor(test$trip_id)
#items$trip_id <- as.factor(items$trip_id)


#Create Target
train[,5]<- abs(as.numeric(difftime( strptime( substr(train[,4],1,19) , "%Y-%m-%d %H:%M:%S") ,
 strptime( substr(train[,5],1,19) , "%Y-%m-%d %H:%M:%S"),units="secs")))
test[,5]<- abs(as.numeric(difftime( strptime( substr(test[,4],1,19) , "%Y-%m-%d %H:%M:%S") ,
 strptime( substr(test[,5],1,19) , "%Y-%m-%d %H:%M:%S"),units="secs")))
names(train)[5] <- "timediff"
names(test)[5] <- "timediff"

#Department Features  :  total depts, and individual dept flags
length(unique(items$item_department)) #22 total depts
trip_temp <- unique( items[, c(1,3)])
departmentsVisited <- as.data.frame(table(trip_temp$trip_id))
names(departmentsVisited) <-  c("trip_id" , "deptsVisited")

train <- merge(train, departmentsVisited, by = c("trip_id"))
test <- merge(test, departmentsVisited, by = c("trip_id"))

flagMaker <- function( dataframe , factors , trip_by_factor  ){
	for(i in factors ) {
		subset_vec <- subset(trip_by_factor$trip_id , trip_by_factor[,2] == i )
		var_temp <- as.numeric(ifelse( dataframe$trip_id %in% subset_vec,1,0))
		dataframe <- data.frame( dataframe , var_temp , stringsAsFactors =  FALSE)
		names(dataframe)[length(dataframe)] <- i
	}
	return(dataframe)
}

train <- flagMaker(train , unique(items$item_department) , trip_temp)
test <- flagMaker(test , unique(items$item_department) , trip_temp)

length(unique(items$item_id)) #22 total depts
item_temp <- unique( items[, c(1,2)])
itemsNeeded <- as.data.frame(table(trip_temp$trip_id))
names(itemsNeeded) <-  c("trip_id" , "itemsNeeded")

train <- merge(train, itemsNeeded, by = c("trip_id"))
test <- merge(test, itemsNeeded, by = c("trip_id"))


#Time Features  :  day of week and hour
hour <- function( x ){
	format(strptime( substr( x ,1,19) , "%Y-%m-%d %H:%M:%S"),"%H")
}
weekday <- function( x ){
	format(strptime( substr( x ,1,19) , "%Y-%m-%d %H:%M:%S"),"%A")
}
month <- function( x ){
	format(strptime( substr( x ,1,19) , "%Y-%m-%d %H:%M:%S"),"%B")
}
year <- function( x ){
	as.numeric(format(strptime( substr( x ,1,19) , "%Y-%m-%d %H:%M:%S"),"%Y"))
}
year <- function( x ){
	as.numeric(format(strptime( substr( x ,1,19) , "%Y-%m-%d %H:%M:%S"),"%Y"))
}

time_temp<- data.frame( c(train$trip_id , test$trip_id) , c(train[,4] , test[,4]) )
names(time_temp)<- c("trip_id" , "time")

hour_temp <- data.frame(trip_id = time_temp$trip_id ,
 timeframe = as.character(paste0("hour_", hour(time_temp$time ) )) , stringsAsFactors =  FALSE)
weekday_temp <- data.frame( trip_id = time_temp$trip_id ,
 timeframe = as.character(weekday(time_temp$time )) , stringsAsFactors =  FALSE)
month_temp <- data.frame(trip_id = time_temp$trip_id ,
 timeframe = as.character(month(time_temp$time )) , stringsAsFactors =  FALSE)

train <- flagMaker(train , unique(hour_temp$timeframe) , hour_temp)
test <- flagMaker(test , unique(hour_temp$timeframe) , hour_temp)
train <- flagMaker(train , unique(weekday_temp$timeframe) , weekday_temp)
test <- flagMaker(test , unique(weekday_temp$timeframe) , weekday_temp)
train <- flagMaker(train , unique(month_temp$timeframe) , trip_temp)
test <- flagMaker(test , unique(month_temp$timeframe) , trip_temp)

#Shopper Information
train$shopper_birth_year <- 2014 - train$shopper_birth_year
test$shopper_birth_year <- 2014 - test$shopper_birth_year
names(train)[7] <- "age"
names(test)[7] <- "age"


employeeTenureCalculation <- function( startDate , deliveryDate ){
	difftime( strptime( substr( startDate ,1,19) , "%Y-%m-%d %H:%M:%S") ,
	strptime( substr( deliveryDate ,1,19) , "%Y-%m-%d %H:%M:%S"),units="weeks")
}
train$shopper_hired_date <- abs(as.integer(employeeTenureCalculation(train$shopper_hired_date , train$shopping_started_at  )))
test$shopper_hired_date <- abs(as.integer(employeeTenureCalculation(test$shopper_hired_date , test$shopping_started_at  )))
names(train)[6] <- "tenure"
names(test)[6] <- "tenure"

#Build final feature set for model
dropColumns <- c("trip_id" , "shopper_id" , "shopping_started_at" )
train <- train[ , !names(train) %in% dropColumns]


library(randomForest)
set.seed(515)

rf_out <- tuneRF(train[,-2], train$timediff, stepFactor=1.5 , doBest = TRUE)
predOut <- predict(rf_out, test)
test_mse <- mean((predOut - test$timediff)^2)
out.DF <- data.frame( trip_id = test$trip_id , shopping_time = round(predOut,0) )
write.table(out.DF, file = "output.csv" , row.names = FALSE, quote = FALSE , sep = ",")

DF <- data.frame( trip_id = test$trip_id , pred = round(predOut,0) , actual = test$timediff )

rf_clean_out <- tuneRF(train[,-2], train$timediff, stepFactor = 1.5 , doBest = TRUE)
test_clean <- test[!test$timediff == 0 ,]
predOut_clean <- predict(rf_clean_out, test_clean)
test_clean_mse <- mean((predOut_clean - test_clean$timediff)^2)
clean_out.DF <- data.frame( trip_id = test_clean$trip_id , shopping_time = round(predOut_clean,0))
write.table(clean_out.DF, file = "clean.csv" , row.names = FALSE , quote = FALSE , sep = ",")




