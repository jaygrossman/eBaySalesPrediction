########################################################
# SCRIPT FUNCTION
########################################################
# This script uses CART to calculate predictions
# Predictions over 49.99 are filtered out
# Use Knn to predict the $5 Interval the auction price will result in
# Predictions filter for only auctions where predicted interval is within + or - $10 of the interval of the Predicted Price

library(rpart)
library(rpart.plot)
library(FNN)
library(ggplot2)

########################################################
# BUILD DECISION TREE
########################################################

train.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TrainingSubset.csv', sep=',', h=T)
# optimized set of features
train.data <- train.raw.data[c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid", "AuctionHitCountAvgRatio", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")]
train.data$Price=log(train.data$Price)

tree<-rpart(Price ~.,data=train.data)

# draw decision tree
rpart.plot(tree,type=2,extra=1)


########################################################
# BUILD CART Prediction
########################################################

test.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TestSubset.csv', sep=',', h=T)
test.data <- test.raw.data[c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid", "AuctionHitCountAvgRatio", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")]
test.data$Price=log(test.data$Price)

# make predictions
predCart<-predict(tree, test.data, type = "vector")

# add fields to test.data
test.data[["SalePrice"]] <- exp(test.data$Price)
test.data[["Prediction"]] <- exp(predCart)
test.data[["Interval"]] <- (round((test.data$Prediction)/5+1)*5)

# calculate the RMSE, standard deviation, mean difference
sqrt(mean(test.data$SalePrice-test.data$Prediction)^2)
sd(test.data$Prediction)
sum(test.data$SalePrice-test.data$Prediction)/sum(test.data$SalePrice)


# create title for results plot
title <- paste('Auction Prediction Results - CART', sep='')
# create results plot
prediction.plot <- ggplot(test.data, aes(x=SalePrice, y=Prediction)) + geom_point() + geom_line() + ggtitle(title)+ stat_smooth()
print(prediction.plot)


#########################################################################
# perform KNN prediction on PriceBucket for non AvgPrice features
#########################################################################

trainKnn.data  <- train.raw.data[c("PriceBuckets", "Price", "AuctionMedianPrice", "ItemAuctionSellPercent",  "StartingBid", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")]
testKnn.data <- test.raw.data[c("PriceBuckets", "Price", "AuctionMedianPrice", "ItemAuctionSellPercent", "StartingBid", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "SellerAuctionSaleCount", "AuctionCount")]
cl <- factor(trainKnn.data$PriceBuckets)
cl.test <- factor(testKnn.data$PriceBuckets)
predKnn<-knn(trainKnn.data, testKnn.data, cl, k = 3, prob=TRUE)

test.data[["PriceBucket"]] <- test.raw.data$PriceBuckets
test.data[["PriceBucketPrediction"]] <- as.numeric(as.character(predKnn))
# RMSE of clustering
sqrt(mean(test.data$PriceBucket-test.data$PriceBucketPrediction)^2)

# percent of items within group
nrow(subset(test.data, PriceBucket == PriceBucketPrediction))/nrow(test.data)

# percent of items within one group
nrow(subset(test.data, (PriceBucket == PriceBucketPrediction | PriceBucket+5 == PriceBucketPrediction | PriceBucket-5 == PriceBucketPrediction)))/nrow(test.data)

# percent of items within two groups
nrow(subset(test.data, (PriceBucket == PriceBucketPrediction | PriceBucket+5 == PriceBucketPrediction | PriceBucket-5 == PriceBucketPrediction| PriceBucket+10 == PriceBucketPrediction | PriceBucket-10 == PriceBucketPrediction)))/nrow(test.data)


#########################################################################
# Filter for items with Price < $50
#########################################################################

filtered.test.data<-subset(test.data,Prediction<50)
# calculate the RMSE, standard deviation, mean difference, percent of test.data
sqrt(mean(filtered.test.data$SalePrice-filtered.test.data$Prediction)^2)
sd(filtered.test.data$Prediction)
sum(filtered.test.data$SalePrice-filtered.test.data$Prediction)/sum(filtered.test.data$SalePrice)
nrow(filtered.test.data)/nrow(test.data)


#########################################################################
# Filter for items within 2 intervals 
#########################################################################
twointervals.filtered.test.data<-subset(test.data, (Interval == PriceBucketPrediction | Interval+5 == PriceBucketPrediction | Interval-5 == PriceBucketPrediction| Interval+10 == PriceBucketPrediction | Interval-10 == PriceBucketPrediction))
# calculate the RMSE, standard deviation, mean difference, percent of test.data
sqrt(mean(twointervals.filtered.test.data$SalePrice-twointervals.filtered.test.data$Prediction)^2)
sd(twointervals.filtered.test.data$Prediction)
sum(twointervals.filtered.test.data$SalePrice-twointervals.filtered.test.data$Prediction)/sum(twointervals.filtered.test.data$SalePrice)
nrow(twointervals.filtered.test.data)/nrow(test.data)




# create title for results plot
title <- paste('Auction Prediction Results - Ensemble', sep='')
# create results plot
prediction.plot <- ggplot(twointervals.filtered.test.data, aes(x=SalePrice, y=Prediction)) + geom_point() + geom_line() + ggtitle(title)+ stat_smooth() 
print(prediction.plot)