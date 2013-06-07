########################################################
# SCRIPT FUNCTION
########################################################
# This script uses logisitc regression to predict if an auction will result in a sale, whether QuantitySold will be 0 or 1 

library(gbm)
train.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TrainingSet.csv', sep=',', h=T)


# baseline using AvgPrice to predict sales
(nrow(subset(train.raw.data , AvgPrice <= Price & QuantitySold == 1))+nrow(subset(train.raw.data, AvgPrice > Price & QuantitySold == 0)))/nrow(train.raw.data)

# training set size
nrow(train.raw.data)
#count sold items
nrow(subset(train.raw.data, QuantitySold == 1))
# count not sold items
nrow(subset(train.raw.data, QuantitySold == 0))
# sell rate
nrow(subset(train.raw.data, QuantitySold == 1))/nrow(train.raw.data)

train.data.success <- train.raw.data [c("QuantitySold","AuctionMedianPrice", "Price","PricePercent","StartingBidPercent","SellerClosePercent","StartingBid","AvgPrice","HitCount","AuctionAvgHitCount","ItemAuctionSellPercent","SellerSaleAvgPriceRatio","AuctionHitCountAvgRatio","BestOffer", "IsHOF","ItemListedCount","AuctionCount","AuctionSaleCount","SellerAuctionCount","SellerAuctionSaleCount")]

# standardize training data points between 1 and 100
train.data.success$AuctionMedianPricePercent <-train.data.success$Price/(train.data.success$AuctionMedianPrice*100)
train.data.success$PricePercent <-train.data.success$PricePercent*100
train.data.success$SellerClosePercent <-train.data.success$SellerClosePercent*100
train.data.success$StartingBidPercent <-train.data.success$StartingBidPercent*100
train.data.success$ItemAuctionSellPercent <-train.data.success$ItemAuctionSellPercent*100


test.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TestSet.csv', sep=',', h=T)



# training set size
nrow(test.raw.data)
# count sold items
nrow(subset(test.raw.data, QuantitySold == 1))
# count not sold items
nrow(subset(test.raw.data, QuantitySold == 0))
# sell rate
nrow(subset(test.raw.data, QuantitySold == 1))/nrow(test.raw.data)


test.data.success <- test.raw.data[c("QuantitySold","AuctionMedianPrice", "Price","PricePercent","StartingBidPercent","SellerClosePercent","StartingBid","AvgPrice","HitCount","AuctionAvgHitCount","ItemAuctionSellPercent","SellerSaleAvgPriceRatio","AuctionHitCountAvgRatio","BestOffer", "IsHOF","ItemListedCount","AuctionCount","AuctionSaleCount","SellerAuctionCount","SellerAuctionSaleCount")]

# standardize test data points between 1 and 100
test.data.success$AuctionMedianPricePercent <-test.data.success$Price/(test.data.success$AuctionMedianPrice*100)
test.data.success$PricePercent <-test.data.success$PricePercent*100
test.data.success$SellerClosePercent <-test.data.success$SellerClosePercent*100
test.data.success$StartingBidPercent <-test.data.success$StartingBidPercent*100
test.data.success$ItemAuctionSellPercent <-test.data.success$ItemAuctionSellPercent*100

# perform LR model
glm.out = glm(QuantitySold ~ AuctionSaleCount+AuctionMedianPrice+StartingBidPercent+SellerClosePercent+ItemAuctionSellPercent, family=binomial(logit), data=train.data.success)
summary(glm.out)

# make predictions
lm.pred<-predict(glm.out, test.data.success, type="response")

# add fields to test.data
test.data.success[["Prediction"]] <- lm.pred

# percent of accurate predictions of sales
(nrow(subset(test.data.success, Prediction<0.5 & QuantitySold == 0)) + nrow(subset(test.data.success, Prediction>=0.5 & QuantitySold == 1))) / nrow(test.data.success)
