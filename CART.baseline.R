########################################################
# SCRIPT FUNCTION
########################################################
# This script uses CART to calculate predictions for Price using only AvgPrice

library(rpart)
library(rpart.plot)
library(FNN)
library(ggplot2)

########################################################
# BUILD DECISION TREE
########################################################

train.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TrainingSubset.csv', sep=',', h=T)
# optimized set of features
train.data <- train.raw.data[c("Price", "AvgPrice")]
train.data$Price=log(train.data$Price)

tree<-rpart(Price ~.,data=train.data)

# draw decision tree
rpart.plot(tree,type=2,extra=1)


########################################################
# BUILD CART Prediction
########################################################

test.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TestSubset.csv', sep=',', h=T)
test.data <- test.raw.data[c("Price", "AvgPrice")]
test.data$Price=log(test.data$Price)

# make predictions
predCart<-predict(tree, test.data, type = "vector")

# add fields to test.data
test.data[["SalePrice"]] <- exp(test.data$Price)
test.data[["Prediction"]] <- exp(predCart)
test.data[["AvgPrice"]] <- test.raw.data$AvgPrice

# calculate the RMSE, standard deviation, mean difference
sqrt(mean(test.data$SalePrice-test.data$Prediction)^2)
sd(test.data$Prediction)
sum(test.data$SalePrice-test.data$Prediction)/sum(test.data$SalePrice)


# create title for results plot
title <- paste('Auction Prediction Results - CART', sep='')
# create results plot
prediction.plot <- ggplot(test.data, aes(x=SalePrice, y=Prediction)) + geom_point() + geom_line() + ggtitle(title)+ stat_smooth() #+ scale_y_continuous(limits = c(00, 125)) # + scale_x_continuous(limits = c(00, 325))
print(prediction.plot)

