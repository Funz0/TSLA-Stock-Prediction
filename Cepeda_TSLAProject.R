# Alejandro Cepeda
# LIS 4217 Final Project
# TESLA Stock Prediction Model

#set working directory
setwd("I:/My Drive/University/USF/FALL 21/Adv Stats & Analytics/Final Project")

# libraries
require(ggplot2)
require(caret)

# read in data
TSLA <- read.csv("TSLA.csv")
head(TSLA)
str(TSLA)

# convert to time series object
TSLA.adjclose <- ts(TSLA$Adj.Close, frequency=252, start=c(2020,3))
plot(TSLA.adjclose, ylab="Adjusted Closing Stock Price", main="Tesla, Inc. (TSLA)")
TSLA.Open <- ts(TSLA$Open, frequency=252, start=c(2020,3))
plot(TSLA.Open, ylab="Opening Stock Price", main="Tesla, Inc. (TSLA)")
TSLA.High <- ts(TSLA$High, frequency=252, start=c(2020,3))
plot(TSLA.High, ylab="Highest Stock Price", main="Tesla, Inc. (TSLA)")
TSLA.Low <- ts(TSLA$Low, frequency=252, start=c(2020,3))
plot(TSLA.Low, ylab="Lowest Stock Price", main="Tesla, Inc. (TSLA)")
TSLA.Close <- ts(TSLA$Close, frequency=252, start=c(2020,3))
plot(TSLA.Close, ylab="Closing Stock Price", main="Tesla, Inc. (TSLA)")
# create and store train/test splits 70/30 based on Volume
set.seed(123)
index <- createDataPartition(TSLA$Adj.Close, p=0.70, list=F)

train <- TSLA[index,]
test <- TSLA[-index,]

# fit linear model using train set
TSLA.model <- lm(Adj.Close~High, data=train)
TSLA.model
# five fold cross validation
ctrl <- trainControl(method="repeatedcv", 
                     number=5,
                     repeats=5)
ctrl

# train model using cross validation
TSLA.lm <- train(Adj.Close~High, data=train,
                 method="lm",
                 trControl=ctrl)

# store the RMSE
summary(TSLA.lm)
RMSE.train <- sqrt(mean(TSLA.model$residuals^2))
RMSE.train
# predict on test data
pred <- predict(TSLA.lm, newdata=test)
pred
# compute error of prediction and test data
err <- pred - test$Adj.Close

# store test data RMSE
"RMSE.test"
RMSE.test <- sqrt(mean(err^2))
RMSE.test

# compare train/test RMSEs
RMSE.train - RMSE.test

# predict on full dataset
full.pred <- predict(TSLA.model, newdata=TSLA, type="response")
full.err <- full.pred - TSLA$Adj.Close

# compute full dataset RMSE
"RMSE.full"
RMSE.full <- sqrt(mean(full.err^2))
RMSE.full

# plot results
require(ggiraphExtra)
ggPredict(TSLA.model, se=TRUE, interactive=TRUE, show.summary = TRUE)
