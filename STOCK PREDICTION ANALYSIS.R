#loading dataset

dataset = read.csv('TSLA.csv')

#Structure of the dataset
str(dataset)

#Summary of the dataset
summary(dataset)

summary(dataset$Close)

summary(dataset$Volume)

plot(dataset)

# Splitting the dataset into the Training set and Test set

library(caTools)
set.seed(123)
split = sample.split(dataset$Open, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
df1_train=scale(subset(training_set,select=-c(Date,Close)))
df2_test = scale(subset(test_set,select=-c(Date,Close)))

training_set = cbind(df1_train, training_set$Date, training_set$Close)
test_set= cbind(df2_test,test_set$Date,test_set$Close)

training_set = as.data.frame(training_set)
test_set= as.data.frame(test_set)

class(test_set$Volume)
test_set$Volume = as.numeric(test_set$Volume)
training_set$Volume = as.numeric(training_set$Volume)

class(test_set$Volume)

# Fitting Linear Regression to the Training set
regressor = lm(Open ~ Volume, data = training_set)
summary.data.frame(regressor)

# Predicting the Test set results
test_set$predClose = predict(regressor, data.frame(Volume = as.numeric(test_set$Volume)))
test_set$predClose
plot(test_set$predClose)

# Visualising the Training set results
library(ggplot2)
ggplot() +geom_point(aes(x = dataset$Open, y = dataset$Volume), colour = 'purple')

ggplot()+ geom_point(aes(x = training_set$Open, y = predict(regressor, newdata = training_set)),
                     colour = 'red') + ggtitle('training set vs Predicted values')+ xlab('Open ') + ylab('Volume')


# Visualising the Training set results
lm <- summary(dataset)$r.squared
print(paste("linear model has an r-squared value of ", round(lm, 3), sep = ""))



#scatterplot
with(training_set,plot(Open, Volume))
abline(regressor)

with(training_set,plot(Open, Volume))
abline(h=1)


library(zoo)
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)


Open_prices=dataset[,2]
High_prices=dataset[,3]
Low_prices=dataset[,4]
Close_prices=dataset[,5]
Adjusted_prices=dataset[,6]
volume_prices=dataset[,7]

par(mfrow=c(2,3))

plot(Open_prices,main="Open Price")
plot(High_prices,main="High Price")
plot(Low_prices,main="Low Price")
plot(Close_prices,main="close Price")
plot(volume_prices,main="volume Price")
plot(Adjusted_prices,main="Adjusted Price")

predic_price=Adjusted_prices

Acf(predic_price,main='ACF for Differenced Series')
Pacf(predic_price,main='PACF for Differenced Series',col='#cc0000')

Auto_cf=Acf(predic_price,plot=FALSE)
Auto_cf

PAuto_cf=Pacf(predic_price)

return_data<- 100*diff(log(predic_price))

data_return_train<- return_data[1:(0.9*length(return_data))]

data_return_test <-return_data[(0.9*length(return_data)+1):length(return_data)]

auto.arima(data_return_train,seasonal=FALSE)

fit <- Arima(data_return_train,order=c(1,0,0))

preds <- predict(fit,n.ahead=(length(return_data)-(0.9*length(return_data))))$pred
preds 

test_forecast <- forecast(fit,h=15)
test_forecast

par(mfrow=c(1,1))

plot(test_forecast,main='Arima forecast For Tesla Stock')

accuracy(preds,data_return_test)

#univariating the timeseries

print(adf.test(predic_price))



##############################   KMEANS   ###########################################################
dataset = read.csv('TSLA.csv')

dataset$Volume<-as.numeric(dataset$Volume)

library(ClusterR)

library(cluster)

set.seed(2416)

kmeans.re <- kmeans(na.omit(dataset$Volume),3) # this helps in omitting NA 
kmeans.re


cm <- table(dataset$Volume, kmeans.re$cluster)
cm

plot(dataset[c("Open", "Close")])

plot(dataset[c("Open", "Close")], 
     col = kmeans.re$cluster)

plot(dataset[c("Open", "Close")], 
     col = kmeans.re$cluster, 
     main = "K-means with 3 clusters")

kmeans.re$centers


y_kmeans <- kmeans.re$cluster
clusplot(dataset[, c("High", "Close")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster Stock"),
         xlab = 'High',
         ylab = 'Close')


