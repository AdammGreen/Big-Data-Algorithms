# install.packages("forecast")
# install.packages("tseries")

library("forecast")
library("tseries")
library('ggplot2')

          
# Load the dataset
          
Fish_data <- read.csv('/Users/adamdanielgreen/Desktop/Business Statistics/Times Series-20231219/Fish_dataset.csv',header=T)

# Convert the variable Date into the 'date' format
Fish_data$Date <- as.Date(Fish_data$start,format = "%d/%m/%Y")

# Plot the time series

library(ggplot2)

ggplot(Fish_data,aes(Date,sales))+geom_line()+scale_x_date('Date')+ylab("Fish Sales")+xlab("")

# Create a time series object to store the variable sales

sales_ts = ts(Fish_data[, c('sales')])
          
# Use the function tsclean() to identify and replace outliers. 
# We take the log of the time series

Fish_data$clean_sales_ts<-log(tsclean(sales_ts))
          ggplot()+
            geom_line(data=Fish_data,aes(x=Date,y=clean_sales_ts))+
            ylab('Cleaned sales')

# To apply an ARMA model, the dataset needs to be a stationary. The *diff()* function is used to differenciate once and the result is plotted

plot(diff(Fish_data$clean_sales_ts),ylab="Diff_clean_sales_ts")
          abline(a = 0 , b = 0)

            
# Stationary test on the differenciated database
          
adf.test(diff(Fish_data$clean_sales_ts), alternative = "stationary")
          
# ACF and PACF plots for the differenced series
          
par(mfrow=c(1,2))
acf(diff(Fish_data$clean_sales_ts), lag.max=100 , main="" )
pacf(diff(Fish_data$clean_sales_ts), lag.max=100 , main="")

# Positive picks are observed in the ACF at h=12: the differenced time series of the daily number of bikes seems to be periodic;
# We will look for the best model using the function <font color=green>*auto.arima()*</font>
            
model_fit<-auto.arima(Fish_data$clean_sales_ts,max.p=5,
                                max.q=5,max.P=5,max.Q=5,max.d=3,max.D=3,seasonal=TRUE)
model_fit

# Adequation tests of residuals
          
plot(model_fit$residuals, ylab = "Residuals")
          abline(a=0 , b=0)
          
          
# Adequation tests of residuals: normality
          
par(mfrow=c(1,2))
hist(model_fit$residuals , xlab= "Residuals " , xlim=c (-2,2))
qqnorm(model_fit$residuals, main="" )
qqline(model_fit$residuals)

          
# Adequation tests of residuals: white noise
par(mfrow=c(1,2))
acf(model_fit$residuals, lag.max=100 , main="" )
pacf(model_fit$residuals, lag.max=100 , main="")

# Change of P and Q
          
model_fit2<-arima(Fish_data$clean_sales_ts, order=c(1,1,1), 
                            seasonal= list(order=c(1,1,1),period=12))
model_fit2

# Adequation tests of residuals
          
par(mfrow=c(1,2))
acf(model_fit2$residuals, lag.max=100 , main="" )
pacf(model_fit2$residuals, lag.max=100 , main="")

# Adequation tests of residuals

par(mfrow=c(1,2))
hist(model_fit2$residuals , xlab= "Residuals " , xlim=c (-2,2))
qqnorm(model_fit2$residuals, main="" )
qqline(model_fit2$residuals)

          
# Adequation tests of residuals

jarque.bera.test(model_fit2$residuals)
      
# Forecasting: predict the next 30 days total rental bikes
          
model_fit2.predict <- forecast(model_fit2, h=30)

model_fit2.predict$mean

# Forecasting: predict the next 30 days total rental bikes
          
plot(model_fit2.predict)

          
          
          
          
          
          