remove(list=ls())
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries)
library(graphics)
library(zoo)


globalsales <- read.csv('Global Superstore.csv',stringsAsFactors = F)
str(globalsales)

# Check if any duplicated rows
sum(duplicated(globalsales))  # Returns 0 meaning no duplicated rows
sum(is.na(globalsales)) # 41296
sapply(globalsales, function(x) sum(is.na(x)))  # Postal Code column has Missing Values. We can ignore it as we would not be
# be dealing with this Coulmn.

# Select the subset of the data frame to work further

globalsales <- globalsales[,names(globalsales) %in% c('Order.Date','Market','Segment','Sales','Quantity','Profit')]

# Change Market and Segment as factors

globalsales$Market <-  as.factor(globalsales$Market)
globalsales$Segment <- as.factor(globalsales$Segment)

# Convert Order.Date column into Date column

globalsales$Order.Date <- as.Date(globalsales$Order.Date,'%d-%m-%Y')

# Creating another Column with Year-Mon

globalsales$yearmon <- as.yearmon(globalsales$Order.Date, "%Y-%m")

# Create a month Variable. Jan 2011 will be month 1 and Dec 2014 will be month 48

globalsales$Month <- ((globalsales$yearmon-min(globalsales$yearmon))*12)+1

# Create data frames by Market and Segments

# Create an empty list to hold all the dataframes which can be used in further data processing

dflist <- list()


for(market in levels(globalsales$Market)){         # Iterate through levels of Market
  for (segment in levels(globalsales$Segment))     # Iterate through levels of Segment
  {
    name <- paste(market,segment,sep='_')          # Creating a dataframe name with Market and Segment names
    temp <- assign(name,arrange(subset(globalsales,(globalsales$Market == market & globalsales$Segment == segment)),Order.Date)) #  Creating actual df and assigning name to it
    dflist[[name]] <- temp                         # Add data frame to the list
  }
  
}

length(dflist)   # 21 - Double check if all 21 df's have been created


# Creating a empty Data frame to hold Market Segment, Profit and CV values to compare.

subdf <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("MarketSegment", "TotalProfit", "CV"))  
subdf <- as_tibble(subdf)

# Iterate through each dataframe and create an aggregated data frames ( aggregate, Sales, Profit and Quantity on Date-Mon)

for(i in names(dflist)){
  MarketSegment <- i        # Assign MarketSegment name to a variable
  salesdf <- paste(MarketSegment,'sales',sep = '_')
  Quantdf <- paste(MarketSegment,'Quant',sep = '_')
  Profitdf <- paste(MarketSegment,'Profit',sep = '_')
  df <- dflist[[i]]         # Assign dataframe to a variable
  Salesaggdf <- assign(salesdf,as.data.frame(df %>% 
                                               group_by(Month) %>% 
                                               summarise(MonthlySale=sum(Sales)))) # Aggregate on Sales
  Quantityaggdf <-  assign(Quantdf,as.data.frame(df %>% 
                                                   group_by(Month) %>% 
                                                   summarise(MonthlyQuantity=sum(Quantity))))
  Profitaggdf  <- assign(Profitdf,as.data.frame(df %>% 
                                                  group_by(Month) %>% 
                                                  summarise(MonthlyProfit=sum(Profit))))
  #MonthlyQuantity=sum(Quantity),      # Aggregate on Quantity
  #MonthlyProfit=sum(Profit))))        # Aggregate on Profit

  TotalProfit <- sum(Profitaggdf$MonthlyProfit)                               # Creating Total Profit of each Market Segment
  CV <- (sd(Profitaggdf$MonthlyProfit) / mean(Profitaggdf$MonthlyProfit))*100       # Calculate the Coefficient of Variation 
  profitcv <- c(MarketSegment,TotalProfit,CV)                           # Creating a vector of the above calc values
  subdf <- add_row(subdf,MarketSegment = MarketSegment,TotalProfit = TotalProfit,CV = CV) # adding the vector to the dataframe
}

subdf <- as.data.frame(subdf) # Converting tibble to a data frame
subdf %>%
  arrange(desc(TotalProfit)) %>%
  slice(1:2) %>%
   ggplot(., aes( x=MarketSegment, y=TotalProfit, fill=CV)) + geom_bar(stat = "identity") +
   geom_text(aes(label=TotalProfit),hjust=0.5, vjust=0) + theme(axis.text.x = element_text(angle = 90))

#   MarketSegment TotalProfit   CV
#1  APAC_Consumer    222817.6  63.21323
#2    EU_Consumer    188687.7  62.43052

ggplot(subdf,aes(CV,TotalProfit,col=as.factor(MarketSegment)))+       # From the plot we can clearly see that 
  geom_point()+                                                       # APAC_Consumer and EU_Consumer has high profits
  geom_text(aes(label=MarketSegment),hjust=0, vjust=0)                # and Low CV. 

ggplot(subdf,aes(MarketSegment,CV))+
  geom_bar(stat="identity",fill = "gray50")+ 
  geom_text(aes(label = round(as.double(CV),digits =1))) +
  coord_flip()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# To test the accuracy of forecast, let's separate out the last 6 months values from the dataset, 
# After aggregating the transaction level data into the monthly data, check your 6 months forecast using the out-of-sample figures

APAC_Consumer_sales_total_ts <- ts(APAC_Consumer_sales$MonthlySale)
APAC_Consumer_Quant_total_ts <- ts(APAC_Consumer_Quant$MonthlyQuantity)
EU_Consumer_sales_total_ts <- ts(EU_Consumer_sales$MonthlySale)
EU_Consumer_Quant_total_ts <- ts(EU_Consumer_Quant$MonthlyQuantity)

# Now lets get first 42 months data
APAC_Consumer_Quant_train <- APAC_Consumer_Quant[1:(length(APAC_Consumer_Quant$Month)-6),]
APAC_Consumer_sales_train <- APAC_Consumer_sales[1:(length(APAC_Consumer_sales$Month)-6),]
EU_Consumer_Quant_train <- EU_Consumer_Quant[1:(length(EU_Consumer_Quant$Month)-6),]
EU_Consumer_sales_train <- EU_Consumer_sales[1:(length(EU_Consumer_Quant$Month)-6),]

# Creating Time series of Sales and Quantity for each Market Segment
# Time series of APAC_Consumer Market Segment
APAC_Consumer_sales_ts <- ts(APAC_Consumer_sales_train$MonthlySale)
APAC_Consumer_quantity_ts <- ts(APAC_Consumer_Quant_train$MonthlyQuantity)

# Time series of EU_Consumer Market Segment
EU_Consumer_sales_ts <- ts(EU_Consumer_sales_train$MonthlySale)
EU_Consumer_quantity_ts <- ts(EU_Consumer_Quant_train$MonthlyQuantity)

####################################### Lets start with smoothing the series.  #########################################

############################################## APAC Consumer Sales #####################################################
plot(APAC_Consumer_sales_ts)

##Decomposition of mutiplicative time Series
ts_air = ts(APAC_Consumer_sales_train$MonthlySale, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")
plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)


w<-2
APAC_Consumer_sales_smoothed <- stats::filter(APAC_Consumer_sales_ts,
                                               filter=rep(1/(2*w+1),(2*w+1)),
                                               method='convolution', sides=2)
#Smoothing left end of the time series

diff <- APAC_Consumer_sales_smoothed[w+2] - APAC_Consumer_sales_smoothed[w+1]
 for (i in seq(w,1,-1)) {
   diff <- APAC_Consumer_sales_smoothed[w+2] - APAC_Consumer_sales_smoothed[w+1]
   APAC_Consumer_sales_smoothed[i] <- APAC_Consumer_sales_smoothed[i+1] - diff
}
# #Smoothing right end of the time series
n <- length(APAC_Consumer_sales_ts)
diff <- APAC_Consumer_sales_smoothed[n-w] - APAC_Consumer_sales_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
   APAC_Consumer_sales_smoothed[i] <- APAC_Consumer_sales_smoothed[i-1] + diff
}

plot(APAC_Consumer_sales_ts)
lines(APAC_Consumer_sales_smoothed, col="blue", lwd=2)

############ Predict future sales using classical decomposition 

APAC_Sales_timevalues <- APAC_Consumer_sales_train$Month

APAC_Consumer_sales_smoothed_df <- as.data.frame(cbind(APAC_Sales_timevalues, as.vector(APAC_Consumer_sales_smoothed)))
colnames(APAC_Consumer_sales_smoothed_df) <- c('Month', 'MonthlySales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

plot(APAC_Consumer_sales_smoothed)

RMSE = function(predicted, original){
  sqrt(mean((predicted - original)^2))
}

APAC_sales_lmfit <- lm(MonthlySales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                       + Month, data=APAC_Consumer_sales_smoothed_df)

APAC_sales_global_pred <- predict(APAC_sales_lmfit, Month=APAC_Sales_timevalues)

RMSE(APAC_sales_global_pred,APAC_Consumer_sales_smoothed_df$MonthlySales)

# Tried multiple combinations of sinusoidal and Polynomial degrees. Found the best values of 0.6 for seasonality and 3 for plotting poly trend.
# (sin-0.5, poly-3, RMSE - 1752) 
# (Sin-0.6, poly-3, RMSE - 1877)
# (sin-0.5, poly-2, RMSE - 2493) 
# (sin-0.6, poly-2, RMSE - 4215.797)
# (sin-0.7, poly-3, RMSE - 4126.637)
# (sin-0.7, poly-2, RMSE - 5167.574)

summary(APAC_sales_global_pred)
lines(APAC_Sales_timevalues, APAC_sales_global_pred, col='red', lwd=2)
#Now, let's look at the locally predictable series  

#We modelled it as an ARMA series. Lets remove global pred from TS to get local pred component

APAC_Consumer_sales_local_pred <- APAC_Consumer_sales_ts - APAC_sales_global_pred
#Plot local pred to check if we have removed the global pred
plot(APAC_Consumer_sales_local_pred, col='blue', type = "l") # No trend and seasonality
# Now we will have to perform ARMA modelling on local pred. lets use ACF and PACF to get the appropriate P and Q values
# Lets plot ACF and PACF to find p and q values
acf(APAC_Consumer_sales_local_pred) # P = 0
acf(APAC_Consumer_sales_local_pred, type="partial") # q = 0

# Lets double check with auto arima
armafit <- auto.arima(APAC_Consumer_sales_local_pred)
tsdiag(armafit)
armafit
# Series: APAC_Consumer_sales_local_pred 
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 86951734:  log likelihood=-443.49
# AIC=888.99   AICc=889.09   BIC=890.72

# Check if the residual series is white noise or not. 
APAC_resi <- APAC_Consumer_sales_local_pred - fitted(armafit)

adf.test(APAC_resi,alternative = "stationary")  # P is less than 0.05. so we reject H0 that TS is not stationary
kpss.test(APAC_resi) # P > 0.1 is greater that limit 0.05, so we accept H0 that TS is stationary.
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
APAC_Consumer_sales_to_pred <- APAC_Consumer_sales[(length(APAC_Consumer_sales$Month)-5):length(APAC_Consumer_sales$Month),]

APAC_Consumer_sales_to_pred_tv <- APAC_Consumer_sales_to_pred$Month

APAC_Sales_global_pred_out <- predict(APAC_sales_lmfit,data.frame(Month =APAC_Consumer_sales_to_pred_tv))

APAC_Sales_fcast <- APAC_Sales_global_pred_out
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(APAC_Sales_fcast,APAC_Consumer_sales_to_pred[,2])[5]
MAPE_class_dec  # 23.86525

#Let's also plot the predictions along with original values, to get a visual feel of the fit
class_dec_pred <- c(ts(APAC_sales_global_pred),ts(APAC_Sales_global_pred_out))
plot(APAC_Consumer_sales_total_ts)
lines(class_dec_pred, col = "red")

#Lets now do and ARIMA fit for the APAC Consumer sales segment.
APAC_sales_autoarima <- auto.arima(APAC_Consumer_sales_ts)
APAC_sales_autoarima
# Series: APAC_Consumer_sales_ts 
# ARIMA(0,1,1) 
# Coefficients:
#   ma1
# -0.7559
# s.e.   0.1381
# sigma^2 estimated as 174361555:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66

tsdiag(APAC_sales_autoarima)
plot(APAC_sales_autoarima$x, col="black")
lines(fitted(APAC_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise
APAC_sales_resi_auto_arima <- APAC_Consumer_sales_ts - fitted(APAC_sales_autoarima)

adf.test(APAC_sales_resi_auto_arima,alternative = "stationary") # P values less than cutoff(0.05), hence rejecting H0.
kpss.test(APAC_sales_resi_auto_arima) # P value greater than cutoff(0.05), hence not enough evidance to reject H0

#Also, let's evaluate the model using MAPE
APAC_sales_fcast_auto_arima <- predict(APAC_sales_autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(APAC_sales_fcast_auto_arima$pred,APAC_Consumer_sales_to_pred[,2])[5]
MAPE_auto_arima # 27.68952

#Lastly, let's plot the predictions along with original values, to get a visual feel of the fit
APAC_sales_auto_arima_pred <- c(fitted(APAC_sales_autoarima),ts(APAC_sales_fcast_auto_arima$pred))
plot(APAC_Consumer_sales_total_ts, col = "black")
lines(APAC_sales_auto_arima_pred, col = "red")

##Conclusion: 
# The MAPE value of classical decomposition is less than that of AUTO ARIMA model. The log likelihood is also higher for classical decomposition and also 
# it has low AIC values compared to AUTO ARIMA method.
####################Forcasting Next 6 months #########################
#lets use most optimal model i.e classical decomposition
new_forecast_sales <- predict(APAC_sales_lmfit, newdata=data.frame(Month=seq(49,54)))

plot(ts(c(ts(APAC_Consumer_sales$MonthlySale),ts(new_forecast_sales))),col='red')
lines(ts(APAC_Consumer_sales$MonthlySale),col='blue')


############################################## APAC Consumer Quantity forecasting #####################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(APAC_Consumer_quantity_ts)

##Decomposition of mutiplicative time Series
ts_air  <- ts(APAC_Consumer_Quant_train$MonthlyQuantity, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")
plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)

# Smoothen the TS with filter method
w <-2
APAC_Consumer_quantity_smoothed <- stats::filter(APAC_Consumer_quantity_ts, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)
#Smoothing left end of the time series
diff <- APAC_Consumer_quantity_smoothed[w+2] - APAC_Consumer_quantity_smoothed[w+1]
for (i in seq(w,1,-1)) {
  APAC_Consumer_quantity_smoothed[i] <- APAC_Consumer_quantity_smoothed[i+1] - diff
}
#Smoothing right end of the time series
n <- length(APAC_Consumer_quantity_ts)
diff <- APAC_Consumer_quantity_smoothed[n-w] - APAC_Consumer_quantity_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Consumer_quantity_smoothed[i] <- APAC_Consumer_quantity_smoothed[i-1] + diff
}

plot(APAC_Consumer_quantity_ts)
lines(APAC_Consumer_quantity_smoothed,col='blue',lwd=2)

#########3# Lets predict future sales using classical decomposition 
APAC_quantity_timevalues <- APAC_Consumer_Quant_train$Month
APAC_Consumer_quantity_smoothed_df <- as.data.frame(cbind(APAC_quantity_timevalues, as.vector(APAC_Consumer_quantity_smoothed)))
colnames(APAC_Consumer_quantity_smoothed_df) <- c('Month', 'MonthlyQuantity')
# Lets fit a model to get the trend and seasonality in the data.
plot(APAC_Consumer_quantity_smoothed)

APAC_quantity_lmfit <- lm(MonthlyQuantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                          +Month, data=APAC_Consumer_quantity_smoothed_df)

APAC_quantity_global_pred <- predict(APAC_quantity_lmfit, Month=APAC_quantity_timevalues)

RMSE(APAC_quantity_global_pred,APAC_Consumer_quantity_smoothed_df$MonthlyQuantity)
# Tried multiple combinations of sinusoidal and Polynomial degrees. Found the best values of 0.5 for seasonality and 3 for plotting poly trend.
# (sin-0.5, poly-3, RMSE - 20.2) 
# (Sin-0.5, poly-2, RMSE - 25)
# (sin-0.6, poly-3, RMSE - 20.79)  
summary(APAC_quantity_global_pred)
lines(APAC_quantity_timevalues, APAC_quantity_global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series  
#Modelled it as an ARMA series
APAC_Consumer_quantity_local_pred <- APAC_Consumer_quantity_ts - APAC_quantity_global_pred
plot(APAC_Consumer_quantity_local_pred, col='blue', type = "l")
acf(APAC_Consumer_quantity_local_pred)

acf(APAC_Consumer_quantity_local_pred, type="partial")
armafit <- auto.arima(APAC_Consumer_quantity_local_pred)

tsdiag(armafit)
armafit

# Series: APAC_Consumer_quantity_local_pred 
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 11108:  log likelihood=-255.22
# AIC=512.44   AICc=512.54   BIC=514.18

#We'll check if the residual series is white noise
APAC_quant_resi <- APAC_Consumer_quantity_local_pred - fitted(armafit)

adf.test(APAC_quant_resi,alternative = "stationary")  # p-value = 0.01 < 0.05 meaning we reject H0
kpss.test(APAC_quant_resi) # P-value > 0.1 meaning not enough evidance to reject H0.

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
APAC_Consumer_quantity_to_pred <- APAC_Consumer_Quant[(length(APAC_Consumer_Quant$Month)-5):length(APAC_Consumer_Quant$Month),]

APAC_Consumer_quantity_to_pred_tv <- APAC_Consumer_quantity_to_pred$Month

APAC_quantity_global_pred_out <- predict(APAC_quantity_lmfit,data.frame(Month =APAC_Consumer_quantity_to_pred_tv))

APAC_quantity_fcast <- APAC_quantity_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(APAC_quantity_fcast,APAC_Consumer_quantity_to_pred[,2])[5]
MAPE_class_dec #32.23957

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(APAC_quantity_global_pred),ts(APAC_quantity_global_pred_out))
plot(APAC_Consumer_Quant_total_ts, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

APAC_quantity_autoarima <- auto.arima(APAC_Consumer_quantity_ts)
APAC_quantity_autoarima
# Series: APAC_Consumer_quantity_ts 
# ARIMA(0,1,0) 
# sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85

tsdiag(APAC_quantity_autoarima)
plot(APAC_quantity_autoarima$x, col="black")
lines(fitted(APAC_quantity_autoarima), col="red")

#Again, let's check if the residual series is white noise
APAC_quantity_resi_auto_arima <- APAC_Consumer_quantity_ts - fitted(APAC_quantity_autoarima)

adf.test(APAC_quantity_resi_auto_arima,alternative = "stationary") # P values < cutoff (0.05), we reject H0
kpss.test(APAC_quantity_resi_auto_arima) # P Value greater than cutoff (0.05). failed to reject H0

#Also, let's evaluate the model using MAPE
APAC_quantity_fcast_auto_arima <- predict(APAC_quantity_autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(APAC_sales_fcast_auto_arima$pred,APAC_Consumer_sales_to_pred[,2])[5]
MAPE_auto_arima #27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

APAC_quantity_auto_arima_pred <- c(fitted(APAC_quantity_autoarima),ts(APAC_quantity_fcast_auto_arima$pred))
plot(APAC_Consumer_Quant_total_ts, col = "black")
lines(APAC_quantity_auto_arima_pred, col = "red")

##Conclusion: 
# The Auto ARIMA model is doing in terms of MAPE value. it is lower than the classical decomposition method.
# from the plot we can see it kind of predict same as original time series.

####################Forcasting Next 6 months #########################
Forecast_sales <- forecast(auto.arima(APAC_Consumer_Quant_total_ts), h=6)
Forecast_sales
plot(Forecast_sales)

################################################ EU CONSUMER ###########################################################
########################################################################################################################
############################################## EU Consumer Sales #####################################################

plot(EU_Consumer_sales_ts)

##Decomposition of mutiplicative time Series
ts_air  <- ts(EU_Consumer_sales_train$MonthlySale, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")
plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)

w <-1

EU_Consumer_sales_smoothed <- stats::filter(EU_Consumer_sales_ts, 
                                            filter=rep(1/(2*w+1),(2*w+1)), 
                                            method='convolution', sides=2)
plot(EU_Consumer_sales_ts)

lines(EU_Consumer_sales_smoothed, col="blue", lwd=2)


#Smoothing left end of the time series

for (i in seq(w,1,-1)) {
  diff <- EU_Consumer_sales_smoothed[w+2] - EU_Consumer_sales_smoothed[w+1]
  EU_Consumer_sales_smoothed[i] <- EU_Consumer_sales_smoothed[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Consumer_sales_ts)
diff <- EU_Consumer_sales_smoothed[n-w] - EU_Consumer_sales_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_sales_smoothed[i] <- EU_Consumer_sales_smoothed[i-1] + diff
}

plot(EU_Consumer_sales_ts)
lines(EU_Consumer_sales_smoothed, col="blue", lwd=2)


# Lets predict future sales using classical decomposition 
EU_Sales_timevalues <- EU_Consumer_sales_train$Month

EU_Consumer_sales_smoothed_df <- as.data.frame(cbind(EU_Sales_timevalues, as.vector(EU_Consumer_sales_smoothed)))
colnames(EU_Consumer_sales_smoothed_df) <- c('Month', 'MonthlySales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

plot(EU_Consumer_sales_smoothed)

EU_sales_lmfit <- lm(MonthlySales ~ sin(0.6*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3)
                     + Month, data=EU_Consumer_sales_smoothed_df)

EU_sales_global_pred <- predict(EU_sales_lmfit, Month=EU_Sales_timevalues)
summary(EU_sales_global_pred)
RMSE(EU_sales_global_pred,EU_Consumer_sales_smoothed_df$MonthlySales) 

# Tried multiple combinations of sinusoidal and Polynomial degrees. Found the best values of 0.6 for seasonality and 3 for plotting poly trend.
# (sin-0.5, poly-3, RMSE - 3991.79) 
# (Sin-0.5, poly-2, RMSE - 4003.83)
# (sin-0.6, poly-3, RMSE - 3472.066) 
# (sin-0.6, poly-2, RMSE - 3510.416)
# (sin-0.7, poly-3, RMSE - 3516.918)
# (sin-0.7, poly-2, RMSE - 3950.082)

plot(EU_sales_global_pred)
lines(EU_Sales_timevalues, EU_sales_global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series  

#We will model it as an ARMA series

EU_Consumer_sales_local_pred <- EU_Consumer_sales_ts - EU_sales_global_pred
plot(EU_Consumer_sales_local_pred, col='blue', type = "l")
acf(EU_Consumer_sales_local_pred)
acf(EU_Consumer_sales_local_pred, type="partial")
armafit <- auto.arima(EU_Consumer_sales_local_pred)
summary(armafit)
tsdiag(armafit)
armafit

# Series: EU_Consumer_sales_local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 9439649:  log likelihood=-445.22
#AIC=892.44   AICc=892.54   BIC=894.18

#Check if the residual series is white noise

EU_sales_resi <- EU_Consumer_sales_local_pred - fitted(armafit)
adf.test(EU_sales_resi,alternative = "stationary")  # P is less than 0.05. so we reject null hypo that TS is not stationary
kpss.test(EU_sales_resi)
##Dickey-Fuller = -6.3541, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary
#KPSS Level = 0.021547, Truncation lag parameter = 1, p-value = 0.1
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
plot(EU_Consumer_sales_local_pred)
EU_Consumer_sales_to_pred <- EU_Consumer_sales[(length(EU_Consumer_sales$Month)-5):length(EU_Consumer_sales$Month),]
colnames(EU_Consumer_sales_to_pred) <- c('Month', 'MonthlySales')
EU_sales_to_pred_tv <- EU_Consumer_sales_to_pred$Month

EU_Sales_global_pred_out <- predict(EU_sales_lmfit,data.frame(Month =EU_sales_to_pred_tv))
EU_Sales_fcast <- EU_Sales_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

EU_sales_MAPE_class_dec <- accuracy(EU_Sales_fcast,EU_Consumer_sales_to_pred[,2])[5]
EU_sales_MAPE_class_dec
#31.58102

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

EU_sale_class_dec_pred <- c(ts(EU_sales_global_pred),ts(EU_Sales_global_pred_out))
plot(EU_Consumer_sales_total_ts, col = "black")
lines(EU_sale_class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit
acf(EU_Consumer_sales_ts)
pacf(EU_Consumer_sales_ts)

EU_sales_autoarima <- auto.arima(EU_Consumer_sales_ts)
summary(EU_sales_autoarima)

#ARIMA(2,1,0) 
#Coefficients:
#  ar1      ar2
#-0.5796  -0.4906
#s.e.   0.1346   0.1310
#sigma^2 estimated as 168564623:  log likelihood=-445.84
#AIC=897.67   AICc=898.32   BIC=902.81


tsdiag(EU_sales_autoarima)
plot(EU_sales_autoarima$x, col="black")
lines(fitted(EU_sales_autoarima), col="red")

#Again, let's check if the residual series is white noise

EU_sales_resi_auto_arima <- EU_Consumer_sales_ts - fitted(EU_sales_autoarima)
adf.test(EU_sales_resi_auto_arima,alternative = "stationary") # p-value = 0.01 - alternative hypothesis: stationary
kpss.test(EU_sales_resi_auto_arima) ## KPSS Level = 0.05314

#Also, let's evaluate the model using MAPE

EU_sales_fcast_auto_arima <- predict(EU_sales_autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(EU_sales_fcast_auto_arima$pred,EU_Consumer_sales_to_pred[,2])[5]
MAPE_auto_arima
# 28.9226
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

EU_sales_auto_arima_pred <- c(fitted(EU_sales_autoarima),ts(EU_sales_fcast_auto_arima$pred))

plot(EU_Consumer_sales_total_ts, col = "black")
lines(EU_sales_auto_arima_pred, col = "red")

##Conclusion: 

#ARIMA has less MAPE value than Classical decomposition model
#So Auto Arima model performed well than Classical decomposition model. 

####################Forcasting Next 6 months #########################

Forecast_sales <- forecast(auto.arima(EU_Consumer_sales_total_ts), h=6)
plot(Forecast_sales)

############################################## EU Consumer Qualtity #####################################################

##Decomposition of mutiplicative time Series
ts_air  <- ts(EU_Consumer_Quant_train$MonthlyQuantity, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")
plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)

# Smoothen the TS with filter method
w <-1

EU_Consumer_quantity_smoothed <- stats::filter(EU_Consumer_quantity_ts, 
                                               filter=rep(1/(2*w+1),(2*w+1)), 

                                                                                              method='convolution', sides=2)
plot(EU_Consumer_quantity_ts)
lines(EU_Consumer_quantity_smoothed, col="blue", lwd=2)


#Smoothing left end of the time series

for (i in seq(w,1,-1)) {
  diff <- EU_Consumer_quantity_smoothed[w+2] - EU_Consumer_quantity_smoothed[w+1]
  EU_Consumer_quantity_smoothed[i] <- EU_Consumer_quantity_smoothed[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Consumer_quantity_ts)
diff <- EU_Consumer_quantity_smoothed[n-w] - EU_Consumer_quantity_smoothed[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Consumer_quantity_smoothed[i] <- EU_Consumer_quantity_smoothed[i-1] + diff
}

plot(EU_Consumer_quantity_smoothed)
lines(EU_Consumer_quantity_smoothed, col="blue", lwd=2)


# Lets predict future quantity using classical decomposition 
EU_quantity_timevalues <- EU_Consumer_Quant_train$Month

EU_Consumer_quantity_smoothed_df <- as.data.frame(cbind(EU_quantity_timevalues, as.vector(EU_Consumer_quantity_smoothed)))
colnames(EU_Consumer_quantity_smoothed_df) <- c('Month', 'Monthlyquantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

plot(EU_Consumer_quantity_smoothed)


EU_quantity_lmfit <- lm(Monthlyquantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                        + Month, data=EU_Consumer_quantity_smoothed_df)

EU_quantity_global_pred <- predict(EU_quantity_lmfit, Month=EU_quantity_timevalues)
summary(EU_quantity_global_pred)
RMSE(EU_quantity_global_pred,EU_Consumer_quantity_smoothed_df$Monthlyquantity) 
#32.4124
# Tried multiple combinations of sinusoidal and Polynomial degrees. Found the best values of 0.6 for seasonality and 3 for plotting poly trend.
# (sin-0.5, poly-3, RMSE - 35.25006) 
# (Sin-0.5, poly-2, RMSE - 35.53535)
# (sin-0.6, poly-3, RMSE - 32.4124) 
# (sin-0.6, poly-2, RMSE - 33.81755)
# (sin-0.7, poly-3, RMSE - 33.37074)
# (sin-0.7, poly-2, RMSE - 36.15735)

plot(EU_quantity_global_pred)
lines(EU_quantity_timevalues, EU_quantity_global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series  

#Modelled it as an ARMA series

EU_Consumer_quantity_local_pred <- EU_Consumer_quantity_ts - EU_quantity_global_pred
plot(EU_Consumer_quantity_local_pred, col='blue', type = "l")
acf(EU_Consumer_quantity_local_pred)
acf(EU_Consumer_quantity_local_pred, type="partial")
armafit <- auto.arima(EU_Consumer_quantity_local_pred)
summary(armafit)
tsdiag(armafit)
armafit

# Series: EU_Consumer_quantity_local_pred 
#ARIMA(2,0,0) with zero mean 

#sigma^2 estimated as 7284:  log likelihood=-245.89
#AIC=497.79   AICc=498.42   BIC=503

#Check if the residual series is white noise

EU_quantity_resi <- EU_Consumer_quantity_local_pred - fitted(armafit)
adf.test(EU_quantity_resi,alternative = "stationary")  # P is less than 0.05. so we reject null hypo that TS is not stationary
kpss.test(EU_quantity_resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
plot(EU_Consumer_quantity_local_pred)
EU_Consumer_quantity_to_pred <- EU_Consumer_Quant[(length(EU_Consumer_Quant$Month)-5):length(EU_Consumer_Quant$Month),]
colnames(EU_Consumer_quantity_to_pred) <- c('Month', 'Monthlyquantity')
EU_quantity_to_pred_tv <- EU_Consumer_quantity_to_pred$Month

EU_quantity_global_pred_out <- predict(EU_quantity_lmfit,data.frame(Month =EU_quantity_to_pred_tv))
EU_quantity_fcast <- EU_quantity_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

EU_quantity_MAPE_class_dec <- accuracy(EU_quantity_fcast,EU_Consumer_quantity_to_pred[,2])[5]
EU_quantity_MAPE_class_dec
#30.39741

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

EU_sale_class_dec_pred <- c(ts(EU_quantity_global_pred),ts(EU_quantity_global_pred_out))
plot(EU_Consumer_Quant_total_ts, col = "black")
lines(EU_sale_class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

EU_quantity_autoarima <- auto.arima(EU_Consumer_quantity_ts)
summary(EU_quantity_autoarima)

#ARIMA(2,1,0)  
#Coefficients:
#  ar1      ar2
#      -0.7359  -0.5879
#s.e.   0.1224   0.1185
#sigma^2 estimated as 21185:  log likelihood=-261.9
#AIC=529.8   AICc=530.44   BIC=534.94

tsdiag(EU_quantity_autoarima)
plot(EU_quantity_autoarima$x, col="black")
lines(fitted(EU_quantity_autoarima), col="red")

#Again, let's check if the residual series is white noise

EU_quantity_resi_auto_arima <- EU_Consumer_quantity_ts - fitted(EU_quantity_autoarima)
adf.test(EU_quantity_resi_auto_arima,alternative = "stationary") # p-value = 0.04521 - alternative hypothesis: stationary
kpss.test(EU_quantity_resi_auto_arima) ## KPSS Level = 0.047939

#Also, let's evaluate the model using MAPE

EU_quantity_fcast_auto_arima <- predict(EU_quantity_autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(EU_quantity_fcast_auto_arima$pred,EU_Consumer_quantity_to_pred[,2])[5]
MAPE_auto_arima
# 30.13319
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

EU_quantity_auto_arima_pred <- c(fitted(EU_quantity_autoarima),ts(EU_quantity_fcast_auto_arima$pred))

plot(EU_Consumer_Quant_total_ts, col = "black")
lines(EU_quantity_auto_arima_pred, col = "red")

##Conclusion: 

#ARIMA has less MAPE value than Classical decomposition model
#So Classical decomposition model performed well than the Auto Arima model. 

####################Forcasting Next 6 months #########################
Forecast_sales <- forecast(auto.arima(EU_Consumer_Quant_total_ts), h=6)
Forecast_sales
plot(Forecast_sales)