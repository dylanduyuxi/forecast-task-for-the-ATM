library(tsutils)
library(forecast)
library(smooth)
library(tseries)
library(greybox)

data<-read.csv("data.csv")
sum(is.na(data))
data[,1][is.na(data[,1])]=mean(na.exclude(data[,1]))
data[,2][is.na(data[,2])]=mean(na.exclude(data[,2]))
N67<-ts(data[,1],frequency = 365,start = c(1996,3,18),end = c(1998,5,17))
N107<-ts(data[,2],frequency = 365,start = c(1996,3,18),end = c(1998,5,17))
#plot the data
plot(N67)
plot(N107)
hist(N67)
hist(N107)
#decompose N67
d67_ad<-decomp(N67,decomposition = "additive",outplot = TRUE)
d67_mu<-decomp(N67,decomposition = "multiplicative",outplot = TRUE)
d67_au<-decomp(N67,decomposition = "auto",outplot = TRUE)
#decompose N107
d107_ad<-decomp(N107,decomposition = "additive",outplot = TRUE)
d107_mu<-decomp(N107,decomposition = "multiplicative",outplot = TRUE)
d107_au<-decomp(N107,decomposition = "auto",outplot = TRUE)

#acf
tsdisplay(N67)
tsdisplay(N107)
#kpss
kpss.test(N67)
kpss.test(N107)
#ORDER
ndiffs(N67)
nsdiffs(N67)
diff_N67<-diff(N67)
diff_N67<-diff(diff_N67,lag = 7)
kpss.test(diff_N67)
tsdisplay(diff_N67)

ndiffs(N107)
nsdiffs(N107)
diff_N107<-diff(N107)
diff_N107<-diff(N107,lag = 7)
kpss.test(diff_N107)
tsdisplay(diff_N107)

#irredgualr analysis
acf(d67_ad$irregular)
acf(d67_mu$irregular)
acf(d107_ad$irregular)
acf(d107_mu$irregular)





#split the series
N67_train<-ts(N67[1:521],frequency = 365,start = c(1996,3,18))
N67_test<-ts(N67[522:733],frequency = 365,end = c(1998,5,17))
N107_train<-ts(N107[1:521],frequency = 365,start = c(1996,3,18))
N107_test<-ts(N107[522:733],frequency = 365,end = c(1998,5,17))



#naive
N67_naive<-naive(N67_train)
N67_naifore<-N67_naive$mean[1]
N107_naive<-naive(N107_train)
N107_naifore<-N107_naive$mean[1]
plot(N67_naive)
plot(N107_naive)

sum(N67_train==0)
#SES(level) N67
ETS_ANN_opt  <- ets(N67_train, "ANN")
AIC(ETS_ANN_opt)

#Damped trend(trend)
ets_AAdN <- ets(N67_train, model="AAN", damped=TRUE)
AIC(ets_AAdN)
ets_AAN <- ets(N67_train, model="AAN")
AIC(ets_AAN)
ets_ANA<-ets(ts(N67_train,frequency = 12), model="ANA")
AIC(ets_ANA)
#holt
ho67_ad<-ets(ts(N67_train,frequency = 12), model="AAA", damped=FALSE)
AIC(ho67_ad)
ho67d_ad<-ets(ts(N67_train,frequency = 12), model="AAA", damped=TRUE)
AIC(ho67d_ad)




#get the best one by auto
ets_ZZZ <- ets(ts(N67_train,frequency = 12), model="ZZZ")


hist(ets_ZZZ$residuals)
qqnorm(y=ets_ZZZ$residuals)
qqline(ets_ZZZ$residuals)
tsdisplay(ets_ZZZ$residuals)



sum(N107_train==0)
#SES(level) N107
ETS_ANN7 <- ets(N107_train, "ANN")
AIC(ETS_ANN)

#Damped trend(trend)
ets_AAdN <- ets(N107_train, model="AAN", damped=TRUE)
AIC(ets_AAdN)
ets_AAN <- ets(N107_train, model="AAN")
AIC(ets_AAN)
ets_ANA<-ets(ts(N107_train,frequency = 12), model="ANA")
AIC(ets_ANA)
#
ho107_ad<-ets(ts(N107_train,frequency = 12), model="AAA", damped=FALSE)
AIC(ho107_ad)
ho107d_ad<-ets(ts(N107_train,frequency = 12), model="AAA", damped=TRUE)
AIC(ho107d_ad)



#get the best one by auto
ets_ZZZ <- ets(ts(N107_train,frequency = 12), model="ZZZ")
AIC(ets_ZZZ)
ets_ZZZ
#
hist(ets_ZZZ$residuals)
qqnorm(y=ets_ZZZ$residuals)
qqline(ets_ZZZ$residuals)
tsdisplay(ets_ZZZ$residuals)


#ARIMA
#kpss
kpss.test(N67_train)
#mannally ARIMA
#Box-Jenkins Methodology

N67_train<-ts(N67_train,frequency = 7)
#
diff_N67<-diff(N67_train)
kpss.test(diff_N67)#enough
tsdisplay(diff_N67)


diff_N67<-diff(diff_N67,lag = 7)
tsdisplay(diff_N67)
#according to the graph,start with the ARIMA()
arifit67<-Arima(N67_train,order = c(6,1,0),seasonal = c(0,1,0))
checkresiduals(arifit67)
arifit67<-Arima(N67_train,order = c(6,1,1),seasonal = c(0,1,0))
checkresiduals(arifit67)
arifit67<-Arima(N67_train,order = c(6,1,1),seasonal = c(0,1,2))
checkresiduals(arifit67)

coef(arifit67)

AIC(arifit67)
tsdisplay(arifit67$residuals)
hist(arifit67$residuals)
qqnorm(arifit67$residuals)
qqline(arifit67$residuals)

#ACF and PACF
arifitauto67<-auto.arima(N67_train)# get the auto outcome
#AIC
AIC(arifitauto67)
tsdisplay(arifitauto67$residuals)
qqnorm(arifitauto67$residuals)
qqline(arifitauto67$residuals)



ARIMA
#kpss
kpss.test(N107_train)
#mannally ARIMA
#Box-Jenkins Methodology

tsdisplay(N107_train)
N107_train<-ts(N107_train,frequency = 7)

diff_N107<-diff(N107_train,lag = 7)
tsdisplay(diff_N107)
#according to the graph,start with the ARIMA()
arifit107<-Arima(N107_train,order = c(2,0,0),seasonal = c(0,1,0))
checkresiduals(arifit107)
tsdisplay(arifit107$residuals)

arifit107<-Arima(N107_train,order = c(2,0,0),seasonal = c(7,1,0))
checkresiduals(arifit107)


coef(arifit107)

AIC(arifit107)
tsdisplay(arifit107$residuals)
hist(arifit107$residuals)
qqnorm(arifit107$residuals)
qqline(arifit107$residuals)

#ACF and PACF
arifitauto107<-auto.arima(N107_train)# get the auto outcome
#AIC
AIC(arifitauto107)
tsdisplay(arifitauto107$residuals)
hist(arifitauto107$residuals)
qqnorm(arifitauto107$residuals)
qqline(arifitauto107$residuals)


#regression
#plot the variable
N67train<-ts(N67[1:518],frequency = 365,start = c(1996,3,18))
N67test<-ts(N67[519:733],frequency = 365,end = c(1998,5,17))

#season
tsdisplay(N67train)
#fit
#create dummy
D1<-rep(c(1,0,0,0,0,0,0),74)
D2<-rep(c(0,1,0,0,0,0,0),74)
D3<-rep(c(0,0,1,0,0,0,0),74)
D4<-rep(c(0,0,0,1,0,0,0),74)
D5<-rep(c(0,0,0,0,1,0,0),74)
D6<-rep(c(0,0,0,0,0,1,0),74)
D7<-rep(c(0,0,0,0,0,0,1),74)
#new dataset
redata<-ts(cbind(N67train,D1,D2,D3,D4,D5,D6,D7))
colnames(redata) <- c("N67","D1","D2","D3","D4","D5","D6","D7")
fit2<-lm(N67~ .,data=redata)
summary(fit2)


#lag add
L1_N67<-lag(ts(redata[,"N67"]),-1)
L2_N67<-lag(ts(redata[,"N67"]),-2)
L3_N67<-lag(ts(redata[,"N67"]),-3)
L4_N67<-lag(ts(redata[,"N67"]),-4)
L5_N67<-lag(ts(redata[,"N67"]),-5)
L6_N67<-lag(ts(redata[,"N67"]),-6)
L7_N67<-lag(ts(redata[,"N67"]),-7)
data_colnames <- colnames(redata)
redata <- cbind(ts(redata),L1_N67,L2_N67,L3_N67,L4_N67,L5_N67,L6_N67,L7_N67)
colnames(redata) <- c(data_colnames,"L1_N67","L2_N67","L3_N67","L4_N67","L5_N67","L6_N67","L7_N67")
redata[is.na(redata)]=0

#delet Dummy
fit3<-lm(N67~D2+D4+D5+D6+L1_N67+L2_N67+L3_N67+L4_N67+L5_N67+L6_N67+L7_N67 ,data=redata)
summary(fit3)

#time trend
N67_trend<-c(1:525)
data_colnames <- colnames(redata)
redata <- cbind(redata,ts(N67_trend))
colnames(redata) <- c(data_colnames,"N67_trend")
fit4 <- lm(N67 ~ D2+D4+D6+L1_N67+L2_N67+N67_trend, data=redata)
summary(fit4)
fit5 <- lm(N67 ~ D2+D4+D6+L1_N67+L2_N67, data=redata)
summary(fit5)
tsdisplay(residuals(fit5))
#detect the correlation
sum(cor(redata,use = "c")>0.7)
#above all,finish

#AIC
  AIC(fit5)
tsdisplay(fit5$residuals)
hist(fit5$residuals)
qqnorm(fit5$residuals)
qqline(fit5$residuals)

#auto version

fit0 <- lm(N67 ~ 1, data=redata)
fitf<- lm(N67~.,data=redata)
fitauto67<-step(fit0,formula(fitf),direction = "both")
summary(fitauto67)
#AIC
AIC(fitauto67)
tsdisplay(fitauto67$residuals)
hist(fitauto67$residuals)
qqnorm(y=fitauto67$residuals)
qqline(fitauto67$residuals)



#plot the variable
N107train<-ts(N107[1:518],frequency = 365,start = c(1996,3,18))
N107test<-ts(N107[519:733],frequency = 365,end = c(1998,5,18))
plot(N107train)
#season
tsdisplay(N107train)
#fit
#season£¬dummy
D1<-rep(c(1,0,0,0,0,0,0),74)
D2<-rep(c(0,1,0,0,0,0,0),74)
D3<-rep(c(0,0,1,0,0,0,0),74)
D4<-rep(c(0,0,0,1,0,0,0),74)
D5<-rep(c(0,0,0,0,1,0,0),74)
D6<-rep(c(0,0,0,0,0,1,0),74)
D7<-rep(c(0,0,0,0,0,0,1),74)
#new dataset
redata<-ts(cbind(N107train,D1,D2,D3,D4,D5,D6,D7))
colnames(redata) <- c("N107","D1","D2","D3","D4","D5","D6","D7")
fit6<-lm(N107~ .,data=redata)
summary(fit6)


#lag add
L1_N107<-lag(ts(redata[,"N107"]),-1)
L2_N107<-lag(ts(redata[,"N107"]),-2)
L3_N107<-lag(ts(redata[,"N107"]),-3)
L4_N107<-lag(ts(redata[,"N107"]),-4)
L5_N107<-lag(ts(redata[,"N107"]),-5)
L6_N107<-lag(ts(redata[,"N107"]),-6)
L7_N107<-lag(ts(redata[,"N107"]),-7)
data_colnames <- colnames(redata)
redata <- cbind(ts(redata),L1_N107,L2_N107,L3_N107,L4_N107,L5_N107,L6_N107,L7_N107)
colnames(redata) <- c(data_colnames,"L1_N107","L2_N107","L3_N107","L4_N107","L5_N107","L6_N107","L7_N107")
redata[is.na(redata)]=0

#D
fit7<-lm(N107~D3+D4+D5+D6+L1_N107+L2_N107+L3_N107+L4_N107+L5_N107+L6_N107+L7_N107 ,data=redata)
summary(fit7)

#time trend
N107_trend<-c(1:518)
data_colnames <- colnames(redata)
redata <- cbind(redata,ts(N107_trend))
colnames(redata) <- c(data_colnames,"N107_trend")
fit8 <- lm(N107 ~ D4+D5+D6+L1_N107+L2_N107+L3_N107+L4_N107+L7_N107+N107_trend, data=redata)
summary(fit8)
fit9 <- lm(N107 ~  D4+D5+D6+L1_N107+L2_N107+L4_N107+L7_N107, data=redata)
summary(fit9)
tsdisplay(residuals(fit9))
#detect the correlation
sum(cor(redata,use = "c")>0.7)

#above all,finish

#AIC
AIC(fit9)
tsdisplay(fit9$residuals)
hist(fit9$residuals)
qqnorm(fit9$residuals)
qqline(fit9$residuals)

#auto version

fit0 <- lm(N107 ~ 1, data=redata)
fitf<- lm(N107~.,data=redata)
fitauto107<-step(fit0,formula(fitf),direction = "both")
summary(fitauto)
#AIC
AIC(fitauto107)
tsdisplay(fitauto107$residuals)
hist(fitauto107$residuals)
qqnorm(y=fitauto107$residuals)
qqline(fitauto107$residuals)



#model selection get the number by change "ourforecast"
# Parameter setting
origins <- 6 # We would like to predict for 6 rolling origins
ourHorizon <- 215
NumberofModel <- 1 # You can generalize this code for more models
nnData <- data # This could be another time series
#Create two NA arrays for loading the results
ourForecasts <- array (NA,c(origins, ourHorizon, ncol(nnData)))
ourHoldouts <- array (NA,c(origins, ourHorizon, ncol(nnData)))
# Double loop
newts1<-ts(redata,start =519,end = 733)
for (i in 1:ncol(nnData)) {
  for (j in 1:origins ) {
    ourData <- nnData [,i]
    ourData <- ts(ourData,frequency=7) #given a daily data
    datalength <- length (ourData)
    trainlength <- datalength - ourHorizon - origins + 1
    datatest <- ourData [(trainlength + 1): datalength]
    ourtrainset <- ts (ourData [ 1:(trainlength+j-1) ], frequency=7, start=start(ourData))
    ourHoldouts [j,,i] <- datatest [j-1+(1:ourHorizon)]
    # Produce forecasts
    ourForecasts [j,,i] <- predict(fitauto67,newts1) }}#!
#calculate the sMAPE
qq<-abs(ourHoldouts[,,]-ourForecasts[,,])
bb<-(abs(ourHoldouts[,,])+abs(ourForecasts[,,]))/2
sMAPE <- colMeans(colMeans(qq/bb))


#naive GMRAE forecast(ets_ZZZ,h = 212)
ets_fortest=predict(fitauto107,newts1)#!
ets_naitest=rep(N107_naifore,215)#!
a<-abs(abs(array(N107test)-array(ets_fortest))/abs(array(N107test)-array(ets_naitest)))#£¡
a[is.infinite(a)]=1
gm_ets<-prod(a)**(1/length(a))
gm_ets


# do the forecast N67
#ets forecast

ETS_ANN_opt  <- ets(N67, "ANN")
etsfor<-forecast(ETS_ANN_opt,14)
etsfor$mean
plot(etsfor)



#ARIMA forecast

arimafor<-forecast(arifit67,14)
arimafor$mean
plot(arimafor)

#regression
D1<-rep(c(1,0,0,0,0,0,0),74)
D2<-rep(c(0,1,0,0,0,0,0),74)
D3<-rep(c(0,0,1,0,0,0,0),74)
D4<-rep(c(0,0,0,1,0,0,0),74)
D5<-rep(c(0,0,0,0,1,0,0),74)
D6<-rep(c(0,0,0,0,0,1,0),74)
D7<-rep(c(0,0,0,0,0,0,1),74)
#new dataset
redata<-ts(cbind(N67train,D1,D2,D3,D4,D5,D6,D7))
colnames(redata) <- c("N67","D1","D2","D3","D4","D5","D6","D7")
#lag add
L1_N67<-lag(ts(redata[,"N67"]),-1)
L2_N67<-lag(ts(redata[,"N67"]),-2)
L3_N67<-lag(ts(redata[,"N67"]),-3)
L4_N67<-lag(ts(redata[,"N67"]),-4)
L5_N67<-lag(ts(redata[,"N67"]),-5)
L6_N67<-lag(ts(redata[,"N67"]),-6)
L7_N67<-lag(ts(redata[,"N67"]),-7)
data_colnames <- colnames(redata)
redata <- cbind(ts(redata),L1_N67,L2_N67,L3_N67,L4_N67,L5_N67,L6_N67,L7_N67)
colnames(redata) <- c(data_colnames,"L1_N67","L2_N67","L3_N67","L4_N67","L5_N67","L6_N67","L7_N67")
redata[is.na(redata)]=0

#time trend
N67_trend<-c(1:518)
data_colnames <- colnames(redata)
redata <- cbind(redata,ts(N67_trend))
colnames(redata) <- c(data_colnames,"N67_trend")
newts<-ts(redata,start = 733,end = 746)
repredict<-predict(fit5,newts)
repredict
plot(repredict,type = "l")


# do the forecast N107
#ets forecast
ETS_ANN_opt  <- ets(N107, "ANN")
etsfor<-forecast(ETS_ANN_opt,14)
etsfor$mean
plot(etsfor)



#ARIMA forecast
arifit107<-Arima(N107,order = c(2,0,0),seasonal = c(0,1,0))
arimafor<-forecast(arifit107,14)
arimafor$mean
plot(arimafor)

#regression

D1<-rep(c(1,0,0,0,0,0,0),74)
D2<-rep(c(0,1,0,0,0,0,0),74)
D3<-rep(c(0,0,1,0,0,0,0),74)
D4<-rep(c(0,0,0,1,0,0,0),74)
D5<-rep(c(0,0,0,0,1,0,0),74)
D6<-rep(c(0,0,0,0,0,1,0),74)
D7<-rep(c(0,0,0,0,0,0,1),74)
#new dataset
redata<-ts(cbind(N107train,D1,D2,D3,D4,D5,D6,D7))
colnames(redata) <- c("N107","D1","D2","D3","D4","D5","D6","D7")
L1_N107<-lag(ts(redata[,"N107"]),-1)
L2_N107<-lag(ts(redata[,"N107"]),-2)
L3_N107<-lag(ts(redata[,"N107"]),-3)
L4_N107<-lag(ts(redata[,"N107"]),-4)
L5_N107<-lag(ts(redata[,"N107"]),-5)
L6_N107<-lag(ts(redata[,"N107"]),-6)
L7_N107<-lag(ts(redata[,"N107"]),-7)
data_colnames <- colnames(redata)
redata <- cbind(ts(redata),L1_N107,L2_N107,L3_N107,L4_N107,L5_N107,L6_N107,L7_N107)
colnames(redata) <- c(data_colnames,"L1_N107","L2_N107","L3_N107","L4_N107","L5_N107","L6_N107","L7_N107")
N107_trend<-c(1:518)
data_colnames <- colnames(redata)
redata <- cbind(redata,ts(N107_trend))
colnames(redata) <- c(data_colnames,"N107_trend")
redata[is.na(redata)]=0
newts<-ts(redata,start = 733,end = 746)
repredict<-predict(fit9,newts)
repredict
plot(repredict,type = "l")


