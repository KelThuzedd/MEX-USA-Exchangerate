data <-read.csv("F:/ñí/âğåìåííûå ğÿäû/ïğîåêò/itog.txt", sep='\t', dec=',')
install.packages("forecast")

install.packages("lmtest")
install.packages("tseries")
install.packages("vars")
install.packages("urca")
install.packages("TSA")
install.packages("Matrix")
install.packages("matlib")
install.packages("portes")
library("portes")
library("forecast")
library("lmtest")
library("tseries")
library("vars")
library("urca")
library("TSA")
library("Matrix")
library("matlib")


#EXCHANGERATE
Pacf(data$EXCHANGERATE)

ur.df(data$EXCHANGERATE, type="drift", lags = 1, 
      selectlags = "Fixed")

#CPIUSA

plot(data$CPIUSA)
Pacf(data$CPIUSA)
a<-ur.df(data$CPIUSA, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)

#UNRATEUSA

plot(data$UNRATEUSA)
Pacf(data$UNRATEUSA)
a<-ur.df(data$UNRATEUSA, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)


#d1M1USA

plot(d1M1USA)
Pacf(d1M1USA)
a<-ur.df(d1M1USA, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)

#d1M1USA

plot(d1M2USA)
Pacf(d1M2USA)
a<-ur.df(d1M2USA, type="drift", lags = 9, 
         selectlags = "Fixed")
summary(a)


#d1UNRATEUSA

plot(d1UNRATEUSA)
Pacf(d1UNRATEUSA)
a<-ur.df(d1UNRATEUSA, type="drift", lags = 2, 
         selectlags = "Fixed")
summary(a)



#d1M1USA

plot(d1M2USA)
Pacf(d1M2USA)
a<-ur.df(d1M2USA, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)


#d1M1USA

plot(d1M2USA)
Pacf(d1M2USA)
a<-ur.df(d1M2USA, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)

#


plot(data$Crude)
Pacf(data$Crude)
a<-ur.df(data$Gold, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)


#



plot(data$Crude)
Pacf(data$Crude)
a<-ur.df(data$Gold, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)

#


plot(data$Crude)
Pacf(data$Crude)
a<-ur.df(data$Gold, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)


#


plot(data$Crude)
Pacf(data$Crude)
a<-ur.df(data$Gold, type="drift", lags = 1, 
         selectlags = "Fixed")
summary(a)

d1EXCHANGERATE<-diff(data$EXCHANGERATE, differences=1)
d1CPIUSA<-diff(data$CPIUSA, differences=1)
d1M1USA<-diff(data$M1USA, differences=1)
d1M2USA<-diff(data$M2USA, differences=1)
d1UNRATEUSA<-diff(data$UNRATEUSA, differences=1)
d1PPIUSA<-diff(data$PPIUSA, differences=1)
d1GDPUSA<-diff(data$GDPUSA, differences=1)
d1CPIMEX<-diff(data$CPIMEX, differences=1)
d1UNRATEMEX<-diff(data$UNRATEMEX, differences=1)
d1PPIMEX<-diff(data$PPIMEX, differences=1)
d1GDPMEX<-diff(data$GDPMEX, differences=1)


d2EXCHANGERATE<-diff(data$EXCHANGERATE, differences=2)
d2CPIUSA<-diff(data$CPIUSA, differences=2)
d2M1USA<-diff(data$M1USA, differences=2)
d2M2USA<-diff(data$M2USA, differences=2)
d2UNRATEUSA<-diff(data$UNRATEUSA, differences=2)
d2PPIUSA<-diff(data$PPIUSA, differences=2)
d2GDPUSA<-diff(data$GDPUSA, differences=2)
d2CPIMEX<-diff(data$CPIMEX, differences=2)
d2UNRATEMEX<-diff(data$UNRATEMEX, differences=2)
d2PPIMEX<-diff(data$PPIMEX, differences=2)
d2GDPMEX<-diff(data$GDPMEX, differences=2)



VAR analysis

Öåëü - ôàêòîğû âëèÿşùèå íà EXCHANGERATE

Àíàëèç êğîññêîğğåëÿöèè


#d2CPIUSA

ccf(d2EXCHANGERATE, d2CPIUSA, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2CPIUSA, d2EXCHANGERATE, order = 10)
grangertest(d2EXCHANGERATE, d2CPIUSA, order = 50)


#d2M1USA

ccf(d2EXCHANGERATE, d2M1USA, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2M1USA, d2EXCHANGERATE, order = 11)
grangertest(d2EXCHANGERATE, d2M1USA, order = 22)



#d2M2USA

ccf(d2EXCHANGERATE, d2M2USA, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2M2USA, d2EXCHANGERATE, order = 1)
grangertest(d2EXCHANGERATE, d2M2USA, order = 12)



#d2UNRATEUSA

ccf(d2EXCHANGERATE, d2UNRATEUSA, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2UNRATEUSA, d2EXCHANGERATE, order = 3)
grangertest(d2EXCHANGERATE, d2UNRATEUSA, order = 21)


#d2PPIUSA

ccf(d2EXCHANGERATE, d2PPIUSA, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2PPIUSA, d2EXCHANGERATE, order = 2)
grangertest(d2EXCHANGERATE, d2PPIUSA, order = 21)


#GDPUSA
ccf(d2EXCHANGERATE, d2GDPUSA, lag.max = 24, type = c("correlation"),
    plot = TRUE)
grangertest(d2GDPUSA, d2EXCHANGERATE, order = 7)
grangertest(d2EXCHANGERATE, d2GDPUSA, order = 21)




#d2CPIMEX

ccf(d2EXCHANGERATE, d2CPIMEX, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2CPIMEX, d2EXCHANGERATE, order = 1)
grangertest(d2EXCHANGERATE, d2CPIMEX, order = 9)



#d2UNRATEMEX

ccf(d2EXCHANGERATE, d2UNRATEMEX, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2UNRATEMEX, d2EXCHANGERATE, order = 10)
grangertest(d2EXCHANGERATE, d2UNRATEMEX, order = 21)


#d2PPIMEX

ccf(d2EXCHANGERATE, d2PPIMEX, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2PPIMEX, d2EXCHANGERATE, order = 1)
grangertest(d2EXCHANGERATE, d2PPIMEX, order = 14)



#d2GDPMEX

ccf(d2EXCHANGERATE, d2GDPMEX, lag.max = 30, type = c("correlation"),
    plot = TRUE)
grangertest(d2GDPMEX, d2EXCHANGERATE, order = 3)
grangertest(d2EXCHANGERATE, d2GDPMEX, order = 21)


eacf(d2EXCHANGERATE)
for_EXCHANGERATE <- Arima(data$EXCHANGERATE, c(1,2,1), include.constant =TRUE, method = c("CSS-ML"))  
coeftest(for_EXCHANGERATE)
summary(for_EXCHANGERATE)

Acf(residuals(for_EXCHANGERATE))
Box.test(residuals(for_EXCHANGERATE), lag = 2, type = c("Ljung-Box"), fitdf = 5)
#ARIMA

eacf(d2EXCHANGERATE)
modelMSCIRUS <- Arima(data$EXCHANGERATE, c(0,2,2), include.constant=TRUE, method = c("CSS"))  
summary(modelMSCIRUS)
Acf(residuals(modelMSCIRUS))
n<-length(coef(modelMSCIRUS))-1

Box.test(residuals(modelMSCIRUS), lag = 2, type = c("Ljung-Box"), fitdf = n)
x<-var$p-n+1
rss<-sum(modelMSCIRUS$residuals[x:length(residuals(modelMSCIRUS))]^2)

#d2CPIUSA

d1_l1 <- c(0,d1CPIUSA[1:length(d1CPIUSA)-1])
d1_l2 <- c(0,0,d1CPIUSA[2:length(d1CPIUSA)-2])



#Sup-F test (supWald)
stat <- Fstats(d1M1USA ~ d1_l1, from = 0.1, to = NULL)
plot(stat, alpha = 0.01)
lines(breakpoints(stat))
a<-breakpoints(stat)
a$breakpoints
sctest(stat, type = "supF")


'Bai Perron'
d1<-ts(d1GDPMEX, start=1)
d1_l1 <- c(0,d1[1:length(d1)-1])
stat <- breakpoints(d1 ~ d1_l1)
summary(stat)
plot(stat)
## compute breakdates corresponding to the
## breakpoints of minimum BIC segmentation
breakdates(stat)

