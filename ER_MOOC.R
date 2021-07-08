library(tidyr)
library(dplyr)
library(lmtest)
library(ggplot2)

#creating new columns for indicator li1 and li2 with 1 quarter lag
casestudy <- casestudy %>%
  mutate(newli1= lag(casestudy$li1),
         newli2= lag(casestudy$li2))
casestudy["newli1"]<- c(lag(casestudy$li1))
casestudy["newli2"]<- c(lag(casestudy$li2))

#creating new columns for indicator li1 and li2 with 2 quarter lag
casestudy <- casestudy %>%
  mutate(lag_b_li1= lag(casestudy$li1,2),
         lag_b_li2= lag(casestudy$li2,2))
casestudy["lag_b_li1"]<- c(lag(casestudy$li1,2))
casestudy["lag_b_li2"]<- c(lag(casestudy$li2,2))


#to sort the data and use the data from 1951 to 2010
casestudy <- casestudy %>%
  separate(Date, into =c("year","quarter"), sep=4)
datareq <- subset(casestudy, year>1950)
datareq1 <- subset(datareq, year<2011)
datareq1$GDPIMPR<-as.factor(datareq1$GDPIMPR)
datareq1$year<-as.numeric(datareq1$year)

#Question 1
model1 <- glm(data= datareq1, datareq1$GDPIMPR ~ 1 ,  family="binomial")
summary(model1)
model2 <- glm(data= datareq1, datareq1$GDPIMPR~ datareq1$newli1 , family="binomial")
summary(model2)
model3 <- glm(data= datareq1, datareq1$GDPIMPR ~ datareq1$newli2, family="binomial")
summary(model3)
model4 <- glm(data= datareq1, datareq1$GDPIMPR~ datareq1$newli1 + datareq1$newli2, family="binomial")
summary(model4)

#likelihood ratio test
lmtest::lrtest(model1,model4)
lmtest::lrtest(model2,model4)
lmtest::lrtest(model3,model4)

#Question 2
model2.2 <- glm(data= datareq1, datareq1$GDPIMPR ~ datareq1$newli1 + datareq1$lag_b_li2, family="binomial")
summary(model2.2)
model2.3 <- glm(data= datareq1, datareq1$GDPIMPR ~ datareq1$newli2 + datareq1$lag_b_li1, family="binomial")
summary(model2.3)
model2.4 <- glm(data= datareq1, datareq1$GDPIMPR ~ datareq1$lag_b_li1 + datareq1$lag_b_li2, family="binomial")
summary(model2.4)

mcfadden2<- 1- logLik(model4)/logLik(model1)
mcfadden2
mcfadden2<- 1- logLik(model2.2)/logLik(model1)
mcfadden2
mcfadden2<- 1- logLik(model2.3)/logLik(model1)
mcfadden2

mcfadden2<- 1- logLik(model2.4)/logLik(model1)
mcfadden2

#Question 3
datareq2<- subset(datareq, year>=2011)
datareq2$GDPIMPR<-as.numeric(datareq2$GDPIMPR)
model2.3a <- glm(data= datareq2, datareq2$GDPIMPR ~ datareq2$newli2 + datareq2$lag_b_li1, family="binomial")
summary(model2.3a)
pp <- predict(model2.3a, newdata = datareq2)
matrix<- table(actual = datareq2$GDPIMPR, predicted= pp >0.5)
matrix
hit_rate<- (matrix[[1,1]] + matrix[[2,2]])/sum(matrix)
hit_rate

#Question 4
datareq1$GrowthRate<-as.numeric(datareq1$GrowthRate)
datareq1 <- datareq1 %>%
  mutate(LOGGDP_lag1 = lag(LOGGDP),
         GrowthRate_lag = lag(GrowthRate))
model4 <- lm(data = datareq1, GrowthRate ~ T + LOGGDP_lag1 + GrowthRate_lag )
summary(model4)
#nonstationary

#Question 5
datareq1 <- datareq1 %>%
  mutate(GrowthRate_lag= lag(GrowthRate))
datareq1$GrowthRate_lag<-as.numeric(datareq1$GrowthRate_lag)
model5 <- lm(data = datareq1, GrowthRate ~ GrowthRate_lag + newli1 + newli2)
summary(model5)
model5a <- lm(data = datareq1, GrowthRate ~ GrowthRate_lag + newli1 + lag_b_li2)
summary(model5a)
model5b <- lm(data = datareq1, GrowthRate ~ GrowthRate_lag +lag_b_li1+ newli2)
summary(model5b)
model5c <- lm(data = datareq1, GrowthRate ~ GrowthRate_lag +lag_b_li1 + lag_b_li2)
summary(model5c)

#question 6
str(datareq1)
datareq1$GrowthRate<-as.numeric(datareq1$GrowthRate)
bgtest(formula =GrowthRate ~ GrowthRate_lag + newli1 + newli2 , order=1, data =datareq1)
#pvalue comparison
#there is no serial correlation

#Question 7

datareq2 <- datareq2 %>%
  mutate(GrowthRate_lag= lag(GrowthRate))
datareq2$GrowthRate_lag<-as.numeric(datareq2$GrowthRate_lag)
datareq2$GrowthRate<-as.numeric(datareq2$GrowthRate)

prediction <- predict(model5, newdata = datareq2)
datareq2$prediction<-as.numeric(datareq2$prediction)
rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}
rmse <- rmse(actual = datareq2$GrowthRate, predicted = prediction)
rmse