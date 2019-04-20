## ------------------------------------------------------------------------
day <- read.csv("day.csv")
str(day)

## ------------------------------------------------------------------------
boxplot(day$cnt,main="total daily count")

## ------------------------------------------------------------------------
cnt <- day[ , c(-1,-2,-4,-5,-7)]
str(cnt)

## ------------------------------------------------------------------------
cnt$holiday.weekend <- ifelse(cnt$holiday ==1 | cnt$workingday ==0, 1, 0)
str(cnt)

## ------------------------------------------------------------------------
cnt <- cnt[ , c(-2,-3,-6)]
str(cnt)

## ------------------------------------------------------------------------
library(psych)
pairs.panels(cnt[c("season","holiday.weekend","weathersit","temp","hum","windspeed","cnt")])

## ------------------------------------------------------------------------
scatter.smooth(cnt$temp,cnt$cnt,col="red")

## ------------------------------------------------------------------------
scatter.smooth(cnt$hum,cnt$cnt,col="blue")

## ------------------------------------------------------------------------
model_null <- lm(cnt~1,data=cnt)
model_full <- lm(cnt~factor(holiday.weekend)+factor(season)+factor(weathersit)+poly(temp,2, raw=FALSE)+poly(hum, 2, raw = FALSE)+windspeed,data=cnt)

## ------------------------------------------------------------------------
step(model_null,scope = list(lower=model_null,upper=model_full),direction = "forward")

## ------------------------------------------------------------------------
model_additive<-lm(cnt~factor(season)+factor(holiday.weekend)+factor(weathersit)+temp+hum+windspeed,data=cnt)
summary(model_additive)

## ------------------------------------------------------------------------
model_additive2 <- lm(cnt~factor(season)+factor(weathersit)+temp+hum+windspeed,data=cnt)
summary(model_additive2)

## ------------------------------------------------------------------------
model_interact <- lm(cnt~factor(season)*temp+factor(weathersit)+hum+windspeed,data=cnt)
summary(model_interact)

## ------------------------------------------------------------------------
model_poly1 <- lm(cnt~poly(temp,2,raw = FALSE)+factor(season)+factor(weathersit)+hum+windspeed ,data=cnt)
summary(model_poly1)

## ------------------------------------------------------------------------
model_poly2 <- lm(cnt~poly(temp,2,raw = FALSE)+poly(hum,2,raw = FALSE)+factor(season)+factor(weathersit)+windspeed,data=cnt)
summary(model_poly2)

## ------------------------------------------------------------------------
model_final <- model_poly2
summary(model_final)

## ------------------------------------------------------------------------
anova(model_final,model_additive2)

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
boxplot(casual~holiday.weekend,data=cnt, main="casual users",xlab="holiday/weekend",ylab = "count")
boxplot(registered~holiday.weekend,data=cnt,main="registered users",xlab="holiday/weekend", ylab = "count")

## ------------------------------------------------------------------------
library(psych)
pairs.panels(cnt[c("season","holiday.weekend","weathersit","temp","hum","windspeed","casual")])

## ------------------------------------------------------------------------
model_casual <- lm(casual~poly(temp,2,raw = FALSE)+poly(hum,2,raw = FALSE)+factor(season)+factor(weathersit)+windspeed+holiday.weekend,data=cnt)
summary(model_casual)

## ------------------------------------------------------------------------
library(psych)
pairs.panels(cnt[c("season","holiday.weekend","weathersit","temp","hum","windspeed","registered")])

## ------------------------------------------------------------------------
model_register <- lm(registered~poly(temp,2,raw = FALSE)+poly(hum,2,raw = FALSE)+factor(season)+factor(weathersit)+windspeed+holiday.weekend,data=cnt)
summary(model_register)

## ------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model_final)

## ------------------------------------------------------------------------
shapiro.test(model_final$residuals)

## ------------------------------------------------------------------------
library(car)
ncvTest(model_final)

## ------------------------------------------------------------------------
library(car)
residualPlots(model_final)

## ------------------------------------------------------------------------
outlierTest(model_final)

## ------------------------------------------------------------------------
influencePlot(model_final)

## ------------------------------------------------------------------------
cnt_sub <- cnt[-69, ]

## ------------------------------------------------------------------------
model_sub <- lm(cnt~poly(temp,2,raw = FALSE)+poly(hum,2,raw = FALSE)+factor(season)+factor(weathersit)+windspeed,data=cnt_sub)
summary(model_sub)

## ------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(model_sub)

## ------------------------------------------------------------------------
library(car)
ncvTest(model_sub)

