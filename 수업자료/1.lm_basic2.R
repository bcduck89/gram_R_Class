require(graphics)

data(cars)
cars
fit<-lm(dist~.,data=cars)
anova(fit)
summary(fit)

fit$coefficients[2] # w
fit$coefficients[1] # b

# y=wX+b
pred1<-fit$coefficients[2] * cars$speed + fit$coefficients[1]
pred2<-predict(fit,cars)
#residual = true - predicted
a = pred1 - pred2
a
ts.plot(pred2-cars$dist)
fit$residuals
fit$fitted.values

data(mtcars)
mtcars

fit2<-lm(mpg~.,data=mtcars)
anova(fit2)
summary(fit2)
## p-value가 아주 작음 귀무가설 기각 그래서 변수가 설명력을 갓는다.

ts.plot(fit2$residuals)

sum(fit2$residuals)

sum(fit2$residuals^2) # SSE
mean((fit2$residuals^2)) # MSE
##회귀 분석의 경우 모델의 성능지표 MSE / MAPE / MAE
##귀무가설 다중회귀 하나의 변수라도 유의 하지않다

index<-abs(fit2$coefficients)[-1] >0.5
index
var<-names(index)[index==T]
var
fo<-paste0("mpg~",paste(var,collapse = "+"))
fo
fit3<-lm(fo,data=mtcars)
summary(fit3)
mean(fit3$residuals^2)
## 과적합 -> 데이터의 수를 늘리거나 / 적절한 변수를 선택 


