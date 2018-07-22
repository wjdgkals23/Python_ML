rm(list=ls())
# 데이터
library(MASS)
library(psych)
library(car)
data(Boston)
str(Boston)
?Boston
Boston
##### 변수설명 ########################################
# crim : per capita crime rate by town.
# zn :proportion of residential land zoned for lots over 25,000 sq.ft.
# indus :proportion of non-retail business acres per town.
# chas : Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox : nitrogen oxides concentration (parts per 10 million).
# rm : average number of rooms per dwelling.
# age : proportion of owner-occupied units built prior to 1940.
# dis : weighted mean of distances to five Boston employment centres.
# rad : index of accessibility to radial highways. # 범주?
# tax : full-value property-tax rate per \$10,000.
# ptratio : pupil-teacher ratio by townry.
# black : 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat : lower status of the population (percent).
# medv : median value of owner-occupied homes in \$1000s.
#######################################################

##### 회귀분석하기 #####
### medv(본인 소유의 주택가격(중앙값) 단위: $1,000)을 예측해주세요
summary(Boston)

sum(is.na(Boston))
# 결측치 확인
# str(Boston) 에서 rad와 chas는 고속도로 번호와 더미 데이터로 추정되어 예측변수에서 제외 시킨다.
plot(Boston) # 한번봐보..

fit.empty <- lm(medv ~ 1, Boston)
fit.full <- lm(medv~.-rad -chas, Boston)
#단계적 변수선택법 실행
fit.both <- step(fit.empty, list(lower=fit.empty, upper=fit.full), direction = "both")

summary(fit.both)
# p 값이 충분히 작으므로 해당 모델의 계수는 유의하다
# 변수에 대한 p 값도 충분히 작으므로 각 설명변수가 모두 유의하다.

par(mfrow = c(2,2))
plot(fit.both)
#모델의 모양을 확인하다.

outlierTest(fit.both) # 0.05 이하 이므로 이상점 판정
Boston <- Boston[-c(369, 372, 373, 370), ] # 이상점 제거

#이상점 제거 후 모델 재생
fit.empty <- lm(medv ~ 1, Boston) # 제거 후 회귀 모델 재적합
fit.full <- lm(medv~. -rad -chas, Boston)
fit.both <- step(fit.empty, list(lower=fit.empty, upper=fit.full), direction = "both")

#다시 한번 모델을 확인한다.
par(mfrow = c(2,2))
plot(fit.both)

vif(fit.both) 
# 전부 5이하로 다중공산성 문제가 없어보인다.
e <- resid(fit.both)
shapiro.test(e)
# 잔차에 대한 정규성을 검증한 결과 정규성을 가지고 있다.

# 생성한 모델로 예측해본다.
predict(fit.both)
