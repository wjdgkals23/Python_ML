######### 분석과제!
rm(list=ls())
### setwd
setwd("C:/future/toBigs/10기/2주차/KNN LDA/data")

### packages
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)

#### load data
pro.train <- fread('profiles_train.csv')
pro.train <- as.data.frame(pro.train)
click.train <- fread('click_train.csv')
click.train <- as.data.frame(click.train)
pro.test <- fread('profiles_test.csv')
pro.test <- as.data.frame(pro.test)
click.test <- fread('click_test.csv')
click.test <- as.data.frame(click.test)
pro.train %>% head
str(pro.train)


#### data preproc
### dplyr package : https://wsyang.com/2014/02/introduction-to-dplyr/
### 변수추가 1. DT
a <-click %>% group_by(id) %>% summarise(DT = sum(st_t)) # or aggregate
pro.train <- inner_join(pro.train,a) # or merge
pro.train %>% head

### 변수추가 2. ~

### 변수추가 3. ~

### 변수추가 4. ~

#### data partition

#### modeling

######### 분석과제 안에 들어있는 구현, Vote classifier
### function 인자값의 개수(a,b,c,d,...)는 마음껏 추가해도됩니다!
### 밑의 가이드라인은 마음대로 바꾸셔도 됩니다.
###  a : 앙상블할 모델의 갯수
###  b,c,d ... : method
### 1. 과제서 제시한 3개의 모델 우선!
### 2. 나아가서 일반화 및 파라매터 조정도 해봅시다!
voteClassifier <- function(a,b,c,d) {
  # 학습 부분, caret패키지 함수로 해주세요!
  
  # 예측 부분
  
  # predict count 부분
  
  # 최종 결과물 반환
  return()
}
predict.ensemble <- voteClassifier()
confusionMatrix(predict.ensemble, test.set)

#### 최종 결과물 제출
pred.test <- predict(~~모델, pro.test)
pred.test <- as.data.frame(pred.test)
names(pred.test) <- "gen"
write.csv(pred.test,'본인이름.csv',row.names =F)
###