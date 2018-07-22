install.packages("ROCR")
rm(list= ls())

library(e1071)
library(mlbench)
library(caret)
library(ROCR)

## 데이터 구조 확인
data(BreastCancer)
str(BreastCancer)
BreastCancer <- na.omit(BreastCancer)

## 데이터를 확인하고 na 값을 처리한다.
## ordered factor는 가중치가 있는 명목형 변수? -> numeric으로 전환해도 무관하다.
## Class 1,2 -> 0,1 형태로 변환한다.
for(i in 1:10){
  BreastCancer[,i] <- as.numeric(BreastCancer[,i])
}
BreastCancer$Class = as.numeric(BreastCancer$Class)-1

# 변형된 데이터 형태를 확인한다.
str(BreastCancer)
# 50개의 정확도를 담을 빈 vector를 선언한다.
overall <- c()

##Train/Test 를 50회 랜덤하게 나눈다.
##로지스틱 분류모델을 만들고 Test 데이터에 대한 50 번 acc를 평균을 낸다
for (i in 1:50){
  idx = createDataPartition(BreastCancer$Class, p = 0.7, list = F)
  BC_train = BreastCancer[idx,]
  BC_test = BreastCancer[-idx,]
  select1 = colnames(BreastCancer)[c(2:10)]
  formula1 = formula(paste("Class~",paste(select1, collapse=" + ")))
  model.glm1 = glm(formula1, BC_train, family = binomial)
  pred.glm1 = as.numeric(predict(model.glm1, BC_test, type = "response") > 0.5)
  d = confusionMatrix(as.factor(pred.glm1), as.factor(BC_test$Class))
  overall <- c(overall, d$overall['Accuracy'])
}


## 평균을 계산한다.
result = mean(overall) 
result

#ROC 곡선을 통해 분류기의 성능의 변화를 확인한다.
pred.glm <- prediction(as.numeric(pred.glm1), as.numeric(BC_test$Class))
perf_glm <- performance(pred.glm, measure = "tpr", x.measure = "fpr")
plot(perf_glm, main = "ROC curve for GLM", col = "blue", lwd = 2)

#AUC
auc_glm = performance(pred.glm, measure = "auc")
auc_glm@y.values[[1]]
#약 0.96의 값으로 신뢰성 있는 모델이다?