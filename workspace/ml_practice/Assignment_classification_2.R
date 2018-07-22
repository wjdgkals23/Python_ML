load("psub.RData")
library(caret)

data = psub[c("AGEP","SEX","COW","PINCP","SCHL")]
# 요구한 변수만 추출

summary(data$SCHL) 
# 학위 종류 확인 (1,5,8,9) 학사 이하 학위

schl.names = as.factor(names(table(data$SCHL))) # 학위 종류 이름 추출
data["bachdeg"] <- as.factor(ifelse(data$SCHL == schl.names[c(1)] | data$SCHL == schl.names[c(5)] | data$SCHL == schl.names[c(8)] | data$SCHL == schl.names[c(9)],0, 1))
# 분류를 예측할 변수 생성하여 추가하기

str(data)
# 데이터 구조확인

idx <- createDataPartition(data$bachdeg, p=0.7, list=F)
data_train = data[idx,]
data_test = data[-idx,]
# TEST,TRAIN 7:3 분할

select1 = colnames(data)[c(1,2,3,4)] #schl을 제외한 변수 사용
formula1 = formula(paste("bachdeg~",paste(select1, collapse=" + ")))
model1 <- glm(formula1, data_train, family = binomial)
summary(model1)

pred.glm1 = as.numeric(predict(model1, data_test, type = "response") > 0.5)
confusionMatrix(as.factor(pred.glm1),as.factor(data_test$bachdeg))

