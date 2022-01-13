# 이항 로지스틱 개념 실습
# 참고:https://wikidocs.net/34033

# satisfaction_level : 직무 만족도
# last_evaluation : 마지막 평가점수
# number_project : 진행 프로젝트 수
# average_monthly_hours : 월평균 근무시간
# time_spend_company : 근속년수
# work_accident : 사건사고 여부(0: 없음, 1: 있음)
# left : 이직 여부(0: 잔류, 1: 이직)
# promotion_last_5years: 최근 5년간 승진여부(0: 승진 x, 1: 승진)
# department : 부서
# salary : 임금 수준

data = read.csv('rdata/HR_comma_sep.csv')
str(data)
sum(is.na(data))

data$salary <- factor(data$salary, levels = c('low', 'medium', 'high'))

Logistic  = glm(left ~ satisfaction_level + salary + time_spend_company, family = binomial(), data=data)
summary(Logistic)
exp(cbind(OR = coef(Logistic), confint(Logistic)))


Log_odds = predict(Logistic, newdata=data)
Probability = predict(Logistic, newdata= data, type = 'response')
PREDICTED_C = ifelse(Probability > 0.5 , 1 , 0)
PREDICTED_C = as.factor(PREDICTED_C)

# install.packages(c("caret","e1071"))
library(caret)
confusionMatrix(data$left, PREDICTED_C)
