# 이항 로지스틱 개념 실습
# 참고: https://www.youtube.com/watch?v=nyU96C2-LCI&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=33
# 종속변수가 이분형 범주를 가질 때 독립변수로부터 결과변수의 범주를 예측
# 특정  사건이 발생할 확률을 직접 추정
## 종속변수의 예측값은 항상 0과 1 사이의 확률
## 이 값이 기준값(예를 들면 0.5)보다 크면 사건이 발생하고 기준값보다 작으면 사건이 발생하지 않는 것으로 예측
# install.packages('modeldata')
library(modeldata)
data(mlc_churn)
str(mlc_churn)

churn <- mlc_churn
churn <- churn[-c(1, 3)]
churn$churn <- factor(ifelse(churn$churn=='no', 1, 2), # 고객 미이탈 = 1, 고객 이탈 = 2
                      level=c(1, 2),
                      labels=c('no', 'yes')) # 사건 발생 = "고객 이탈"
str(churn)
dim(churn)

# train, test data split
churn.train <- churn[1:3333, ]
churn.test <- churn[3334:5000, ]

# train, test data의 종속변수(churn) 비율 확인
table(churn.train$churn) # no: 2850, yes: 483
prop.table(table(churn.train$churn)) # no: 0.8550855, yes:0.1449145

table(churn.test$churn) # no: 1443, yes: 224
prop.table(table(churn.test$churn)) # no: 0.8656269, yes:0.1343731

# binomial logistic regression analysis
# 고객 미이탈(1)은 사건 미발생으로 0으로 변환되고, 고객 이탈(2)은 사건 발생으로서 1로 변환된다.
# 따라서 본 분석의 오즈는 고객 이탈일 확률이 미이탈의 확률의 몇배인지를 나타낸다.
# 로지스틱 회귀모델에서 독립변수 회귀계수는 다른 변수의 변화가 없다는 가정 하에서 해당 변수가 한 단위 만큼 변화할 때의 로그 오즈의 변화량을 나타냄
# 회귀계수(+): 독립변수 증가 → 종속변수(로그 오즈) 증가, 회귀계수(-): 독립변수 증가 → 종속변수(로그 오즈) 감소
# 로지스틱 회귀모델에 지수함수를 취하여 종속변수를 오즈로 변환 → 한 단위의 증가에 따른 오즈비 확인 가능
churn.logit <- glm(churn ~ ., data = churn.train, family = binomial(link='logit'))
summary(churn.logit)

# exp(회귀계수) 해석
# 해석1: international_plan은 yes일 때, no일 때보다 고객 이탈률이 미이탈률보다 7.7배 증가(770% 증가)
# 해석2: voice_mail_plan은 yes일 때, no일 때보다 고객 이탈률이 미이탈률보다 0.13배 증가(87.8% 감소)
# 해석3: total_day_charge가 한 단계 증가하면 고객 이탈률이 미이탈률보다 4.53배 증가
# 해석3_1: 두 단계 증가하면 고객 이탈률이 미이탈률보다 4.53^2(=20.521)배 증가
exp(coef(churn.logit))

# 통계적 유의성 검정
# deviance(이탈도): 모델의 비적합도 정도, 작을수록 좋음
# Null deviance: 상수항만 포함된 예측 모델
# Residual deviance: 예측변수가 모두 포함된 (현재 구축한)모델
# 상수항만 포함된 모델보다 예측변수가 포함된 모델의 이탈도가 낮은 것 당연, 작아지는 정도가 통계적으로 의미있는지 중요함
# Null 모델에 비해 개선되는 이탈도가 통계적으로 의미있는 차이인지 검정 필요
# 자유도 차이만큼 Residual deviance가 개선됐는지 검정 → 카이제곱 검정
pchisq(q=2758.3-2158.7, df = 3332-3315, lower.tail = F) # p-value = 1.731898e-116 → 이탈도 차이는 통계적으로 유의함
pchisq(q=churn.logit$null.deviance - churn.logit$deviance, 
       df=churn.logit$df.null - churn.logit$df.residual,
       lower.tail = F) # 위와 동일, p-value = 1.757917e-116 → 이탈도 차이는 통계적으로 유의함

churn.logit.pred <- predict(churn.logit, newdata=churn.test, 
                            type='response') # 사건 발생 확률 출력
head(churn.logit.pred)

churn.logit.pred <- factor(churn.logit.pred > 0.5, 
                           levels=c(FALSE, TRUE),
                           labels=c('no', 'yes')) # 0.5 이상이면 고객 이탈
head(churn.logit.pred)
table(churn.logit.pred) # 고객 이탈: 72명, 고객 미이탈: 1595명

# 혼동행렬
table(churn.test$churn, churn.logit.pred,
      dnn=c('Actual', 'Predicted'))
mean(churn.test$churn == churn.logit.pred) # 정확도: 0.8740252

# 단계별 로지스틱 회귀분석
churn.logit2 <- step(churn.logit)
summary(churn.logit2)

# 관심있는 특정 예측변수가 사건 발생 확률에 미치는 영향 파악
# 예측변수의 변화하는 수준에 따라 사건 발생 확률의 변화 확인
# ex. 고객의 서비스 센터 전화 횟수가 고객 이탈 확률에 미치는 영향에 관심 있다.
## "다른 예측변수들을 일정하게 고정"하여 고객의 서비스 전화 횟수를 바꿔가면서 고객 이탈 확률 변화를 계산한다.
table(churn.test$number_customer_service_calls) # 서비스 센터 전회 횟수 분포 → 0 ~ 7회
testdata <- data.frame(number_customer_service_calls=c(0:7),
                       international_plan="no", # 범주형: 가장 낮은 범주 유형값
                       voice_mail_plan="no",
                       number_vmail_messages=mean(churn.test$number_vmail_messages), # 연속형: 평균값
                       total_day_charge=mean(churn.test$total_day_charge),
                       total_eve_minutes=mean(churn.test$total_eve_minutes),
                       total_night_charge=mean(churn.test$total_night_charge),
                       total_intl_calls=mean(churn.test$total_intl_calls),
                       total_intl_charge=mean(churn.test$total_intl_charge))
head(testdata)
testdata$prob <- predict(churn.logit2, newdata = testdata, type = 'response')
testdata[c("number_customer_service_calls", 'prob')] # 전화 횟수 증가 → 이탈률 증가

# 과산포 문제 검정
# 종속변수의 실제 분산이 이항분포에서 기대되는 분산보다 더 클 때 발생
# 검정의 표준오차를 왜곡시켜 회귀계수의 유의성 검정을 부정확하게 만드는 위험 존재
# 이탈도와 자유도 간의 비율 확인, 1을 크게 상회하면 과산포 의심
deviance(churn.logit2)/df.residual(churn.logit2) # 0.6505038, 과산포 없음

# 통계 검정
# H0: 과산포 비율이 1이다.
fit.origin <- glm(churn ~ international_plan + 
                    voice_mail_plan + 
                    number_vmail_messages + 
                    total_day_charge + 
                    total_eve_minutes + 
                    total_night_charge + 
                    total_intl_calls + 
                    total_intl_charge +
                    number_customer_service_calls,
                  family = binomial(),
                  data=churn.train)

fit.overids <- glm(churn ~ international_plan + 
                     voice_mail_plan + 
                     number_vmail_messages + 
                     total_day_charge + 
                     total_eve_minutes + 
                     total_night_charge + 
                     total_intl_calls + 
                     total_intl_charge +
                     number_customer_service_calls,
                   family = quasibinomial(),
                   data=churn.train)

pchisq(summary(fit.overids)$dispersion * fit.origin$df.residual,
       fit.origin$df.residual, lower.tail = F) # 0.08385493 → 과산포 가능성 적음

# 다른 예제
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