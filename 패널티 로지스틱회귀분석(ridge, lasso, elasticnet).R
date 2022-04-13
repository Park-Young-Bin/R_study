# 패널티 로지스틱 회귀분석
# 참고 url: https://www.youtube.com/watch?v=LxHMfr120gU
# 지나치게 많은 예측변수를 갖는 로지스틱 회귀모델에 패널티 부과
# 모델 설명력에 덜 기여하는 예측변수의 회귀변수를 0으로 만들거나, 0에 가깝게 축소하여 모델의 복잡성을 줄여줌
# 예측 정확도가 비슷할 경우 일반적으로 예측변수의 개수가 작은(과적합 위험이 작은) 간명한 모델이 바람직함

# 1. 데이터 불러오기----
# install.packages('mlbench')
library(mlbench)
data('PimaIndiansDiabetes2')
str(PimaIndiansDiabetes2)

# 2. 결측치 제거----
PimaIndiansDiabetes3 <- na.omit(PimaIndiansDiabetes2)

# 3. train, test 데이터 분할----
library(caret)
set.seed(123)
train <- createDataPartition(y=PimaIndiansDiabetes3$diabetes,
                             p=0.7, # 결과 변수의 범주 비율을 train, test 동일하게 분할 
                             list = F)
head(train, 10)

diabetes.train <- PimaIndiansDiabetes3[train,]
diabetes.test <- PimaIndiansDiabetes3[-train,]

table(diabetes.train$diabetes) # train 빈도 계산
sum(table(diabetes.train$diabetes))
prop.table(table(diabetes.train$diabetes)) # train 비율 계산

table(diabetes.test$diabetes) # test 빈도 계산
sum(table(diabetes.test$diabetes))
prop.table(table(diabetes.test$diabetes)) # test 비율 계산

# train 데이터 셋 x, y 분할---
x <- model.matrix(diabetes ~ ., diabetes.train) # 모델에 투입할 예측변수 행렬 생성, 범주형 변수를 더미변수로 자동 변환
head(x)
x <- model.matrix(diabetes ~ ., diabetes.train)[, -1]
y <- ifelse(diabetes.train$diabetes=='pos', 1, 0)

# lasso logistic regression analysis----
?cv.glmnet # 교차검증
library(glmnet)
set.seed(123)
diabetes.cv <- cv.glmnet(x=x, # 행렬 형식, 범주형 변수인 경우 더미변수로 변환 필요
          y=y, # 벡터 형식
          family='binomial', 
          alpha=1) # 라소
plot(diabetes.cv)
diabetes.cv$lambda.min
diabetes.cv$lambda.1se # 일정 수준의 예측 정확도를 보장하면서 가능한 간명한 모델 생성, 일반화 가능 높음(과적합 적음)

coef(diabetes.cv, diabetes.cv$lambda.min) # 7개 예측 변수
coef(diabetes.cv, diabetes.cv$lambda.1se) # 6개 예측 변수

## case1. lambda=diabetes.cv$lambda.min
diabetes.gnet1 <- glmnet(x=x, y=y, family='binomial', alpha=1, lambda=diabetes.cv$lambda.min)
diabetes.test.x <- model.matrix(diabetes ~ ., diabetes.test)[, -1]
diabetes.pred1 <- predict(diabetes.gnet1,
                          newx=diabetes.test.x,
                          type='response')
diabetes.pred1 <- ifelse(diabetes.pred1 > 0.5,
                         'pos', 'neg')
table(diabetes.test$diabetes, diabetes.pred1, dnn=c('Actual', 'Predicted'))
mean(diabetes.test$diabetes == diabetes.pred1) # 예측 정확도: 0.7521368

## case2. lambda=diabetes.cv$lambda.1se
diabetes.gnet2 <- glmnet(x=x, y=y, family='binomial', alpha=1, lambda=diabetes.cv$lambda.1se)
diabetes.test.x <- model.matrix(diabetes ~ ., diabetes.test)[, -1]
diabetes.pred2 <- predict(diabetes.gnet2,
                          newx=diabetes.test.x,
                          type='response')
diabetes.pred2 <- ifelse(diabetes.pred2 > 0.5,
                         'pos', 'neg')
table(diabetes.test$diabetes, diabetes.pred2, dnn=c('Actual', 'Predicted')) # 혼돈행렬
mean(diabetes.test$diabetes == diabetes.pred2) # 예측 정확도: 0.7521368

## case3. logit regression
diabetes.logit <- glm(diabetes ~ .,
                      data = diabetes.train,
                      family = binomial(link='logit'))
diabetes.logit.pred <- predict(diabetes.logit,
                               newdata = diabetes.test,
                               type = 'response')
diabetes.logit.pred <- ifelse(diabetes.logit.pred > 0.5,
                         'pos', 'neg')
table(diabetes.test$diabetes, diabetes.logit.pred, dnn=c('Actual', 'Predicted'))
mean(diabetes.test$diabetes == diabetes.logit.pred) # 예측 정확도: 0.7264957

# 결과
# 일반적인 이항로지스틱 회귀 분석 예측 모델 보다 패널티를 적용한 회귀 모델의 예측력이 더 좋다.