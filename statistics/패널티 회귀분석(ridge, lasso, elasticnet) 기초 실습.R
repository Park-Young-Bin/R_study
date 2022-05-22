# 패널티 회귀분석(ridge, lasso, elasticnet)
# 참고: https://www.youtube.com/watch?v=3OEwk2VxZdE&list=WL&index=1&t=1170s
# 지나치게 많은 독립변수를 갖는 모델에 패널티 부과하는 방식으로 간명한 회귀모델 생성
# 모델 성능에 크게 기여하지 못하는 변수의 영향력을 축소하거나 모델에서 제거
# 최소자승법에 의한 잔차(관측값-예측값)의 제곱합과 패널티항의 합이 최소가 되는 회귀계수를 추정

# 1. 릿지(Ridge) 회귀분석
# 모델에 설명력에 기여 못하는 독립변수의 회귀계수 크기를 0에 근접하도록 축소
# L2-norm 패널티항으로 회귀모델에 패널티 부과 → 회귀계수 축소 
# 패널티의 크기는 "람다"로 지정
# "람다" 증가 → 패널티 영향 증가, 릿지 회귀모형의 회귀계수 감소
# 독립변수의 척도에 크게 영향을 받으므로 표준화 과정 필요
# 모든 독립변수가 회귀모형에 포함되므로 변수선택방법 불가능

# 2. 라소(Lasso) 회귀분석
# 모델의 설명력에 기여 못하는 독립변수의 회귀계수를 0으로 만듦
# L1-norm 패널티항으로 회귀모델에 패널티 부과 → 회귀계수 축소
# 패널티의 크기는 "람다"로 지정
# 변수선택을 통해 설명력이 우수한 독립변수만 모델에 활용할 수 있고, 모델 복잡성 축소 가능

# 릿지 vs 라소
# 릿지: 많은 독립변수의 선형결합으로 결과 예측 가능, 독립변수들의 회귀계수 크기가 비슷할 때 우수한 성능
# 라소: 일부 독립변수 회귀계수가 크고, 나머지 독립변수 회귀계수가 매우 작을 때 우수한 성능

# 3. 일래스틱넷(Elastic Net) 회귀분석
# L1-norm과 L2-norm 모두 이용하여 회귀모델에 패널티 부과
# 회귀계수를 축소할수도, 0으로 만들 수도 있음

library(MASS)
str(Boston)

# 데이터 셋 분할
library(caret)
set.seed(123)
train <- createDataPartition(y=Boston$medv, # 자동으로 비율 조절
                             p=0.7, # 훈련 데이터 비율
                             list = F) # 행렬 인덱스로 출력, default(True: 리스트 인덱스)
head(train)

Boston.train <- Boston[train, ]
Boston.test <- Boston[-train, ]

nrow(Boston.train)
nrow(Boston.test)

# install.packages('glmnet')
library(glmnet)
?glmnet
# glmnet(x, y, ~) 
# x: 행렬 형식(숫자만 가능하기에 범주형 변수를 더미 변수로 변환해야 함), y: 벡터 형식
# alpha: 0(릿지), 1(라소), 0~1(일래스틱넷)
# lambda: 패널티 크기 조정

x <- model.matrix(medv ~ ., Boston.train) # 모델에 투입할 예측변수 행렬 생성, 범주형 변수를 더미변수로 자동 변환
head(x)
x <- model.matrix(medv ~ ., Boston.train)[,-1]
y <- Boston.train$medv

# ridge regression analysis----
?cv.glmnet
set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, 
                       family = "gaussian", # family = "gaussian": mse를 최소로 만드는 lambda를 찾음
                       alpha=0 # 릿지 회귀
                       ) # k-묶음 교차검증 → 최적의 lambda 찾기

plot(Boston.cv) # lambda에 따른 mse의 변화 추이 확인
# 예측 오차를 최소화 하는 log lambda: 약 -0.4
# 그래프 상단의 13: 예측변수 개수

str(Boston.cv) # 정보 확인
Boston.cv$lambda.min # mse를 최소화 하는 실제 lambda 값: 0.6647797
log(Boston.cv$lambda.min) # 최적의 lambda 값: -0.4082995

Boston.gnet <- glmnet(x=x, y=y, family = "gaussian", alpha=0, lambda=Boston.cv$lambda.min) # 모델 생성
coef(Boston.gnet) # 회귀계수

Boston.test.x <- model.matrix(medv ~ ., Boston.test)[,-1]
Boston.pred <- predict(Boston.gnet, newx = Boston.test.x)
head(Boston.pred)

# 성능 평가 (RMSE, MAE: 값이 작을수록 좋음)
postResample(pred=Boston.pred, obs=Boston.test$medv)

# lasso regression analysis----
set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, 
                       family = "gaussian", # family = "gaussian": mse를 최소로 만드는 lambda를 찾음
                       alpha=1 # 라소 회귀
                       ) # k-묶음 교차검증 → 최적의 lambda 찾기

plot(Boston.cv) # 중요하지 않은 예측변수의 회귀계수를 0으로 만들기에 log lambda에 따라 상당의 예측변수 개수가 달라짐

Boston.cv$lambda.min # mse를 최소화 하는 실제 lambda 값: 0.3716657
log(Boston.cv$lambda.min) # 최적의 lambda 값: -4.432009

Boston.cv$lambda.1se # 0.3716657
log(Boston.cv$lambda.1se) # -0.9897604

coef(Boston.cv, Boston.cv$lambda.min) # 1개 회귀계수 제거
coef(Boston.cv, Boston.cv$lambda.1se) # 3개 회귀계수 제거

# (lambda.min vs lambda.1se) 성능 비교 → lambda.min 선택
Boston.gnet1 <- glmnet(x=x, y=y, family = "gaussian", alpha=1, lambda=Boston.cv$lambda.min)
Boston.pred1 <- predict(Boston.gnet1, newx = Boston.test.x)
postResample(pred=Boston.pred1, obs=Boston.test$medv) # 성능 평가

Boston.gnet2 <- glmnet(x=x, y=y, family = "gaussian", alpha=1, lambda=Boston.cv$lambda.1se)
Boston.pred2 <- predict(Boston.gnet2, newx = Boston.test.x)
postResample(pred=Boston.pred2, obs=Boston.test$medv) # 성능 평가

# elasticnet regression analysis----
set.seed(123)
Boston.cv <- train(form=medv ~ ., data=Boston.train, # train(): 교차검증 수행 함수
                   method = 'glmnet',
                   trControl = trainControl(method='cv', # k-묶음 교차검증 방법
                                            number=10),
                   tuneLength = 10 # alpha, lambda 각각 10개씩 조합하여 교차검증 수행
                   )
Boston.cv$bestTune

Boston.gnet <- glmnet(x=x, y=y, family = "gaussian", 
                      alpha=Boston.cv$bestTune$alpha, 
                      lambda = Boston.cv$bestTune$lambda)
coef(Boston.gnet)

# 성능 평가
Boston.pred <- predict(Boston.gnet, newx = Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)

# comparison(ridge, lasso, elasticnet)----
# 예측정확도가 비슷하다면 좀 더 간명한 모델을 선택한다.
lambda <- 10^seq(-5, 5, length=100)
lambda

set.seed(123)
ridge <- train(form=medv ~ ., data=Boston.train, # train(): 교차검증 수행 함수
                   method = 'glmnet',
                   trControl = trainControl(method='cv', # k-묶음 교차검증 방법
                                            number=10),
                   tuneGrid = expand.grid(alpha=0, lambda=lambda))
coef(ridge$finalModel, ridge$bestTune$lambda)
ridge.pred <- predict(ridge, Boston.test)
postResample(pred = ridge.pred, obs = Boston.test$medv)

set.seed(123)
lasso <- train(form=medv ~ ., data=Boston.train, # train(): 교차검증 수행 함수
               method = 'glmnet',
               trControl = trainControl(method='cv', # k-묶음 교차검증 방법
                                        number=10),
               tuneGrid = expand.grid(alpha=1, lambda=lambda))
coef(lasso$finalModel, lasso$bestTune$lambda)
lasso.pred <- predict(lasso, Boston.test)
postResample(pred = lasso.pred, obs = Boston.test$medv)

set.seed(123)
elastic <- train(form=medv ~ ., data=Boston.train,
               method = 'glmnet',
               trControl = trainControl(method='cv', # k-묶음 교차검증 방법
                                        number=10),
               tuneLength=10)
coef(elastic$finalModel, elastic$bestTune$lambda)
elastic <- predict(elastic, Boston.test)
postResample(pred = elastic, obs = Boston.test$medv)

models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(resamples(models))
summary(resamples(models), metric = "RMSE")

# 결과1: 유의확률이 모두 1이므로 세 회귀모델 간에 예측성능에 대해 유의한 차이 없음
# 결과2: lasso, elastic: 예측 변수 개수가 한 개 줄었음, 따라서 두 회귀모델은 ridge 모델보다 예측모델의 일반화 관점에서 두 회귀모델을  사용하는 것이 바람직함.
summary(diff(resamples(models), metric = "RMSE")) 