# 다항 로지스틱 회귀분석 개념 정리
# 참고: https://www.youtube.com/watch?v=4ibxb0OpgWA
# 독립변수로부터 세 개 이상의 사건(범주)을 갖는 종속변수의 사건발생확률 예측
# 사건발생확률은 새로운 케이스의 소속범주를 예측하는 데 활용
# g개의 범주가 있을 때 총(g-1)개의 다항 로지스틱 회귀모델이 생성되며, 각 범주당 하나씩 기준 범주에 대한 로그오즈(로짓)를 설명하는 회귀모델로 정의

library(EffectStars)
data(PID) # 미국의 유권자 944명의 정치 성향 데이터
str(PID)
levels(PID$PID)

library(VGAM)
pid.mlogit <- vglm(PID ~ ., family = multinomial(), data = PID)

# 결과1: 종속변수의 범주가 3개이므로 총 2개 모델 생성
# 결과2: 마지막 범주('Republican')가 기준 범주로 자동 지정
# 결과3: Education 변수더미변수로 자동 변환됨(low가 기준변수로서 0으로, hight는 1로 코딩)
summary(pid.mlogit)

# exp(회귀계수) 해석
# 해석1: TVnews(1)오즈비: 1.045 / 뉴스 시청일이 하루 늘어나면, Democrate일 가능성이 Republican일 가능성에 비해 4.5% 증가(1.045배 증가)
# 해석2: Education(1)오즈비: 0.749 / 교육 수준 한 단위 증가(low → high)는 Democrate일 가능성을 Republican일 가능성 대비 25.1% 감소(0.749배 증가)
# 해석3: 하지만 TVnews(1)과 Educationhigh(1)은 통계적으로 유의하지 않음
# 해석4: Income(1) 오즈비: 0.9835 / 소득 한 단위를 증가시키면 Democrate일 가능성이 Republican일 가능성에 비해 1.6% 감소, 소득 변화는 Democrate일 가능성과 Republican일 가능성에 유의한 영향을 미침
exp(coef(pid.mlogit))

# 각 범주별 예측 확률 계산(케이스 별로 세 범주 중 분류될 확률 추정)
pid.mlogit.pred <- fitted(pid.mlogit)
head(pid.mlogit.pred)

# 독립변수 수준에 따른 사건발생 확률 변화 
# ex1. 교육수준이 정치 성향에 미치는 영향
# 결과: 교육수준 증가로 Democrat일 확률 감소, Republican일 확률을 증가시키지만 교육수준은 통계적 유의X
testdata <- data.frame(Education=c('low', 'high'), 
                       TVnews=mean(PID$TVnews), # 평균값 고정
                       Income=mean(PID$Income),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population)) # 가상의 데이터 생성
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type='response')
cbind(testdata, pid.mlogit.pred)

# ex2. 소득이 정치 성향에 미치는 영향
# 결과: 저소득층은 Democrat, 고소득층은 Republican로 예측될 확률이 높음, 소득과 정치성향은 밀첩한 관계가 있음
testdata <- data.frame(Education=rep('low', 5), # 가장 낮음 범주로 고정
                       TVnews=mean(PID$TVnews), # 평균값 고정
                       Income=seq(20, 100, 20),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population)) # 가상의 데이터 생성
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type='response')
cbind(testdata, pid.mlogit.pred)

# 다른 예제
library(MASS)
str(fgl) # 214개의 유리 성분 데이터
levels(fgl$type)
head(fgl)

fgl.scaled <- cbind(scale(fgl[, 1:9]), fgl[10]) # 변수 스케일링
head(fgl.scaled)

# train, test data set
set.seed(123)
train <- sample(nrow(fgl), 0.7*nrow(fgl))
fgl.train <- fgl.scaled[train,]
fgl.test <- fgl.scaled[-train,]

sum(table(fgl.train$type)) # 149
table(fgl.train$type)
sum(table(fgl.test$type)) # 65
table(fgl.test$type)

# 모델 생성
library(nnet)
fgl.mlogit <- multinom(type ~., data=fgl.train) # 기준 범주=첫 번째 범주(WinNF)
summary(fgl.mlogit)

# 회귀계수 유의성 검정
z <- summary(fgl.mlogit)$coefficients / summary(fgl.mlogit)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
print(p, digits=3)

# 새로운 데이터에 대한 예측 확률 추정
fgl.mlogit.pred <- predict(fgl.mlogit, newdata=fgl.test, type='probs')
head(fgl.mlogit.pred)
cbind(round(fgl.mlogit.pred, 3), fgl.test['type'])

# 테스트 데이터에 대한 예측 정확도
max.col(fgl.mlogit.pred) # 가장 높은 예측 확률을 갖는 열 인덱스 반환
fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)] # 열 인덱스에 대응되는 열 이름 선택
head(fgl.mlogit.pred)

# 혼동 행렬
table(fgl.test$type, 
      fgl.mlogit.pred, 
      dnn=c('Actual', 'Predicted'))
table(fgl.test$type, 
      factor(fgl.mlogit.pred, 
             levels=c(fgl.test$type),
             labels=c(fgl.test$type)),
      dnn=c('Actual', 'Predicted')) # 위치 수정
mean(fgl.test$type == fgl.mlogit.pred) # 예측 정확도: 0.6307692

# 교차검증
fgl.mlogit.cv <- numeric()
for (i in 1:100) {
  train <- sample(nrow(fgl), 0.7*nrow(fgl))
  fgl.train <- fgl.scaled[train,]
  fgl.test <- fgl.scaled[-train,]
  fgl.mlogit <- multinom(type ~., data=fgl.train)
  fgl.mlogit.pred <- predict(fgl.mlogit,
                             newdata=fgl.test,
                             type='probs')
  fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
  fgl.mlogit.cv[i] <- mean(fgl.test$type == fgl.mlogit.pred)
}

fgl.mlogit.cv
summary(fgl.mlogit.cv) # mean = 0.6077
boxplot(fgl.mlogit.cv, horizontal=T,
        col='tomato', xlab='Accuracy', main='Accuracy for Forensic Glass (100 samples)')