# ADP 실기 준비_R 시계열 분석 1탄----
# 참고: https://ckmoong.tistory.com/7
library(TTR)
library(forecast)
library(tseries)

# load data
data <- scan('http://robjhyndman.com/tsdldata/data/nybirths.dat')
data # 1946.01 ~ 1959.12 월별 출생자수
class(data)

# create time series data
data.ts <- ts(data=data, start = c(1946, 1), end=c(1959, 12), frequency = 12)
data.ts
plot(data.ts, main='뉴욕 월별 출생자 수') # 평균과 분산이 증가하는 경향

# decompose data
# data.decomp <- stl(data.ts, s.window = 'periodic')
# plot(data.decomp)
autoplot(decompose(data.ts))

# 정상성 확인
ndiffs(log(data.ts)) # 차분 횟수 파악
data.diff <- diff(log(data.ts))
adf.test(data.diff, alternative = 'stationary', k=0) # p-value<0.05이므로 정상성 만족

# ACF, PACF 도표 확인
acf(data.diff, lag.max = 20)
pacf(data.diff, lag.max = 20)

# modeling
data.arima <- auto.arima(data.ts)
data.arima

# 모델 성능 평가
tsdiag(data.arima) # 증감패턴/잔차의 자기상관/자기상관성

qqnorm(data.arima$residuals, pch=21, col='black',
       bg='gold', main='Q-Q Plot of Residuals')
qqline(data.arima$residuals, col = 'royalblue', lwd=2) # 정규성
Box.test(data.arima$residuals, type="Ljung-Box") # 자기상관성

# 향후 2년 미래 예측
data.arima.pred <- forecast(data.arima, h = 24) # 향후 2년 예측
data.arima.pred
plot(data.arima.pred)

# ADP 실기 준비_R 시계열 분석 2탄----
# 참고: https://ckmoong.tistory.com/10?category=933194
library(TTR)
library(forecast)
library(tseries)
# load data
king <- scan('https://robjhyndman.com/tsdldata/misc/kings.dat', skip=3)
king # 42년 간 영국 왕들이 사망한 나이

# create time series data
king.ts <- ts(king)
plot(king.ts)

# 정상성 확인
adf.test(king.ts) # p-value > 0.05이므로 정상성 만족

# 차분
ndiffs(king.ts) # 1회
king.diff <- diff(king.ts)
plot(king.diff) # 평균과 분산이 시간에 다라 의존하지 않음

# ACF, PACF 도표
acf(king.diff, lag.max = 20) # q = 1
pacf(king.diff, lag.max=20) # p = 3

# 3 type modeling
king.arima <- auto.arima(king.ts)
king.arima
# 후보1: AR(3) → ARIMA(3,1,0)
# 후보2: MA(1) → ARIMA(0,1,1)
# 후보3: ARMA(3,1) → ARIMA(3,1,1)

# 모델 성능 평가
arima(king.ts, order=c(3,1,0))
arima(king.ts, order=c(0,1,1))
arima(king.ts, order=c(3,1,1))

# 예측
train <- subset(king.ts, end=length(king.ts)-9) # 최근 8년 데이터를 제외한 나머지 데이터
test <- subset(king.ts, start=length(king.ts)-8) # 최근 8년 데이터
plot(train)

train.diff <- diff(train, differences = 1) # 차분
plot(train.diff)

acf(train.diff, lag.max=20) # q=1 → MA(1) → ARIMA(0,1,1)
Fit1 <- arima(train, order=c(0,1,1))
Fit1 %>% forecast(h=8) %>% autoplot() + autolayer(test, lwd=2)

pacf(train.diff, lag.max=20) # p=3 → AR(3) → ARIMA(3,1,0)
Fit2 <- arima(train, order=c(3,1,0))
Fit2 %>% forecast(h=8) %>% autoplot() + autolayer(test, lwd=2)

auto.arima(train)
Fit3 <- auto.arima(train)
Fit3 %>% forecast(h=8) %>% autoplot() + autolayer(test, lwd=2)

king.test1 <- Arima(test, model=Fit1)
accuracy(king.test1) # ARIMA(0,1,1)

king.test2 <- Arima(test, model=Fit2)
accuracy(king.test2) # ARIMA(3,1,0)

king.test3 <- Arima(test, model=Fit3)
accuracy(king.test3) # ARIMA(0,0,0)

# ADP 실기 준비_R 시계열 분석 3탄----
# 참고: https://todayisbetterthanyesterday.tistory.com/37
library(tseries)
library(TTR)
library(forecast)

# load data
ldeaths # 영국 월별 폐질환 사망자수 데이터
class(ldeaths)
plot(ldeaths, main='raw plot')

# 시계열 데이터 분해 
ldeaths.decomp <- decompose(ldeaths) # 분해 시계열
ldeaths.decomp$seasonal # 계절 요인
autoplot(ldeaths.decomp)

# 계절 요인 제거
ldeaths.decomp.adj <- ldeaths - ldeaths.decomp$seasonal
plot(ldeaths.decomp.adj)

# 차분
ldeaths.diff <- diff(ldeaths, differences = 1) # 1회 차분
plot(ldeaths.diff, main='1 Difference ldeaths plot')

# 정상성 파악
adf.test(ldeaths.diff) # 정상성 만족

# ACF, PACF 도표 작성
acf(ldeaths.diff) # q = 1
pacf(ldeaths.diff) # p = 0

# modeling
ldeath.arima <- auto.arima(ldeaths)
ldeath.arima

# 성능 평가
tsdiag(ldeath.arima)
qqnorm(ldeath.arima$residuals, pch=21, col='black',
       bg='gold', main='Q-Q Plot of Residuals')
qqline(ldeath.arima$residuals, col = 'royalblue', lwd=2)

Box.test(ldeath.arima$residuals, type="Ljung-Box") # 자기상관 만족

# 향후 2년 예측
ldeath.arima.pred <- forecast(ldeath.arima, h = 12*2) # 향후 2년 예측
ldeath.arima.pred
plot(ldeath.arima.pred, col='darkgreen', lwd=2,
     flty=1, flwd=3,
     fcol='royalblue', shadecols = c('mistyrose', 'salmon'),
     xlab='Year', ylab='Count',
     main='Forecast for ldeath')
