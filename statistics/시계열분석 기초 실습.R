# 시계열 데이터----

# 시계열분석 개요
# 시계열분석은 시간의 흐름에 따라 일정한 간격으로 사건을 관찰하여 기록한 데이터를 바탕으로 미래를 예측
# 과거의 일련의 관측값을 분석하여 이를 모델링하고, 이 예측 모델을 바탕으로 미래 관측값 예측
# 일반적으로 추세성분, 계절성분, 불규칙성분 등으로 구성되고, 이 성분들에 의해 변동된다고 가정

# 시계열 분석 절차
# 1) 시계열 데이터 생성
# 2) 탐색적 분석을 통해 데이터 특성 이해
## 시각화를 통해 시계열 데이터의 변동 패턴 관찰, 성분분해 작업으로 추세, 계절, 불규칙 성분으로 세분화
# 3) 미래 관측값에 대한 예측
## 지수모델링(관측값에 가중 평균) 기법, ARIMA(관측값과 오차 간의 상관) 기법

# 시계열 데이터 생성
# 1) 시계열 객체 생성
# 2) 시계열 객체에는 관측값뿐 아니라 시계열의 시작 시점과 종료 시점, 주기(월, 분기, 연도별) 등의 정보 포함

url <- "http://jse.amstat.org/datasets/utility.dat.txt"
utility <- read.table(url)
head(utility) # 1990.05~1997.05 미국 보스턴 가구당 월별 전기 소비량

utility.ts <- ts(data=utility[7], # V7 변수만 사용
                 start = c(1990,9),
                 frequency = 12) # 단위 시간 당 관측값 개수(=단위 시간 당 반복 주기)
utility.ts
class(utility.ts)
plot(utility.ts, col='salmon', lwd=2,
     xlab='Year', ylab='Electricity Usage',
     main='Electricity Usage Trend of Boston Area')

start(utility.ts) # 처음 관측값의 시간, 1990.05
end(utility.ts) # 마지막 관측값의 시간, 1997.05
frequency(utility.ts) # 단위 시간에 포함된 관측값 개수, 12
deltat(utility.ts) # 관측값 간에 시간 간격(=1/12, 0.08333, 월간 간격 의미)
time(utility.ts) # 동일 간격의 시계열 값 반환
cycle(utility.ts) # 각 관측값에 대응되는 주기의 일련변호 반환
window(utility.ts, start=c(1991, 1), end=c(1992, 6)) # 시계열 데이터의 서브셋 데이터 생성
window(utility.ts, start=c(1991, 1), frequency=1) # 특정 주기에 대한 관측값만 추출, 연간 1개씩 데이터 추출
window(utility.ts, start=c(1991, 7), frequency=1) # 매년 7월의 관측값 출력
window(utility.ts, start=c(1990, 9), frequency=2) # 6개월 간격, 매년 9월, 3월 관측값 출력
window(utility.ts, start=c(1991, 1), frequency=4) # 연간 4개 관측값 출력, 1월, 4월, 7월, 10월

# 시계열 데이터 분해----
# 시계열 데이터의 관측값을 변동 요인에 따라 구성 성분으로 분해하는 과정
# 시계열 데이터는 일반적으로 관측값의 전반적 상승 또는 하락 경향을 나타내는 추세 성분과 설명 안 되는 오차(error)를 나타내는 불규칙 성분으로 구성
# 시계열 데이터에 주기가 존재하고 계절적 요인의 영향을 받아 변동한다면 계절 성분이 추가로 포함

# 비계절 데이터 분해
# 추세성분과 불규칙 성분으로 구성된 시계열 데이터
# 불규칙적 변동 요인을 제거 또는 완화하여 의미 있는 추세 패턴 파악
# 평활법: 단순이동평균법(=중심이동평균법)

nhtemp # 1912~1971 연평균 기온 시계열 데이터, 연도별 데이터(Frequency = 1), 연간 관측값이 1개이므로 계절 요인 없음
plot(nhtemp, col='dimgray', lwd=2, ylab='Temperature', main='Base Time Series')

library(forecast)
ma(nhtemp, 3) # 단순이동평균 계산 ma(data, 관측값 개수(k))
ma(nhtemp, 7)
ma(nhtemp, 11)

# k증가 → 불규칙 패턴 점차 사라짐, 명확한 추세 파악
# 적정 수준의 추세 패턴을 찾을 때까지 반복 시행착오 과정을 통해 k값 결정
par(mfrow = c(3, 1))
plot(ma(nhtemp, 3), col='red', lwd=2, ylab='Temperature', main='Simple Moving Average (k=3)')
plot(ma(nhtemp, 7), col='green3', lwd=2, ylab='Temperature', main='Simple Moving Average (k=7)')
plot(ma(nhtemp, 11), col='blue', lwd=2, ylab='Temperature', main='Simple Moving Average (k=11)')
par(mfrow = c(1, 1))

# 계절 데이터 분해
# 추세 성분 만으로 데이터의 특성을 설명하기란 부족
# 추세성분: 시간 흐름에 따른 수준(level, 관측값 크기) 변화
# 계절성분: 단위 기간 내에서의 순환주기의 영향
# 불규칙성분: 추세 성분과 계절 성분에 의해 설명되지 않는 영향
# 가법모델: 변동 폭이 일정한 경우 적합
# 승법모델: 변동폭이 수준에 비례하여 증폭하는 경우 적합

co2 # 1959.01 ~ 1997.12까지 월별 co2 농도 시계열 데이터
co2 <- window(co2, start=c(1985, 1), end=c(1996, 12))
co2

# 가법모델을 바탕으로 추세/계절/불규칙 성분으로 분해
co2.decomp <- stl(co2, s.window = 'periodic')
# s.window: 계절 효과 추출을 위한 구간 범위 지정, 계절 요인이 시간 흐름에 따라 얼마나 빠르게 변화하도록 허용할지 결정
# 작은 숫자 → 기간 계간 구간 짧음 → 빠르게 변하는 패턴 / 큰 숫자 → 기간 계간 구간 긺 → 느리게 변하는 패턴
# 최소 7 이상의 홀수
# periodic: 계절 효과가 전 시계열 기간에 걸쳐 모두 동일하도록 지정
# 예를 들어 월별 시계열 데이터의 월별 계절 효과는 모든 해에 걸쳐 동일, 1월 계절 효과는 해마다 동일하고 2월도 모든 해에 걸쳐서 동일
co2.decomp

# 결과1: 위에서 부터 원본, 계절성분, 추세성분, 불규칙성분의 시계열 그래프
# 결과2: 계절성분은 "periodic"으로 통일시켰으므로 매년 동일한 패턴, 4~5월에 농도가 가장 높고, 9~10월에 농도가 가장 낮음
# 결과3: 점진적으로 상승하는 추세
# 결과4: 우측 막대는 변동폭 비교하기 위한 목적으로 사용됨, 길이는 다르지만 크기는 모두 같음(각 성분마다 y값이 다름), 불규칙성분의 막대가 가장 길지만 세 구성 성분 가운데 변동폭이 가장 작음을 의미
plot(co2.decomp, col='darkcyan', col.range='skyblue',
     lwd=2,
     main='Decomposition of CO2 Concentration Time Series')

co2.decomp$time.series # 각 관측값의 성분별 분해 결과 확인
co2
-0.03679325 + 345.1764 -0.3495613287 # co2의 첫번째 관측값과 일치(344.79)

# 계절적 영향이 반영되지 않은 시계열 그래프
co2.adj <- co2 - co2.decomp$time.series[,"seasonal"]
co2.adj
plot(co2.adj, col='tomato', lwd=2,
     xlab='Year', ylab='CO2 Concentration (Parts Per Million)',
     main='Decomposition of CO2 Concentration Time Series Without Seasonal Effect')

# 전체 시계열 데이터를 월별 서브셋으로 만든 후, 각 서브셋에 대한 시계열 그래프 생성
# 수평선 = 평균
monthplot(co2, col='slateblue', lwd=2,
          xlab='Month', ylab='CO2 Concentration (Parts Per Million)',
          main='Month Plot')

# 연도별 서브셋의 시계열 그래프 생성
seasonplot(co2, col='sienna', lwd=2, year.labels = T,
          xlab='Month', ylab='CO2 Concentration (Parts Per Million)',
          main='Season Plot')

AirPassengers # 월별 국제선 항공기 승객수
plot(AirPassengers, col='maroon', lwd=2,
     xlab='Year', ylab='Air Passengers (Thousands)',
     main='Air Passengers') # 승법모델로 설명될 수 있음

lair <- log(AirPassengers)
plot(lair, col='navy', lwd=2,
     xlab='Year', ylab='Air Passengers (Log(Thousands))',
     main='Log Transformation of Air Passengers') # 변동성 안정화 → 가법모델로 성분 분해 가능

lair.decomp <- stl(lair, s.window='periodic') # 가법모델 적용

# 결과1: 7,8월에 계절효과가 두드러짐
# 결과2: 전반적으로 상승하는 추세
plot(lair.decomp, col='chocolate', col.range = 'orange',
     lwd=2,
     main='Decomposition of Log Transformation of Air Passengers')

lair.decomp$time.series # 각 관측값의 성분별 분해 결과 확인

# 위의 결과는 log로 변환한 값이므로 지수함수 적용
# 결과1: 승객수가 7, 8월에 각각 24%, 23% 증가했음
# 결과2: 승각수가 11, 12월에 각각 19%, 10% 감소했음
exp(lair.decomp$time.series) # 원래 값

# 지수예측모델----
# 1) 단순지수평활법
LakeHuron # 1875~1972년, 매년 측정한 호수 수위 시계열 데이터

# 해석1: 눈에 띄는 상승/하락 추세 없음, 연도별 측정값으로 구성되었기에 계절 요인 없음
# 수준과 불규칙 요인만 반영된 단순지수평활법을 이용한 시계열 분석 고려
plot(LakeHuron, col='royalblue', lwd=2,
     xlab='Year', ylab='Level (Feet)',
     main = 'Annual Level of Lake Huron')

# 단순지수평활법에 의한 예측모델 생성
# 해석: alpha가 1에 가까우므로 최근 관측값이 예측 시 가장 우선하여 고려됨
library(forecast)
lake.ets <- ets(LakeHuron, model='ANN') # 불규칙(가법), 추세X, 계절X
lake.ets

lake.ets.pred <- forecast(lake.ets, h=1) # 예측할 기간 개수 설정(h=1, 1년 후)
lake.ets.pred # 1년 후, 579.96

plot(lake.ets.pred, col='royalblue', lwd=2,
     xlab='Year', ylab='Level (Feet)',
     main = 'Forecast for Annual Level of Lake Huron')

# 예측 모델 성능 파악
accuracy(lake.ets)
?accuracy

# 2) 홀트지수평활법
# install.packages('fpp')
library(fpp)
elecsales # 남호주의 전력 판매량 기록 시계열 데이터

# 해석: 대체로 상승 추세, 연도별 측정값으로 구성됐으므로 계절 요인X 
plot(elecsales, col='royalblue', lwd=2,
     xlab='Year', ylab='Electricity Sales (Gwh)',
     main = 'Electricity Sales in South Australia')

# 홀트지수평활법에 의한 예측 모델 생성
# alpha, beta 값이 0에 가까움 → 예측할 때 과거 관측값을 우선하여 고려함
elecsales.ets <- ets(elecsales, model='AAN') # 불규칙(가법), 추세(가법), 계절X
elecsales.ets

elecsales.ets.pred <- forecast(elecsales.ets, h = 5) # 5년 예측
elecsales.ets.pred

plot(elecsales.ets.pred, col='royalblue', lwd=2,
     flty=3, flwd=3, shadecols = c("lavender", "mistyrose"),
     xlab='Year', ylab='Electricity Sales (Gwh)',
     main = 'Forecast for Electricity Sales in South Australia')

# 3) 홀트원터스지수평활법
AirPassengers
# 해석1: 관측값 크기와 비례해 변동폭 증가 → 승법모델로 설명될 가능성 존재
# 해석2: 홀트원터스지수평활법은 시계열 데이터 구성 성분이 가법적으로 결합하는 것을 가정함 → 각 관측값에 log 취함
plot(AirPassengers, col='royalblue', lwd=2,
     xlab='Year', ylab='Air passengers (Thousand Persons)',
     main = 'Air Passengers') 

lair <- log(AirPassengers)
# 해석: 변동성이 안정됐으므로 가법모델 적용 가능
plot(lair, col='royalblue', lwd=2,
     xlab='Year', ylab='Air passengers (Log(Thousand Persons))',
     main = 'Air Passengers') 

# 홀트윈터스지수평활법을 이용한 예측 모델 생성
lair.ets <- ets(lair, model='AAA') # 불규칙, 추세, 계절 모두 가법적
lair.ets

lair.ets.pred <- forecast(lair.ets, h=12) # 월별 데이터이므로 향후 1년 예측을 위해 h=12 지정
lair.ets.pred

plot(lair.ets.pred, col='salmon', lwd=2,
     fcol='indianred1', flwd=3,
     xlab='Year', ylab='Air passengers (Log(Thousand Persons))',
     main = 'Forecast for Air Passengers') 

# log가 적용되었으므로 원래 값으로 변환 필요
lair.ets.pred$mean # 예측값
lair.ets.pred$lower # 신뢰구간 하한
lair.ets.pred$upper # 신뢰구간 상한

air.mean <- exp(lair.ets.pred$mean)
air.lower <- exp(lair.ets.pred$lower)
air.upper <- exp(lair.ets.pred$upper)
air.pred <- cbind(air.mean, air.lower, air.upper)
air.pred

# ets() 함수에 모델인수를 지정하지 않으면 여러 대안을 비교한 후, 가장 우수한 모델을 선정함
library(fpp)
austourists # 호주 방문 여행객의 체류기간을 분기별로 기록한 시계열 데이터

austourists.ets <- ets(austourists)
austourists.ets # ETS(M,A,M) 모델이 선택됨

plot(forecast(austourists.ets, 12), col='cornflowerblue', lwd=2,
     fcol='royalblue', flwd=3,
     flty=1, shadecols = c('mistyrose', 'salmon'),
     xlab='Year', ylab='Total Visitor Nights',
     main = 'Forecast for International Tourists to Australia') 
