# 1. 시계열 데이터 생성 및 파악----
# 참고영상: https://www.youtube.com/watch?v=KSaok7u09wc&t=194s
# 참고자료: https://otexts.com/fppkr/decomposition.html

# 시계열 데이터(Time Series Data)란?
#하나의 변수에 대한 시간에 따른 관측치를 말함. 
#예. 기후 데이터, 주가지수, ...

# 시계열 분석(Time Series Analysis)이란?
#시간의 흐름에 따라 일정한 간격으로 사건을 기록한 데이터를 바탕으로 미래의 관측값을 예측하는 분석 기법, 과거 일련의 관측값을 모델링하고, 이 모델을 바탕으로 미래의 관측값을 예측함. 일반적으로 추세요인, 순환요인, 계절요인, 불규칙요인으로 구성되며 이 요인들에 의해 변동된다고 가정함. 회귀모형과 달리 다른 변수를 도입하지 않고 자신의 변수의 과거 패턴이 미래에도 계속된다는 가정하에 변수의 과거 값을 바탕으로 미래를 예측함.

# 시계열 분석 요인 4가지
#추세요인: 데이터의 값이 시간에 따라 커지거나 작아지거나 수평인 추세
#순환요인: 명확한 이유없이 알려지지 않은 주기를 가지고 변화하는 요인
#계절요인: 고정된 주기에 따라 트랜드가 있는 요인, 예시로 1주일, 1년, 분기, 계절 단위 등
#불규칙요인: 추세, 순환, 계절 요인에 해당하지 않는 불규칙적인 요인

# 시계열 데이터 생성
#사용 데이터(url: http://jse.amstat.org/jse_data_archive.htm )
#1990년~1997년 5월까지 보스턴 지역의 가스 난방 에너지와 월별 기후 정보를 갖고 있다.

# 데이터 저장
url <- "http://jse.amstat.org/datasets/utility.dat.txt"
utility <- read.table(url)
utility

# 주요 변수 설명
#V1 : 관측 월(mmm-yy)형식
#V2 : 보스턴의 월 평균 기온
#V7 : 월 총 에너지 사용량

# 시계열 데이터 생성
#시계열 데이터를 분석하기 위해서는 관측값으로 시작 시점과 종료 시점, 주기(ex.월별, 분기별, 연도별)등의 정보가 포함돼야 한다. ts() 함수를 이용해 utility 데이터의 V7변수(월 총 에너지 사용량)를를 시계열 데이터로 만들어보겠다.
#ts(): 시계열 개체를 만드는데 사용하는 함수
#ts(데이터: 관측된 시계열 값, 
#  start: 첫 관측 기준 시간, 
#  end: 마지막 관측 기준 시간(코드에서는 생략), 
#  frequency: 단위 시간당 관측 개수(년도별: 1, 분기별: 4, 월별: 12, 일병: 7...))

utility.ts <- ts(data=utility$V7, 
                 start=c(1990, 9), # c(연도, 월)
                 frequency = 12)
utility.ts

# 데이터 구조 확인
class(utility.ts)

# 시계열 그래프 그리기
#결과: 계절에 따라 등락하고 점진적으로 증가하는 경향을 볼 수 있음
plot(utility.ts, col = 'salmon', lwd = 2, xlab = 'Year',
     ylab = 'Electricity Usage', main = 'Electricity Usage Trend of Boston Area')

# 시계열 데이터 파악하기
#시계열 데이터의 시작날, 마지막 날 알기
start(utility.ts)
end(utility.ts)

#단위시간에 계산된 관측값 개수를 반환 → 연간 관측값 개수: 12
frequency(utility.ts)

#관측값 간의 시간 간격을 산출(frequency 값의 역수)
deltat(utility.ts) # 1/12 = 0.083(= 1년을 1로 했을 때 월 간 간격을 의미)

#관측값 간의 간격을 동일하게 => 연간 12개의 값을 0.083의 동일한 가격을 갖는 시계열 벡터로 변환
time(utility.ts)

#각 관측값에 대응되는 주기의 일련번호를 반환
cycle(utility.ts)

#window(): start와 end 사이에 있는 객체 추출 => 1991 1월 ~ 1992년 6월까지의 데이터만 추출하기
window(utility.ts, start=c(1991, 1), end=c(1992, 6))

#연간 1개의 관측값
window(utility.ts, start = c(1991,1), frequency = 1) # 매년 1월 말
window(utility.ts, start = c(1991,7), frequency = 1) # 매년 7월 말

#연간 1개의 관측값(6개월 간격) 
window(utility.ts, start = c(1991,1), frequency = 2)
window(utility.ts, start = c(1990,9), frequency = 2) # 매년 9월, 3월
 
#분기별 관측값
window(utility.ts, start = c(1991,1), frequency = 4) # 매년 1, 4, 7, 10월

# 2. 평활법-이동평균법----
# 참고영상1: https://www.youtube.com/watch?v=cUS2uISiwCA&t=236s
# 참고영상2: https://www.youtube.com/watch?v=2fKRRkDRzck&list=PLCt8K88AxcKMTamDcvj95eTBzo5osoWHk&index=2
# 참고자료: https://otexts.com/fppkr/moving-averages.html

# 비계절 데이터 분해
#불규칙적 변동 요닝을 제거 또는 완화아여 의미 있는 추세 패턴 파악

# 평활법(Smoothing)이란?
#데이터를 처리하기 위한 일종의 통계 기법. "평균을 취해서 시계열 데이터의 잡음(noise)을 제거 또는 감소"시켜 "시계열 데이터를 좀 더 정확하게 파악"하고 분석할 수 있도록 함. 주로 이용하는 평활법의 종류로는 대표적으로 이동평균법, 지수평활법 등이 있음.
#평활법에는 여러 종류가 있지만 대표적인 이동평균법, 지수평활법 3 종류(단순지수평활법, 이중지수평활법, 홀트-윈터 지수 평활법)

# 이동평균법
#매 시점에서 직전 N개 시점의 데이터 평균을 산출하여 다음 시점의 예측치로 사용. 즉, 과거로부터 현재까지의 시계열 자료를 대상으로 일정기간별 이동평균을 계산하고, 이들의 추세를 파악하여 다음 기간을 예측하는 방법.

# 이동평균법의 특징
#간단하고 쉽게 미래 예측 가능
#자료의 수가 많고 안정된 패턴을 보이는 경우 예측 품질(quality)가 높음
#특정 기간 안에 속하는 시계열에 대해서는 동일한 가중치 부여
#일반적으로 시계열 자료에 뚜렷한 추세가 있거나 불규칙변동이 심하지 않은 경우에는 짧은 기간(N의 개수 작음)의 평균을 사용, 반대로 불규칙변동이 심한 경우 긴 기간(N의 개수 큼)의 평균을 사용
#N값이 커질수록 좀 더 수평적인 모습을 보임
#그렇기 때문에 이동평균법에서 가장 중요한 것은 적절한 기간의 N의 개수를 지정하는 것! N 값이 변동됨에 따라서 불규칙한 값이 사라지고 명확한 추세가 나타나는 것을 확인할 수 있음. N값은 적정수준의 패턴을 찾을 때까지 반복적인 시행착오과정을 겪을 수 있음.

# 이동평균법 R code

# 사용데이터
nhtemp

# nhtemp 데이터 그래프 그리기
plot(nhtemp, col = 'dimgray', lwd = 2,
     ylab='Temperature', main = 'Base Time Series')
#결과: 대체적으로 상승하는 모양이나 연도별로 변동이 큰 것을 확인할 수 있다.(불규칙)

# install.packages('forecast')
library(forecast)

# 이동평균법
# N=3(관측값 개수) 일 때
ma(nhtemp, 3) # k=3일 때 이동평균

# N=7 일 때
ma(nhtemp, 7)

# N=11 일 때
ma(nhtemp, 11)

# 그래프 작성
#결과: N값이 커질수록 급격한 곡선이 적당히 완만해지면서 증가하는 추세가 보이는 것을 확인할 수 있다.
par(mfrow = c(1, 3))
plot(ma(nhtemp, 3), col = 'red', lwd=2,
     ylab='Temperature', main="Base Time Series(k=3)")
plot(ma(nhtemp, 7), col = 'green3', lwd=2,
     ylab='Temperature', main="Base Time Series(k=7)")
plot(ma(nhtemp, 11), col = 'blue', lwd=2,
     ylab='Temperature', main="Base Time Series(k=11)")
par(mfrow = c(1, 1))

# 계절 데이터 분해
co2
co2 <- window(co2, start=c(1985, 1), end=c(1996, 12))
co2

# 시계열 데이터의 성분 분해
#stl(): 가법모델을 바탕으로 시계열 데이터를 추세 성분, 계절 성분, 불규칙 성분으로 분해
#s.window: 계절 효과를 추출하기 위한 기간 산출 범위 설정
#작은 값을 넣으면 기간 계산 구간이 작아지기에 더 빠르게 변하는 패턴 파악
#최소 7이상의 홀수 지정
co2.decomp <- stl(co2, s.window = "periodic") # periodic: 계절 효과가 전 시계열 기간에 걸쳐 모두 동일
co2.decomp

plot(co2.decomp, col='darkcyan', col.range="skyblue",
     lwd=2, main='Dcomposition of CO2 Concentraion Time Series')

co2.decomp$time.series
-0.03679325 + 345.1764 -0.3495613287
co2

# 계절적 영향이 반영되지 않은 시계열 그래프(추세와 불규칙 요인만 반영된 그래프)
co2.adj <- co2 - co2.decomp$time.series[, "seasonal"]
co2.adj
plot(co2.adj, col = 'tomato', lwd=2, xlab='Year', 
     ylab = 'CO2 Concentration (Parts Per Million)',
     main='CO2 Concentraion Time Series without Seasonal Effect')

# 전체 시계열 데이터를 월별 subset으로 만든 후에 각 subset에 대한 시계열 데이터 생성
#수평선 = 각 subset에 대한 평균
monthplot(co2, col='slateblue', lwd = 2,
          xlab='Month', ylab = 'CO2 Concentration (Parts Per Million)',
          main = 'Month Plot')

# 연도별 subset의 시계열 그래프
seasonplot(co2, col='sienna', lwd = 2, year.labels = T,
          xlab='Month', ylab = 'CO2 Concentration (Parts Per Million)',
          main = 'Season Plot')

AirPassengers # 국제선 항공기 승객 인원
#결과: 시계열의 변동성이 관측값의 크기에 비례하여 증폭됨 → 승법모델로 설명할 수 있음
plot(AirPassengers, col = 'maroon', lwd=2,
     xlab='Year', ylab='Air Passengers (Thousands)',
     main='Air Passengers')

lair <- log(AirPassengers)
#결과: 변동성이 안정화됨 → 가법모델로 성분 분해가 가능한 형태로 변경됨 → stl 함수로 성분 변환 가능
plot(lair, col = 'navy', lwd=2,
     xlab='Year', ylab='Air Passengers (Log(Thousands))',
     main='Log Transformation Air Passengers')

lair.decomp <- stl(lair, s.window = 'periodic')
plot(lair.decomp, col='chocolate', col.range='orange',
     lwd=2,
     main='Decomposition of Log Transformed Air Passengers')

lair.decomp$time.series # log로 변환된 값
exp(lair.decomp$time.series) # 지수를 사용하여 원래 값 확인
