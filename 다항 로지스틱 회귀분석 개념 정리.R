# 다항 로지스틱 회귀분석 개념 정리
# 참고자료: https://www.youtube.com/watch?v=4ibxb0OpgWA

# install.packages('EffectStars')
# install.packages('VGAM')
library(EffectStars)
data(PID)
str(PID)
levels(PID$PID)

library(VGAM)
pid.mlogit <- vglm(PID ~ ., family = multinomial(), data = PID)
summary(pid.mlogit)
# 다항로지스틱 회귀분석을 종속변수의 범주가 g개인 경우, (g-1)개의 모델을 생성한다.
# 여기서 결과 범주인 PID 범주는 3개("Democrat", "Independent", "Republican" )이므로 총 2개의 모델이 생성되었다.
# summary() 함수 결과, 2개의 절편과 각 변수당 2개 씩의 회귀계수가 계산되었다.
# Education 변수는 범주형 변수이므로 모델 내에서 더미변수로 자동 변환됨(low가 기준변수로서 0으로, hight는 1로 코딩됨)
# vglm() 함수는 마지막 범주를 기준 범주로 사용한다. 본 분석에서는 "Republican"이 기준 범주로 사용되었다.
# Republican이 기준 범주이기 때문에 추정된 두 개의 회귀모델은 Republican에 대한 나머지 두 범주의 로그오즈를 각 예측변수의 회귀계수의 선형결합으로 표현할 수 있다.
# 각 예측변수의 회귀계수는 다른 독립변수들이 일정하다는 가정하에서 해당 예측변수의 한 단위의 증가가 가져오는 로그오즈의 변화량을 나타낸다.
# 예를 들어 Democrate 대 Republican 회귀모델을 보면 뉴스를 시청한 일수가 하루 늘어나면 로그오즈는 0.044만큼 증가한다. 그리고 교육수준 한 단위 증가는 로그오즈를 0.282만큼 감소시킨다.
# 교육수준이 한 단위 증가한다는 의미는 low에서 high로 증가한다는 것을 의미한다.
# 소득이 한 단위 증가할 때마다 로그오즈는 0.017만큼 감소한다.
# 하지만 다항 로지스틱 회귀모형 양변에 지수함수를 취해서 결과변수를 로그오즈가 아닌 오즈로 변환한다.
# 이렇게 결과 변수가 오즈인 회귀모델을 만들면 새롭게 계산된 회귀계수를 이용해서 예측변수 한 단위 증가에 따른 오즈의 변화비율을 알 수 있다. 
# 오즈의 변화비율 = 다른 예측변수가 동일하다는 가정하에서 예측변수가 한단위 증가할 때 어떤 한 범주에 속할 확률이 기준 범주에 속할 확률대비 몇 배가 더 큰지/작은지 알 수 있다.

exp(coef(pid.mlogit))
# 숫자 1은 Democrate 대 Republican 회귀모델이다.
# 숫자 2는 Independent 대 Republica" 회귀모델이다.

## 해석 예시
# 뉴스 시청일수 오즈비는 1.045이다. 뉴스 시청일이 하루 늘어날 경우, Democrate일 가능성이 Republican일 가능성에 비해 4.5% 증가한다.
# 교육 수준에 대한 오즈비는 0.749이다. 교육 수준 한 단위 증가는 Democrate일 가능성을 Republican일 가능성 대비 25.1% 감소시킨다. 다시 말해, 교육수준이 low에서 high로 바뀌면 Democrate일 가능성은 Republican일 가능성에 비해 25.1% 작아진다는 것을 의미한다.
# 하지만 TVnews:1과 Educationhigh:1은 통계적으로 유의하지 않다. 따라서 해석할 때 유의한다.
# 소득에 대한 오즈비는 0.9835이다. 소득 한 단위를 증가시키면 Democrate일 가능성이 Republican일 가능성에 비해 1.6% 감소한다. 또한 이러한 영향력은 통계적으로 유의하다. 따라서 소득의 변화는 Democrate일 가능성과 Republican일 가능성에 유의한 영향을 미치는 것을 확인할 수 있다.

# 다항 로지스틱 회귀모델에 대한 각 범주별 예측확률은 fitted() 함수를 이용하여 알 수 있다.
# 각 케이스 별로 분류할 확률이 추정됨을 볼 수 있다.
pid.mlogit.pred <- fitted(pid.mlogit)
head(pid.mlogit.pred)
