# 회귀분석----

data <- iris
head(iris)
lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data)

# 내장 데이터 iris를 이용하여 다중회귀분석을 실시한다.
# 회귀분석의 함수는 lm이다.
# 종속변수 ~ 독립변수1 + 독립변수2 + 독립변수3 + ...
# call은 내가 작성한 현재 회귀식을 보여준다.

summary(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data))

# 더 자세한 정보를 알고싶은 경우, summary 함수를 이용한다.
# Pr(>|t|)은 통계에서 말하는 유의확률(p-value)이다. 숫자가 작을 수록 좋으며 보통 0.05보다 작아야 통계적으로 의미가 있다.
# Coefficients(계수)의 Estimate(추정치)를 통해 회귀 계수들의 추정치를 알 수 있다. 어떤 변수의 계수가 0이라면 그 변수는 모형에 아무런 영향을 주지 않는다.
# R-squared: 모형의 설명력을 뜻함, 회귀식을 통해서 계산된 예측 값이 실제 y값을 얼마나 설명하는지를 뜻한다, 숫자가 클수록 큰 설명력을 갖는다. 
##다중회귀라면 Adjusted R-squared(수정된 결정계수)를 확인한다. > 전체 데이터의 85.57% 정도를 설명한다.

summary(step(lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data)))

# step 함수를 추가하여 단계적 회귀분석 실시, PC가 알아서 설명력이 최대가 되는 변수들만 추려준다.
# F-statistic(F 통계량)은 해당 모형이 유의미한지 나타낸다. 보통 p값이 0.05보다 작으면 모형이 유의하다.

study_daily <- c(6, 7, 3, 4, 8, 6, 4, 5, 5, 2, 4, 5, 6, 8, 2)
TOEIC <- c(770, 790, 590, 615, 870, 790, 600, 650, 640, 490, 455, 585, 765, 840, 430)
data <- data.frame(study_daily, TOEIC)
head(data)

library(ggplot2)
ggplot(data = data, aes(x = study_daily, y = TOEIC)) + 
  geom_point() + 
  stat_smooth(color = "#FC4E07", method = "lm") # 선형추세선 (lm 으로 선형회귀식적용)

# 단순회귀분석  
summary(lm(TOEIC ~ study_daily, data))


