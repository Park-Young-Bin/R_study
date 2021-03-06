# 통계계산 및 실습2 기말 프로젝트

# 1. 두 집단의 모비율 검정
n1 <- 1001; n2 <- 1001;
phat1 <- 475/1001
phat2 <- 347/1001

phat <- (475 + 347) / (n1 + n2)
prop.test(x = c(475, 347), n = c(1001, 1001), alternative = 'greater', correct = T)

# 2 카이제곱 검정
library(gmodels)
mpg <- as.data.frame(ggplot2::mpg)
print(fit.t <- table(mpg$class, mpg$drv))
print(fit.chi <- CrossTable(fit.t, expected = T, chisq = T))

# 3. 상관분석
cor.test(iris$Sepal.Length, iris$Petal.Length)

# 4. 단순선형회귀분석
library(ggplot2)
result <- lm(price ~ carat, data = diamonds)
plot(diamonds$carat, diamonds$price, main = '회귀결과', xlab = 'carat', ylab = 'price')
abline(result, col = 'blue', lwd = 3)
summary(result)

# 5. 일원배치분산분석 + 사후분석
colnames(iris)
oneway.test(Petal.Width ~ Species, data = iris)
TukeyHSD(aov(Petal.Width ~ Species, data = iris))
plot(TukeyHSD(aov(Petal.Width ~ Species, data = iris)))
