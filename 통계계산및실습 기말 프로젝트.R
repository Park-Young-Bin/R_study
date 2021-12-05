# 통계계산 및 실습2 기말 프로젝트

# 1
n1 <- 1001; n2 <- 1001;
phat1 <- 475/1001
phat2 <- 347/1001

phat <- (475 + 347) / (n1 + n2)
prop.test(x = c(475, 347), n = c(1001, 1001), alternative = 'greater', correct = T)

# 2
library(gmodels)
mpg <- as.data.frame(ggplot2::mpg)
print(fit.t <- table(mpg$class, mpg$drv))
print(fit.chi <- CrossTable(fit.t, expected = T, chisq = T))
