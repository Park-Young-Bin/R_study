# 필요한 패키지 설치 및 로드
# install.packages('writexl')
library(writexl)

# 데이터 생성
dat_M = c(117, 108, 105, 89, 101, 93, 96, 108, 108, 
          94, 93, 112, 92, 91, 100, 96, 120, 86, 96, 95)
dat_F = c(121, 101, 102, 114, 103, 105, 101, 131, 96, 109, 
          109, 113, 115, 94, 108, 96, 110, 112, 120, 100)

# 독립표본 t 검정
t.test(dat_M, dat_F, var.equal = T)

# 데이터 프레임 생성 후 엑셀 파일로 저장
df <- data.frame(dat_M = c(117, 108, 105, 89, 101, 93, 96, 108, 108, 
                           94, 93, 112, 92, 91, 100, 96, 120, 86, 96, 95),
                 dat_F = c(121, 101, 102, 114, 103, 105, 101, 131, 96, 109, 
                           109, 113, 115, 94, 108, 96, 110, 112, 120, 100))
write_xlsx(df, 'df.xlsx')

# 데이터 생성
data <- data.frame(first = c(0.430,0.266,0.567,0.531,0.707,
                             0.716,0.651,0.589,0.469,0.723),
                   second = c(0.415,0.238,0.390,0.410,0.605,
                              0.609,0.632,0.523,0.411,0.612))

# 쌍체표본 t 검정
t.test(data$first, data$second, paired = TRUE)

# 엑셀 파일 저장
write_xlsx(data, 'data.xlsx')

# 단일모집단 평균 구간추정

# 패키지 설치 및 로드
install.packages('ggplot2')
library(ggplot2)

# 데이터 불러오기 및 확인
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)

# 배기량 평균
mean(mpg$displ) # 3.471795

# 샘플링
sample(mpg$displ, 10)

# 단일모집단 평균에 대한 신뢰구간 계산
n <- 10
a <- c(6.1, 3.1, 2.4, 3.0, 5.4, 5.2, 3.5, 4.0, 4.7, 4.0)
xbar <- mean(a)
s <- sd(a)
alpha_95 <- .05
se_1 <- s/sqrt(n)
ms_95 <- qt(1-alpha_95/2, n-1) * se_1
xbar + c(-ms_90, ms_95)

# 단일모집단 비율에 대한 신뢰구간 계산
phat <-  0.307
se <- sqrt(phat*(1-phat)/n)
me_95 <- qnorm(1-alpha_95/2) *se
phat + c(-me_95, +me_95)

# 단일모집단 표준편차에 대한 신뢰구간 계산
# 데이터 확인
head(iris)

# 행, 열 개수 확인
dim(iris)

# 컬럼명 확인
colnames(iris)

# 신뢰구간 계산
x <- sample(iris$Petal.Length, 15)
n <- length(x)
s <- sd(x)
alpha_95 <- 0.05
se_1 <- s/sqrt(n)
((n-1)*s^2)/c(qchisq(c(1-alpha_95/2, alpha_95/2), df=n-1))

# 단일모집단 평균에 대한 가설검정
# 패키지 설치 및 로드
# install.packages('ggplot2')
library(ggplot2)

# 데이터 불러오기 및 확인
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)

# 배기량 평균/표준편차
mean(mpg$cty) # 16.85897
sd(mpg$cty) # 4.255946

# 가설검정 p-값 계산
n <- 50
xbar <- mean(sample(mpg$cty, n))
z <- (xbar - 16.85897)/(4.255946/sqrt(n))
pnorm(z)

# 단일모집단 비율에 대한 가설검정
prop.test(x = 290, n = 1001, p=0.35, alternative = 'two.sided', correct = F)

# 단일모집단 표준편차에 대한 가설검정
n <- 40
s <- sd(sample(iris$Sepal.Length, 40))
sigma <- 0.8
chi <- ((n-1)*s^2)/ (sigma^2)
chi