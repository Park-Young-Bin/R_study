exam <- read.csv("csv_exam.csv")

exam[] # 조건 없이 전체 데이터 출력

exam[1, ] # 1행 추출

exam[2, ] # 2행 추출

# 1반 이면서 수학 점수가 50점 이상
exam[exam$class == 1 & exam$math >= 50, ]

# 영어 점수가 90점 미만이거나 과학 점수가 50점 미만
exam[exam$english < 90 | exam$science < 50, ]

exam[ ,1] # 첫 번째 열 추출
exam[ ,2] # 두 번째 열 추출

exam[ , "class"] # class 변수 추출
exam[ , "math"] # math 변수 추출
exam[ , c("class", "math", "english")] # class, math, english 변수 추출

# 행, 변수 동시 추출
exam[1,3]

# 행 인덱스, 열 변수명
exam[5, "english"]

# 행 부등호 조건, 열 변수명
exam[exam$math >= 50, "english"]

# 행 부등호 조건, 열 변수명
exam[exam$math >= 50, c("english", "science")]

library(dplyr)
exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  group_by(class) %>% 
  summarise(tot = sum(math, english, science)) %>% 
  mutate(aver = tot/3)

exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  mutate(tot = (math + english + science)/3) %>% 
  group_by(class) %>% 
  summarise(mean_tot = mean(tot))

exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  group_by(class) %>% 
  summarise(tot = sum(math, english, science))

exam %>% 
  filter(math >= 50 & english >= 80) %>% 
  mutate(tot = (math + english + science)/3) %>% 
  group_by(class) %>% 
  summarise(mean_tot = mean(tot))

exam$tot <- (exam$math + exam$english + exam$science)/3
aggregate(data = exam[exam$math >= 50 & exam$english >= 80], tot~class, mean)

?aggregate

mpg <- as.data.frame(ggplot2::mpg)
mpg %>% 
  filter(class %in% c("compact", "suv")) %>% 
  group_by(class) %>% 
  mutate(tot = (cty + hwy)/2) %>% 
  summarise(mean_tot = mean(tot))

mpg$tot <- (mpg$cty + mpg$hwy)/2  
aggregate(data = mpg[mpg$class = "compact" & mpg$class = "suv", ], tot~))

mpg$tot <- (mpg$cty + mpg$hwy)/2
df_comp <- mpg[mpg$class == "compact",]
df_suv <- mpg[mpg$class == "suv", ]
mean(df_comp$tot)
mean(df_suv$tot)

var1 <- c(1,2,3,1,2) # numeric 변수 생성
var2 <- factor(c(1,2,3,1,2)) # factor 변수 생성

var1+2 # numeric 변수로 연산
var2+2 # factor 변수로 연산

class(var1)
class(var2)

levels(var1)
levels(var2)

var3 <- c("a","b","b","c") # 문자 변수 생성
var4 <- factor(c("a","b","b","c")) # 문자로된 fatocr 변수 생성

var3
var4

class(var3)
class(var4)

mean(var1)
mean(var2)

var2 <- as.numeric(var2) # numberic 타입으로 변환
mean(var2)

class(var2) # 타입 확인
levels(var2) # 범주 확인

class(mpg$drv)
mpg$drv <- as.factor(mpg$drv)
class(mpg$drv)
levels(mpg$drv)

a <- 1
a
b <- "hello"
b

class(a)
class(b)

x1 <- data.frame(var1 = c(1,2,3),
                 var2 = c("a","b","c"))
a1
class(a1)

# 매트릭스 만들기 - 1~12로 2열
x2 <- matrix(c(1:12), ncol = 2)
x2
class(x2)

a2 <- matrix(1:12, nrow=4, byrow=T)
a2

# array 만들기 - 1~20으로 2행 x 5열 x 2차원
x3 <- array(1:20,dim = c(2, 5, 2))
x3

# 리스트 생성 - 앞에서 생성한 데이터 구조 활용
x4 <- list(f1 = a, # 벡터
           f2 = x1, # 데이터 프레임
           f3 = x2, # 매트릭스
           f4 = x3) # 어레이
x4

mpg <- ggplot2::mpg
x <- boxplot(mpg$cty)
x

x$stats[ ,1]
x$stats[,1][3]
