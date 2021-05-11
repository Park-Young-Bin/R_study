english <- c(90, 80, 60, 70) # 영어 점수 변수 생성
english

math <- c(50, 60, 100, 20) # 수학 점수 변수 생성
math

# english, math로 데이터 프레임 생성해서 df_midterm에 할당
df_midterm <- data.frame(english, math)
df_midterm

class <- c(1, 1, 2, 2)
df_midterm <- data.frame(english, math, class)
df_midterm

mean(df_midterm$english) # df_midterm의 english로 평균 산출
mean(df_midterm$math) # df_midterm의 math로 평균 산출

df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 200),
                         class = c(1, 1, 2, 2))
df_midterm

df_market <- data.frame("제품" = c("사과", " 딸기", "수박"),
                        "가격" = c(1800, 1500, 3000),
                        "판매량" = c(24, 38, 13))
df_market

mean(df_market$"가격")
mean(df_market$"판매량")

install.packages("readxl")
library(readxl)

df_exam <- read_excel("excel_exam.xlsx") # 엑셀 파일 불러와 df_exam에 할당
df_exam
df_exam <- read_excel("C:/Users/user/Desktop/R/excel_exam.xlsx")
df_exam

mean(df_exam$english)
mean(df_exam$science)

df_exam_novar <- read_excel("excel_exam_novar.xlsx")
df_exam_novar
df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam_novar

# 엑셀 파일의 세 번째 시트에 있는 데이터 불러오기
df_excel_sheet <- read_excel("excel_exam_sheet.xlsx", sheet = 3)
df_excel_sheet

df_csv_exam <- read.csv("csv_exam.csv")
df_csv_exam

df_csv_exam <- read.csv("csv_exam.csv", stringsAsFactors = F)

df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 200),
                         class = c(1, 1, 2, 2))
df_midterm
write.csv(df_midterm, file = "df_midterm.csv")

saveRDS(df_midterm, file = "df_midterm.rds")

rm(df_midterm)

df_midterm

df_midterm <- readRDS("df_midterm.rds")
df_midterm

load("df_midterm.rda")

exam <- read.csv("csv_exam.csv")
head(exam) # 앞에서부터 6행까지 출력
head(exam, 10) # 앞에서부터 10행까지 출력

tail(exam) # 뒤에서부터 6행까지 출력력
tail(exam, 10) # 뒤에서부터 10행까지 출력

View(exam) # 데이터 뷰어 창에서 exam 데이터 확인

dim(exam) # 행, 열 출력

str(exam) # 데이터 속성 확인

summary(exam) # 요약 통계량 출력

# ggplot2의 mpg 데이터를 데이터 프레임 형태로 불러오기
mpg <- as.data.frame(ggplot2::mpg)

head(mpg)
tail(mpg)
dim(mpg)
str(mpg)

?mpg

summary(mpg)

df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_raw
install.packages("dplyr")
library(dplyr)

df_new <- df_raw # 복사본 생성
df_new # 출력

df_new <- rename(df_raw, v2 = var2) # var2를 v2로 수정
df_new

mpg <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg
mpg_new <- rename(mpg, city=cty, high=hwy)
head(mpg_new)
head(mpg)

df <- data.frame(var1 = c(4,3,8),
                  var2 = c(2,6,1))
df
df$var_sum <- df$var1 + df$var2 # var_sum 파생변수 생성
df

df$var_mean <- (df$var1 + df$var2)/2 # var_mean 파생변수 생성
df

mpg$total <- (mpg$cty + mpg$hwy)/2 # 통합 연비 변수 생성
head(mpg)
mean(mpg$total)

summary(mpg$total) # 요약 통계량 산출
hist(mpg$total) # 히스토그램 생성

mpg$test <- ifelse(mpg$total >= 20, "pass", "fail") # 20 이상이면 pass, 그렇지 않으면 fail 부여
head(mpg, 20)

table(mpg$test) # 연비 합격 빈도표 생성

library(ggplot2) # ggplot2 로드
qplot(mpg$test) # 연비 합격 빈도 막대 그래프 생성

mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 20 ,"B", "c"))
head(mpg, 20) # 데이터 확인

table(mpg$grade) # 등급 빈도표 생성
qplot(mpg$grade) # 등급 빈도 막대 그래프 생성

# A, B, C, D 등급 부여
mpg$grade2 <- ifelse(mpg$total >= 30 ,"A",
                     ifelse(mpg$total >= 25 ,"B",
                            ifelse(mpg$total >= 20 ,"C", "D")))
head(mpg, 20)                     
table(mpg$grade2)
qplot(mpg$grade2)

midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

library(dplyr)
midwest_new <- midwest
midwest_new <- rename(midwest, total = poptotal, asian = popasian)
head(midwest_new, 1)
head(midwest, 1)

midwest_new$percent <- (midwest_new$asian / midwest_new$total)*100
hist(midwest_new$percent)

mean(midwest_new$percent)
midwest_new$aver <- ifelse(midwest_new$percent > 0.4872462, "large", "small")

table(midwest_new$aver)
library(ggplot2)
qplot(midwest_new$aver)
