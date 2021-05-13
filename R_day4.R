df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df

is.na(df) # 결측치 확인

table(is.na(df))

table(is.na(df$sex)) # sex 결측치 빈도 출력
table(is.na(df$score)) # score 결측치 빈도 출력

mean(df$sex) # 평균 산출
mean(df$score) # 합계 산출

library(dplyr)
df %>% filter(is.na(score)) # score가 NA인 데이터만 출력
df %>% filter(!is.na(score)) # score 결측치 제거

df_nomiss <- df %>% filter(!is.na(score)) # score 결측치 제거
mean(df_nomiss$score) # score 평균 산출
sum(df_nomiss$score) # score 합계 산출

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex)) # score, sex 결측치 제거
df_nomiss # 출력

df_nomiss2 <- na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2

mean(df$score, na.rm = T) # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T) # 결측치 제외하고 합계 산출

exam <- read.csv("csv_exam.csv") # 데이터 불러오기
exam[c(3, 8, 15), "math"] <- NA # 3, 8, 15행의 math에 NA 할당
exam
exam %>% summarise(mean_math = mean(math)) # math 평균 산출

# math 결측치 제외하고 평균 산출
exam %>% summarise(mean_math = mean(math, na.rm = T))

exam %>% summarise(mean_math = mean(math, na.rm = T), # 평균 산출
                   sum_math = sum(math, na.rm = T), # 합계 산출
                   median_math = median(math, na.rm = T)) # 중앙값 산출

mean(exam$math, na.rm = T) # 결측치 제외하고 math 평균 산출

exam$math <- ifelse(is.na(exam$math), 55, exam$math) # math가 NA면 55로 대체
table(is.na(exam$math)) # 결측치 빈도표 생성
exam # 출력
mean(exam$math) # math 평균 산출

mpg <- as.data.frame(ggplot2::mpg) # mpg 데이터 불러오기
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA # NA 할당하기

table(is.na(mpg$drv))
table(is.na(mpg$hwy))

library(dplyr)
mpg %>% 
  group_by(drv) %>% 
  filter(!is.na(hwy)) %>% 
  summarise(mean_hwy = mean(hwy))

outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

table(outlier$sex)
table(outlier$score)

# sex가 3보다 크면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
# score가 5보다 크면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)

outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats # 상자 그림 통계치 출력

# 12~37 벗어나면 NA 할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy, na.rm = T))

mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)

table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c(4, "f", "r"), mpg$drv, NA)
table(mpg$drv)

boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)

mpg %>% 
  group_by(drv) %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  summarise(mean_cty = mean(cty))

