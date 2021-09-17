library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

exam %>% filter(class == 1) # exam에서 class가 1인 경우만 추출해 출력
exam %>%  filter(class != 1) # 1반이 아닌 경우
exam %>%  filter(math > 50) # 수학 점수가 50점을 초과한 경우
exam %>%  filter(math < 50) # 수학 점수가 50점 미만인 경우
exam %>% filter(english >= 80) # 영어 점수가 80점 이상인 경우

# 1반이면서 수학 점수가 50점 이상인 경우
exam  %>%  filter(class == 1 & math >= 50)

# 2반이면서 영어 점수가 80점 이상인 경우
exam %>% filter(class == 2 & english >= 80)

# 영어 점수가 90점 미만이거나 과학 점수가 50점 미만인 경우
exam %>%  filter(english < 90 | science < 50)

# 1, 3, 5반에 해당하면 추출
exam %>%  filter(class == 1 | class == 3 | class == 5)

exam %>% filter(class %in% c(1, 3, 5)) # 1, 3, 5반에 해당하면 추출

class1 <- exam %>%  filter(class == 1) # class가 1인 행 추출, class1에 할당
class2 <- exam %>%  filter(class == 2) # class가 2인 행 추출, class2에 할당
mean(class1$math) # 1반의 수학 점수 평균
mean(class2$math) # 2반의 수학 점수 평균

library(ggplot2)
mpg_displ_4 <- mpg %>% filter(displ <= 4) # 배기량이 4 이하인 경우
mpg_displ_5 <- mpg %>% filter(displ >= 5) # 배기량이 5 이상인 경우
mean(mpg_displ_4$hwy)
mean(mpg_displ_5$hwy)

str(mpg)

new_audi <- mpg %>% filter(manufacturer == "audi") # 자동차 제조 회사가 audi인 경우
new_toyota <- mpg %>%  filter(manufacturer == "toyota") # 자동차 제조 회사가 toyota인 경우

mean(new_audi$cty)
mean(new_toyota$cty)

mpg_manu <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg_manu$hwy)

exam %>% select(math) # math 추출
exam %>% select(english) # english 추출 
exam %>% select(class, math, english) # class, math, english 변수 추출
exam %>% select(-math) # math 제외
exam %>% select(-math, -english) #math, english 제외

# class가 1인 행만 추출한 다음 english 추출
exam %>% filter(class == 1) %>% select(english)

exam %>% 
  filter(class == 1) %>% # class가 1인 행 추출
  select(english) # english 추출

exam %>% 
  select(id, math) %>%  # id, math 추출
  head # 앞부분 6행까지 추출

exam %>% 
  select(id, math) %>%  # id, math 추출
  head(10) # 앞부분 10행까지 추출

mpg <- as.data.frame(ggplot2::mpg)
df <- mpg %>% select(class, cty)
head(df, 10)

mpg_suv <- mpg %>% filter(class == "suv")
mpg_compact <- mpg %>% filter(class == "compact")s
mean(mpg_suv$cty)
mean(mpg_compact$cty)

exam %>% arrange(math) # math 오름차순 정렬
exam %>% arrange(desc(math)) # math 내림차순 정렬
exam %>% arrange(class, math) # class 및 math 오름차순 정렬

mpg <- as.data.frame(ggplot2::mpg)
mpg_audi %>% 
          filter(manufacturer == "audi") %>% 
          arrange(desc(hwy)) %>% 
          head(5)

exam %>% 
  mutate(total = math + english + science) %>% # 총합 변수 추가
  head # 일부 추출

exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>% 
  head

exam %>% 
  mutate(test = ifelse(science >= 60 ,"pass", "fail")) %>% 
  head

exam %>% 
  mutate(total = math + english + science) %>% # 총합 변수 추가
  arrange(total) %>% # 총합 변수 기준 정렬
  head # 일부 추출

mpg <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg

mpg_new <- mpg_new %>% mutate(total = cty + hwy)
mpg_new

mpg_new <- mpg_new %>% mutate(mean = (cty + hwy)/2)

mpg_new %>% 
  mutate(mean = (cty + hwy)/2) %>% 
  arrange(desc(mean)) %>% 
  head(3)

mpg_new %>% 
  arrange(desc(mean)) %>% 
  head(3)
     
exam %>% summarise(mean_math = mean(math)) # math 평균 산출

exam %>% 
  group_by(class) %>% # class별로 분리
  summarise(mean_math = mean(math)) # math 평균 산출

exam %>% 
  group_by(class) %>%  # class별로 분리
  summarise(mean_math = mean(math), # math 평균
            sum_math = sum(math), # math 합계
            median_math = median(math), # math 중앙값
            n = n()) # 학생 수

mpg %>% 
  group_by(manufacturer, drv) %>% # 회사별, 구동 방식별 분리
  summarise(mean_cty = mean(cty)) %>%  # cty 평균 산출
  head(10)

mpg %>% 
  group_by(manufacturer) %>%  # 회사별 분리
  filter(class == "suv") %>%  # suv 추출
  mutate(tot = (cty+hwy)/2) %>% # 통합 연비 변수 생성
  summarise(mean_tot = mean(tot)) %>% # 통합 연비 평균 산출
  arrange(desc(mean_tot)) %>% # 내림차순
  head(5) # 1~5위까지 출력

mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "compact") %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# 중간고사 데이터 생성
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
# 기말고사 데이터 생성
test2 <- data.frame(id=c(1,2,3,4,5),
                    final = c(70,83,65,95,80))
test1 # test1 출력
test2 # test2 출력
total <- left_join(test1, test2, by = "id") # id를 기준으로 합쳐 total에 할당
total # total 출력

name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim", "lee", "park", "choi", "hung"))
name

exam_name <- left_join(exam, name, by = "class")
exam_name

# 학생 1~5번 시험 데이터 생성
group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))
# 학생 6~10번 시험 데이터 생성
group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70,83,65,95,80))

group_all <- bind_rows(group_a, group_b)
group_all

fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel

mpg <- as.data.frame(ggplot2::mpg)
mpg <- left_join(mpg, fuel, by = "fl")

mpg %>% select(model, fl, price_fl) %>% head(5)

