library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

welfare <- raw_welfare

welfare <- rename(welfare,
                   sex = h10_g3,
                   birth = h10_g4,
                   marriage = h10_g10,
                   regligion = h10_g11,
                   income = p1002_8aq1,
                   code_job = h10_eco9,
                   code_region = h10_reg7)

welfare$age <- 2015 - welfare$birth + 1

welfare <- welfare %>%  
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <=59 , "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

summary(welfare$income)
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ageg_income

ggplot(data=ageg_income, aes(x = ageg, y = mean_income)) + 
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

table(welfare$sex)
welfare$sex <- if_else(welfare$sex == 1 ," male", "female")
table(welfare$sex)
qplot(welfare$sex)

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

# 성별, 연령별 월급 평균표 만들기
sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

# 그래프 만들기
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) +
  geom_line()

class(welfare$code_job)

table(welfare$code_job)

list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, by = "code_job")

welfare %>%
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

job_income <- welfare %>% 
  filter(!is.na(income) & !is.na(job)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

# 하위 10위 추출
bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)
