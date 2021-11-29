# install.packages('formattable') # 천단위 표시 가능한 패키지
# install.packages('gmodels')
# install.packages('VGAM')
library(gmodels)
library(VGAM) # 로지스틱 회귀분석
library(formattable)
library(writexl)
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape)

appen <- read_excel('t18appen.xlsx')
ind <- read_excel('t18ind.xlsx')

select_ind <- ind %>% 
  select(PIDWON, C14, C3, C4_0, i_MEDICALEXP1)

select_appen <- appen %>% 
  select(PIDWON, SJ1, SH16, SH18, SH20, SH21, SH23, SH24, SH25)

# PIDWON 기준으로 merge
merge_df <- merge(select_ind, select_appen, by = 'PIDWON')

# 변수명 및 값 변경
# 변수명 변경(영 -> 한)
rename_df <- dplyr::rename(merge_df, 
                           '성별' = C3,
                           '출생년도' = C4_0,
                           '장애여부' = C14,
                           '운동능력' = SJ1,
                           '와병률' = SH16,
                           '개인지출의료비' = i_MEDICALEXP1,
                           '결근결석' = SH18,
                           '시력문제' = SH20,
                           '청력문제' = SH21,
                           '기억력' = SH23,
                           '의사결정' = SH24,
                           '질병/손상 등으로 활동제한' = SH25)

# 성별 변수 변환
rename_df$성별 <- ifelse(rename_df$성별 == 1, '남', '여')

# 나이 및 연령대 계산
rename_df$나이 <- 2018 - rename_df$출생년도 + 1

rename_df$연령대 <- ifelse(rename_df$나이 < 8, "학령기 이전", 
                        ifelse(rename_df$나이 >= 8 & rename_df$나이 <= 19, '미성년',
                               ifelse(rename_df$나이 >=20 & rename_df$나이 <= 39, '청년',
                                      ifelse(rename_df$나이 >= 40 & rename_df$나이 <= 64, '중년', '고령'))))

# 연령대 변수 facotr형 변환(chr -> factor)
data$연령대<- factor(data$연령대, levels=c("미성년", '청년', '중년', '고령'))

# 장애인/비장애인 분류
rename_df$장애여부 <- ifelse(rename_df$장애여부 == -1, '비장애인', '장애인')

# 종속변수(운동능력) 변환
table(rename_df$운동능력)
rename_df$운동능력 <- ifelse(rename_df$운동능력 == 1, '걷기 지장 없음', 
                         ifelse(rename_df$운동능력 == 2, '걷기 다소 지장 있음', 
                                ifelse(rename_df$운동능력 == 3, '종일 누워있음', NA)))

# 와병률 변수 변환
rename_df$와병률 <- ifelse(rename_df$와병률 == 1, '예',
                        ifelse(rename_df$와병률 == 2, '아니오', NA))
# 결근결석 변수 변환
rename_df$결근결석 <- ifelse(rename_df$결근결석 == 1, '예', 
                         ifelse(rename_df$결근결석 == 2, '아니오', 
                                ifelse(rename_df$결근결석 == 3, '학교/직장 안다님', NA)))

# 시력문제 변수 변환
rename_df$시력문제 <- ifelse(rename_df$시력문제 == 1, '문제 없음', 
                         ifelse(rename_df$시력문제 == 2, '조금 문제 있음', 
                                ifelse(rename_df$시력문제 == 3, '많이 문제 있음', 
                                       ifelse(rename_df$시력문제 == 4, '전혀 보지 못함', NA))))

# 청력문제 변수 변환
rename_df$청력문제 <- ifelse(rename_df$청력문제 == 1, '문제 없음', 
                         ifelse(rename_df$청력문제 == 2, '조금 문제 있음', 
                                ifelse(rename_df$청력문제 == 3, '많이 문제 있음', 
                                       ifelse(rename_df$청력문제 == 4, '전혀 듣지 못함', NA))))

# 기억력 변수 변환
rename_df$기억력 <- ifelse(rename_df$기억력 == 1, '문제 있음', 
                        ifelse(rename_df$기억력 == 2, '문제 없음', NA))

# 의사결정 변수 변환
rename_df$의사결정 <- ifelse(rename_df$의사결정 == 1, '문제 있음', 
                         ifelse(rename_df$의사결정 == 2, '문제 없음', NA))

# 질병/손상 등으로 활동제한 변수 변환
rename_df$'질병/손상 등으로 활동제한' <- ifelse(rename_df$'질병/손상 등으로 활동제한' == 1, '문제 있음', 
                                     ifelse(rename_df$'질병/손상 등으로 활동제한' == 2, '문제 없음', NA))

# PIDWON, 나이, 출생년도 삭제
data <- rename_df %>% select(-c(PIDWON, 출생년도))
str(data)

# NA 확인
sum(is.na(data)) # 547개(회귀분석시, 제거 필수)

# 연령별 연간 개인지출의료비 분포 확인(boxplot)
# boxplot(개인지출의료비 ~ 연령대, data)
boxplot(개인지출의료비 ~ 연령대, data, 
               cex.main = 1.6, cex.axis = 1.2, 
               # las = 1,
               outline = F, 
               ylim = c(0, 3500000), 
               xlab = '', ylab = '개인지출의료비(단위: 원)',
               main = '연령대별 연간 개인지출의료비 상자그림(이상치 제거)') 
grid(ny=23)
boxplot(개인지출의료비 ~ 연령대, data, 
               cex.main = 1.6, cex.axis = 1.2, 
               # las = 1,
               outline = F, 
               ylim = c(0, 3500000), 
               xlab = '', ylab = '개인지출의료비(단위: 원)',
               main = '연령대별 연간 개인지출의료비 상자그림(이상치 제거)', add = TRUE, ann = FALSE) 

boxplot(개인지출의료비 ~ 연령대, data)$stats # 상자그림 통계치 출력
# 아래쪽 이상치 경계
# 1사분위수
# 중앙값
# 3사분위수
# 위쪽 이상치 경계

# 각 연령별 연간 개인지출의료비 분포(boxplot)

## 미성년
boxplot(data[data$연령대 == '미성년', '개인지출의료비'])$stats
summary(data[data$연령대 == '미성년', '개인지출의료비'])

ggplot(data = data %>% filter(연령대 == '미성년'), aes(y = 개인지출의료비)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 261650))
  
p1 <- ggplot(data = data %>% filter(연령대 == '미성년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  # ggtitle('미성년 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_blank()) +
  # scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 261650)) # 이상치 제거 상자그림
p1

# ggplot(data = data %>% filter(연령대 == '미성년'), aes(x = 개인지출의료비)) + 
#   geom_histogram() + 
#   scale_y_continuous(labels = scales::comma)

## 청년
boxplot(data[data$연령대 == '청년', '개인지출의료비'])$stats
summary(data[data$연령대 == '청년', '개인지출의료비'])

ggplot(data = data %>% filter(연령대 == '청년'), aes(y = 개인지출의료비)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma)

ggplot(data = data %>% filter(연령대 == '청년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(labels = scales::comma)

p2 <- ggplot(data = data %>% filter(연령대 == '청년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  ylab(' ') +
  theme(axis.text.x=element_blank()) +
  # gtitle('청년 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 752040)) # 이상치 제거 상자그림

# ggplot(data = data %>% filter(연령대 == '청년'), aes(x = 개인지출의료비)) + 
#   geom_histogram() + 
#   scale_y_continuous(labels = scales::comma)+ 
#   scale_x_continuous(labels = scales::comma)

## 중년
boxplot(data[data$연령대 == '중년', '개인지출의료비'])$stats
data[data$연령대 == '중년', '개인지출의료비']
summary(data[data$연령대 == '중년', '개인지출의료비'])

ggplot(data = data %>% filter(연령대 == '중년'), aes(y = 개인지출의료비)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma)

ggplot(data = data %>% filter(연령대 == '중년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA)+ 
  scale_y_continuous(labels = scales::comma)

p3 <- ggplot(data = data %>% filter(연령대 == '중년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ylab(' ')+
  # ggtitle('중년 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 1852160)) # 이상치 제거 상자그림

# ggplot(data = data %>% filter(연령대 == '중년'), aes(x = 개인지출의료비)) + 
#   geom_histogram() + 
#   scale_y_continuous(labels = scales::comma)+ 
#   scale_x_continuous(labels = scales::comma)

## 고령
boxplot(data[data$연령대 == '고령', '개인지출의료비'])$stats
summary(data[data$연령대 == '고령', '개인지출의료비'])
ggplot(data = data %>% filter(연령대 == '고령'), aes(y = 개인지출의료비)) + 
  geom_boxplot()+ 
  scale_y_continuous(labels = scales::comma)
ggplot(data = data %>% filter(연령대 == '고령'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(labels = scales::comma)

p4 <- ggplot(data = data %>% filter(연령대 == '고령'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ylab(' ')+
  #ggtitle('고령 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 3148560)) # 이상치 제거 상자그림

# ggplot(data=data, aes(x=연령대, 개인지출의료비, fill = 연령대))+
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   geom_boxplot(outlier.shape = NA) +
#   xlab('') + 
#   ylab('연간 개인지출의료비(단위: 원)') +
#   scale_x_discrete(limits=c("미성년", "청년", "중년", "고령")) +
#   ggtitle('연령별 연간 개인지출의료비 상자그림') +
#   theme(plot.title = element_text(face='bold',
#                                   size=20,
#                                   hjust = .5),
#         legend.position = "")

# library(gridExtra)
# grid.arrange(p1, p2, p3, p4, ncol = 4)

# 연령별 연간 개인지출의료비 분포 확인(histogram)
# qplot(data = data, 개인지출의료비, fill=연령대, geom="histogram") + 
#   scale_x_continuous(labels = scales::comma) +
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   facet_grid(연령대~.)

# 연령별 연간 개인지출의료비 분포 확인(bar chart)
# data_age <- data %>% group_by(연령대) %>% summarise(n = n(), avg_price = mean(개인지출의료비))
# data_age
# 
# ggplot(data = data_age, aes(x = 연령대, y = avg_price, fill = 연령대)) +
#   geom_col(width = .8) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_discrete(limits=c("미성년", '청년', '중년', '고령')) +
#   theme_minimal() +
#   geom_text(aes(label = paste0(comma(avg_price, format = 'd'), '원')), vjust=-.25, 
#             position = position_dodge(.9), size = 4) +
#   geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', position = position_stack(vjust=0.5)) +
#   # scale_fill_viridis_d() +
#   xlab('') +
#   ylab('연간 평균 개인지출의료비(단위: 원)') +
#   ggtitle('연령별 연간 평균 개인지출의료비') +
#   theme(plot.title = element_text(face='bold',
#                                   size=20,
#                                   hjust = .5),
#         legend.position = " ")

# 연령대 분포
ggplot(data, aes(x=나이)) + 
  geom_histogram(binwidth = 1.9) +
  theme_minimal() +
  ylab('응답자 수(명)') +
  ggtitle('응답자 연령대 히스토그램')+
  scale_x_continuous(breaks=seq(min(data$나이), max(data$나이), by = 10)) +
  scale_x_continuous(breaks=seq(min(data$나이), max(data$나이), by = 10)) +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))

max(data$나이) # 102세
min(data$나이) # 19세


# 미성년 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '미성년', '개인지출의료비'])$stats
age_min <- data[data$연령대 == '미성년', ]
age_min$개인지출의료비 <- ifelse(age_min$개인지출의료비 < 0 | age_min$개인지출의료비 > 261650, 
                          NA, age_min$개인지출의료비)
mean(age_min$개인지출의료비, na.rm = T)
sum(is.na(age_min$개인지출의료비)) # NA 개수

# 청년 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '청년', '개인지출의료비'])$stats
age_young <- data[data$연령대 == '청년', ]
age_young$개인지출의료비 <- ifelse(age_young$개인지출의료비 < 0 | age_young$개인지출의료비 > 752040, 
                            NA, age_young$개인지출의료비)
mean(age_young$개인지출의료비, na.rm = T)
sum(is.na(age_young$개인지출의료비)) # NA 개수

# 중년 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '중년', '개인지출의료비'])$stats
age_long <- data[data$연령대 == '중년', ]
age_long$개인지출의료비 <- ifelse(age_long$개인지출의료비 < 0 | age_long$개인지출의료비 > 1852160, 
                           NA, age_long$개인지출의료비)
mean(age_long$개인지출의료비, na.rm = T)
sum(is.na(age_long$개인지출의료비)) # NA 개수

# 고령 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '고령', '개인지출의료비'])$stats
age_old <- data[data$연령대 == '고령', ]
age_old$개인지출의료비 <- ifelse(age_old$개인지출의료비 < 0 | age_old$개인지출의료비 > 1852160, 
                          NA, age_old$개인지출의료비)
mean(age_old$개인지출의료비, na.rm = T)
sum(is.na(age_old$개인지출의료비)) # NA 개수


# 연령별 연간 중앙값 개인지출의료비
dt_age <- data %>% group_by(연령대) %>% summarise(n = n(), median_price = median(개인지출의료비))
dt_age

ggplot(data = dt_age, aes(x = reorder(연령대, median_price), y = median_price, fill = 연령대)) +
  geom_col(width = .8) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  geom_text(aes(label = paste0(comma(median_price, format = 'd'), '원')), vjust=-.25, 
            position = position_dodge(.9), size = 4) +
  # geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', 
  # position = position_stack(vjust=0.5)) +
  xlab('') +
  ylab('연간 개인지출의료비(단위: 원)') +
  ggtitle('연령별 연간 개인지출의료비 중앙값') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")
# 다중 로지스틱 회귀분석----
# install.packages('gmodels')
# install.packages('VGAM')
# library(gmodels)
library(VGAM) # 로지스틱 회귀분석

table(data$운동능력) # 12,880개
sum(is.na(data)) # 547(운동능력 -> 533개)

# NA 행 제거
vglm_data <- na.omit(data) %>% select(-나이)

str(vglm_data)

# 범주형 변수 -> factor형 변환(chr -> factor)
vglm_data$운동능력 <- factor(vglm_data$운동능력,
                         levels = c('걷기 지장 없음', '걷기 다소 지장 있음', '종일 누워있음'))

table(vglm_data$장애여부)
vglm_data$장애여부 <- factor(vglm_data$장애여부, levels = c('비장애인', '장애인'))

vglm_data$성별 <- factor(vglm_data$성별, levels = c('남', '여'))

vglm_data$연령대 <- factor(vglm_data$연령대, levels = c('미성년', '청년', '중년', '고령'))

table(vglm_data$와병률)
vglm_data$와병률 <- factor(vglm_data$와병률, levels = c('예', '아니오'))

table(vglm_data$결근결석)
vglm_data$결근결석 <- factor(vglm_data$결근결석, levels = c('학교/직장 안다님', '예', '아니오'))

table(vglm_data$시력문제)
vglm_data$시력문제 <- factor(vglm_data$시력문제,
                         levels = c('문제 없음', '조금 문제 있음', '많이 문제 있음', '전혀 보지 못함'))

vglm_data$청력문제 <- factor(vglm_data$청력문제,
                         levels = c('문제 없음', '조금 문제 있음', '많이 문제 있음', '전혀 듣지 못함'))

vglm_data$기억력 <- factor(vglm_data$기억력, levels = c('문제 없음', '문제 있음'))

vglm_data$의사결정 <- factor(vglm_data$의사결정,levels = c('문제 없음', '문제 있음'))

vglm_data$`질병/손상 등으로 활동제한` <- factor(vglm_data$`질병/손상 등으로 활동제한`,
                                     levels = c('문제 없음', '문제 있음'))

str(vglm_data)
pid.mlogit <- vglm(운동능력 ~., family  = multinomial(), data=vglm_data)
summary(pid.mlogit)

# install.packages("car")
# library(car)
# vif(vglm(운동능력 ~., family = multinomial(), data=vglm_data)) # 다중공선성 확인(범주형으로 안된다면 수치로 변환해서 해보기)

# log(odds)는 해석상의 어려움이 있기 때문에 다항 로지스틱 회귀모형의 양변에 지수함수를 취해 예측변수 한 단위 증가에 따른 odds의 변화 비율을 
exp(coef(pid.mlogit)) # e^(회귀계수) -> 오즈비 파악 => 

fitted(pid.mlogit)

#library(nnet)
#summary(multinom(운동능력 ~., data=vglm_data))
