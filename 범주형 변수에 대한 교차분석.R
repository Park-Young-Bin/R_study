# 범주형 변수에 대한 교차분석(카이제곱 검정 및 피셔 )
# 참고문헌: https://kilhwan.github.io/bizstat-book/ch-categoricalDescStat.html

# devtools::install_github("kilhwan/bizstatp")
library(bizstatp)
head(course)

# 절대 빈도표 만들기(xtabs)
xtabs(~ year, data=course) # 열 이름이 함께 출력됨

# 조건을 적용한 절대 빈도표 만들기
xtabs(~ year, data=course, subset = gender == 'M') # 남학생의 학년별 절대 빈도표

xtabs(~ year, data=course, subset = class == 2) # 2분반의 학년별 절대 빈도표

# 파이차트 그리기
freqYear <- xtabs(~ year, data = course); freqYear

pie(freqYear, main = '학년별 수강생 비율')

yearLabels <- paste0(names(freqYear), "학년: ", round(proportions(freqYear) * 100, digits=2),"%") # 라이블 생성

pie(freqYear, label = yearLabels, main = "학년별 수강생 비율")

# 2차원 교차표 생성

# (1) talbe 
# 첫 번째 행렬: 1분반의 성별-학년 교차표, 두 번째 행렬: 2분반의 성별-학년 교차표
table(course$gender, course$year, course$class) 

# (2) xtabs
freqGenderYearClass <- xtabs(~ gender + year + class, data=course); freqGenderYearClass

# 범주형 변수의 분석 사례
## 새로운 치료법은 아무 치료도 하지 않은 것보다 효과가 있는가?
## 새로운 치료법 효과는 성별로 차이가 있는가?
Arthritis
summary(Arthritis)

tti <- xtabs(~ Treatment + Improved, data = Arthritis)
tti

round(prop.table(tti, margin = 1)*100, 2)

chisq.test(tti) # 치료법에 따라 증상 개선 효과에 차이가 있다.

ggplot(Arthritis, aes(x = Treatment, fill = Improved)) + 
  geom_bar(position = "dodge")

ggplot(Arthritis, aes(x = Treatment, fill = Improved)) + 
  geom_bar(position = "fill")

tsi_placebo <- xtabs(~ Sex + Improved, data = Arthritis, subset = Treatment == 'Placebo')
tsi_placebo

proportions(tsi_placebo, margin = 1) 

# 기대빈도가 5보다 작은 셀이 20%이상인 경우, 피셔의 정확 검정을 수행한다.
# 통계적으로 유의미한 차이라고 할 수 없다.
fisher.test(tsi_placebo) 

# 이혼에 대한 사회조사 데이터 분석
head(BrokenMarriage)

# 성별 이혼 빈도 교차표
gbtbs <- xtabs(~ gender + broken, data = BrokenMarriage)
gbtbs

# 이미 요약된 빈도로 요약된 데이터를 가지고 xtabs() 함수로 빈도표나 교차표를 만드려면 
# 다음처럼 수식의 좌변에 이미 요약된 빈도를 가진 열을 기술해야 한다.
tgb <- xtabs(Freq ~ gender + broken, data = BrokenMarriage)
chisq.test(tgb) 

# 카이제곱 검정과 피셔 검정 동시 진행(warning 발생 시, fisher 결과 해석)

# install.packages('gmodels')
# install.packages('rcompanion')
library(gmodels)
library(rcompanion)

print(fit.fis <- CrossTable(as.matrix(gbtbs), expected = T, chisq = T, fisher = T))

# 통계적으로 유의한 경우에만 아래 코드를 이용해서 사후검정 실시
# p.adj.Fisher or p.adj.Chisq 값이 0.05보다 작으면 통계적으로 유의한 차이가 있다.
print(fit.fisph <- pairwiseNominalIndependence(as.matrix(gbtbs)))
