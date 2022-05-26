# T-test(One Sample)----
# 참고: https://mansoostat.tistory.com/13?category=659368

# 1) 예시
# 고혈압 환자군에서 평균 수축기 혈압(SBP)이 150mmHg 이라고 하는데, 
# 연구를 통해 환자군의 모집단 평균 수축기 혈압도 동일할까?

# 자료 생성
subject <- c(1:30)
SBP <- c(sample(145:160, 30, replace=T))
dat <- data.frame(subject, SBP)
head(dat)

mean(dat$SBP) # 표본 평균

# One sample t-test
# p-value = 0.03943이고, 유의수준 0.05보다 작으므로 H0을 기각한다.
t.test(dat$SBP, mu = 150)

# 2) 예시
# 참고: https://blog.naver.com/PostView.naver?blogId=shoutjoy&logNo=221793939950&redirect=Dlog&widgetTypeCall=true&directAccess=false

# ex. 기말고사 성적은 24점보다 유의하게 높은가?
# 결과: p-value = 0.1696이고, 유의수준 0.05보다 크므로 24점보다 높다고 할 수 없다.
final <- c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(final, alternative = 'greater', mu=24)

# T-test(두 독립 표본 검정)----
# 참고: https://mansoostat.tistory.com/16

# 예시
# A라는 치료를 받은 그룹(g1, 30명)과 B라는 치료를 받은 그룹(g2, 30명)이 있고
# 두 그룹 간에 수축기 혈압(SBP)이 다른지 알아보고자 한다.

# 자료 생성
set.seed(1); SBP.g1 <- c(sample(140:160, 30, replace=T))
set.seed(2); SBP.g2 <- c(sample(140:160, 30, replace=T))
dat <- data.frame(SBP.g1, SBP.g2)
head(dat)

# 등분산 검정
# H0: 두 집단의 분산은 동일하다.  vs  H1: not H0
var.test(SBP.g1, SBP.g2) # p-value = 0.3548 > 0.05 → H0 채택

library(car) # Levene's test를 이용한 검정
leveneTest(dat$SBP.g1, dat$SBP.g2, location = 'mean')

# 정규성 검정
# 참고: https://kbkb456.tistory.com/93
shapiro.test(dat$SBP.g1) # p-value = 0.06094 → H0 채택
shapiro.test(dat$SBP.g2) # p-value = 0.3965 → H0 채택

# t-test
# t-검정 결과 p-value=0.5693로 0.05보다 크기 때문에 두 집단의 평균이 다르다고 할 수 없다.
t.test(dat$SBP.g1, dat$SBP.g2, alternative = 'two.sided', var.equal = TRUE)

# T-test(대응 표본 검정)----
# 참고: https://blog.naver.com/PostView.naver?blogId=shoutjoy&logNo=221793939950&redirect=Dlog&widgetTypeCall=true&directAccess=false

# 예시
# 학원을 다닌 후 기말고사 성적이 학원을 다니기 전 중간고사 성적보다 높은가?

# 자료 생성
mid = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
final = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)

mean(mid) # 23.1
mean(final) # 25.1

# Paired t-test
# p-value = 0.0007749이므로 H0기각
t.test(final, mid, alternative = 'greater', paired = T) # final이 더 큰 값인가?
t.test(mid, final, alternative = 'less', paired = T) # mid가 더 작은 값인가?

# Z-test(One Sample)----
# 참고: https://www.statology.org/z-test-in-r/

# 예시
# 특정 모집단의 IQ가 평균 μ = 100이고 표준 편차가 σ = 15인 정규 분포를 따른다.
# 연구자는 새로운 약물이 IQ 수준에 미치는 영향을 알고자 20명의 환자를 모집하여 한 달에 동안 사용하고 월말에 IQ 수준 기록
# 새로운 약물이 IQ 수준에 상당한 차이를 유발하는가?

# install.packages('BSDA')
library(BSDA)

# enter IQ levels for 20 patients
data = c(88, 92, 94, 94, 96, 97, 97, 97, 99, 99,
         105, 109, 109, 109, 110, 112, 112, 113, 114, 115)

# perform one sample z-test
z.test(data, mu = 100, sigma.x = 15)

# Z-test(Two Sample)----
# 참고: https://www.statology.org/z-test-in-r/

# 예시
# 서로 다른 두 도시에 있는 개인의 IQ 수준이 각각 인구 표준 편차가 15인 정규 분포를 따른다.
# 한 과학자는 도시 A와 도시 B의 개인 간의 평균 IQ 수준이 다른지 알고 싶어 각 도시에서 20명의 단순 무작위 표본을 선택하고 그들의 IQ 수준 기록
# 두 도시 간에 평균 IQ 수준이 다른가?

# enter IQ levels for 20 individuals from each city
cityA = c(82, 84, 85, 89, 91, 91, 92, 94, 99, 99,
          105, 109, 109, 109, 110, 112, 112, 113, 114, 114)

cityB = c(90, 91, 91, 91, 95, 95, 99, 99, 108, 109,
          109, 114, 115, 116, 117, 117, 128, 129, 130, 133)

# perform two sample z-test
z.test(cityA, cityB, mu=0, sigma.x = 15, sigma.y = 15)

# One-way ANOVA----

# 예시
# 총 6개의 살충제 효과에 차이가 있는지 검정하라
# H0: 살충제 효과에 차이가 없다.
# H1: not H0

str(InsectSprays) # count: 살아남은 회충 수, spray: 스프레이 종류
head(InsectSprays)

tapply(InsectSprays$count, InsectSprays$spray, length) # 관측값 개수
tapply(InsectSprays$count, InsectSprays$spray, mean) # 평균 → 살충제별로 효과 차이가 크다
tapply(InsectSprays$count, InsectSprays$spray, sd) # 표준편차

# install.packages('gplots')
library(gplots)
windows(width=12, height=8)
plotmeans(count ~ spray, data=InsectSprays) # 집단별 평균과 평균의 신뢰구간(95%) 표시
plotmeans(count ~ spray, data=InsectSprays,
          barcol = "tomato", barwidth = 3, # 신뢰구간 서식
          col='cornflowerblue', lwd=2, # 꺽은 선 서식
          xlab='Type of Spray', ylab='Inspect Count',
          main = 'Performance of Insect Sprays')

boxplot(count ~ spray, data=InsectSprays, col='tomato',
        xlab='Type of Spray', ylab='Inspect Count',
        main = 'Performance of Insect Sprays')

# one-way anova test(aov 함수는 항상 등분산을 가정함)
spray.aov <- aov(count ~ spray, data=InsectSprays)
summary(spray.aov) # H0기각

# 개별 집단간 평균 차이 확인
# 해석: 살충제 C를 선택하는 것이 반드시 좋은 결정은 아니다. 가격이 더 비쌀 수 있기 때문이다.
# 만일 C, D의 효과가 비슷하고 D 가격이 더 낮다면 D를 선택하는 것이 더 옳은 결정이다.
# 따라서 사후 검정을 통해 두 살충제에 통계적으로 유의한 차이가 있는지 확인한다.
model.tables(spray.aov, type="mean") # 전체 평균과 각 집단 평균
model.tables(spray.aov, type="effects") # 집단 별로 각 집단 평균과 전체 평균의 차이

# 사후 분석1(다중 비교)
# 결과: D-C, p = 0.4920707 → 두 살충제에 유의한 차이가 없다.
sprays.compare <- TukeyHSD(spray.aov)
sprays.compare # list type(diff, lwr, upr, p adj)
str(sprays.compare)
sprays.compare$spray['D-C',] # 원하는 부분만 출력
plot(sprays.compare) # 신뢰구간에 0 포함 → 두 집단 간에 차이 없음
plot(sprays.compare, col='blue', las=1) # y축 회전

# 사후 분석2(다중 비교)
library(multcomp)
tuk.hsd <- glht(model=spray.aov, linfct=mcp(spray='Tukey'))
cld(tuk.hsd, level=0.05) # 같은 문자를 공유하는 살충제는 서로 평균이 다르지 않음을 나타냄
plot(cld(tuk.hsd, level=0.05), col='orange', las=1) # 장점: 사후 분석1의 그래프보다 범주 개수가 많을 때 사용하면 좋고, 범주별로 종속변수의 분포 확인 가능

# 분산분석 가정(정규성, 등분산성)
# 반드시 충족해야하는 것은 아니지만 신뢰할 수 있는 분산분석 결과를 얻을 수 있음
library(car)
qqPlot(InsectSprays$count, id=F, pch=20, col='deepskyblue', # id=F: 이상치 표시 안 함
       xlab='Theoretical Quantiles', ylab='Empirical Quantiles',
       main='Q-Q Plot') # 점선 사이는 95% 신뢰구간 의미

shapiro.test(InsectSprays$count) # 정규성 충족 불가, 하지만 분산분석은 정규성 가정에 크게 제한받지 않음
# 관측값이 작고, 비정상적 관측값이 있으면 평균과 분산에 큰 영향을 줄 수 있으므로 제거하고 분산분석하는 것이 좋음

# 이상점 존재 여부 확인
outlierTest(spray.aov) # Bonferroni p-value = 0.8499(>0.05)이므로 이상점이 존재하지 않음

# 집단 간 분산의 동일성 여부(levene 검정 or bartlett 검정)
leveneTest(count ~ spray, data=InsectSprays) # 등분산 가정 미충족
bartlett.test(count ~ spray, data=InsectSprays) # 등분산 가정 미충족

# 등분산 가정 미충족시 분산분석(oneway.test 사용, oneway.test 함수는 다중 비교 불가능)
# 등분산을 만족하는지에 따라 F값과 자유도가 달라짐
# 등분산 가정이 만족하면 보다 적극적으로 H0을 기각하는 검정 결과를 얻을 수 있음
oneway.test(count ~ spray, data = InsectSprays) # H0 기각
summary(aov(count ~ spray, data = InsectSprays)) # 위의 결과와 동일

oneway.test(count ~ spray, data = InsectSprays, var.equal = T) # 등분산 가정 조건 삽입

# + 등분산이 아닐 경우 사후 검정----
oneway.test(count ~ spray, data=InsectSprays, var.equal=FALSE)

# 1) 모든 가능한 범주쌍 생성
library(dplyr)
pairs <- expand.grid(levels(InsectSprays$spray), levels(InsectSprays$spray),
                     stringsAsFactors=FALSE) %>% 
        filter(Var1 != Var2) %>%
        mutate(key=paste0(pmin(Var1, Var2), pmax(Var1, Var2), sep="")) %>%
        distinct(key, .keep_all=TRUE) %>%
        select(-key)
pairs

# 2) 각 범주쌍에 oneway.test() 함수 적용 + 각 비교쌍별 F값과 유의확률 추출
library(purrr)
oneway <- function(Var1, Var2) {
        oneway.test(count ~ spray, filter(InsectSprays, spray %in% c(Var1, Var2)),
                    var.equal=FALSE)[c("statistic", "p.value")]
}

pairs.compare <- pairs %>% cbind(., map2_dfr(.$Var1, .$Var2, oneway))
pairs.compare

# Two-way ANOVA----
# 데이터 소개: 기니피그 대상으로 비타민 C와 보충제 투여량이 이빨 성장에 미치는 영향력 기록
# 독립변수: supp(비타민 보충제), dose(보충제 투여량)
# 종속변수: len(이빨 길이)
str(ToothGrowth)

# dose(num -> factor)
ToothGrowth$dose <- factor(ToothGrowth$dose,
                           levels = c(.5, 1, 2),
                           labels = c('low', 'med', 'high'))
str(ToothGrowth$dose)

# 변수별 통계량 확인
# 투여량이 많을수록 이빨 성장에 큰 영향을 미침
# 투여량이 high인 경우 OJ < VC
with(ToothGrowth, tapply(len, list(supp, dose), length))
with(ToothGrowth, tapply(len, list(supp, dose), mean))
with(ToothGrowth, tapply(len, list(supp, dose), sd))

# two-way anova test
ToothGrowth.aov <- aov(len ~ supp * dose, data=ToothGrowth)
ToothGrowth.aov <- aov(len ~ supp + dose + supp:dose, data=ToothGrowth) # 위와 같은 코드

summary(ToothGrowth.aov)

model.tables(ToothGrowth.aov, type='means') # 전체 평균과 각 집단 평균

# Visualization
windows(width=12, height=8)
boxplot(len ~ supp * dose, data=ToothGrowth)
boxplot(len ~ supp * dose, data=ToothGrowth,
        col=c('deeppink', 'yellowgreen'), las = 1,
        xlab='Vitamin C Type', ylab='Tooth Growth',
        main='Effects of Vitamin C on Tooth Growth') # 서식 추가

interaction.plot(x.factor = ToothGrowth$dose, # 상호작용 그래프(두 변수 간에 상호작용 파악)
                 trace.factor = ToothGrowth$supp,
                 response = ToothGrowth$len,
                 trace.label = 'Supplement',# 범례 제목
                 las=1, type='b', pch=c(1, 19),
                 col=c('blue', 'red'),
                 xlab='Dose Level', ylab='Tooth Length',
                 main='Interaction Plot for Tooth Growth of Guinea Pigs') 

library(gplots)
interaction(ToothGrowth$supp, ToothGrowth$dose, sep=" ") # 독립변수 간 조합쌍 생성
plotmeans(len ~ interaction(supp, dose, sep=" "), data=ToothGrowth, col=c('red', 'green3'),
          xlab='Supplement and Dose Combination',
          ylab = 'Tooth Growth',
          main='Means Plot for Tooth Growth of Guinea Pigs')  # 집단별 평균과 평균의 신뢰구간(95%) 표

plotmeans(len ~ interaction(supp, dose, sep=" "), data=ToothGrowth, 
          connect = list(c(1,3,5), c(2,4,6)),
          col=c('red', 'green3'),
          mean.labels=T,
          xlab='Supplement and Dose Combination',
          ylab = 'Tooth Growth',
          main='Means Plot for Tooth Growth of Guinea Pigs') # Dose별로 값 연결

coplot(len ~ dose | supp, data=ToothGrowth,
       col='steelblue', pch=19) # 조건에 따른 그래프 출력(supp별 dose와 len 간의 관계)

coplot(len ~ dose | supp, data=ToothGrowth,
       col='steelblue', pch=19,
       panel = panel.smooth, lwd=2, col.smooth='darkorange',
       xlab='Doae Level', ylab='Tooth Growth') # 직선 추가
# 해석: supp별로 이빨 성장에 미치는 영향이 투여량이 증가함에 따라서 달라짐

# install.packages('HH')
library(HH)
# 주효과와 상호작용효과를 동시에 확인
# 대각선 상자도표: 주효과, 비대각선: 상호작용효과
interaction2wt(len ~ supp*dose, data = ToothGrowth)

# 사후 분석(다중 비교)
TukeyHSD(ToothGrowth.aov)
plot(TukeyHSD(ToothGrowth.aov), col="blue", las=1)
TukeyHSD(ToothGrowth.aov, which=c('dose'), conf.level=.99) # 원하는 부분만 출력

# ANCOVA(공분산분석)----
# 참고: https://www.youtube.com/watch?v=AWtFFSoaEAM&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=14
# 분산분석에 공변량을 추가하여 분산분석 모델 확장
# 공변량을 통제하여 독립변수의 순수한 영향 검정
# 공변량은 연속형 변수를 가정

# install.packages('faraway')
library(faraway)
str(sexab) # 아동기 성폭력 경험이 성인 정신 건강에 미치는 영향을 분석한 연구 결과 일부
# cpa(공변량): 아동기 신체적 학대
# ptsd(종속변수): 정신 건강
# csa(독립변수): 성폭력 피해 여부
# ?sexab

tapply(sexab$ptsd, sexab$csa, length)
tapply(sexab$ptsd, sexab$csa, mean)
tapply(sexab$ptsd, sexab$csa, sd)

# 평행성 검정(csa와 cpa의 상호작용 확인)
summary(aov(ptsd ~ csa * cpa, data=sexab)) # p-value = 0.39702 → 평행성 만족

# 정규성 검정
shapiro.test(sexab$ptsd) # p-value = 0.3758 → 정규성 만족

# 등분산성 검정
library(car)
leveneTest(ptsd ~ csa, data=sexab) # p-value = 0.6599 → 등분산성 만족
bartlett.test(ptsd ~ csa, data=sexab) # p-value = 0.8919 → 등분산성 만족

# 1) one-way anova(공변량을 보정하지 않음)
# 결론: 아동기 성폭력 경험 여부에 따라 성인의 ptsd에 영향을 미친다.
sexab.aov <- aov(ptsd ~ csa, data=sexab)
summary(sexab.aov)

# 2) ancova test
# 순수한 영향력을 파악하기 위해 'cpa(아동기 신체적 학대)'를 고려해야 함
# 결론1: cpa는 ptsd 관련 있음 
# 결론2: cpa를 통제한 후에도 csa에 따라 ptsd에 영향을 미침
sexab.aov <- aov(ptsd ~ cpa + csa, data=sexab) # cpa가 일정할 때 csa에 따른 ptsd 차이 분석
summary(sexab.aov)

# 공변량 영향 제거
# cpa 영향을 제거한 후에 조정된 ptsd 집단 평균 차이 파악
# install.packages('effects')
library(effects)
effect("csa", sexab.aov) # 11.544429(Abused),  5.271677(NotAbused)
tapply(sexab$ptsd, sexab$csa, mean) # 11.941093(Abused),  4.695874(NotAbused) → 약간 차이 존재

# 3) 공분산 결과 visualization
# 기울기 같은 이유: csa가 ptsd에 미치는 영향이 두 집단에서 일정하도록 공변량을 통제했기 때문
# 결과1: 아동기 신체적 학대 경험이 증가할수록 ptsd 증가
# 결과2: 아동기 성폭력 경험 집단이 그렇지 않은 집단보다 더 큰 ptsd를 겪음
# cf. 본 분석은 독립변수(csa)가 2개 레벨로 이루어져 있기에 사후분석 필요 없음
library(HH)
windiws(width=12, height=8)
ancova(ptsd ~ cpa + csa, data=sexab)

# 예제(사후검정 방법 포함)----
library(car)
head(Salaries)
?Salaries
str(Salaries)

# 예제: 재직기간(yrs.service)을 통제한 상태에서 직급(rank) 간 연봉의 차이를 검정

tapply(Salaries$salary, Salaries$rank, length)
tapply(Salaries$salary, Salaries$rank, mean)
tapply(Salaries$salary, Salaries$rank, sd)

# 가정1: 평행성
summary(aov(salary ~ yrs.service*rank, data=Salaries)) # p-value = 0.615 → 평행성 만족

# 가정2: 정규성
shapiro.test(Salaries$salary) # p-value = 6.076e-09 → 정규성 불만족

# 가정3: 등분산성
library(car)
leveneTest(salary ~ rank, data=Salaries) # p-value = 4.477e-16 → 등분산성 불만족
bartlett.test(salary ~ rank, data=Salaries) # p-value < 2.2e-16 → 등분산성 불만족

# 1) one-way anova(공변량을 보정하지 않음)
# 결론: 직급에 따라 연봉에 차이가 있다.
Salaries.1aov <- aov(salary ~ rank, data=Salaries)
summary(Salaries.1aov)

# 2) ancova test
# 결론1: 재직기간에 따라 salary에 차이가 있다.
# 결론2: 재직기간을 통제한 후에도 rank에 따라 salary에 영향을 미침
Salaries.aov <- aov(salary ~ yrs.service + rank, data=Salaries)
summary(Salaries.aov)

# 공변량 영향 제거
# cpa 영향을 제거한 후에 조정된 ptsd 집단 평균 차이 파악
# install.packages('effects')
library(effects)
effect("rank", Salaries.aov)
tapply(Salaries$salary, Salaries$rank, mean) # 비교 → 약간 차이 존재

# 사후 검정
# TukeyHSD() 함수 or multcomp 패키지의 glht() 함수 사용 
# 두 방법은 다중비교 방식이나 유의확률(p-value)을 산출하는 방식이 다르기 때문에 결과가 상이할 수 있음. 
# glht() 함수를 이용한 사후분석은 TukeyHSD() 함수에 비해 덜 엄격한 가정을 필요로 하고 적용할 수 있는 모델의 범위가 더 넓음.

library(multcomp)
posthoc <- glht(model = Salaries.aov, linfct = mcp(rank='Tukey'))
summary(posthoc)

# 3) 공분산 결과 visualization
# 기울기 같은 이유: csa가 ptsd에 미치는 영향이 두 집단에서 일정하도록 공변량을 통제했기 때문
# 결과: 아동기 성폭력 경험 집단이 그렇지 않은 집단보다 더 큰 ptsd를 겪음
# cf. 본 분석은 독립변수(csa)가 2개 레벨로 이루어져 있기에 사후분석 필요 없음
library(HH)
windiws(width=12, height=8)
ancova(salary ~ yrs.service + rank, data=Salaries)

# Repeated Measures ANOVA(반복측정 분산분석)----
# 참고: https://www.youtube.com/watch?v=3YsIUsSZ7kY&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=15
# 동일한 대상에 대해 여러 번 반복측정하여 반복측정 집단 간에 차이가 존재하는지 검정
# 동일 집단에 속한 대상들 간에 집단 내 차이 규명 

str(CO2)
# Plant: 나무 종류(Type별 3종류)
# Type(독립변수, 집단 간 요인): 지역(2개 범주)
# conc(독립변수, 집단 내 요인, 반복측정값): CO2 농도(7개 범주)
# uptake(종속변수): CO2 흡수율

CO2sub <- subset(CO2, Treatment=="chilled")
CO2sub$conc <- factor(CO2sub$conc)
str(CO2sub)

head(CO2sub) # conc(CO2 농도)별로 uptake가 반복 측정된 data

# 반복측정 일원분산분석: (종속변수) ~ (집단 내 요인) + Error(Subject/집단 내 요인)
# 반복측정 이원분산분석: (종속변수) ~ (집단 간 요인)*(집단 내 요인) + Error(Subject/집단 내 요인)
# 결과1: Type(나무의 출신 지역)에 따라서 uptake(CO2 흡수율)에 차이가 있음
# 결과2: conc(CO2 농도)에 따라서 uptake(CO2 흡수율)에 차이가 있음
# 결과3: Type(나무의 출신 지역)와 conc(CO2 농도) 간에 상호작용 효과 존재
CO2sub.aov <- aov(uptake ~ Type*conc + Error(Plant/conc), data=CO2sub) 
summary(CO2sub.aov)

# 결과1(주효과): Type에 따라 uptake에 차이가 있음
# 결과2(주효과): conc이 증가함에 따라 uptake가 증가함
# 결과3(상호작용효과): conc가 증가하면서 퀘백 나무가 미시시피보다 uptake 증가폭이 큼
boxplot(uptake ~ Type*conc, data=CO2sub,
        col=c('deepskyblue', 'green3'),
        las=2, cex.axis=.7,
        xlab='', ylab='Carbon dioxide uptake rate',
        main='Effects of Plant Type and CO2 on Carbon Dioxide Uptake')
legend('topleft', inset=.02,
       legend=c('Quebec', 'Mississippi'),
       fill=c('deepskyblue', 'green3'))

library(HH)
interaction2wt(uptake ~ Type*conc, data=CO2sub)

# 사후 검정
with(CO2sub, pairwise.t.test(uptake, conc, paired=T, p.adjust.method="bonferroni"))
with(CO2sub, pairwise.t.test(uptake, Type, paired=T, p.adjust.method="bonferroni"))

# 구형성 가정을 적용한 RM ANOVA----
# H0:  측정값 간 차이의 분산/공분산이 동일하다.
# install.packages('rstatix')
library(rstatix)
ToothGrowth$id <- rep(1:10, 6)
aov <- anova_test(data=ToothGrowth, dv=len, wid=id, between=supp, within=dose)

# $`Mauchly's Test for Sphericity`에서 p-value=0.908이므로 구형성 만족
aov

# 반복측정 일원분산분석 예제----
# 참고: https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=crow83&logNo=221503065741

# 예제: 총 6명의 피실험자가 있고, 어떤 약을 투여한다고 할 때, 투약전(pre), 3개월 후(after3m), 6개월 후(after6m)의 혈중농도를 비교한다. 경과 시간에 따른 혈중농도에 차이가 있을까?

id<-c(1,2,3,4,5,6) 
pre<-c(45,42,36,39,51,44) 
after3m<-c(50,42,41,35,55,49) 
after6m<-c(55,45,43,40,59,56) 
d<-data.frame(id, pre, after3m, after6m)
d

library(reshape2)
d.m<-melt(d,id.vars="id") 
colnames(d.m)<-c("id","time","value") 
d.m$id<-factor(d.m$id) 
d.m$time<-factor(d.m$time) 
head(d.m)

# 개인별 혈중농도변화 그래프
library(ggplot2)
ggplot(data = d.m, aes(x = time, y = value))+
        geom_line(aes(group=id, col=id))+
        geom_point(aes(col=id))

# 평균치 변화
library(dplyr)
ds <- d.m %>% group_by(time) %>% summarise(mean = mean(value), sd=sd(value))
ggplot(ds,aes(x=time, y=mean))+
        geom_point()+
        geom_line(group=1)

# rm anova test
summary(aov(value ~ time + Error(id/time), data=d.m))

# 사후 분석
with(d.m, pairwise.t.test(value, time, paired=T, p.adjust.method="bonferroni"))

# 반복측정 이원분산분석 예제----
# 참고: https://m.blog.naver.com/crow83/221503104750
# 독립변수: group, time(반복측정)

df <- read.csv('data/rmanova.csv')
head(df)
df$id <- 1:196
df$id <- factor(df$id)
df$group <- factor(df$group)
str(df)

m <- melt(data = df, id.vars = c('id', 'group'))
head(m)
colnames(m) <- c('id', 'group', 'time', 'value')
head(m)

# rm anova test
a <- aov(value ~ group * time + Error(id/time), data=m)
summary(a)

interaction.plot(x.factor = m$time, 
                 trace.factor = m$group, 
                 response = m$value,
                 type='b', pch=c(2,4,6), legend = F, col=c(3,4,6), las=1,
                 xlab='Time', ylab='Mean of Value')
legend('topleft', legend=c('Group1', 'Group2', 'Group3'),
       pch=c(2,4,6), col=c(3,4,6), bg='gray90')

# 사후 분석
with(m, pairwise.t.test(value, time, paired=T, p.adjust.method = 'bonferroni'))
# with(m, pairwise.t.test(value, group, paired=T, p.adjust.method = 'bonferroni'))

# MANOVA(다변량 분산분석)----
# 참고1: https://www.youtube.com/watch?v=ovNU9MzKTW0&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=16
# 참고2: https://m.blog.naver.com/bsj104/221716847881
# 두 개 이상의 종속변수가 있을 경우 집단별 차이를 동시에 검정, 연구 타당성 증대

# install.packages('heplots')
library(heplots)
str(Skulls) # 인간의 두개골 크기 측정 자료
head(Skulls)
# epoch(독립변수): 이집트 시대를 5 범주로 구분
# mb(종속변수): 두개골 폭
# bh(종속변수): 두개골 높이
# bl(종속변수): 두개골 길이
# nh(종속변수): 코 높이

library(dplyr)
sample_n(Skulls, 10)

# 예제: 시대에 따라 두개골 측정값이 다른지 검정하라.
# tip: 종속변수들을 결합하여 하나의 '행렬'로 변환해야 한다.

attach(Skulls) # 데이터를 R 검색 경로에 추가하여 변수명으로 바로 접근
search()

y <- cbind(mb, bh, bl, nh)
head(y)
aggregate(y, by=list(epoch), mean)

# install.packages('pastecs')
library(pastecs)
options(digits=3)
by(mb, epoch, stat.desc, basic=F) # 각 그룹별 기술통계량
by(bh, epoch, stat.desc, basic=F)
by(bl, epoch, stat.desc, basic=F)
by(nh, epoch, stat.desc, basic=F)

# 가정1: 분산공분산행렬의 동질성 검정
# 결과: 그룹마다 분산이 다르다.
by(y, epoch, cov)

# 가정2: 다변량 정규성
# install.packages('mvnormtest')
library(mvnormtest)
# 결과: 한 집단을 제외하고는 다변량정규성 불만족
mshapiro.test(t(Skulls[1:30, 2:5]))
mshapiro.test(t(Skulls[31:60, 2:5]))
mshapiro.test(t(Skulls[61:90, 2:5]))
mshapiro.test(t(Skulls[91:120, 2:5]))
mshapiro.test(t(Skulls[121:150, 2:5]))

# manova test
Skulls.manova <- manova(y ~ epoch)
summary(Skulls.manova) # H0 기각, 시대에 따라 두개골 측정값에 차이가 있다.

# 사후 분석(일변량 검정 통계량)
# 결과: nh를 제외한 두개골 측정값에 차이가 있다.
summary.aov(Skulls.manova)

detach(Skulls) # 데이터를 R 검색 경로에서 제거

# chi-square test----
# 독립성 검정: 두 범주형 변수 간의 관련성이 모집단에서 존재하는지 검정
# 적합성 검정: 범주별 빈도를 바탕으로 모집단에서 기대되는 비율 분포가 존재하는지 검정

# ex. 안전벨트 착용과 승객 안전 간의 관계 파악
survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol=2)
dimnames(survivors) <- list(Status=c('minor injury', 'major injury', 'dead'), # 행 이름
                            Seatbelts =c('With seatbelt', 'Without seatbelt')) # 열 이름
survivors
addmargins(survivors) # 행, 열 합계
addmargins(survivors, 2) # 열 합계
prop.table(addmargins(survivors, 2), 2) # 열의 합이 100%인 비율 교차표 생성

# 인사이트: 안전벨트 착용은 승객의 안전과 관계가 있다.
addmargins(prop.table(addmargins(survivors, 2), 2), 1) # 열의 비율이 1인 비율 교차표 생성

windows(width=7.0, height=5.5)
barplot(survivors, ylim=c(0, 2500), las =1,
        col=c('yellowgreen', 'lightsalmon', 'orangered'),
        ylab='Freguency', main='Frequency of Survivors') # 상대적인 비교 어렵
legend(0.2, 2500, rownames(survivors), 
       fill=c('yellowgreen', 'lightsalmon', 'orangered'))

survivors.prop <- prop.table(survivors, 2)
barplot(survivors.prop*100, las =1,
        col=c('yellowgreen', 'lightsalmon', 'orangered'),
        ylab='Percent', main='Percent of Survivors') # 퍼센트로 수정한 그래프

# 카이제곱 검정
# 기대빈도와 관측빈도의 비교를 통해 계산되는 '카이제곱'을 가설검정을 위한 검정통계량으로 사용
# 관측빈도: 교차표 상의 실제 빈도
# 기대빈도: 변수 간 서로 관련성이 없을 때(H0이 사실이라는 가정 下) 기대할 수 있는 예상 빈도
# (관측빈도-기대빈도)의 차이가 작을수록 H0채택 확률 높음, 카이제곱 값이 클수록 H0기각할 가능성 높음

# 1) pchisq()
pchisq(45.91, df=(3-1)*(2-1), lower.tail = F) # 1.073421e-10 → H0기각

# 2) qchisq()
qchisq(0.05, df=(3-1)*(2-1), lower.tail = F) # 카이스퀘어 값 = 5.991465 < 45.91 → H0기각

# 독립성 검정----
# 1) table 형식의 독립성 검정

str(Titanic)

Titanic.margin <- margin.table(Titanic, c(4, 1)) # c(생존여부, 승객구분)
Titanic.margin

addmargins(Titanic.margin) # 행, 열 합계
addmargins(Titanic.margin, 2) # 열 합계
prop.table(addmargins(Titanic.margin, 2), 2)
addmargins(prop.table(addmargins(Titanic.margin, 2), 2), 1)

chisq.test(Titanic.margin) # H0기각

# 두 변수 간 관련성 강도 파악
library(vcd)
assocstats(Titanic.margin) # 값이 클수록 두 변수 간 관련성이 크다는 것을 의미

# 시각화
# Pearson residuals = 관측빈도 - 기대빈도
# 파랑: 관측빈도 > 기대빈도
# 빨강: 관측빈도 < 기대빈도
# 파랑, 빨강에 가까운 셀들은 H0기각에 크게 기여함
mosaic(Titanic.margin, 
       shade=T) # H0을 기각하는데 있어서 어떤 범주쌍이 큰 기여를 하는지 알 수 있음

# 2) data frame 형식의 독립성 검정
library(MASS)
str(survey)
# Flod: 팔짱을 낄 때 어느 손이 위로 가는가
# 예제: 성별에 따라 팔짱 낄 때 손의 위치에 차이가 있는가?

chisq.test(survey$Fold, survey$Sex) # H0 기각 못함

crsstb <- table(survey$Fold, survey$Sex)
crsstb
chisq.test(crsstb) # H0 기각 못함

# 적합성 검정----
# 관측한 빈도를 토대로 모집단에서의 집단별 비율 분포 검정

# 예제: 세 이동통신사의 올해 시장점유율은 동일한가?
chisq.test(c(60, 55, 35)) # H0 기각

# 예제: 전문가에 따르면 a, b, c의 시장점유율은 45%, 30%, 25%를 따른다고 주장한다. 이 주장이 옳은가?
oc <- c(60, 55, 35)
null.p <- c(0.45, 0.30, 0.25) # 검정 비율을 별도로 지정
chisq.test(oc, p=null.p) # H0 기각 불가

# 예제: 작년 85명의 사용자 중에 a회사는 45명, b회사 24명, c회사 15명이다. 올해의 조사결과와 작년의 결과가 동일한가? 
chisq.test(oc, p=c(45, 25, 15)/85) # H0 기각, 작년과 올해의 결과에 차이가 있음

# table 형식의 적합성 검정
# 예제: 어느 생리학자는 미국 인구분포 상 검은 머리가 25%를 차지하고 갈색 50%, 빨강 10%, 금발 15%를 차지한다고 주장한다.
# 이 주장이 옳은가? 
str(HairEyeColor)
hairs <- margin.table(HairEyeColor, margin=1)
hairs

chisq.test(hairs, p=c(0.25, .5, .1, .15)) # H0 기각, 생리학자의 주장은 받아들이기 어렵

# data frame 형식의 적합성 검정
library(MASS)
str(survey)

smokers <- table(survey$Smoke)
smokers

# 비흡연자가 70%를 자치하고 나머지 흡연자는 각각 10%씩 비율을 차지한다고 알려져 있다. 옳은 가?
chisq.test(smokers, p=c(.1, .7, .1, .1)) # H0 기각, 알려진 것과 다름

# Correlation Analysis----
library(MASS)
str(cats)

# ex. 고양이 몸무게와 신장 무게의 관계
plot(cats$Hwt ~ cats$Bwt, 
     col='forestgreen', pch=19, 
     xlab='Body Weight (kg)', ylab = 'Heart Weight (g)',
     main = 'Body Weight and Heart Weight of Cats')

# 상관계수
# 두 변수 간의 선형관계의 강도 측정
# 선형변환에 의해 영향을 받지 않음

cor(cats$Hwt, cats$Bwt) # 0.8041274
with(cats, cor(Bwt, Hwt)) # 같은 결과

?cor
# use = "everything: 결측값이 포함되면 NA 출력
# use = "complete.obs": 하나라도 NA가 포함되면 행 자체 삭제 → 데이터 손실 우려
# use = "pairwise.complete.obs": 분석에 사용한 변수에 대해서만 결측값이 존재할 때 해당 케이스만 제외

# Pearson vs Spearman
# Pearson 상관계수: 정규성 가정 필요
# Spearman 상관계수: 정규성 가정을 충족하지 못하는 서열척도의 데이터를 바탕으로 계산됨, 순위 데이터를 바탕으로 계산되기에 이상점에 덜 민감
# Pearson 상관계수와 Spearman 상관계수가 많이 다르면 Pearson 상관계수에 큰 영향을 미치는 이상점이 데이터에 포함될 가능성 있음 → 이상치 제거 후 Pearson 상관계수 계산 수행

cor.test(cats$Hwt, cats$Bwt) # 두 변수의 유의성 검정에만 사용 가능
cor.test(cats$Hwt, cats$Bwt, alternative = 'greater', # H0: 모집단에서 상관계수가 0보다 작거나 같다.
         conf.level = 0.99)
cor.test(~ Bwt + Hwt, data=cats) # 동일한 결과

cor.test(~ Bwt + Hwt, data=cats, subset=(Sex=="F")) # 암컷 고양이 한정

# 상관계수 행렬 형태
str(iris)
cor(iris[-5])

iris.cor <- cor(iris[-5])
class(iris.cor)
iris.cor["Petal.Width", 'Petal.Length'] # "Petal.Width", 'Petal.Length' 간의 상관계수

# 2개 이상의 변수 간 상관분석(유의성 검정) 및 상관계수 파악
library(psych)
corr.test(iris[-5])
print(corr.test(iris[-5]), short=F)

str(state.x77)
cor(state.x77)

pairs.panels(state.x77) # 산점도, 상관관계, 히스토그램 동시 출력
pairs.panels(state.x77, pch=21, bg='red', hist.col='gold',
             main='Correlation Plot of US States Data')

# install.packages('corrgram')
library(corrgram)
# 왼/아 → 오/위: + 상관관계
# 왼/위 → 오/아: - 상관관계
# 채도가 짙을수록 높은 상관관계
corrgram(state.x77, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel = panel.txt, # pie 너비: 상관 정도
         order=T, main = 'Corrgram of US States Data')

cols <- colorRampPalette(c('red', 'pink', 'green', 'blue')) # 상관 크기: red < pink < green < blue
corrgram(state.x77, col.regions = cols, # pie chart 색 지정
         lower.panel=panel.pie,
         upper.panel=panel.conf, text.panel = panel.txt,
         order=F, main = 'Corrgram of US States Data')

# partial correlation analysis(편상관관계)----
# 편상관계수
# 두 변수 간의 순수한 상관관계
# 하나 이상의 다른 변수의 영향을 통제한 상태에서 관심의 대상인 두 변수 간의 선형적 관련성 측정
# 가짜 상관을 찾아내는 데 활용(연봉과 혈압 ~ 나이)
## 일반적으로 연봉과 혈압은 양의 상관성, 이유는 두 변수가 제3의 변수인 나이와 관련 있기 때문 → 나이 통제 필요 → 나이의 영향을 일정하게 유지
## 두 변수 간에 상관이 존재하듯 보이지만 두 변수가 진짜 상관을 갖은 제3의 변수와 상관을 갖기 때문에 발생하는 경우라면 두 변수는 가짜 상관이 발생할 수 있음
# 숨겨진 관계를 찾는 데 활용(구매필요성과 구매의향 ~ 소득)
## 변수 ㄱ, ㄴ이 상관을 갖지 않는다면, 제3의 변수 ㄷ이 ㄱ과 양의 상관, ㄴ은 ㄷ와 음의 상관을 갖고 있기 때문일 수 있음, 변수 ㄷ을 통해 양, 음의 상관관계가 상쇄되어 ㄱ, ㄴ 간에 상관이 드러나지 않았을 수 있음

str(mtcars)
mtcars2 <- mtcars[,  c('mpg', 'cyl', 'hp', 'wt')]
cor(mtcars2)

# 실린더 개수와 무게의 영향을 통제 후, 연비와 마력의 상관 파악
# 결과1: mpg와 hp 간의 높은 상관계수는 cyl와 wt의 영향을 받았을 가능성이 있다.
# 결과2: 순수한 영향력 = -0.2758932
# install.packages('ggm')
library(ggm)
pcor(c(1, 3, 2, 4), # 인덱스 번호, mpg(1), hp(3), 나머지 통제할 변수(cyl_2, wt_4)
     cov(mtcars2)) # 공분산 행렬
pcor(c('mpg', 'hp', 'cyl', 'wt'), cov(mtcars2)) # 동일한 결과

# 유의성 검정
# 결과1: p-value = 0.1400152 → H0 채택
# 결과2: cyl과 wt를 통제하면 mpg와 hp 간의 순수한 상관관계는 존재하지 않는다.
# 결과3:두 변수 간의 상관관계(-0.7761684)는 상당 부분 cyl와 wt로 일어났음을 알 수 있다.
pcor.test(pcor(c(1, 3, 2, 4), cov(mtcars2)),
               q=2, # 통제할 변수 개수(cyl, wt)
               n=nrow(mtcars)) # 관측값 개수(표본 크기)

# 편상관계수와 이에 대한 유의성 검정
# 각 변수쌍 간에 편상관계수를 산출할 때 나머지 변수는 통제 변수로 작용
# install.packages('ppcor')
library(ppcor)
pcor(mtcars2) # 모든 변수쌍 간의 편상관계수와 유의확률 추출

# 특정 두 변수만의 편상관계수와 유의확률 산출
pcor.test(mtcars2['mpg'], mtcars2['hp'], mtcars2[c('cyl', 'wt')])

# 단순 회귀 분석----
# 참고: https://www.youtube.com/watch?v=8opxpVeWmGY&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=22
# 절편: 독립변수의 값이 0이라는 것이 의미있는 숫자가 아닌 한, 절편이 갖는 의미는 제한적이다.
library(car)
str(Prestige)
head(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
class(Prestige.lm)
Prestige.lm

# 교육기간의 최소가 6이므로 독립변수의 값이 0일 때, 소득이 -2853.6이라는 것은 문제가 있음
plot(Prestige$income ~ Prestige$education,
     col='cornflowerblue', pch=19,
     xlab='Education (years)', ylab='Income (dollars)',
     main='Education and Income') # 산점도
abline(Prestige.lm, col = 'salmon', lwd=2) # 회귀식

summary(Prestige.lm)
# Residuals: 일반적으로 정규분포(평균=0)를 따름, 중위수가 음수이므로 오른쪽으로 꼬리가 긴 분포
# Residual standard error: 회귀선을 중심으로 상하로 변동하는 관측값의 표준 변동성, 작을수록 모델 적합도 좋음
# F-statistic: 회귀계수가 모두 0이라는 유의성 검정
# 단순회귀분석은 독립변수가 1개 이므로 '회귀계수의 유의성 검정'과 '회귀식의 유의성 검정'이 같다.

# 가정(선형성, 등분산성, 정규성, 이상치 파악)
# 참고1: https://wikidocs.net/34027
# 참고2: https://muzukphysics.tistory.com/entry/%ED%86%B5%EA%B3%84-%EB%B6%84%EC%84%9D-7-%ED%9A%8C%EA%B7%80%EB%AA%A8%EB%8D%B8-%EC%A0%81%ED%95%A9%EB%8F%84-%ED%8F%89%EA%B0%80-%EB%B0%A9%EB%B2%95-with-R-%EC%9E%94%EC%B0%A8-%EA%B2%B0%EC%A0%95%EA%B3%84%EC%88%98-F-T
par(mfrow = c(2,2))
plot(Prestige.lm)
par(mfrow = c(1,1))

# + 잔차 정규성 검정
res <-  residuals(Prestige.lm)
shapiro.test(res) # p-value = 1.281e-08, H0기각, 정규성 만족하지 않음

# + 잔차 독립성 검정
# install.packages('lmtest')
library(lmtest)
dwtest(Prestige.lm) # p-value = 0.00818, H0기각, 독립성 만족하지 않음

coef(summary(Prestige.lm)) # 회귀계수 유의성
anova(Prestige.lm) # 회귀식 유의성
coef(Prestige.lm) # 회귀계수
confint(Prestige.lm) # 회귀계수 신뢰구간
confint(Prestige.lm, level = .99)
fitted(Prestige.lm)[1:3] # 회귀식에 의한 예측값 산출
resid(Prestige.lm)[1:3] # 관측값과 예측값 간의 잔차 출력
Prestige$income[1:3] # fitted()의 결과와 비교, 이 둘의 차이는 resid()

# 예측하기
Prestige.new <- data.frame(education=c(5, 10, 15))
predict(Prestige.lm, newdata = Prestige.new) # 교육기간이 5년, 10년, 15년일 때 예측
predict(Prestige.lm, newdata = Prestige.new, interval = 'confidence') # 예측값에 대한 95% 신뢰구간

# 조건을 적용해 두 집단 간 회귀분석 결과 비교
# 결과1: 평균보다 많은 교육을 받은 집단은 교육 기간 1년 당 소득은 1455달러가 증가한다.
# 결과2: 두 집단에 따라 1년 교육기간이 소득에 미치는 영향이 크다.
mean(Prestige$education)

lm(income ~ education, data=Prestige,
   subset=(education > mean(Prestige$education))) # 교육기간이 평균보다 큰 집단에 대해서만 회귀분석

lm(income ~ education, data=Prestige,
   subset=(education <= mean(Prestige$education)))

# 다항회귀분석----
# 참고: https://www.youtube.com/watch?v=53N4NQ1bgCA&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=23
# 한 개의 연속형 독립변수를 이용해 한 개의 연속형 종속변수 예측
# 단순회귀분석과 달리 선형관계는 독립변수의 n차 다항식으로 모델링
# 관측값을 통과하는 추세선을 그렸을 때, <n-1>개의 굴절이 관찰되면 일반적으로 n차 다항식으로 모델링
# 일반적으로 3차항을 초과하는 다항회귀모델은 흔하지 않음

# ex. 교육기간과 소득 간 관계 파악

library(car)
str(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
summary(Prestige.lm)

# 교육기간에 따라 소득 증가 폭이 다르다. → 다른 회귀선 고려
plot(Prestige$income ~ Prestige$education,
     col='cornflowerblue', pch=19,
     xlab='Education (years)', ylab='Income (dollars)',
     main='Education and Income') # 산점도
abline(Prestige.lm, col = 'salmon', lwd=2) # 회귀식

# 조건을 적용해 두 집단 간 회귀분석 결과 비교
# 결과1: 평균보다 많은 교육을 받은 집단은 교육 기간 1년 당 소득은 1455달러 증가한다.
# 결과2: 평균보다 적은 교육을 받은 집단은 교육 기간 1년 당 소득이 281.8달러 증가한다.
# 결과3: 두 집단에 있어서 회귀선의 기울기가 다른 것은 단일 직선의 회귀선보다 한 개의 굴절은 갖는 곡선이 보다 모형을 잘 설명할 수 있을 것이라 유추
lm(income ~ education, data=Prestige,
   subset=(education > mean(Prestige$education))) # 교육기간이 평균보다 긴 집단에 대해서만 회귀분석

lm(income ~ education, data=Prestige,
   subset=(education <= mean(Prestige$education))) # 교육기간이 평균보다 짧은 집단에 대해서만 회귀분석

# 개별 데이터에 충실한 곡선의 loess 추세선이 현재 데이터셋을 보다 잘 설명함
scatterplot(income ~ education, data=Prestige,
            pch=19, col='orangered', cex=1.2,
            regLine=list(method=lm, # lm: 선형 회귀선 생성
                         lty=2, lwd=3, col='royalblue'), # regLine: 산점도 사이를 지나갈 직선
            smooth=list(smoother=loessLine, # loessLine: 데이터 구간별로 가장 적합한 추세선 적용
                        spread=F, # F: loess 추세선 양쪽에 나타나는 변동성을 보여주는 선 제외
                        lty.smooth=1, lwd.smooth=3, col.smooth='green3'), # smooth: 곡선으로 표현할 추세선
            xlab='Education (years)', ylab='Income (dollars)',
            main='Education and Income')

# 다항 vs 단순 비교
# 결과1: Residual standard error, R-squared 측면에서 다항회귀모델이 단순회귀모델보다 우수함
# 결과2: 다항회귀모델에서 이차항(education^2)이 단일항보다 유의한데, 이는 두 독립변수 간에 강한 상관간계가 이유이다. 
# + 독립변수 간에 상관이 크다면 종속변수와 강한 선형 관계를 갖는 독립변수로 인해 다른 독립변수가 통계적 유의성이 드러나지 않을 수 있음 → 다중공선성
# + 이차항은 원래 변수를 기준으로 계산되에 다항회귀분석은 다중공선성이 발생할 가능성이 높음. 
# + 하지만 회귀식 자체가 통계적으로 유의하기 때문에 예측이 큰 목적이라면 큰 문제 없음
Prestige.ploy <- lm(income ~ education + I(education^2), data=Prestige) # 다항
summary(Prestige.ploy)

Prestige.lm <- lm(income ~ education, data=Prestige) # 단순
summary(Prestige.lm)

plot(Prestige$income ~ Prestige$education,
     col='darkorange', pch=19,
     xlab='Education (years)', ylab='Income (dollars)',
     main='Education and Income') # 산점도
library(dplyr)
lines(arrange(data.frame(Prestige$education, fitted(Prestige.ploy)), # data.frame(x축, y축)
      Prestige$education), col='cornflowerblue', lwd=2) # education 기준으로 오름차순 정렬

# ex2. 분출 대기 시간과 분출 지속시간 간의 관계
str(faithful)
scatterplot(eruptions ~ waiting, data=faithful,
            pch=19, col='deepskyblue', cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col='blueviolet'),
            smooth=list(smoother=loessLine, spread=F,
                        lty.smooth=1, lwd.smooth=3, col.smooth='coral'),
            xlab='Waiting (minutes)', ylab='Eruptions (minutes)',
            main='Waiting Time Between and the Duration of the Eruptions') # 2굴절 → 3차 다항식

faithful.ploy <- lm(eruptions ~ waiting + I(waiting^2) + I(waiting^3), 
                    data=faithful) # 다항
summary(faithful.ploy)

# 단순회귀모델과 비교
# 결과1: Residual standard error, R-squared 측면에서 다항회귀모델이 단순회귀모델보다 우수함
# 결과2: 3차항을 포함한 모델이 두 변수 간의 관계를 설명하는데 있어서 높은 적합도를 보임
faithful.lm <- lm(eruptions ~ waiting, data=faithful) # 단순
summary(faithful.lm)

# 다중회귀분석----
str(mtcars)
mtcars <- mtcars[c('mpg','hp', 'wt', 'disp', 'drat')]
head(mtcars)

summary(mtcars)
cor(mtcars)

library(car)
scatterplotMatrix(mtcars, pch=19, col='royalblue', cex=1.2,
                  regLing=list(method=lm, lty=1, lwd=3, col='salmon'), # regLine: 산점도 사이를 지나갈 직선
                  smooth=list(smoother=loessLine, spread=F,  # loessLine: 데이터 구간별로 가장 적합한 추세선 적용
                              lty.smooth=1, lwd.smooth=3, col.smooth='forestgreen'),
                  main='Car Performance')

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)

# 표준화계수 구하기(1)
# 결과: 가장 크게 연비에 미치는 변수는 'wt', 가장 작은 영향력은 'disp'
mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data=mtcars)
summary(mtcars.lm)

# 표준화계수 구하기(2)
# devtools::install_github("cran/QuantPsyc")
library(QuantPsyc)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
lm.beta(mtcars.lm)

# 회귀분석 가정----
# 참고: https://www.youtube.com/watch?v=sbnq3xTC1Uk&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=25
# 선형성: 종속변수와 독립변수 간의 관계는 선형이다.
# 정규성: 독립변수값에 대해 대응되는 종속변수 값들의 분포는 정규분포이다.
# 등분산성: 독립변수값에 대해 대응되는 종속변수 값들의 분포는 모두 동일한 분산을 갖는다.
# 독립성: 모든 관측값은 서로 독립이다. 하나의 관측값은 다른 관측값에 영향을 주지 않는다.

str(mtcars)
mtcars.lm <- lm(mpg ~ hp + wt + disp ~ drat, data = mtcars)
plot(mtcars.lm)
# 선형성: 어떠한 패턴이 보이므로 선형성 만족 부족
# 독립성: 일부 점들이 대각선을 벗어나므로 독립성 만족 부족
# 등분산성: 수평 추세선이 관측되므로 등분산성 만족
# 이상점/영향력: 

library(car)
vif(mtcars.lm) # 10 초과하는 값 없음

# 회귀모델 진단 결과로 회귀분석의 가정이 심각하게 위배되었을 경우, 회귀모델 수정
# 관측값 제거, 변수 변환, 변수 추가/제거를 통해 회귀모델 수정
# 이상점/영향점 → 관측값 제거
# 선형성, 정규성, 등분산성 가정 미충족 → 변수 변환
## 선형성의 가정을 위배하면 독립변수를 변환
## 정규성/등분산의 가정을 위배하면 종속변수를 변환
# 다중공선성 → 변수 제거(주목적이 예측을 위한 것이면 큰 신경X, 개별 예측 변수에 대한 통계적 해석이 목적이면 해결해야 함)

# mpg의 정규화를 위한 종속변수의 람다 추정
# powerTransform(): x^(람다)를 최대한 정규분포에 가깝도록 만드는 람다 추정
powerTransform(mtcars$mpg) # mpg^(0.02956537)으로 정규화 가능 → 0에 근접한 값이므로 로그 변환(ln)

# lambda=1 가설 검정: x^(1)을 의미하며 변환하지 않음 의미
# 변환의 필요성 검정
summary(powerTransform(mtcars$mpg)) # p-value = 0.07307 이므로 H0채택, 람다 변환 필요 없음

# 선형성을 계산하기 위한 독립변수의 람다 추정
# 결과1: 두 독립변수의 p-value 모두 0.05 이하이므로 변환 가능
# 결과2: hp:(-0.568), wt(-0.417) → 모두 -0.5에 근접하기에 hp^(-0.5)과 wt^(-0.5)으로 변환 가능
boxTidwell(mpg ~ hp + wt, data=mtcars) 

# 등분산성을 계산하기 위한 종속변수의 람다 추정
# 결과: 람다가 0.5853955으로 추정되었으므로 mpg^(0.5)으로 대체하여 회귀모델의 등분산성 만족 가능
spreadLevelPlot(lm(mpg ~ hp + wt, data=mtcars)) # 잔차와 예측값 간의 관계의 그래프와 등분산성을 계산하기 위한 람다 산출

# 회귀모델 선택(예측정확도, 간명도)----
# 참고: https://www.youtube.com/watch?v=sm9hBlfP0nA&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=26
str(mtcars)

# 중첩모델 생성
# 독립변수 관점에서 한 모델이 다른 모델의 완전한 부분집합인 형태의 모델
mtcars.lm1 <- lm(mpg ~ hp + wt, data=mtcars)
mtcars.lm2 <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)

# 중첩된 모델 간 적합도 비교
# 추가로 투입된 disp와 drat 변수가 기존의 hp와 wt 변수 이상의 추가적인 설명력을 제공하는지 검정
# H0: 두 개의 변수로 인해 증가하는 R^2값이 0이다.
# 결과1: p-value=0.4178이므로 추가 변수들(disp, drat)은 회귀모델 예측력 향상에 기여하지 못함
# 결과2: hp와 wt만으로 생성된 간명한 회귀모델을 사용하는 것이 바람직함
anova(mtcars.lm1, mtcars.lm2)

# AIC를 이용한 모델 간 비교
# 모델의 예측 정확도와 간명도를 함께 고려, AIC 값이 작을수록 우수한 모델
# 결과: mtcars.lm1의 AIC 값이 더 작으므로 mtcars.lm2보다 우수한 모델
AIC(mtcars.lm1, mtcars.lm2) 

# 변수 선택
# 전진선택법: 상수항만을 모델부터 시작해 단계별로 한 번에 한 개씩 독립변수를 모델에 포함
# 후진선택법: 모든 독립변수가 포함된 모델부터 시작하여 단계별로 한 번에 한 개씩 독립변수를 모델에 제거
# 단계선택법: 새로 진입되거나 제거되는 변수가 더 이상 존재하지 않은 때까지 진입/제거 과정 반복

?step
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
step(mtcars.lm, direction = 'backward')

# 가능한 모든 모델을 탐색하고 각 모델의 적합도 평가
# 가능한 모든 모델이 탐색되고 평가되지 않기 때문에 위의 변수 선택 방법은 최적 회귀모델을 보장하지 않음
# 결과: adjR^2 관점에서 가장 적합한 모델은 hp, wt, drat가 포함된 모델(맨위 그래프)이다.
library(leaps)
mtcars.regsubsets <- regsubsets(x=mpg ~ hp + wt + disp + drat, data=mtcars,
           nbest=4) # 독립변수의 각 subset 크기별로 탐색할 모델 개수 선정

library(RColorBrewer)
?RColorBrewer
plot(mtcars.regsubsets, scale='adjr2',
     col=brewer.pal(9, 'Pastel1'),
     main='All Subsets Regression')

str(summary(mtcars.regsubsets)) # 여러 적합도 지표 확인

# 추정된 모든 회귀모델의 adjR^2 추출
summary(mtcars.regsubsets)$adjr2
which.max(summary(mtcars.regsubsets)$adjr2) # 가장 큰 adjR^2의 회귀모델 순서 출력, 9번째
coef(mtcars.regsubsets, 9) # 가장 적합한 모델(9번째 회귀모델)의 변수의 회귀계수 추출

# 더미변수 회귀분석----
# 기준범주: 더미변수의 값이 모두 0인 범주
str(InsectSprays)
levels(InsectSprays$spray)

tapply(InsectSprays$count, InsectSprays$spray, mean)

# 결론 A와 C, D, E는 통계적으로 유의한 차이가 있음
sprays.lm <- lm(count ~ spray, data = InsectSprays)
summary(sprays.lm)

contrasts(InsectSprays$spray) # 더미변수 코딩 구조 확인

# 살충제 간에 효과 차이 검정(분산분석, 사후 검정)
sprays.aov <- aov(count ~ spray, data=InsectSprays)
summary(sprays.aov) # H0 기각
TukeyHSD(sprays.aov)

# 기준범주 변경
respray <- relevel(InsectSprays$spray, re=6) # 살충제 F를 기준 범주로 저장
contrasts(respray)

sprays.lm <- lm(count ~ respray, data = InsectSprays)
summary(sprays.lm)

# 매개효과분석----
# 참고: https://www.youtube.com/watch?v=2PamBrMhJ1Y&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=28
str(mtcars)

# 1단계: 종속 ~ 독립
model.total <- lm(mpg ~ disp, data=mtcars)
summary(model.total)

# 2단계: 매개 ~ 독립
model.M <- lm(wt ~ disp, data=mtcars)
summary(model.M)

# 3단계: 종속 ~ 독립(통제) + 매개
# 결과1: mpg와 disp간에 직접 효과는 모집단에서 존재하지 않음
# 결과2: mpg와 disp간에 관계는 통제적으로 유의했지만(1단계에서 총효과 존재), 3단계에서는 wt 투입으로 인해  mpg와 disp간에 직접 효과가 사라짐 
# 결과3: 따라서 wt는 disp와 mpg 간의 관계를 완전매개한다고 볼 수 있음
model.Y <- lm(mpg ~ disp + wt, data=mtcars)
summary(model.Y)

# disp와 mpg 간의 간접효과 계산
# 간접효과 = 독립변수와 매개변수의 회귀계수(2단계)와 매개변수와 종속변수의 회귀계수(3단계)의 곱
0.007 * (-3.351)

# 간접효과의 통계적 유의성 검정1
# 소벨 검정: 매개변수가 존재할 때 독립변수가 종속변수에 미치는 영향이 통계적으로 유의하게 감소하는지 검정 
# install.packages('multilevel')
library(multilevel)
model.sobel <- sobel(pred=mtcars$disp, med=mtcars$wt, out=mtcars$mpg)
model.sobel

pnorm(abs(model.sobel$z.value), lower.tail=F) # 오른쪽 꼬리부분 면적
pnorm(abs(model.sobel$z.value), lower.tail=F)*2 # 왼쪽 꼬리부분 면적 + 오른쪽 꼬리부분 면적, 0.05보다 작기에 매개효과가 존재함

# 간접효과의 통계적 유의성 검정2
# install.packages('bda')
library(bda)
mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg) # p-value=0.00548이므로 disp과 mpg간의 관계는 wt에 의해서 유의하게 매개됨

# bootsprapping에 의한 매개효과 분석
# 소벨 검정보다 선호되는 분석 방법, 경험적 분포를 통해 간접효과의 통계적 유의성 검정
# install.packages('mediation')
library(mediation)
model.M <- lm(wt ~ disp, data=mtcars) # 매개변수 모델
model.Y <- lm(mpg ~ disp + wt, data=mtcars) # 종속변수 모델
set.seed(123)
model.mediation <- mediate(model.m=model.M,
                           model.y=model.Y,
                           treat='disp',
                           mediator='wt',
                           boot=T, sims=500) # sims: 추출할 표본의 수(default=1000)

# Total Effect: 총효과(독립변수가 종속변수에 미치는 영향력)
# ADE: 직접효과(종속변수에 대한 독립변수의 영향력)
# ACME: 간접효과(매개효과, 총효과-직접효과)
# 결과1: 간접효과의 p-value=0.004이므로 매개효과는 존재한다.
# 결과2: |직접효과계수|가 |총효과계수|보다 작은 것으로 보아 무게는 배기량과 연비 간에 관계를 부분매개한다고 볼 수 있다.
# if, 유의수준 0.01 이하의 조건을 적용하면 직접효과(ADE)는 사라지므로 무게는 배기량과 연비 간에 관계를 완전매개한다고 볼 수 있다. 
summary(model.mediation)

plot(model.mediation, cex=1.2, col='royalblue', lwd=2,
     main='Mediation Effect Analysis')

# 조절효과분석----
# 참고: https://www.youtube.com/watch?v=XBt0IS8a6Is
str(mtcars)

# 결과: mpg와 hp 간의 변화는 wt에 따라 달라진다.
mtcars.lm <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(mtcars.lm)

round(mean(mtcars$wt),1)
round(sd(mtcars$wt), 1)

# 상호작용효과 시각화1
# 결과: wt가 증가함에 따라 hp과 mpg 간 관계가 점차 약해진다.(기울기 감소)
library(effects)
m <- round(mean(mtcars$wt),1)
s <- round(sd(mtcars$wt), 1)
m;s

plot(effect(term="hp:wt", mod=mtcars.lm, 
            xlevels=list(wt=c(m-s, m, m+s))),
     lines=list(multiline=T, lwd=2,
                lty=c(3, 2, 1), 
                col=c('royalblue', 'violet', 'maroon')),
     main='Interaction Plot for Horsepower and Weight')

# 상호작용효과 시각화2
# install.packages('rockchalk')
library(rockchalk)
plotSlopes(model=mtcars.lm, plotx='hp', modx='wt',
           modxVals = 'std.dev.', col=rainbow(3), # modxVals: 일정하게 유지할 조절변수 값 지정
           main='Interaction Plot for Horsepower and Weight')

# 조절매개효과모델----
# 참고: https://www.youtube.com/watch?v=wQm6n18Nmxs&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=30
# 매개 변수에 의해 매개된 두 변수(독립변수와 종속변수) 간 직접적 또는 간접적 영향관계에 제4의 변수(조절변수)가 영향을 미치는지 검정
# 조절효과는 매개효과모델의 모든 경로에서 발생 가능

str(mtcars)
model.M <- lm(wt ~ disp * am, data=mtcars) # 매개변수모델
# model.M <- lm(wt ~ disp + am + disp:am, data=mtcars)
model.Y <- lm(mpg ~ disp * am + wt * am, data=mtcars) # 종속변수모델
# model.Y <- lm(mpg ~ disp + am + disp:am + wt + wt:am, data=mtcars)

# 조절변수에 따른 독립변수, 매개변수, 종속변수 간의 영향관계 파악
# 결과1: 수동변속기가 자동변속기보다 간접효과가 더 크고, 통계적으로 유의하다.
# 결과2: 하지만 변속기의 간접효과의 차이는 통계적으로 검증된 것은 아니다.
library(mediation)
set.seed(12)
model.med1 <- mediate(model.m=model.M, # 매개변수모델
                     model.y=model.Y, # 종속변수모델
                     covariates=list(am=0), # 조절변수의 수준 지정(0, 자동 변속기)
                     treat = 'disp', # 독립변수
                     mediator='wt', # 매개변수
                     boot=T, sims=500)
summary(model.med1)

set.seed(12)
model.med2 <- mediate(model.m=model.M, # 매개변수모델
                      model.y=model.Y, # 종속변수모델
                      covariates=list(am=1), # 조절변수의 수준 지정(1, 수동 변속기)
                      treat = 'disp', # 독립변수
                      mediator='wt', # 매개변수
                      boot=T, sims=500)
summary(model.med2)

# 상호작용항이 포함된 매개변수모델과 종속변수모델을 이용해 매개효과분석 수행
set.seed(12)
model.med <- mediate(model.m = model.M,
                     model.y = model.Y,
                     treat = 'disp', mediator = 'wt',
                     boot = T, sims=500)

# 매개효과모델에서의 조절 효과 검증
# 결과1: 간접효과의 차이 검정 결과, disp가 wt를 경유해서 mpg에 미치는 간접효과의 차이 검정: p-value = 0.044
# disp가 wt를 매개로 mpg에 미치는 영향은 am 유형에 따라 차이가 있다.
# 결과2: 직접효과의 차이 검정 결과, disp가 mpg에 미치는 직접효과의 차이 검정: p-value = 0.98
# am 유형에 따른 직접효과의 차이가 없다.
set.seed(12)
test.modmed(object = model.med, 
           covariates.1 = list(am=0), covariates.2 = list(am=1),
           sims=500)

# 일반선형모델----
# 선형회귀분석에서 결과변수는 연속형 변수이면서 정규분포를 따라야함.
# 하지만 대부분의 문제에서는 결과변수가 범주형일 수 있고, 횟수일 수도 있기에, 이러한 변수는 정규분포를 따르지 않음
# 선형회귀모델을 확장하여 정규분포를 따르지 않는 결과변수를 수용할 수 있는 회귀모델 생성 필요
# ex. 로지스틱회귀모델, 포아송회귀모델, ...  

# 이항로지스틱 회귀분석(binomial logistic regression analysis)----
# 참고: https://www.youtube.com/watch?v=nyU96C2-LCI&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=33
# 종속변수가 이분형 범주를 가질 때 독립변수로부터 결과변수의 범주를 예측
# 특정  사건이 발생할 확률을 직접 추정
## 종속변수의 예측값은 항상 0과 1 사이의 확률
## 이 값이 기준값(예를 들면 0.5)보다 크면 사건이 발생하고 기준값보다 작으면 사건이 발생하지 않는 것으로 예측
# install.packages('modeldata')
library(modeldata)
data(mlc_churn)
str(mlc_churn)

churn <- mlc_churn
churn <- churn[-c(1, 3)]
churn$churn <- factor(ifelse(churn$churn=='no', 1, 2), # 고객 미이탈 = 1, 고객 이탈 = 2
                      level=c(1, 2),
                      labels=c('no', 'yes')) # 사건 발생 = "고객 이탈"
str(churn)
dim(churn)

# train, test data split
churn.train <- churn[1:3333, ]
churn.test <- churn[3334:5000, ]

# train, test data의 종속변수(churn) 비율 확인
table(churn.train$churn) # no: 2850, yes: 483
prop.table(table(churn.train$churn)) # no: 0.8550855, yes:0.1449145

table(churn.test$churn) # no: 1443, yes: 224
prop.table(table(churn.test$churn)) # no: 0.8656269, yes:0.1343731

# binomial logistic regression analysis
# 고객 미이탈(1)은 사건 미발생으로 0으로 변환되고, 고객 이탈(2)은 사건 발생으로서 1로 변환된다.
# 따라서 본 분석의 오즈는 고객 이탈일 확률이 미이탈 확률의 몇배인지 나타낸다.
# 로지스틱 회귀모델에서 독립변수 회귀계수는 다른 변수의 변화가 없다는 가정 하에서 해당 변수가 한 단위 만큼 변화할 때의 로그 오즈의 변화량을 나타냄
# 회귀계수(+): 독립변수 증가 → 종속변수(로그 오즈) 증가, 회귀계수(-): 독립변수 증가 → 종속변수(로그 오즈) 감소
# 로지스틱 회귀모델에 지수함수를 취하여 종속변수를 오즈로 변환 → 한 단위의 증가에 따른 오즈비 확인 가능
churn.logit <- glm(churn ~ ., data = churn.train, family = binomial(link='logit'))
summary(churn.logit)

# exp(회귀계수) 해석
# 해석1: international_plan은 yes일 때, no일 때보다 고객 이탈률이 미이탈률보다 7.7배 증가(770% 증가)
# 해석2: voice_mail_plan은 yes일 때, no일 때보다 고객 이탈률이 미이탈률보다 0.13배 증가(87.8% 감소)
# 해석3: total_day_charge가 한 단계 증가하면 고객 이탈률이 미이탈률보다 4.53배 증가
# 해석3_1: 두 단계 증가하면 고객 이탈률이 미이탈률보다 4.53^2(=20.521)배 증가
exp(coef(churn.logit))

# 통계적 유의성 검정
# deviance(이탈도): 모델의 비적합도 정도, 작을수록 좋음
# Null deviance: 상수항만 포함된 예측 모델
# Residual deviance: 예측변수가 모두 포함된 (현재 구축한)모델
# 상수항만 포함된 모델보다 예측변수가 포함된 모델의 이탈도가 낮은 것 당연, 작아지는 정도가 통계적으로 의미있는지 중요함
# Null 모델에 비해 개선되는 이탈도가 통계적으로 의미있는 차이인지 검정 필요
# 자유도 차이만큼 Residual deviance가 개선됐는지 검정 → 카이제곱 검정
pchisq(q=2758.3-2158.7, df = 3332-3315, lower.tail = F) # p-value = 1.731898e-116 → 이탈도 차이는 통계적으로 유의함
pchisq(q=churn.logit$null.deviance - churn.logit$deviance, 
       df=churn.logit$df.null - churn.logit$df.residual,
       lower.tail = F) # 위와 동일, p-value = 1.757917e-116 → 이탈도 차이는 통계적으로 유의함

churn.logit.pred <- predict(churn.logit, newdata=churn.test, 
                            type='response') # 사건 발생 확률 출력
head(churn.logit.pred)

churn.logit.pred <- factor(churn.logit.pred > 0.5, 
                           levels=c(FALSE, TRUE),
                           labels=c('no', 'yes')) # 0.5 이상이면 고객 이탈
head(churn.logit.pred)
table(churn.logit.pred) # 고객 이탈: 72명, 고객 미이탈: 1595명

# 혼동행렬
table(churn.test$churn, churn.logit.pred,
      dnn=c('Actual', 'Predicted'))
mean(churn.test$churn == churn.logit.pred) # 정확도: 0.8740252

# 단계별 로지스틱 회귀분석
churn.logit2 <- step(churn.logit)
summary(churn.logit2)

# 관심있는 특정 독립변수가 사건 발생 확률에 미치는 영향 파악
# 독립변수의 변화하는 수준에 따라 사건 발생 확률의 변화 확인
# ex. 고객의 서비스 센터 전화 횟수가 고객 이탈 확률에 미치는 영향에 관심 있다.
## "다른 예측변수들을 일정하게 고정"하고 고객의 서비스 전화 횟수를 바꿔가면서 고객 이탈 확률 변화를 계산
table(churn.test$number_customer_service_calls) # 서비스 센터 전회 횟수 분포 → 0 ~ 7회
testdata <- data.frame(number_customer_service_calls=c(0:7),
                       international_plan="no", # 범주형: 가장 낮은 범주 유형값
                       voice_mail_plan="no",
                       number_vmail_messages=mean(churn.test$number_vmail_messages), # 연속형: 평균값
                       total_day_charge=mean(churn.test$total_day_charge),
                       total_eve_minutes=mean(churn.test$total_eve_minutes),
                       total_night_charge=mean(churn.test$total_night_charge),
                       total_intl_calls=mean(churn.test$total_intl_calls),
                       total_intl_charge=mean(churn.test$total_intl_charge))
head(testdata)
testdata$prob <- predict(churn.logit2, newdata = testdata, type = 'response')
testdata[c("number_customer_service_calls", 'prob')] # 전화 횟수 증가 → 이탈률 증가

# 과산포 문제 검정
# 종속변수의 실제 분산이 이항분포에서 기대되는 분산보다 더 클 때 발생
# 검정의 표준오차를 왜곡시켜 회귀계수의 유의성 검정을 부정확하게 만드는 위험 존재
# 이탈도와 자유도 간의 비율 확인, 1을 크게 상회하면 과산포 의심
deviance(churn.logit2)/df.residual(churn.logit2) # 0.6505038, 과산포 없음

# 통계 검정
# H0: 과산포 비율이 1이다.
fit.origin <- glm(churn ~ international_plan + 
                    voice_mail_plan + 
                    number_vmail_messages + 
                    total_day_charge + 
                    total_eve_minutes + 
                    total_night_charge + 
                    total_intl_calls + 
                    total_intl_charge +
                    number_customer_service_calls,
                  family = binomial(),
                  data=churn.train)

fit.overids <- glm(churn ~ international_plan + 
                    voice_mail_plan + 
                    number_vmail_messages + 
                    total_day_charge + 
                    total_eve_minutes + 
                    total_night_charge + 
                    total_intl_calls + 
                    total_intl_charge +
                    number_customer_service_calls,
                  family = quasibinomial(),
                  data=churn.train)

pchisq(summary(fit.overids)$dispersion * fit.origin$df.residual,
       fit.origin$df.residual, lower.tail = F) # 0.08385493 → 과산포 가능성 적음

# 다항 로지스틱 회귀분석----
# 참고: https://www.youtube.com/watch?v=4ibxb0OpgWA
# 독립변수로부터 세 개 이상의 사건(범주)을 갖는 종속변수의 사건발생확률 예측
# 사건발생확률은 새로운 케이스의 소속범주를 예측하는 데 활용
# g개의 범주가 있을 때 총(g-1)개의 다항 로지스틱 회귀모델이 생성되며, 각 범주당 하나씩 기준 범주에 대한 로그오즈(로짓)를 설명하는 회귀모델로 정의

library(EffectStars)
data(PID) # 미국의 유권자 944명의 정치 성향 데이터
str(PID)
levels(PID$PID)

library(VGAM)
pid.mlogit <- vglm(PID ~ ., family = multinomial(), data = PID)

# 결과1: 종속변수의 범주가 3개이므로 총 2개 모델 생성
# 결과2: 마지막 범주('Republican')가 기준 범주로 자동 지정
# 결과3: Education 변수는 더미변수로 자동 변환(low가 기준변수로서 0으로, hight는 1로 코딩)
summary(pid.mlogit)

# exp(회귀계수) 해석
# 해석1: TVnews(1)오즈비: 1.045 / 뉴스 시청일이 하루 늘어나면, Democrate일 가능성이 Republican일 가능성에 비해 4.5% 증가(1.045배 증가)
# 해석2: Education(1)오즈비: 0.749 / 교육 수준 한 단위 증가(low → high)는 Democrate일 가능성을 Republican일 가능성 대비 25.1% 감소(0.749배 증가)
# 해석3: 하지만 TVnews(1)과 Educationhigh(1)은 통계적으로 유의하지 않음
# 해석4: Income(1) 오즈비: 0.9835 / 소득 한 단위를 증가시키면 Democrate일 가능성이 Republican일 가능성에 비해 1.6% 감소, 소득 변화는 Democrate일 가능성과 Republican일 가능성에 유의한 영향을 미침
exp(coef(pid.mlogit))

# 각 범주별 예측 확률 계산(케이스 별로 세 범주 중 분류될 확률 추정)
pid.mlogit.pred <- fitted(pid.mlogit)
head(pid.mlogit.pred)

# 독립변수 수준에 따른 사건발생 확률 변화 
# ex1. 교육수준이 정치 성향에 미치는 영향
# 결과: Education 증가로 Democrat일 확률 감소, Republican일 확률이 증가하지만 Education은 통계적 유의X
testdata <- data.frame(Education=c('low', 'high'), 
                       TVnews=mean(PID$TVnews), # 평균값 고정
                       Income=mean(PID$Income),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population)) # 가상의 데이터 생성
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type='response')
cbind(testdata, pid.mlogit.pred)

# ex2. 소득이 정치 성향에 미치는 영향
# 결과: 저소득층은 Democrat, 고소득층은 Republican로 예측될 확률이 높음, 소득과 정치성향은 밀첩한 관계가 있음
testdata <- data.frame(Education=rep('low', 5), # 가장 낮음 범주로 고정
                       TVnews=mean(PID$TVnews), # 평균값 고정
                       Income=seq(20, 100, 20),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population)) # 가상의 데이터 생성
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type='response')
cbind(testdata, pid.mlogit.pred)

# 다른 예제
library(MASS)
str(fgl) # 214개의 유리 성분 데이터
levels(fgl$type)
head(fgl)

fgl.scaled <- cbind(scale(fgl[, 1:9]), fgl[10]) # 변수 스케일링
head(fgl.scaled)

# train, test data set
set.seed(123)
train <- sample(nrow(fgl), 0.7*nrow(fgl))
fgl.train <- fgl.scaled[train,]
fgl.test <- fgl.scaled[-train,]

sum(table(fgl.train$type)) # 149
table(fgl.train$type)
sum(table(fgl.test$type)) # 65
table(fgl.test$type)

# 모델 생성
library(nnet)
fgl.mlogit <- multinom(type ~., data=fgl.train) # 기준 범주=첫 번째 범주(WinNF)
summary(fgl.mlogit)

# 회귀계수 유의성 검정
z <- summary(fgl.mlogit)$coefficients / summary(fgl.mlogit)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
print(p, digits=3)

# exp(회귀계수)
exp(coef(fgl.mlogit))

# 새로운 데이터에 대한 예측 확률 추정
fgl.mlogit.pred <- predict(fgl.mlogit, newdata=fgl.test, type='probs')
head(fgl.mlogit.pred)
cbind(round(fgl.mlogit.pred, 3), fgl.test['type'])

# 테스트 데이터에 대한 예측 정확도
max.col(fgl.mlogit.pred) # 가장 높은 예측 확률을 갖는 열 인덱스 반환
fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)] # 열 인덱스에 대응되는 열 이름 선택
head(fgl.mlogit.pred)

# 혼동 행렬
table(fgl.test$type, 
      fgl.mlogit.pred, 
      dnn=c('Actual', 'Predicted'))
table(fgl.test$type, 
      factor(fgl.mlogit.pred, 
             levels=c(fgl.test$type),
             labels=c(fgl.test$type)),
             dnn=c('Actual', 'Predicted')) # 위치 수정
mean(fgl.test$type == fgl.mlogit.pred) # 예측 정확도: 0.6307692

# 교차검증
fgl.mlogit.cv <- numeric()
for (i in 1:100) {
  train <- sample(nrow(fgl), 0.7*nrow(fgl))
  fgl.train <- fgl.scaled[train,]
  fgl.test <- fgl.scaled[-train,]
  fgl.mlogit <- multinom(type ~., data=fgl.train)
  fgl.mlogit.pred <- predict(fgl.mlogit,
                             newdata=fgl.test,
                             type='probs')
  fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
  fgl.mlogit.cv[i] <- mean(fgl.test$type == fgl.mlogit.pred)
}

fgl.mlogit.cv
summary(fgl.mlogit.cv) # mean = 0.6077
boxplot(fgl.mlogit.cv, horizontal=T,
        col='tomato', xlab='Accuracy', main='Accuracy for Forensic Glass (100 samples)')
