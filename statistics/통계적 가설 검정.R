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

# 기말고사 성적은 24점보다 유의하게 높은가?
# p-value = 0.1696이고, 유의수준 0.05보다 크므로 24점보다 높다고 할 수 없다.
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

# one-way anova test
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

# 분산분석 가정(정규성, 등분산성)----
# 반드시 충족해야하는 것은 아니지만 신뢰할 수 있는 분산분석 결과를 얻을 수 있음

library(car)
qqPlot(InsectSprays$count, id=F, pch=20, col='deepskyblue', # id=F: 이상치 표시 안 함
       xlab='Theoretical Quantiles', ylab='Empirical Quantiles',
       main='Q-Q Plot') # 점선 사이는 95% 신뢰구간 의미

shapiro.test(InsectSprays$count) # 정규성 충족 불가, 하지만 분산분석은 정규성 가정에 크게 제한받지 않음
# 관측값이 작고, 비정상적 관측값이 있으면 평균과 분산에 큰 영향을 줄 수 있으므로 제거하고 분산분석하는 것이 좋음

# 이상점 존재 여부 확인
outlierTest(spray.aov) # Bonferroni p(0.8499)이 0.05보다 크므로 이상점이 존재하지 않음

# 집단 간 분산의 동일성 여부(levene 검정 or bartlett 검정)
leveneTest(count ~ spray, data=InsectSprays) # 등분산 가정 미충족
bartlett.test(count ~ spray, data=InsectSprays) # 등분산 가정 미충족

# 등분산 가정 미충족시 분산분석----
# 등분산을 만족하는지에 따라 F값과 자유도가 달라짐
# 등분산 가정이 만족하면 보다 적극적으로 H0을 기각하는 검정 결과를 얻을 수 있음
oneway.test(count ~ spray, data = InsectSprays) # H0 기각 → 다중비교 불가
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

# one-way anova(공변량을 보정하지 않음)
# 결론: 아동기 성폭력 경험 여부에 따라 성인의 ptsd에 영향을 미친다.
sexab.aov <- aov(ptsd ~ csa, data=sexab)
summary(sexab.aov)

# ancova test
# 순수한 영향력을 파악하기 위해 'cpa(아동기 신체적 학대)'를 고려해야 함
# 결론1: cpa는 ptsd 관련 있음 
# 결론2: cpa를 통제한 후에도 csa에 따라 ptsd에 영향을 미침
sexab.aov <- aov(ptsd ~ cpa + csa, data=sexab) # cpa가 일정할 때 csa에 따른 ptsd 차이 분석
summary(sexab.aov)

# 공변량 영향 제거
# cpa 영향을 제거한 후에 조정된 ptsd 집단 평균 차이 파악
# install.packages('effects')
library(effects)
effect("csa", sexab.aov)
tapply(sexab$ptsd, sexab$csa, mean) # 비교 → 약간 차이 존재

# 공분산 결과 visualization
# 기울기 같은 이유: csa가 ptsd에 미치는 영향이 두 집단에서 일정하도록 공변량을 통제했기 때문
# 결과: 아동기 성폭력 경험 집단이 그렇지 않은 집단보다 더 큰 ptsd를 겪음
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

# one-way anova(공변량을 보정하지 않음)
# 결론: 직급에 따라 연봉에 차이가 있다.
Salaries.1aov <- aov(salary ~ rank, data=Salaries)
summary(Salaries.1aov)

# ancova test
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
# TukeyHSD() 함수나 multcomp 패키지의 glht() 함수 사용. 
# 두 방법은 다중비교 방식이나 유의확률(p-value)을 산출하는 방식이 다르기 때문에 결과가 상이할 수 있음. 
# glht() 함수를 이용한 사후분석은 TukeyHSD() 함수에 비해 덜 엄격한 가정을 필요로 하고 적용할 수 있는 모델의 범위가 더 넓음.

library(multcomp)
posthoc <- glht(model = Salaries.aov, linfct = mcp(rank='Tukey'))
summary(posthoc)

# 공분산 결과 visualization
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
ggplot(ds,aes(x=time,y=mean))+
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
