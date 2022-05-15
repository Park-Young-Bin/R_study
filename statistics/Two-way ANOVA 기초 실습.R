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