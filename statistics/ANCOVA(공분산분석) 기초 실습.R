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
effect("csa", sexab.aov)
tapply(sexab$ptsd, sexab$csa, mean) # 비교 → 약간 차이 존재

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