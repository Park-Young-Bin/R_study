# 카이제곱 검정----
# 기대빈도와 관측빈도의 비교를 통해 계산되는 '카이제곱'을 가설검정을 위한 검정통계량으로 사용
# 관측빈도: 교차표 상의 실제 빈도
# 기대빈도: 변수 간 서로 관련성이 없을 때(H0이 사실이라는 가정 下) 기대할 수 있는 예상 빈도
# (관측빈도-기대빈도)의 차이가 작을수록 H0채택 확률 높음
# 카이제곱 값이 클수록 H0기각할 가능성 높음

# 1) pchisq()
pchisq(45.91, df=(3-1)*(2-1), lower.tail = F) # 1.073421e-10 → H0기각

# 2) qchisq()
qchisq(0.05, df=(3-1)*(2-1), lower.tail = F) # 카이스퀘어 값 = 5.991465 < 45.91 → H0기각

# 독립성 검정----
# 1) table 형식의 독립성 검정

str(Titanic)

Titanic.margin <- margin.table(Titanic, c(4, 1)) # c(생존여부, 승객구분)
Titanic.margin

addmargins(Titanic.margin)
addmargins(Titanic.margin, 2)
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

# 적합성 검정---
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
