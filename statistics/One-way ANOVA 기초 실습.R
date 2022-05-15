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