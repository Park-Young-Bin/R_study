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
cor.test(cats$Hwt, cats$Bwt, alternative = 'greater', # H1: 모집단에서 상관계수가 0보다 크다.
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
corrgram(state.x77)
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
pcor(c(1, 3, 2, 4), # 인덱스 번호, mpg(1), hp(3), 나머지 통제할 변수(cyl(2), wt(4))
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
