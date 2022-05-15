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