# [공빅데] 정규화, 표준화
# 참고자료1: https://www.youtube.com/watch?v=iYAiIfW4kTw
# 참고자료2: https://datadoctorblog.com/2021/01/23/R-Preprocessing-normalization/

# 스케일링(scaling)
# 분석대상 변수들의 분포특성이 서로 이질적일 때 특정변수의 영향력이 과대 또는 과소하게 사용되고 편의가 발생할 수 있으므로 스케일링 작업을 통해 변수의 단위와 범위를 비슷하게 만들어서 사용하는 방법. 연속형 변수를 대상으로 수행. 대표적으로 정규화와 표준화 과정이 있음.

# 정규화(normalization)란?----
# 최대값, 최소값을 사용해 원본 데이터의 최소값을 0, 최대값을 1로 만드는 방법. 분석에 같이 사용할 변수들의 단위나 범위가 다를 때 정규화를 통해 비슷한 크기와 분포를 갖도록 표준화 하는 방법. preprocess 함수에서는 레인징(ranging : 0~1로 만드는 방법)으로 사용됨.

datasets::trees
summary(trees)

# 정규화 하기
# 1. MinMax 함수(사용자 정의) 이용

# 함수 정의
minmax <- function(x) {
  return(((x)-min(x))/(max(x)-min(x)))
}

# apply 함수로 각 열에 대한 MinMax함수 적용
trees.minmax <- apply(trees, 2, minmax)

# 데이터 분포 확인
summary(trees.minmax)

# 2. preprocess 함수 사용(데이터 정규화 방법을 기반으로 한 함수)
install.packages('caret')
library(caret)

pre_range_trees <- preProcess(trees, method = 'range')

range_trees <- predict(pre_range_trees, trees)

summary(trees.minmax)

# 표준화(Standardization)란?----
# 특정변수의 분포를 평균이 0, 표준편차가 1인 표준정규분포로 변환하는 것으로 Z-Score변환이라고 함.

# 표준화하기
# 1. Scale 함수 이용: scale(데이터, center = T, scale = T)
std_trees <- scale(trees)
summary(std_trees)

# 2. 표준화 함수(사용자 정의) 이용
standard <- function(x) {
  return(((x)-mean(x))/sd(x))
}

trees.standard <- apply(trees, 2, standard)
summary(std_trees)

# 3. preprecess 함수 이용: preProcess(데이터, method = c('center', 'scale'))
library(caret)
pre_center_trees <- preProcess(trees, method = c('center', 'scale'))

center_trees <- predict(pre_center_trees, trees)

summary(center_trees)

# 번외(정규화와 표준화 비교하기)
hist(range_trees$Girth, freq = F, breaks = seq(0, 1, by= .1))
lines(density(range_trees$Girth))

hist(center_trees$Girth, freq = FALSE)
lines(density(center_trees$Girth))
