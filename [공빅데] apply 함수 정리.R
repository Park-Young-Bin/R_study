# [공빅데] apply 함수 정리

# apply----
x <- matrix(1:20, 4, 5)
x

# 행 단위 연산: margin = 1
apply(X=x, MARGIN = 1, FUN = max)

# 열 단위 연산: margin = 2
apply(X=x, MARGIN = 2, FUN = max)

# 배열
y <- array(1:24, c(4,3,2))
y

apply(y, 1, paste, collapse = ",")

# 데이터: Titanic
Titanic

typeof(Titanic)
str(Titanic)

# 등급별 인원 수
apply(Titanic, 1, sum)

# 등급별 생존여부 수
apply(Titanic, c(1, 4), sum)

# lapply----
# 리스트
trees

typeof(trees)

lapply(trees, mean)
lapply(trees, class)

# sapply----
# sapply:lapply 함수에서 이용에 편리함을 부여한 함수,벡터 형태로 단순화해서 결과 출력
sapply(trees, mean, simplify = T)

# vapply----
vapply(trees, mean, numeric(1))

vapply(trees, mean) # 에러

vapply(trees, fivenum, c(Min. = 0, 'lst Qu.' = 0, Median = 0, '3rd Qu.' = 0, Max. = 0))
