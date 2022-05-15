# aggregate
# 참고: https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=coder1252&logNo=221294821930

# 1. 기본형태----
# 데이터의 특정 컬럼을 기준으로 통계량 계산

# dataframe
# aggregate(x, by, FUN)

# formula
# aggregate(formula, data, FUN)

head(mtcars) # 예제 데이터

# 2. 사용방법----
# 1) dataframe 적용: aggregate(data, by = '기준 컬럼', FUN)

#  cyl 컬럼을 기준으로 나머지 컬럼의 평균값 구하기
aggregate(mtcars, list(cylStandard = mtcars$cyl), mean)

# disp 컬럼이 120 이상 조건 추가
aggregate(mtcars, 
          list(cylStandard = mtcars$cyl,
               dispHigh = mtcars[, 'disp']>120), mean)

# 2) formula 적용: aggregate('함수를 적용하고자 하는 컬럼' ~ '기준이 되는 컬럼', data, FUN)

# cyl 컬럼을 기준으로 wt 컬럼의 평균 구하기
aggregate(wt ~ cyl, data = mtcars, mean)

# carb, gear 컬럼 두가지를 기준으로 wt 의 평균 구하기
aggregate(wt ~ carb + gear, data=mtcars, mean)

# gear 기준으로, disp, wt 평균 구하기
aggregate(cbind(disp, wt) ~ gear, data=mtcars, mean)

# carb, gear 기준으로 disp, wt의 평균값 구하기
aggregate(cbind(disp, wt) ~ carb + gear, data=mtcars, mean)

# 3) dot notaion
# 데이터의 모든 컬럼 선택 가능
head(sleep) # R 내장데이터 sleep 사용

# group을 기준으로 다른 모든 컬럼의 평균값 구하기
aggregate(. ~ group, data=sleep, mean)

# extra 제외한 다른 모든 컬럼을 기준으로 extra의 평균 구하기
aggregate(extra ~ ., data=sleep, mean)
