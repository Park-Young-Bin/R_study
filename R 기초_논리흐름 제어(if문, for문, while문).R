# R 기초 - 논리흐름 제어 🔑 logical flow control | 조건문 if | 반복문 | 루프 loop | for loop
# 참고 영상: https://www.youtube.com/watch?v=wP4tFG0ToIY&list=PLY0OaF78qqGA42e0rXqBWCcF_C9Ib-T1x&index=36

# if문----
x <- pi
y <- 3
if (x > y) x # 3.141593
if (x < y) x # FALSE이므로 출력 결과 없음 

if (x < y) x else y # 3

# if else 연산은 벡터 연산 지원X----
x <- pi
y <- 1:5
if (x < y) x else y # 1 2 3 4 5 # pi와 y의 첫 번째 값인 1만 서로 비교했기에 조건은 FALSE이므로 y가 출력됨
if (x > y) x else y # 3.141593

# ifelse()로 벡터 연산 가능----
test <- c(T, F, T, T, F)
yes <- 1:5
no <- 0
ifelse(test, yes, no) # T이면 yes 실행, F이면 no 실행

ifelse(x > y, x, y)

# switch 함수를 이용한 선택지에 따른 함수 생성----
center <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = 0.1) # 절사 평균
  )
}
x <- c(2,3,5,7,11,13,17,19,23,29)
center(x, "mean")
center(x, "median")
center(x, "trimmed")

# switch 함수의 type 인수에 정의된 선택지가 아닌 다른 값에 대해 처리 방법 지정
center <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = 0.1), # 절사 평균
         "Choose one of mean, median, and trimmed"
  )
}
center(x, "other")

# repeat/break/next
# repeat: 반복
# break: 반복 멈춤
# next: 반복 멈추고 다음 단계로 이동
repeat print("hello") # 무한 반복

i <- 5
repeat {if (i > 25) break # i > 25 이면 break!
  else { # 조건이 F 일때 수행
    print(i)
    i <- i+5}
}

# while
i <- 5
while (i <= 25) { # TRUE 조건이 끝나면 while문 자동 종료
  print(i)
  i <-  i + 5
}

# for
# for (var in list) statement
for (i in seq(from=5, to = 25, by = 5)) print(i)

for (i in seq(from=5, to = 25, by = 5)) i # 결과 출력X

# for loop 내에서 생성된 객체는 함수와 달리 사라지지 않음
# for loop 내에서 객체의 값이 바뀌면 작업 공간에도 바뀐 값이 적용됨
i <- 1
for (i in seq(from=5, to = 25, by = 5)) i
i # 25
