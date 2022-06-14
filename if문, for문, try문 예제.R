# if문----
grade <- 75
if (grade >= 75) {
  print('pass')
} else {
  print('non-pass')
}

vec1 <- c(10, 20, 30)
if (vec1 == 10) {
  print('인사부')
} else {
  print('총무부')
}

grade <- 'A'
if (grade == 'A') {
  print('pass') 
} else if (grade == 'B') {
  print('보류')
} else {
  print('non-pass')
}

vec1 <- c(10, 20, 30)
if (vec1 == 10) {
  print('인사부')
} else if (vec1 == 20) {
  print('재무부')
} else {
  print('총무부')
}

# for문----
for (i in 1:10) {
  print(i)
}

for (i in 1:10) {
  cat("2 *", i, "=", 2*i, "\n")
}

for (i in 1:20) {
  if (i %% 2 == 0) {
    print(i)
  }
}

v1 <- 101:200
for (i in 1:length(v1)) {
  if (v1[i] %% 2 == 0) {
    print(v1[i]*2)
  } else {
    print(v1[i]+2)
  }
}

sum <- 0
for(i in 1:100) {
  sum <- sum + i
}
print(sum)

while (i <= 10) {
  print(i)
  i <- i + 1
}

langs <- list('html', 'css', 'js')
for (x in langs) {
  if(x == 'css') {
    break # 지정 조건일 때 반복문 탈출(= 반복 중단)
  }
  print(x)
}

langs <- list('html', 'css', 'js')
for (x in langs) {
  if (x == 'css') {
    next # 지정 조건만 건너뛰고, 다음 조건부터 계속 반복
  }
  print(x)
}

nums <- 1:5
for (x in nums) {
  if (x == 5) {
    print(paste(x, "끝O"))
  } else {
    print(paste(x, "끝X"))
  }
}

langs <- list("HTML", "CSS", "JS")
levels <- list("초급", "중급", "고급")
for (x in langs) {
  for (y in levels) {
    print(paste(x, y))
  }
}

# tryCatch문----
# 예제1
# 참고: https://www.delftstack.com/ko/howto/r/trycatch-in-r/

f1 <- function(x) {
  cat("log(", x, ") = ", (log(x)))
}
f1(10)
f1('f') # error

f1 <- function(x) {
  tryCatch(
    error = function(cnd) "The custom output that we need to print", # 오류 문구 재정의
    cat("log(", x, ") = ", (log(x)))
  )
}
f1('f')

f1 <- function(x) {
  tryCatch(
    error = function(cnd) { # 중괄호: 오류 발생시 실행되는 코드 지정
      print("hello")
      log(10)
    },
    cat("log(", x, ") = ", (log(x)))
  )
}

f1('x')

# 예제2
# 참고: https://shlee1990.tistory.com/660
f <- function(x) {
  if (x >= 0) {
    return (x * x)
  } else {
    stop('error\n')
  }
}

g <- function(x) {
  y <- NULL
  y <- try(f(x), silent = F) # try에서 f함수 실행
  if (class(y) == 'try-error') {
    y <- NA # f함수가 실패한 경우, y에 -1을 대입
  }
  y
}
g(-10)
g(10)

f <- function(x) {
  if (x <= 0) {
    stop('error\n')
  } else if (x %% 2 == 0) {
    warning('warning\n')
  }
}

g <- function(x) {
  y <- 0
  
  tryCatch({
    # 에러나 경고가 발생하면 예외 처리를 하는 코드
    f(x)
    y <- 1
  },
  error = function(e) { # 해당 e에는 오류 메시지가 저장됨
    message("ERROR!")
    message(e)
    y <- 2
  },
  warning = function(e) { # 해당 e에는 경고문이 저장됨
    message('WARNING!')
    message(e)
    y <- 3
  },
  finally = { # 여기에 쓰여진 코드는 반드시 실행함
    message('finish')
    y <- 4
  },
  silent = T
  )
  return (y)
}
g(-10)
g(1)
g(2)
