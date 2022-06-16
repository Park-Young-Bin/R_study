# R 기초 - 함수 function
# 예제1----
transLength <- function(x) {
  tlength <- round(x*.9144, digits=1)
  result <- paste(tlength, "m", sep= "")
  return(result) # 반드시 필요한 요소 아님
}
ls() # 작업 공간 확인

y <- c(100, 150, 200)
transLength(y)

# 예제2----
trans2 <- transLength # 괄호를 붙이지 않음
# trans2 <- transLength() # error 발생
trans2
trans2(y)

# 예제3----
# return 생략
transLength <- function(x) {
  tlength <- round(x*.9144, digits=1)
  result <- paste(tlength, "m", sep= "")
}
transLength(y) # 함수 코드의 마지막이 값을 할당하는 코드로 마무리되었기에 값이 반환되지 않음
print(transLength(y)) # print 함수를 통해 결과값 반환

# 변수 할당 생략
transLength <- function(x) {
  tlength <- round(x*.9144, digits=1)
  paste(tlength, "m", sep= "")
}
transLength(y) # 예제1과 동일한 결과

# 예제4----
# 코드를 끝까지 실행하지 않고 중간에 함수를 벗어나고 싶은 경우
transLength <- function(x) {
  if (!is.numeric(x)) return("Not a Number") # 숫자가 아닌 인자가 들어온 경우
  tlength <- round(x*.9144, digits=1)
  paste(tlength, "m", sep= "")
}
transLength("ABC")

# 예제5----
# 중괄호 무조건 필요하지 않음
# 하지만 코드가 길어지면 가독성 떨어짐
f1 <- function(x, y) {x + y}
f2 <- function(x, y) x + y
f1(1, 3) # 4
f2(1, 3) # 4

transLength <- function(x) paste(round(x*.9144, digits=1), "m", sep= "")
transLength(y)

# 예제6----
# 여러 인자 입력 가능
transLength <- function(x, mult, unit) { # ft 단위의 숫자로 변경
  tlength <- round(x*mult, digits=1)
  paste(tlength, unit, sep= "")
}
transLength(y, mult=3, unit='ft') 
transLength(y, mult=36, unit='in')
transLength(y) # 2개 인수 누락으로 인한 error 발생

# 예제7----
# 인수에 초기값을 지정하여 위의 error 피함
transLength <- function(x, mult=0.9144, unit="m") { # mult, unit에 초기값 지정
  tlength <- round(x*mult, digits=1)
  paste(tlength, unit, sep= "")
}
transLength(y) # error 발생X
transLength(y, mult=3, unit='ft') # 다른 값을 지정하면 이를 반영한 값 출력
transLength(y, 3, 'ft') # 순서대로 입력하면 인자의 이름을 적지 않아도 됨

# 예제8----
# 하지만 함수 내에 다른 함수가 사용될 때 내부 함수의 인수를 모두 지정하면 함수는 너무 많은 인수 리스트를 갖게 됨
# 이때 내부 함수에 사용할 인수를 총칭하는 '...' 이용
transLength <- function(x, mult=0.9144, unit="m", ...) { 
  tlength <- round(x*mult, ...)
  paste(tlength, unit, sep= "")
}
transLength(y, digits=2) # digits값은 ...에 들어감
transLength(y) # round 함수의 digits 기본값은 0이기에 반올림된 정수가 반환됨

transLength <- function(x, mult=0.9144, unit="m", digits=1) { # digits 초기값 지정
  tlength <- round(x*mult, digits = digits)
  paste(tlength, unit, sep= "")
}
transLength(y)

# 예제9----
# 함수 자체를 인수로 취급(function에 'FUN' 인수 추가)
# FUN 인수에도 초기값 지정 가능
transLength <- function(x, mult=0.9144, unit="m", FUN=round, ...) { 
  tlength <- FUN(x*mult, ...)
  paste(tlength, unit, sep= "")
}
transLength(y, FUN=signif, digits = 3) # 지정한 유효숫자만큼 자리수를 만들어 반올림
transLength(y, FUN=floor) # 내림
transLength(y) # 초기값(round)

# local 환경 vs global 환경----
# 함수 내에서 생성된 객체는 함수 실행이 종료된 후에 작업공간에서 사라짐
# 함수를 실행하면 함수는 현재 작업 환경 내에서 일시적으로 로컬 환경을 생성해서 그 안에서 작업하고 함수가 종료되면 로컬 환경은 그 안의 모든 객체와 소멸되기 때문이다.
ls() # f1, f2, trans2, transLength, y

x <- 11:15
scopetest <- function(x) {
  cat('This is x: ', x, '\n')
  rm(x) # x를 작업 공간에서 제거
  cat('This is x after removing x', x, '\n') # x가 존재하기 않기에 error 발생
}
scopetest(x=15:11) 
# 첫 번째 결과: 함수의 인수로 주어진 local 환경에서 생성된 객체 x를 이용함
# 두 번째 결과: rm(x)로 인해 local 환경의 x는 사라졌기에 global 환경에서 생성된 객체 x를 이용함
