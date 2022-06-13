# if문

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

# for문
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

