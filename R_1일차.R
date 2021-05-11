var1<-c(1,2,5,7,8) # 숫자 5개로 구성된 var1 생성성
var1
var2<-c(1:5) # 1~5까지 연속 값으로 var2 생성
var2
var3<-seq(1,5) # 1~5까지 연속 값으로 var3 생성
var3
var4<-seq(1, 10, by = 2) # 1~10까지 2 간격 연속 값으로 var4 생성
var4
var5<-seq(1, 10, by = 3) # 1~10까지 3 간격 연속 값으로 var5 생성
var5
var1+2
var1 + var2

str5<-c("Hello!", "World", "is", "good!")
str5

x<-c(1,2,3)
x
mean(x)
max(x)
min(x)
paste(str5, collapse = ",") # 쉼표를 구분자로 str5의 단어들 하나로 합치기기
paste(str5, collapse = " ")

x_mean<-mean(x)
x_mean

str5_paste<-paste(str5, collapse = " ")
str5_paste
x<-c("a","a","b","c")
x
qplot(x) # 빈도 막대 그래프 출력

# data에 mpg, x축에 hwy 변수 지정해 그래프 생성
qplot(data = mpg, x = hwy )

# x축 cty
qplot(data = mpg, x = cty )

# x축 drv, y축 hwy
qplot(data = mpg, x = drv, y = hwy )

# x축 drv, y축 hwy, 선 그래프 형태
qplot(data = mpg, x = drv, y = hwy, geom = "line")

# x축 drv, y축 hwy, 상자 그림 형태
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot" )

# x축 drv, y축 hwy, 상자 그림 형태, drv별 색 표현
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)

score<-c(80, 60, 70, 50, 90)
mean(score)
mean_score<-mean(score)
