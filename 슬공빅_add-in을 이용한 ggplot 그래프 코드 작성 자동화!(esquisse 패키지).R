# Rstudio add-in을 이용한 ggplot 그래프 코드 작성 자동화! esquisse 패키지 사용법
# 1. esquisse 패키지 설치
# 2. Addins 클릭
# 3. 'ggplot2' builder 클릭
# 4. library(tidyverse) 실행

library(tidyverse)

mydata <- iris
head(iris)
unique(iris$Species)

ggplot(data=mydata, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col = Species))

library(ggplot2)

ggplot(mydata) +
 aes(x = Petal.Length, y = Petal.Width, size = Sepal.Length) +
 geom_point(shape = "circle", 
 colour = "#2E3577") +
 labs(x = "Length", y = "Width", title = "Visualization of the iris data set", 
 subtitle = "Petal distribution of iris by species", caption = "Figure 1. This shows the distribution of the petal length and width by species") +
 theme_bw() +
 theme(legend.position = "top") +
 facet_wrap(vars(Species))
