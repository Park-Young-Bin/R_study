library(ggplot2)

# x축은 displ, y축은 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y =hwy))

# 배경에 산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

# x축 범위 3~6, y축 범위 10~30으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)

library(dplyr)
df_mpg <- mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))
df_mpg

ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), mean_hwy)) + geom_col()
ggplot(data = mpg, aes(x = drv)) + geom_bar()

ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

