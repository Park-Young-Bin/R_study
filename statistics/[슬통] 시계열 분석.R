# 시계열 분석
# 참고자료: https://timeseries-playbook.netlify.app/intro.html
# 참고영상: https://www.youtube.com/watch?v=1Y-xW5OU-Xo&t=98s

# 1.2 fpp3 패키지

# install.packages("fpp3")
# install.packages('janitor')
library(tibble)
library(fpp3)

# tibble()
# tsibble() # time series용 tibble

# 1.3 tsibble 객체
y <- tsibble(
  year = 2015:2019,
  observation = c(123, 39, 78, 52, 110),
  index = year
)
y

olympic_running # tsibble 객체

olympic_running$Length %>% unique()
olympic_running$Sex %>% unique()

# 1.4 csv 파일로부터 tsibble 객체 만들기 연습

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
head(prison)

prison %<>% 
  janitor::clean_names() %>% 
  mutate(quarter = yearquarter(date),
         .keep = "unused") %>% 
  as_tsibble(key = state:indigenous,
             index = quarter)
prison

# 1.5 Time plots
#시계열에서 가장 기본적인 그래프, 시간에 따라서 보고 싶은 반응 변수값 출력
prison %>% 
  filter(state == "ACT", gender == "Female",
         legal =="Remanded", indigenous == "ATSI") %>% 
  autoplot(count) + # autoplot: a generic function to visualize various data object
  labs(title = "시간에 따른 죄수 인구의 변화",
       y = "죄수 (명)",
       x = "시간 (분기)")

# 1.7 Seasonal plot

head(vic_elec) # 호주의 빅토리아 주의 전력 수요량을 30분 단위로 기록한 데이터

vic_elec %>% 
  janitor::clean_names() %>% 
  filter(year(time) == 2012) %>% 
  gg_season(demand, period = "day") + # week, month, year 
  theme(legend.position = 'none') + 
  labs(y='MWh', title = 'Electricity demand')

vic_elec %>% 
  janitor::clean_names() %>% 
  filter(year(time) == 2012) %>% 
  gg_season(demand, period = "week") +  
  theme(legend.position = 'none') + 
  labs(y='MWh', title = 'Electricity demand')

# 1.8 Scatter plots

vic_elec %>% 
  janitor::clean_names() %>% 
  filter(year(time)==2012) %>% 
  autoplot(demand)+ # autoplot: a generic function to visualize various data object
  labs(y='GW',
       title = 'Half-hourly electricity demand: Victoria')

vic_elec %>% 
  janitor::clean_names() %>% 
  filter(year(time) == 2012) %>% 
  autoplot(temperature) +
  labs(y = "Degrees Celsius",
       title = "Half-hourly electricity demand: Victoria")

vic_elec %>% 
  janitor::clean_names() %>% 
  filter(year(time)==2012) %>% 
  ggplot(aes(x=temperature, y=demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

prison %>% 
  group_by(state) %>% 
  summarise(count = sum(count)) %>% 
  ggplot(aes(x = quarter, y = count)) + 
  geom_line() +
  facet_grid(vars(state), scales = 'free_y') + # sacles: free_y, free_x, free
  labs(title = "Australian prison population by states",
      y= "population")

# 1.9 Lag plots
#Lag: 어떤 시계열 벡터가 있을 때, 특정 시점을 기준으로 이전 시점
x <- c(1,3,6,10,15,21,28,36,45,55)
lag_x_1 <- lag(x, n=1)
lag_x_2 <- lag(x, n=2)

lag_x_1
lag_x_2

aus_production # 호주의 분기별 맥주 생산량 정보

aus_production %>% 
  janitor::clean_names() %>% 
  filter(year(quarter) >= 2000) %>% 
  gg_lag(beer, geom = 'point') + # 자기상관성 파악 가능
  labs(x = 'lag(Beer, k)')

# 1.10 Autocorrelation

# ACF 값 직접 구하기: r_2
sum((x - mean(x)) * (lag(x, n = 2) - mean(x)),
    na.rm = TRUE) / (var(x) * (length(x) - 1))

my_x <- tsibble(value = x,
                time = 1:length(x),
                index = time)
my_x %>% 
  ACF(value, lag_max = 9)

my_x %>% 
  ACF(value, lag_max = 9) %>% 
  autoplot() +
  labs(title = "x 벡터의 자기상관함수",
       x = "Time lags",
       y = "Autocorrelation function")

# 2장 시계열 모델의 기초