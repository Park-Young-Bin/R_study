# tibble이 뭘까? tidyr로 tibble 자유자재 다루기! - 데이터 사이언스 기초 tidyverse 정복하기 4탄

a <- c(1:10)
b <- matrix(a, ncol = 2)
b
c <- data.frame(a = 1:10, b = 11:20)
d <- list(a, b, c)
d

# data frame의 새로운 대안
library(tibble)
mtcars
class(mtcars) # data.frame
as_tibble(mtcars) # tibble 변환

tibble(x = 1:2, y = 1, z = x^2 + y)

tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6
)

# tibble vs dataframe
# 1. tibble - strict about subsetting
mtcars %>% names()
my_df <- mtcars[1:6, ]
my_df$dis # 오류 발생X, 'disp'로 인식해서 출력

my_tb <- mtcars[1:6,] %>% as_tibble()
my_tb$dis # error: Unknown or uninitialised column: `dis`.

# 2. tibble - more precise about return values
# 데이터 프레임은 자동으로 벡터로 바꾼다.
# tibble은 그렇지 않다.
class(my_df[,1])
my_df[,1]

class(my_tb[,1])
my_tb[,1] # tibble 반환
my_tb[[1]] # vector 반환

# tibble - list dataframe
my_df$cyl <- list(9, 10:11, 12:14, 'text', as.factor('a'), as.factor('b'))
my_df

my_tb$cyl <- list(9, 10:11, 12:14, 'text', as.factor('a'), as.factor('b'))
my_tb

# tidyr 패키지
# install.packages('tidyr')
library(tidyr)
tibble_wide <- tribble(
  ~name,       ~gender,    ~party,        ~"2019",  ~"2020",  ~"2021",
  "Bomi Kim",   "female",  "opposition",   48.01,    56.99,    57.43,
  "Issac Lee",  "male",    "ruling",       47.85,    30.92,    42.20,
  'Soony Kim',  "female",  'ruling',       33.49,    41.86,    44.54,
  'Jelly Lee',  'female',  'opposition',   42.35,    32.30,    40.19
) # tibble which is not a tidydata

tibble_long <- tibble_wide %>% 
  pivot_longer(
    cols = "2019":"2021", # 컬럼 지정
    names_to = "year", # 컬럼명 지정
    values_to = "approval_rating" # 값 변수명 지정
  ) # reshape2 패키지의 melt 함수와 동일
tibble_long


tibble_long %>% 
  pivot_wider(names_from = year,
              values_from = approval_rating) # 원상복구

# rowwise() and c_across()
library(dplyr)
tibble_wide <- tibble_wide %>% 
  rowwise() %>% 
  mutate(average = mean(c_across("2019":"2021"))) # 한 번에 함수 처리

tibble_wide
