head(CO2)

# sqldf: r 데이터를 SQL문을 이용하여 조작하는 기능 제공 패키지
# install.packages('sqldf')
library(sqldf)

sqldf('select * from CO2')

sqldf('select count(*) 
      from CO2 
      where uptake >= 40')

sqldf('select sum(conc) as S_conc, count(conc) as C_conc
      from CO2
      where Plant = "Qn1"')

# tidyr: 데이터를 정리하도록 도와주는 패키지
library(tidyr)
tibble(CO2)

# 1) unite() : 열 결합 및 분리하기
CO2_unite <- unite(CO2, Treatment, Type, col = 'Tyoe', sep='-')
CO2_unite

# 2) separate(): 셀 분리하기
separate(CO2_unite, Tyoe, sep='-', into=c('Type', 'Treatment'))

# stringr: 문자열을 가능한 쉽게 처리하도록 설계 함수 세트를 제공하는 패키지
install.packages('stringr')
library(stringr)

# 1) str_length(): 길이 수 확인
str_length(c('With R', 'DO it R'))

# 2) str_trim(): 문자열 공백 제거

# 3) str_split(string, pattern, n=Inf): 문자열 분리
str_split('apples and oranges and pears and bananas', ' and ')

# 4) str_c(): 문자열 통합
str_c('apples oranges', 'Pears bananas')

# 5) str_sub(): 문자열 부분 추출
str_sub(c('apples', 'oranges', 'pears', 'bananas'), 1, 3)

# 6) str_detect(string, pattern): 값 있는지 없는지확인 후 logical 값으로 반환
str_detect(c('apples', 'oranges', 'pears', 'bananas'), 'as')
