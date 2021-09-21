# 형태소 분석
# [reference] 
# https://youngwoos.github.io/rmeetup_tidy_textmining/tidy_textmining.html?fbclid=IwAR2mOeM954Qn5UjfXQ5yfm6_Z-NgV7h-1b_d9pP_q7nSbBMhGA9-nZx4WUo#1
# https://github.com/youngwoos/Doit_textmining

# 1. 한 문장을 단어로 분리하기
# '바지' 단어를 사전에 추가하기

# (1) 필요한 패키지 불러오기
library(KoNLP)
library(stringr)
useNIADic()

# (2) 임시 단어 추가: 특학
new_words <- data.frame(term = "특학", tag = "ncn")

buildDictionary(ext_dic = c("woorimalsam", "insighter"), # 우리말씀 사전에 등재
                user_dic = new_words,
                replace_usr_dic = T, # 추가하는 단어를 기존 사전의 내용과 상관없이 대체해서 사용
                category_dic_nms = "all") # 모든 카테고리에 단어 추가
# (3) 추출할 텍스트의 단어를 임시 단어로 바꾸기
x <- "바지: 휠체어에 앉아서 소변을 보기위해 허리가 큰 사이즈를 입어 항상 바지가 내려감"
x <- str_replace_all(x, "바지", "특학")
x
# (4) 단어 추출
y <- extractNoun(x)
y
# (5) 원래 단어로 바꾸기
y <- str_replace_all(y, "특학", "바지")
y

#############################

# 2. 여러 문장을 단어로 분리하기

# 1) 토큰화(tokenization)
# 토큰: 텍스트의 기본 단위 (ex: 단락, 문장, 단어, 형태소)
# 토큰화: 텍스트를 토큰으로 나누는 작업

# 2) tidytext 패키지
# - 텍스트를 정돈된 데이터(Tidy Data) 형태를 유지하면 분석
# - dplyr, ggplot2 패키지와 함게 활용
# - 토큰화하기: unnest_tokens()

# (1) 샘플 텍스트로 작동 원리 알아보기
library(dplyr)
library(tidyverse)
text <- tibble(value = '대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.')
text

# install.packages('tidytext')
library(tidytext)

# (2) 문장 기준 토큰화
text %>% 
  unnest_tokens(input = value, # 토큰화할 텍스트
                output = word, # 토큰을 담을 변수명
                token = 'sentences') # 문장 기준

# (3) 띄어쓰기 기준 토큰화
text %>% 
  unnest_tokens(input = value, # 토큰화할 텍스트
                output = word, # 토큰을 담을 변수명
                token = 'words') # 띄어쓰기 기준

# (4) 문자 기준 토큰화
text %>% 
  unnest_tokens(input = value, # 토큰화할 텍스트
                output = word, # 토큰을 담을 변수명
                token = 'characters') # 단어 기준

# (5) 형태소 추출
library(KoNLP)
extractNoun(text$value)

text <- text %>% 
  unnest_tokens(input = value,
                output = word,
                token = 'sentences') # 문장 기준
text

text %>% 
  unnest_tokens(input = word,
                output = word,
                token = extractNoun) # 명사 기준

#############################

# 3. 단어 빈도 분석하기

# (1) 문재인 대통령 연설문 불러오기
raw_moon <- readLines('data/speech_moon.txt', encoding = 'UTF-8')

# (2) 기본적인 전처리
library(stringr)
moon <- raw_moon %>% 
  str_replace_all("[^가-힣]", ' ') %>%   # 한글만 남기기(띄어쓰기 하기)
  str_squish() %>%                     # 중복 공백 제거
  as_tibble()                          # tibble로 변환

moon

# (3) 명사 기준 토큰화
word_noun <- moon %>% 
  unnest_tokens(input = value, # 토큰화할 텍스트
                output = word, # 토큰을 담을 변수명
                token = extractNoun) # 명사 기준
word_noun

# (4) 단어 빈도 구하기
# - 빈도가 높은 명사를 보면 글쓴이가 무엇을 강조했는지 알 수 있음
# - 연설문이 702개의 명사로 구성됨

word_noun <- word_noun %>% 
  count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
  filter(str_count(word) > 1)  # 두 글자 이상만 남기기

word_noun

# (4)-1) 띄어쓰기 기준 추출
moon %>% unnest_tokens(input = value,
                       output = word,
                       token = 'word') %>%  # 띄어쓰기 기준
  count(word, sort = T) %>% 
  filter(str_conv(word) > 1)

# (4)-2) 명사 추출
moon %>% unnest_tokens(input = value,
                       output = word,
                       token = extractNoun) %>%  # 명사 기준
  count(word, sort = T) %>% 
  filter(str_count(word) > 1)

# (5) 막대 그래프 만들기
top20 <- word_noun %>% head(20)
top20

library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), y = n)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = n), hjust = -0.3) + 
  labs(x = NULL) + 
  theme(text = element_text(family = 'nanumgothic'))

# (6) 워드 클라우드 만들기
# install.packages('showtext')

# (6)-1) 폰트 설정
# [google font] https://fonts.google.com/?subset=korean

library(showtext)
font_add_google(name = 'Black Han Sans', family = 'blackhansans') # 구글 폰트에 올라온 폰트를 설치
showtext_auto() # 설치한 폰트 적용

# install.packages('ggwordcloud')
library(ggwordcloud)
ggplot(word_noun, aes(label = word, size = n, col = n)) + 
  geom_text_wordcloud(seed = 1234, family = 'blackhansans') + 
  scale_radius(limits = c(3, NA),
               range = c(3, 15)) + 
  scale_color_gradient(low = '#66aaf2', high = '#004EA1') + 
  theme_minimal()
