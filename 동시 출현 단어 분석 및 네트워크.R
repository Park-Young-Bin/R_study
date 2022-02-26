# 3-1. 동시 출현 단어 분석----
# https://youngwoos.github.io/rmeetup_tidy_textmining/tidy_textmining.html?fbclid=IwAR3EpcDlhfwhVo6NkPYxpilRUg_nKYJb3ZVSEMthK0UYfRCHpYn2d6MKmro#86

# - 단어 간의 관계를 살펴보는 분석 방법
# - ex. 손-장갑, 머리-모자
# - 단어의 관계를 표현한 의미망 만드는데 활용

# 1. 기본적인 전처리----

# 기생충 기사 댓글 불러오기
library(readr) #  to provide a fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf')
raw_news_comment <- read_csv('data/news_comment_parasite.csv')

# 전처리
library(dplyr)
library(stringr)
# install.packages('textclean')
library(textclean)

news_comment <- raw_news_comment %>% 
  select(reply) %>% 
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "), # 한글만 남기기
         reply = str_squish(reply), # 중복 공백 제거
         id = row_number()) # 행 순서 반환

# 2. 토큰화하기----

# 1) 형태소 분석기를 이용해 품사 기준으로 토큰화하기
# install.packages('tidytext')
library(tidytext)
library(KoNLP)

comment_pos <- news_comment %>% 
  unnest_tokens(input = reply, # 토큰화할 테스트
                output = word, # 토큰을 담을 변수명
                token = SimplePos22, # 문장의 단어를 22개의 품사로 구분
                drop = F) # 원래 열 삭제 안 함

comment_pos %>% 
  select(reply, word)

# 2) 품사 분리하여 행 구성하기
# - 원하는 품사를 추출하기 쉽도록 한 행을 한 품사로 구성하기
# - tidyr::separate_rows(): 
#   - 정규 표현식에 따라 텍스트를 여러 행으로 나누기
#   - sep = "[+]": "+"가 등장할 때마다 행을 나눔

# 품사별로 행 분리
library(tidyr)
comment_pos <- comment_pos %>% 
  separate_rows(word, sep = '[+]') # sep를 기준으로 셀 분할

comment_pos %>% 
  select(word, reply)

# 3. 품사 추출하기----
# (1) 명사 추출하기
noun <- comment_pos %>%
  filter(str_detect(word, "/n")) %>% # "/n"이 붙어있는 단어 추출
  mutate(word = str_remove(word, "/.*$")) # 태그 제거: '/로 시작하는 모든 문자' 제거

noun %>%
  select(word, reply)

# (2) 동사, 형용사 추출하기
# - 동사 "/pv", 형용사:"/pa" 붙어있는 단어 추출
# - 단어 뒤에 태그 대신 '다'를 붙여 이해하기 편하게 수정하기
#   - ex) "받" → "받다", "멋지" → "멋지다"

# 동사, 형용사 추출하기
pvpa <- comment_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%         # "/pv", "/pa" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기

pvpa %>%
  select(word, reply)

# (3) 추출한 데이터 결합하기
# 품사 결합
comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

comment %>%
  select(word, reply)

# 4. 단어 동시 출현 빈도 구하기----
# pairwise_count: 그룹 단위 내에서 단어가 동시에 출현한 횟수를 세어주는 함수, 보통 문장 단위를 그룹으로 처리
# install.packages("widyr")
library(widyr)
pair <- comment %>%
  pairwise_count(item = word,   # 단어
                 feature = id,   # 텍스트 구분 기준
                 sort = T)       # 빈도 높은 순 정렬

pair

# 특정 단어와 자주 함께 사용된 단어 살펴보기
pair %>% filter(item1 == "영화")
pair %>% filter(item1 == "봉준호")



# 3-2. 동시 출현 네트워크----
# - 동시 출현 빈도를 이용해 단어의 관계를 네트워크 형태로 표현
# - 단어들이 어떤 맥락에서 함께 사용되었는지 이해할 수 있다

# 1. 네트워크 그래프 데이터 만들기----
# - tidygraph::as_tbl_graph()
# - 동시 출현 빈도 데이터를 '네트워크 그래프 데이터'로 변환하기(데이터 프레임 틀을 그래프 형식으로 변환)
#   - 단어를 나타내는 노드(node, 꼭짓점)
#   - 단어를 연결하는 엣지(edge, 선)

# install.packages("tidygraph")
library(tidygraph)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

graph_comment

# 2. 네트워크 그래프 만들기----
# install.packages("ggraph")
library(ggraph)
ggraph(graph_comment) +
  geom_edge_link() +                 # 엣지
  geom_node_point() +                # 노드
  geom_node_text(aes(label = name))  # 텍스트

# 1) 그래프 다듬기
library(showtext)
font_add_google(name = 'Nanum Gothic', family = 'nanumgothic')
showtext_auto()

# 2) 엣지 노드의 색깔, 크기, 텍스트 위치 수정
# ggraph(layout = "fr"): 네트워크 형태 결정
#   - 난수를 이용해 매번 형태 달라짐 → set.seed()로 난수 고정

set.seed(1234)                              # 난수 고정
ggraph(graph_comment, layout = "fr") +      # 레이아웃
  geom_edge_link(color = "gray50",          # 엣지 색깔
                 alpha = 0.5) +             # 엣지 명암
  geom_node_point(color = "lightcoral",     # 노드 색깔
                  size = 5) +               # 노드 크기
  geom_node_text(aes(label = name),         # 텍스트 표시
                 repel = T,                 # 노드밖 표시
                 size = 5,                  # 텍스트 크기
                 family = "nanumgothic") +  # 폰트
  theme_graph()                             # 배경 삭제

# 3) 네트워크 그래프 함수 만들기
word_network <- function(x) {
  ggraph(x, layout = 'fr') + 
    geom_edge_link(color = 'gray50') + 
    geom_node_point(color = "lightcoral",
                    size = 5) + 
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5)
}

set.seed(1234)
word_network(graph_comment)

# 3. 유의어 처리하기----
# - 유의어 통일하기: 네트워크 구조가 간결해지고 단어의 관계가 좀 더 분명하게 드러남

# 1) 유의어 처리하기
comment <- comment %>% 
  mutate(word = ifelse(str_detect(word, '감독') &
                         !str_detect(word, '감독상'), '봉준호', word),
         word = ifelse(word == '오르다', '올리다', word),
         word = ifelse(str_detect(word, '축하'), '축하', word))

# 2) 단어 동시  출현 빈도 구하기
pair <- comment %>% 
  pairwise_count(item = word, # 단어
                 feature = id, # 텍스트 구분 기준(문장 단위)
                 sort = T) # 내림차순

# 3) 네트워크 그래프 데이터 만들기
graph_comment <- pair %>% 
  filter( n>= 25) %>% 
  as_tbl_graph() # 네트워크 그래프 데이터 만들기

# 4) 네트워크 그래프 만들기
library(ggraph)
set.seed(1234)
word_network(graph_comment)

# 4. 연결 중심성과 커뮤니티 표현하기----
# - 네트워크 그래프는 단어 노드가 많아 어떤 단어 노드 중심으로 해석할지 판단 어려움
# - 연결 중심성과 커뮤니티를 표현하면 단어의 관계를 더 분명하게 파악할 수 있다.

# 연결 중심성(degree centrality)
# - 노드가 다른 노드들과 얼마나 밀접하게 연결되는지 나타낸 값
# - 연결 중심성으로 노드 크기를 조정하면 어떤 단어를 눈여겨봐야 할지 판단하기 쉬워진다.

# 커뮤니티(community)
# - 단어 간의 관계가 가까워 빈번하게 연결된 노드 집단
# - 노드를 커뮤니티별로 구분 지어 서로 다른 색으로 표현하면 네트워크 구조를 이해하기 쉬워진다.


# 1) 네트워크 그래프 데이터에 연결 중심성, 커뮤니티 변수 추가하기
# 네트워크 그래프 데이터 만들기: as_tbl_graph()
#   - directed = F : 방향성 없도록 설정
#   - group_infomap()은 방향성 없는 네트워크 그래프 데이터에서만 커뮤니티를 찾아줌
#     -연결 중심성 변수 추가하기: centrality_degree()
# - 커뮤니티 변수 추가하기: group_infomap()
#   - 커뮤니티가 정수형 숫자이므로 노드가 그라데이션으로 표현됨
#   - as.factor() : factor 타입으로 변환해 노드 그룹별로 다른 색으로 표현

set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph(directed = F) %>% # 네트워크 그래프 데이터 만들기 + 방향성 없도록 설정
  mutate(centrality = centrality_degree(),    # 연결 중심성
         group = as.factor(group_infomap()))  # 커뮤니티
graph_comment

# 2. 네트워크 그래프에 연결 중심성, 커뮤니티 표현하기
set.seed(1234)
ggraph(graph_comment, layout = "fr") +      # 레이아웃
  geom_edge_link(color = "gray50",          # 엣지 색깔
                 alpha = 0.5) +             # 엣지 명암
  geom_node_point(aes(size = centrality,    # 연결 중심성에 따라 노드 크기 설정
                      color = group),       # 커뮤니티 별로 노드 색깔 다르게
                  show.legend = F) +        # 범례 삭제
  scale_size(range = c(5, 15)) +            # 노드 크기 범위
  geom_node_text(aes(label = name),         # 텍스트 표시
                 repel = T,                 # 노드밖 표시
                 size = 5,                  # 텍스트 크기
                 family = "Noto Serif KR") +  # 폰트
  theme_graph()                             # 배경 삭제
