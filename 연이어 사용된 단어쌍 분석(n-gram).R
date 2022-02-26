# 3-4. 연이어 사용된 단어쌍 분석:n-gram----
# https://youngwoos.github.io/rmeetup_tidy_textmining/tidy_textmining.html?fbclid=IwAR3EpcDlhfwhVo6NkPYxpilRUg_nKYJb3ZVSEMthK0UYfRCHpYn2d6MKmro#137

# 같은 단어도 함께 사용된 단어에 따라 의미가 달라짐
# 어떤 단어는 다른 단어와 연결되어 새로운 의미를 만들어냄
#   ex) '사과를 먹다', '사과를 하다', 감을 잡다', '귀가 얇다'

# 동시 출현 빈도와 파이 계수의 한계: 단어가 함께 사용된 횟수만 고려
# - 단어가 연결될 때 생기는 의미 무시
# - 이해하기 어려운 단어쌍 등장

# 단어가 연결될 때 생기는 의미를 고려하려면 '자주 연이어 사용된 단어'를 살펴봐야 한다.

# 엔그램
# 연이어 사용된 n개의 단어
# - 두 단어 연속: 바이그램(bigram) 또는 2-gram
# - 세 단어 연속: 트라이그램(trigram) 또는 3-gram

# 텍스트를 엔그램으로 토큰화하면
# - 단어 앞뒤에 연이어 사용된 단어를 함께 살펴봄: 얼마나 자주 '연이어' 사용된 단어쌍인가?
# - 단어가 연결될 때 생기는 의미와 맥락을 이해할 수 있음
# - 대다수의 텍스트에 사용된 평범한 단어쌍이 아니라 분명한 의미를 드러내는 단어쌍 발견

# 1. 엔그램으로 토큰화하기----

# 0) 샘플 텍스트로 엔그램 토큰화해보기
# tidytext::unnest_tokens()
# - token = "ngrams"
# - n: 기준 단어 수
text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text

# 바이그램 토큰화
text %>% 
  unnest_tokens(input = value,
                output = word,
                token = 'ngrams',
                n=2)

# 트라이그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 3)

# 2. 기사 댓글로 바이그램 만들기----
# (1) 명사, 동사, 형용사 추출하기
# - comment_pos 이용: 댓글을 형태소로 토큰화 후 품사별로 행 분리
# - 명사, 동사, 형용사를 추출해 결합한 후 두 글자 이상만 남김

comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

# (2) 유의어 처리하기
comment_new <- comment_new %>% 
  mutate(word = ifelse(str_detect(word, '감독') &
                         !str_detect(word, '감독상'), '봉준호', word),
         word = ifelse(word == '오르다', '올리다', word),
         word = ifelse(str_detect(word, '축하'), '축하', word))

# (3) 한 댓글이 하나의 행이 되도록 결합하기
comment_new %>%
  select(word)

line_comment <- comment_new %>% 
  group_by(id) %>% 
  summarise(sentence = paste(word, collapse = ' '))

# (4) (4) 바이그램으로 토큰화하기
bigram_comment <- line_comment %>% 
  unnest_tokens(input = sentence,
                output = bigram,
                token = 'ngrams',
                n = 2)
bigram_comment

# 3. 연이어 사용된 단어쌍 빈도 구하기----
# (1) 바이그램 분리하기

# 바이그램 분리하기
library(tidyr)
bigram_seprated <- bigram_comment %>% 
  separate(bigram, c('word1', 'word2'), sep = ' ')

bigram_seprated

# (2) 단어쌍 빈도 구하기
# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>% 
  count(word1, word2, sort=T) %>% # 내림차순
  na.omit() # 결측치 행 제거

pair_bigram

# (3) 단어쌍 살펴보기

# 동시 출현 단어쌍
pair %>% 
  filter(item1 == '대한민국')

# 바이그램 단어쌍
pair_bigram %>%
  filter(word1 == "대한민국")

# 4. 엔그램으로 네트워크 그래프 만들기----

# 네트워크 그래프 데이터 만들기
graph_bigram <- pair_bigram %>% 
  filter(n >= 8) %>% 
  as_tbl_graph()

# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_bigram)

# 유의어 통일하고 네트워크 그래프 다시 만들기

# bigram_seprated의 유의어 통일, 같은 단어 연속 단어쌍 제거
# 단어쌍 빈도 구하고 결측치 제거

# 유의어 처리
bigram_seprated <- bigram_seprated %>%
  mutate(word1 = ifelse(str_detect(word1, "대단"), "대단", word1),
         word2 = ifelse(str_detect(word2, "대단"), "대단", word2),
         word1 = ifelse(str_detect(word1, "자랑"), "자랑", word1),
         word2 = ifelse(str_detect(word2, "자랑"), "자랑", word2),
         word1 = ifelse(str_detect(word1, "짝짝짝"), "짝짝짝", word1),
         word2 = ifelse(str_detect(word2, "짝짝짝"), "짝짝짝", word2)) %>%
  # 같은 단어 연속 제거
  filter(word1 != word2)

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()

# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),    # 중심성
         group = as.factor(group_infomap()))  # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +         # 레이아웃
  geom_edge_link(color = "gray50",            # 엣지 색깔
                 alpha = 0.5) +               # 엣지 명암
  geom_node_point(aes(size = centrality,      # 노드 크기
                      color = group),         # 노드 색깔
                  show.legend = F) +          # 범례 삭제
  scale_size(range = c(4, 8)) +               # 노드 크기 범위
  geom_node_text(aes(label = name),           # 텍스트 표시
                 repel = T,                   # 노드밖 표시
                 size = 5,                    # 텍스트 크기
                 family = "nanumgothic") +    # 폰트
  theme_graph()                               # 배경 삭제

# 자주 연이어 사용된 단어쌍 중심으로 네트워크 형성
# 단어의 맥락과 의미를 구체적으로 이해할 수 있음
# 개별 단어의 빈도는 낮지만 자주 연이어 사용되고 함께 사용할 때 분명한 의미 지니는 단어쌍 발견
#   ex) '이미경-부회장', '조국-가족'

# 어떤 방법으로 네트워크 그래프를 만드는 게 좋을까

# - 각 방법의 특징이 다르므로 분석 목적에 맞게 선택
# - 세 방법 모두 사용해 분석 결과를 비교하면 텍스트를 다각도로 이해 가능
# 동시 출현 빈도: 자주 사용된 단어 중심으로 단어들의 관계 표현
# 파이 계수: 관련성이 큰 단어쌍 중심으로 표현, 단어 군집을 잘 드러내고 싶을 때
# 엔그램: 연이어 사용될 때 의미를 지니는 단어쌍 중심으로 표현, 단어들이 전반적으로 어떤 관계를 형성하고 있는지 표현할 때
