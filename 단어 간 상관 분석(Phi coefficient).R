# 3-3. 단어 간 상관 분석: Phi coefficient----
# https://youngwoos.github.io/rmeetup_tidy_textmining/tidy_textmining.html?fbclid=IwAR3EpcDlhfwhVo6NkPYxpilRUg_nKYJb3ZVSEMthK0UYfRCHpYn2d6MKmro#123

# 동시 출현 빈도의 한계
# - 대부분의 단어와 자주 함께 사용되는 단어쌍 다수
#     ex) "영화"-"기생충"
# - 다른 단어에 비해 상대적으로 자주 함께 사용된 단어가 무엇인지 살펴봐야 한다

# 파이 계수(phi coefficient)
# - 두 단어가 함께 사용되는 경우가 각각 사용되는 경우에 비해 얼마나 많은지 나타낸 지표
# - 상대적으로 관련성이 큰 단어 파악하는데 활용
#   - 어떤 단어와 자주 함께 사용되지만 다른 단어와는 자주 함께 사용되지 않는 단어

# 파이 계수의 의미
# X, Y 두 단어가 있을 때, 여러 텍스트에서 두 단어의 사용 여부를 놓고 가능한 모든 경우
# - X, Y 모두 있음(a)
# - X, Y 모두 없음(d)
# - X만 있음(b)
# - Y만 있음(c)

# -1 ~ +1
# - +1에 가까울수록 두 단어가 자주 함께 사용되어 관련성이 크다는 의미
# - -1에 가까울수록 함께 사용되는 경우가 드물어 관련성이 작다는 의미

# 1. 파이 계수 구하기----
# widyr::pairwise_cor()
# - item: 단어
# - feature: 텍스트 구분 기준
# - sort = T: 파이 계수 높은순 정렬

word_cors <- comment %>% 
  add_count(word) %>% 
  filter(n >= 20) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors

# 특정 단어와 관련성이 큰 단어 살펴보기
word_cors %>%
  filter(item1 == "대한민국")

word_cors %>%
  filter(item1 == "역사")

# 2. 파이 계수로 막대 그래프 만들기----

# 1) 관심 단어별로 파이 계수가 큰 단어 추출하기
# 관심 단어 목록 생성
target <- c("대한민국", "역사", "수상소감", "조국", "박근혜", "블랙리스트")

top_cors <- word_cors %>% 
  filter(item1 %in% target) %>% 
  group_by(item1) %>% 
  slice_max(correlation, n = 8) # correlation이 가장 큰 8개 행 선택

# 2) 막대그래프 그리기
# 그래프 순서 정하기
top_cors$item1 <- factor(top_cors$item1, levels = target)

library(ggplot2)
ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) + # x축 이름 삭제
  theme(text = element_text(family = "nanumgothic"))

# 3. 파이 계수로 네트워크 그래프 만들기----

# 1) 네트워크 그래프 데이터 만들기. 연결 중심성과 커뮤니티 추가하기
set.seed(1234)
graph_cors <- word_cors %>% 
  filter(correlation >= 0.15) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(), # 연결성: node에 edge가 몇개 연결되어 있는지 보고 네트워크의 '중심성 정도'를 판단하는 방법
         group = as.factor(group_infomap())) # 커뮤니티: 방향성 없는 네트워크 그래프에 데이터에서만 커뮤니티를 찾아주는 함수

# 2) 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,   # 엣지 명암
                     edge_width = correlation),  # 엣지 두께
                 show.legend = F) +              # 범례 삭제
  scale_edge_width(range = c(1, 4)) +            # 엣지 두께 범위
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  theme_graph()
