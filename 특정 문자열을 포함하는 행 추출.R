# 특정 문자열을 포함하는 행 추출
# 참고자료: http://daplus.net/r-%ED%8A%B9%EC%A0%95-%EB%AC%B8%EC%9E%90%EC%97%B4%EC%9D%84-%ED%8F%AC%ED%95%A8%ED%95%98%EB%8A%94-%ED%96%89-%ED%95%84%ED%84%B0%EB%A7%81/

library(dplyr)
library(stringr)

df <- mtcars
df$type <- rownames(df) # 행 이름을 컬럼으로 추가
rownames(df) <- NULL # 행 이름 재설정

df %>% filter(grepl('Toyota|Mazda', type)) # 4건
df %>% filter(str_detect(type, "Toyota|Mazda")) # 4건
