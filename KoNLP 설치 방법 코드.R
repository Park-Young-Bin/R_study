# KoNLP 설치 방법
# first: https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=song_sec&logNo=221800340719
# second: https://blog.naver.com/song_sec/221800361879

install.packages("KoNLP")

# java, rJava 설치
install.packages("multilinguer")
# 이때 mac 사용자는 데스크탑 비밀번호를 물어봅니다. 입력해줘야 설치가 진행됩니다.
library(multilinguer)
install_jdk()

# 위 함수에서 에러가 발생하면 알려주세요
# https://github.com/mrchypark/multilinguer/issues

# 의존성 패키지 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# 아카이브된 버전의 패키지 설치
install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos=NULL, type="source")

library(KoNLP)
