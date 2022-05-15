# T-test(One Sample)----
# 참고: https://mansoostat.tistory.com/13?category=659368

# 1) 예시
# 고혈압 환자군에서 평균 수축기 혈압(SBP)이 150mmHg 이라고 하는데, 
# 연구를 통해 환자군의 모집단 평균 수축기 혈압도 동일할까?

# 자료 생성
subject <- c(1:30)
SBP <- c(sample(145:160, 30, replace=T))
dat <- data.frame(subject, SBP)
head(dat)

mean(dat$SBP) # 표본 평균

# One sample t-test
# p-value = 0.03943이고, 유의수준 0.05보다 작으므로 H0을 기각한다.
t.test(dat$SBP, mu = 150)

# 2) 예시
# 참고: https://blog.naver.com/PostView.naver?blogId=shoutjoy&logNo=221793939950&redirect=Dlog&widgetTypeCall=true&directAccess=false

# 기말고사 성적은 24점보다 유의하게 높은가?
# p-value = 0.1696이고, 유의수준 0.05보다 크므로 24점보다 높다고 할 수 없다.
final <- c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(final, alternative = 'greater', mu=24)

# T-test(두 독립 표본 검정)----
# 참고: https://mansoostat.tistory.com/16

# 예시
# A라는 치료를 받은 그룹(g1, 30명)과 B라는 치료를 받은 그룹(g2, 30명)이 있고
# 두 그룹 간에 수축기 혈압(SBP)이 다른지 알아보고자 한다.

# 자료 생성
set.seed(1); SBP.g1 <- c(sample(140:160, 30, replace=T))
set.seed(2); SBP.g2 <- c(sample(140:160, 30, replace=T))
dat <- data.frame(SBP.g1, SBP.g2)
head(dat)

# 등분산 검정
# H0: 두 집단의 분산은 동일하다.  vs  H1: not H0
var.test(SBP.g1, SBP.g2) # p-value = 0.3548 > 0.05 → H0 채택

library(car) # Levene's test를 이용한 검정
leveneTest(dat$SBP.g1, dat$SBP.g2, location = 'mean')

# 정규성 검정
# 참고: https://kbkb456.tistory.com/93
shapiro.test(dat$SBP.g1) # p-value = 0.06094 → H0 채택
shapiro.test(dat$SBP.g2) # p-value = 0.3965 → H0 채택

# t-test
# t-검정 결과 p-value=0.5693로 0.05보다 크기 때문에 두 집단의 평균이 다르다고 할 수 없다.
t.test(dat$SBP.g1, dat$SBP.g2, alternative = 'two.sided', var.equal = TRUE)

# T-test(대응 표본 검정)----
# 참고: https://blog.naver.com/PostView.naver?blogId=shoutjoy&logNo=221793939950&redirect=Dlog&widgetTypeCall=true&directAccess=false

# 예시
# 학원을 다닌 후 기말고사 성적이 학원을 다니기 전 중간고사 성적보다 높은가?

# 자료 생성
mid = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
final = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)

mean(mid) # 23.1
mean(final) # 25.1

# Paired t-test
# p-value = 0.0007749이므로 H0기각
t.test(final, mid, alternative = 'greater', paired = T) # final이 더 큰 값인가?
t.test(mid, final, alternative = 'less', paired = T) # mid가 더 작은 값인가?