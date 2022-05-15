# Z-test(One Sample)----
# 참고: https://www.statology.org/z-test-in-r/

# 예시
# 특정 모집단의 IQ가 평균 μ = 100이고 표준 편차가 σ = 15인 정규 분포를 따른다.
# 연구자는 새로운 약물이 IQ 수준에 미치는 영향을 알고자 20명의 환자를 모집하여 한 달에 동안 사용하고 월말에 IQ 수준 기록
# 새로운 약물이 IQ 수준에 상당한 차이를 유발하는가?

# install.packages('BSDA')
library(BSDA)

# enter IQ levels for 20 patients
data = c(88, 92, 94, 94, 96, 97, 97, 97, 99, 99,
         105, 109, 109, 109, 110, 112, 112, 113, 114, 115)

# perform one sample z-test
z.test(data, mu = 100, sigma.x = 15)

# Z-test(Two Sample)----
# 참고: https://www.statology.org/z-test-in-r/

# 예시
# 서로 다른 두 도시에 있는 개인의 IQ 수준이 각각 인구 표준 편차가 15인 정규 분포를 따른다.
# 한 과학자는 도시 A와 도시 B의 개인 간의 평균 IQ 수준이 다른지 알고 싶어 각 도시에서 20명의 단순 무작위 표본을 선택하고 그들의 IQ 수준 기록
# 두 도시 간에 평균 IQ 수준이 다른가?

# enter IQ levels for 20 individuals from each city
cityA = c(82, 84, 85, 89, 91, 91, 92, 94, 99, 99,
          105, 109, 109, 109, 110, 112, 112, 113, 114, 114)

cityB = c(90, 91, 91, 91, 95, 95, 99, 99, 108, 109,
          109, 114, 115, 116, 117, 117, 128, 129, 130, 133)

# perform two sample z-test
z.test(cityA, cityB, mu=0, sigma.x = 15, sigma.y = 15)