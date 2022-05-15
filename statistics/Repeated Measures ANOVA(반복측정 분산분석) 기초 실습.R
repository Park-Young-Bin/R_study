# Repeated Measures ANOVA(반복측정 분산분석)----
# 참고: https://www.youtube.com/watch?v=3YsIUsSZ7kY&list=PLY0OaF78qqGAxKX91WuRigHpwBU0C2SB_&index=15
# 동일한 대상에 대해 여러 번 반복측정하여 반복측정 집단 간에 차이가 존재하는지 검정
# 동일 집단에 속한 대상들 간에 집단 내 차이 규명 

str(CO2)
# Plant: 나무 종류(Type별 3종류)
# Type(독립변수, 집단 간 요인): 지역(2개 범주)
# conc(독립변수, 집단 내 요인, 반복측정값): CO2 농도(7개 범주)
# uptake(종속변수): CO2 흡수율

CO2sub <- subset(CO2, Treatment=="chilled")
CO2sub$conc <- factor(CO2sub$conc)
str(CO2sub)

head(CO2sub) # conc(CO2 농도)별로 uptake가 반복 측정된 data

# 반복측정 일원분산분석: (종속변수) ~ (집단 내 요인) + Error(Subject/집단 내 요인)
# 반복측정 이원분산분석: (종속변수) ~ (집단 간 요인)*(집단 내 요인) + Error(Subject/집단 내 요인)
# 결과1: Type(나무의 출신 지역)에 따라서 uptake(CO2 흡수율)에 차이가 있음
# 결과2: conc(CO2 농도)에 따라서 uptake(CO2 흡수율)에 차이가 있음
# 결과3: Type(나무의 출신 지역)와 conc(CO2 농도) 간에 상호작용 효과 존재
CO2sub.aov <- aov(uptake ~ Type*conc + Error(Plant/conc), data=CO2sub) 
summary(CO2sub.aov)

# 결과1(주효과): Type에 따라 uptake에 차이가 있음
# 결과2(주효과): conc이 증가함에 따라 uptake가 증가함
# 결과3(상호작용효과): conc가 증가하면서 퀘백 나무가 미시시피보다 uptake 증가폭이 큼
boxplot(uptake ~ Type*conc, data=CO2sub,
        col=c('deepskyblue', 'green3'),
        las=2, cex.axis=.7,
        xlab='', ylab='Carbon dioxide uptake rate',
        main='Effects of Plant Type and CO2 on Carbon Dioxide Uptake')
legend('topleft', inset=.02,
       legend=c('Quebec', 'Mississippi'),
       fill=c('deepskyblue', 'green3'))

library(HH)
interaction2wt(uptake ~ Type*conc, data=CO2sub)

# 사후 검정
with(CO2sub, pairwise.t.test(uptake, conc, paired=T, p.adjust.method="bonferroni"))
with(CO2sub, pairwise.t.test(uptake, Type, paired=T, p.adjust.method="bonferroni"))

# 반복측정 일원분산분석 예제----
# 참고: https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=crow83&logNo=221503065741

# 예제: 총 6명의 피실험자가 있고, 어떤 약을 투여한다고 할 때, 투약전(pre), 3개월 후(after3m), 6개월 후(after6m)의 혈중농도를 비교한다. 경과 시간에 따른 혈중농도에 차이가 있을까?

id<-c(1,2,3,4,5,6) 
pre<-c(45,42,36,39,51,44) 
after3m<-c(50,42,41,35,55,49) 
after6m<-c(55,45,43,40,59,56) 
d<-data.frame(id, pre, after3m, after6m)
d

library(reshape2)
d.m<-melt(d,id.vars="id") 
colnames(d.m)<-c("id","time","value") 
d.m$id<-factor(d.m$id) 
d.m$time<-factor(d.m$time) 
head(d.m)

# 개인별 혈중농도변화 그래프
library(ggplot2)
ggplot(data = d.m, aes(x = time, y = value))+
  geom_line(aes(group=id, col=id))+
  geom_point(aes(col=id))

# 평균치 변화
library(dplyr)
ds <- d.m %>% group_by(time) %>% summarise(mean = mean(value), sd=sd(value))
ggplot(ds,aes(x=time,y=mean))+
  geom_point()+
  geom_line(group=1)

# rm anova test
summary(aov(value ~ time + Error(id/time), data=d.m))

# 사후 분석
with(d.m, pairwise.t.test(value, time, paired=T, p.adjust.method="bonferroni"))

# 반복측정 이원분산분석 예제----
# 참고: https://m.blog.naver.com/crow83/221503104750
# 독립변수: group, time(반복측정)

df <- read.csv('data/rmanova.csv')
head(df)
df$id <- 1:196
df$id <- factor(df$id)
df$group <- factor(df$group)
str(df)

m <- melt(data = df, id.vars = c('id', 'group'))
head(m)
colnames(m) <- c('id', 'group', 'time', 'value')
head(m)

# rm anova test
a <- aov(value ~ group * time + Error(id/time), data=m)
summary(a)

interaction.plot(x.factor = m$time, 
                 trace.factor = m$group, 
                 response = m$value,
                 type='b', pch=c(2,4,6), legend = F, col=c(3,4,6), las=1,
                 xlab='Time', ylab='Mean of Value')
legend('topleft', legend=c('Group1', 'Group2', 'Group3'),
       pch=c(2,4,6), col=c(3,4,6), bg='gray90')

# 사후 분석
with(m, pairwise.t.test(value, time, paired=T, p.adjust.method = 'bonferroni'))
# with(m, pairwise.t.test(value, group, paired=T, p.adjust.method = 'bonferroni'))