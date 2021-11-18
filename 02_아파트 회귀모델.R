library(car)
library(GGally)
options("scipen"=100)

df<-read.csv("set2.csv")
summary(df)
ggcorr(df,label=T)

apt.model1<-lm(apt ~ hloan + ir + cpi + fed + inc + exp + savings + kospi + hccsi + bond + m2, df)
summary(apt.model1)
# inc, savings, kospi, hccsi 유의미
vif(apt.model1)
# exp, savings, kospi, hccsi, bond vif<10

step(apt.model1,direction="forward")
# apt ~ hloan + ir + cpi + fed + inc + exp + savings + kospi + hccsi + bond + m2
# AIC=784.97

step(apt.model1,direction="backward")
# apt ~ hloan + cpi + fed + inc + savings + kospi + hccsi
# AIC=780.32

step(apt.model1,direction="both")
# apt ~ hloan + cpi + fed + inc + savings + kospi + hccsi
# AIC=780.32

# ==> apt ~ hloan + cpi + fed + inc + savings + kospi + hccsi 모델 분석

apt.model2<-lm(apt ~ hloan + cpi + fed + inc + savings + kospi + hccsi, df)
summary(apt.model2)
# hloan, fed, savings, kospi, hccsi 유의미
vif(apt.model2)
# hloan, cpi vif>10
# ==> hloan, cpi 변수 각각 제거하여 모델링

# hloan 제거
apt.model3<-lm(apt ~ cpi + fed + inc + savings + kospi + hccsi, df)
summary(apt.model3)
# fed, inc, savings, kospi, hccsi 유의미
vif(apt.model3)
# cpi, inc vif>10

# cpi 제거
apt.model4<-lm(apt ~ hloan + fed + inc + savings + kospi + hccsi, df)
summary(apt.model4)
# hloan, fed, savings, kospi, hccsi 유의미
vif(apt.model4)
# hloan, inc vif>10

# hloan, cpi 제거
apt.model5<-lm(apt ~ fed + inc + savings + kospi + hccsi, df)
summary(apt.model5)
# fed, inc, savings, kospi, hccsi 유의미
vif(apt.model5)
# 모든 변수 vif<10

# 모델5의 선형성 보정
apt.model6<-lm(log(apt) ~ log(fed) + log(inc) + log(savings) + log(kospi) + log(hccsi), df)
summary(apt.model6)
# 모든 변수 p-value 감소, R2 증가
