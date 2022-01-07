setwd("~/MSDS 410")

REL <- read.csv("~/MSDS 410/RELIGION.csv")

REL$INCOME[is.na(REL$INCOME)]<-0

REl_1 <- REL %>% filter(REL$RELSCHOL==1)

REL_0 <-REL %>% filter(REL$RELSCHOL==0)

ggplot(data=REl_1,aes(INCOME))+geom_bar(col="blue")+ggtitle("Income for those that attend Religious School")

ggplot(data=REL_0,aes(INCOME))+geom_bar(col="blue")+ggtitle("Income for those tha do not attend Religious School")

REL<- REL %>% mutate(D_INCOME=ifelse(REL$INCOME>=8,1,0))

sum(REL$D_INCOME==0|REL$RELSCHOL==0)
sum(REL$D_INCOME==1|REL$RELSCHOL==0)
sum(REL$D_INCOME==1|REL$RELSCHOL==1)
sum(REL$D_INCOME==0|REL$RELSCHOL==1)

ggplot(data=REL,aes(REL$ATTEND,REL$RELSCHOL))+geom_point()

ggplot(data=REl_1,aes(ATTEND))+geom_bar(col="blue")+ggtitle("ATTEND for those that attend Religious School")

ggplot(data=REL_0,aes(ATTEND))+geom_bar(col="blue")+ggtitle("ATTEND for those that do not attend Religious School")

model_1<- glm(RELSCHOL~RACE,data=REL,family = "binomial")

summary(model_1)
coef(model_1)

model_2<-glm(RELSCHOL~INCOME,data=REL,family = "binomial")

summary(model_2)
coef(model_2)

logit1=-2.6687640+.1409057*REL$INCOME

REL<- REL %>% mutate(pi=exp(logit1)/(1+exp(logit1)))

ggplot(data = REL,aes(REL$INCOME,REL$pi))+geom_point()

model_3<-model_2<-glm(RELSCHOL~ATTEND,data=REL,family = "binomial")

summary(model_3)

logit2=-2.9727+.2269*REL$ATTEND

logit3=-1.0726-1.0911*REL$RACE

REL<- REL %>% mutate(pi_2=exp(logit2)/(1+exp(logit2)))

ggplot(data = REL,aes(REL$ATTEND,REL$pi_2))+geom_point()

model_4<- glm(RELSCHOL~RACE+INCOME+ATTEND,data=REL,family = "binomial")

REL<- REL %>% mutate(pi_3=exp(logit3)/(1+exp(logit3)))

summary(model_4)

REL<- REL %>% mutate(pi_1_estimates=ifelse(pi<.50,0,1)) %>% 
mutate(pi_2_estimates=ifelse(pi_2<.50,0,1)) %>% mutate(pi_3_estimates=ifelse(pi_3<.50,0,1))
