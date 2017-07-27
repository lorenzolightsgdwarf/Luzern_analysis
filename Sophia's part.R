library(foreign)

only_score=read.spss(file = "data only scores.sav",to.data.frame = T)
only_score$RLG=only_score$Post_tot_Score-only_score$Pre_tot_Score
only_score$RLG=ifelse(only_score$RLG<0,only_score$RLG/only_score$Pre_tot_Score, only_score$RLG/(24-only_score$Pre_tot_Score))

melt_score=only_score[,c(1,2,31,43,54)]
melt_score$Pre_tot_Score=melt_score$Pre_tot_Score/7
melt_score$Post_tot_Score=melt_score$Post_tot_Score/7
melt_score$Exp_tot_Score=melt_score$Exp_tot_Score/8

melt_score=melt(melt_score,id.vars = c(1,2))
qplot(data=melt_score,x=variable,y=value,fill=Group,geom="boxplot")


-------------------------
experiment_score=only_score[,c(1,2,35:42)]
experiment_score=melt(experiment_score,id.vars = c(1,2))
experiment_score$Trial=ifelse(experiment_score$variable=="Exp_1_Score",1,ifelse(experiment_score$variable=="Exp_2_Score",2,ifelse(experiment_score$variable=="Exp_3_Score",3,ifelse(experiment_score$variable=="Exp_4_Score",4,ifelse(experiment_score$variable=="Exp_5_Score",5,ifelse(experiment_score$variable=="Exp_6_Score",6,ifelse(experiment_score$variable=="Exp_7_Score",7,8)))))))

ggplot(data=experiment_score,aes(x=as.factor(value)))+
  geom_bar(position=position_dodge())+facet_wrap(~variable,ncol=4)

qplot(data=experiment_score,x=variable,y=value/3,fill=Group,geom="boxplot")

ggplot(data=experiment_score,aes(fill=as.factor(value),x=variable))+
  geom_bar(position=position_dodge())

m=glm(data=experiment_score,value~variable,family=poisson())
summary(multcomp::glht(m,mcp(variable="Tukey")))
------------------------

raw_data=read.spss(file = "raw data with scores.sav",to.data.frame = T)


----------------------

library(reshape)
library(reshape2)
experiment_score=only_score[,c(1,2,35:42)]
experiment_score=melt(experiment_score,id.vars = c(1,2))
experiment_score$Trial=ifelse(experiment_score$variable=="Exp_1_Score",1,ifelse(experiment_score$variable=="Exp_2_Score",2,ifelse(experiment_score$variable=="Exp_3_Score",3,ifelse(experiment_score$variable=="Exp_4_Score",4,ifelse(experiment_score$variable=="Exp_5_Score",5,ifelse(experiment_score$variable=="Exp_6_Score",6,ifelse(experiment_score$variable=="Exp_7_Score",7,8)))))))

ggplot(data=experiment_score,aes(x=as.factor(value)))+
  geom_bar(position=position_dodge())+facet_wrap(~variable,ncol=4)

qplot(data = only_score,x=Group, y=RLG, geom="boxplot")
qplot(data = only_score,x=Group, y=Pre_tot_Score, geom="boxplot")
qplot(data = only_score,x=Group, y=Post_tot_Score, geom="boxplot")

anova(lm(data=only_score,RLG~Group))
anova(lm(data=only_score,Pre_tot_Score~Group))
anova(lm(data=only_score,Post_tot_Score~Group))
wilcox.test(only_score$RLG~only_score$Group)
wilcox.test(only_score$Pre_tot_Score~only_score$Group)
wilcox.test(only_score$Post_tot_Score~only_score$Group)

tmp=only_score[,c("ID","Pre_tot_Score","Post_tot_Score")]
tmp=melt(tmp,id.vars = "ID")

qplot(data=tmp,x=variable,y=value,geom="boxplot")


pre_data=only_score[,c(1,2,24:30)]
pre_data=melt(pre_data,id.vars = c(1,2))
post_data=only_score[,c(1,2,47:53)]
post_data=melt(post_data,id.vars = c(1,2))
pre_data$variable=as.character(pre_data$variable)
pre_data$variable=substr(pre_data$variable,4,nchar(pre_data$variable))
pre_data$variable=as.factor(pre_data$variable)
post_data$variable=as.character(post_data$variable)
post_data$variable=substr(post_data$variable,5,nchar(post_data$variable))
post_data$variable=as.factor(post_data$variable)

pre_post_data=merge(pre_data,post_data,by = c(1,2,3))

colnames(pre_post_data)[3]="exercise"
colnames(pre_post_data)[4]="pre_score"
colnames(pre_post_data)[5]="post_score"
pre_post_data=melt(pre_post_data,id.vars = c(1:3))

qplot(data=pre_post_data,x=exercise,y=value,geom="boxplot",fill=variable)



View(pre_post_data)
pre_data$variable=substr(pre_data$variable,4,nchar(pre_data$variable))
pre_data$variable=as.character(pre_data$variable)
pre_data$variable=substr(pre_data$variable,4,nchar(pre_data$variable))
pre_data$variable=as.factor(pre_data$variable)
post_data$variable=as.character(post_data$variable)
post_data$variable=substr(post_data$variable,5,nchar(post_data$variable))
post_data$variable=as.factor(post_data$variable)
pre_data=melt(pre_data,id.vars = c(1,2,3))
View(pre_data)
View(post_data)
View(pre_data)
pre_data[,4]=null
pre_data[,4]<-null
pre_data[,-4]
pre_data=pre_data[,-4]
pre_post_data=merge(pre_data,post_data)
View(pre_post_data)
pre_post_data=merge(pre_data,post_data,by = c(1,2))
View(pre_post_data)
pre_post_data=merge(pre_data,post_data,by = c(1,2,3))











qplot(data = only_score,x=Group, y=RLG)
qplot(data = only_score,x=Group, y=RLG, geom="boxplot")
anova(lm(data = only_score, RLG~Group))
shapiro.test(only_score$RLG)
aov(data = only_score, RLG~Group)
summary(aov(data = only_score, RLG~Group))
summary(aov(data = only_score, RLG~Group))
summary(data_on)
summary(only_score$RLG)
cor.test(only_score$Pre_tot_Score,only_score$Post_tot_Score)
cor.test(only_score$Pre_tot_Score,only_score$RLG)
qplot(data=only_score,x=Group,y=Exp_tot_Score,geom="boxplot")
cor.test(only_score$Spring_tot,only_score$RLG)
cor.test(only_score$Spring_tot,only_score$Post_tot_Score)
cor.test(only_score$Pre1_MRT,only_score$Post_tot_Score)
cor.test(only_score$Pre1_MRT,only_score$Exp_tot_Score)
cor.test(only_score$GLR,only_score$Exp_tot_Score)
cor.test(only_score$RLG,only_score$Exp_tot_Score)
cor.test(only_score$Post_tot_Score,only_score$Exp_tot_Score)
cor.test(only_score$Pre_tot_Score,only_score$Exp_tot_Score)
cor.test(only_score$RLG,only_score$RLG)
cor.test(only_score$RLG,only_score$Pre1_MRT)
cor.test(only_score$Spring_tot,only_score$Pre1_MRT)
cor.test(only_score$Spring_tot,only_score$Pre_tot_Score)
cor.test(only_score$Spring_tot,only_score$Post_tot_Score)
only_score[,c(1,2,35:42)]
experiment_score=only_score[,c(1,2,35:42)]
library(reshape)
library(reshape2)
experiment_score=melt(experiment_score,id.vars = c(1,2))
View(experiment_score)
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="boxplot")
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="line")
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="smooth")
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="point")
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="smooth")
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="smooth",method="lm")
qplot(data=experiment_score, x=variable,y=value, geom="smooth",method="lm")
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="boxplot")
glm
glm(data=experiment_score,vaule~variable,family=poisson())
glm(data=experiment_score,value~variable,family=poisson())
summary(glm(data=experiment_score,value~variable,family=poisson()))
View(experiment_score)
View(only_score)
post_data=only_score[,c(1,2,47:53)]
qplot(data=post_data, x=variable,y=value,fill=Group, geom="boxplot")
View(post_data)
post_data=melt(post_data,id.vars = c(1,2))
qplot(data=post_data, x=variable,y=value,fill=Group, geom="boxplot")
pre_data=only_score[,c(1,2,24:30)]
pre_data=melt(pre_data,id.vars = c(1,2))
pre_post_data=merge(pre_data,post_data)
pre_post_data=merge(pre_data,post_data,by = c(1,2))
View(pre_post_data)
pre_data$variable=substr(pre_data$variable,4,nchar(pre_data$variable))
pre_data$variable=as.character(pre_data$variable)
pre_data$variable=substr(pre_data$variable,4,nchar(pre_data$variable))
pre_data$variable=as.factor(pre_data$variable)
post_data$variable=as.character(post_data$variable)
post_data$variable=substr(post_data$variable,5,nchar(post_data$variable))
post_data$variable=as.factor(post_data$variable)
pre_data=melt(pre_data,id.vars = c(1,2,3))
View(pre_data)
View(post_data)
View(pre_data)
pre_data[,4]=null
pre_data[,4]<-null
pre_data[,-4]
pre_data=pre_data[,-4]
pre_post_data=merge(pre_data,post_data)
View(pre_post_data)
pre_post_data=merge(pre_data,post_data,by = c(1,2))
View(pre_post_data)
pre_post_data=merge(pre_data,post_data,by = c(1,2,3))
View(pre_post_data)
qplot(data=post_data, x=variable,y=value.y-value.x,fill=Group, geom="boxplot")
qplot(data=pre_post_data, x=variable,y=value.y-value.x,fill=Group, geom="boxplot")
pre_post_data$RLG=pre_post_data$value.y-pre_post_data$value.x
pre_post_data$RLG=ifelse(pre_post_data$RLG<0, pre_post_data$RLG/pre_post_data$value.x,pre_post_data$RLG/(3-pre_post_data$value.x) )
qplot(data=post_data, x=variable,y=RLG,fill=Group, geom="boxplot")
qplot(data=pre_post_data, x=variable,y=RLG,fill=Group, geom="boxplot")
View(pre_post_data)
pre_post_data$RLG=pre_post_data$value.y-pre_post_data$value.x
pre_post_data$RLG=ifelse(pre_post_data$RLG<=0, pre_post_data$RLG/pre_post_data$value.x,pre_post_data$RLG/(3-pre_post_data$value.x) )
qplot(data=pre_post_data, x=variable,y=RLG,fill=Group, geom="boxplot")
View(pre_post_data)
View(only_score)
View(experiment_score)
qplot(data=experiment_score, x=variable,y=value,fill=Group, geom="boxplot")
qplot(data=experiment_score, x=variable,y=value, geom="boxplot")
qplot(data=experiment_score, x=variable,y=value, geom="boxplot")+geom_violin()
qplot(data=experiment_score, x=variable,y=value, geom="boxplot")+geom_violin()+geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
qplot(data=experiment_score, x=variable,y=value, geom="boxplot")+geom_violin()+geom_jitter(shape=16, position=position_jitter(0.2))
qplot(data=experiment_score, x=variable,y=value, geom="boxplot")+geom_violin()
qplot(data=experiment_score, x=variable,y=value)+geom_violin()
experiment_score$Trial=ifelse(experiment_score$variable="Exp_1_Score",1,2)
experiment_score$Trial=ifelse(experiment_score$variable=="Exp_1_Score",1,2)
View(experiment_score)
experiment_score$Trial=ifelse(experiment_score$variable=="Exp_1_Score",1,ifelse(experiment_score$variable=="Exp_2_Score",2,ifelse(experiment_score$variable=="Exp_3_Score",3,ifelse(experiment_score$variable=="Exp_4_Score",4,ifelse(experiment_score$variable=="Exp_5_Score",5,ifelse(experiment_score$variable=="Exp_6_Score",6,ifelse(experiment_score$variable=="Exp_7_Score",7,8)))))))
cor.test(experiment_score$Trial,experiment_score$value)
cor.test(experiment_score$Trial,experiment_score$value,method = "s")
summray(glm(experiment_score,value~Group*Trial,family=poisson()))
summaray(glm(experiment_score,value~Group*Trial,family=poisson()))
summary(glm(experiment_score,value~Group*Trial,family=poisson()))
summary(glm(experiment_score,value~Group,family=poisson()))
summary(glm(data=experiment_score,value~Group*Trial,family=poisson()))
plot(glm(data=experiment_score,value~Group*Trial,family=poisson()))
plot(glm(data=experiment_score,value~Group*Trial,family=gaussia()))
plot(glm(data=experiment_score,value~Group*Trial,family=gaussian()))
summary(glm(data=experiment_score,value~Group*Trial,family=gaussian()))
summary(glm(data=experiment_score,value~Group*variable,family=poisson()))
summary(glm(data=experiment_score,value~variable,family=poisson()))
plot(glm(data=experiment_score,value~variable,family=poisson()))
plot(glm(data=experiment_score,value~variable,family=quasipoisson()))
hist(experiment_score$value)
plot(glm(data=experiment_score,value~variable,family=quasipoisson()))
m=(glm(data=experiment_score,value~variable,family=quasipoisson()))
multcomp::glht(m)
summary(multcomp::glht(m))
summary(multcomp::glht(m,mcp(rank="Tukey"))
)
summary(multcomp::glht(m,mcp(rank="Tukey")))
summary(multcomp::glht(m,mcp(variable="Tukey")))
qplot(data=experiment_score, x=variable,y=value, geom="bar")
qplot(data=experiment_score, x=variable,y=value, geom="barplot")
qplot(data=experiment_score, x=variable,y=value, geom="bar")
qplot(data=experiment_score, x=variable,  geom="bar")
qplot(data=experiment_score, x=variable, group=value, geom="bar")
qplot(data=experiment_score, x=variable, group=value, geom="histogram")
qplot(data=experiment_score, x=variable, group=value, geom="count")
qplot(data=experiment_score, x=variable, group=value)
qplot(data=experiment_score, x=variable, fill=value)
qplot(data=experiment_score, x=variable, fill=value,geom="bar")
experiment_score%>%group_by(c("Group","variable","value"))
experiment_score%>%group_by(c("Group","variable","value"))%>%summarise(test=n())
experiment_score%>%dplyr::group_by(c("Group","variable","value"))%>%summarise(test=n())
experiment_score%>%dplyr::group_by(Group,variable,value))%>%summarise(test=n())
experiment_score%>%dplyr::group_by(Group,variable,value)%>%summarise(test=n())
testexperiment_score%>%dplyr::group_by(Group,variable,value)%>%summarise(test=n())
test=experiment_score%>%dplyr::group_by(Group,variable,value)%>%summarise(test=n())
View(test)
test=experiment_score%>%dplyr::group_by(variable,value)%>%summarise(test=n())
qplot(data=test,x=variable,y=test,fill=value)
qplot(data=test,x=variable,y=test,fill=value,geom="bar")
qplot(data=test,x=variable,y=test,fill=value,geom="boxplot")
qplot(data=test,x=variable,y=test,fill=as.factor(value),geom="boxplot")
qplot(data=test,x=variable,y=test,fill=as.factor(value))
qplot(data=test,x=variable,y=test,color=as.factor(value))
qplot(data=experiment_score, x=variable, fill=value)
qplot(data=experiment_score, x=variable,y=value)
qplot(data=experiment_score, x=variable,y=value/3)
qplot(data=experiment_score, x=variable,y=value/3,geom="boxplot")
View(experiment_score)
qplot(data=experiment_score, x=Trial,y=value/3,geom="boxplot")
qplot(data=experiment_score, x=Trial,y=value/3)
qplot(data=experiment_score, x=Trial,y=value)
qplot(data=experiment_score, x=Trial,y=value,geom="smooth")
qplot(data=experiment_score, x=Trial,y=value,geom="smooth",method="lm")
qplot(data=experiment_score, x=Trial,y=value,geom="smooth",method="glm")
qplot(data=experiment_score, x=Trial,y=value,geom="smooth",method="gam")
qplot(data=experiment_score, x=Trial,y=value,geom="smooth",method="gam")+geom_point()
qplot(data=experiment_score, x=Trial,y=value,geom="point,smooth",method="lm")
qplot(data=experiment_score, x=Trial,y=value,geom=c("point","smooth"),method="lm")
qplot(data=experiment_score, x=Trial,y=value,geom=c("boxplot","smooth"),method="lm")
qplot(data=subset(experiment_score,Trial<8),x=Trial,y=value,geom=c(smooth"),method="lm")
qplot(data=subset(experiment_score,Trial<8),x=Trial,y=value,geom=c("smooth"),method="lm")
cor.test(subset(experiment_score,Trial<8)$Trial,subset(experiment_score,Trial<8)$value)
