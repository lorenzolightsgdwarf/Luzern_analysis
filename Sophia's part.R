library(foreign)

only_score=read.spss(file = "data only scores.sav",to.data.frame = T)
only_score$RLG=only_score$Post_tot_Score-only_score$Pre_tot_Score
only_score$RLG=ifelse(only_score$RLG<0,only_score$RLG/only_score$Pre_tot_Score, only_score$RLG/(24-only_score$Pre_tot_Score))

melt_score=only_score[,c(1,2,31,43,54)]
melt_score$Pre_tot_Score=melt_score$Pre_tot_Score/7
melt_score$Post_tot_Score=melt_score$Post_tot_Score/7
melt_score$Exp_tot_Score=melt_score$Exp_tot_Score/8

melt_score=melt(melt_score,id.vars = c(1,2))
qplot(data=melt_score,x=variable,y=value,fill=Group,geom="boxplot")+
  labs(x="Phase",y="Normalized Score")+
  scale_x_discrete( labels=c("Pre-Test", "Intervention", "Post-Test"))+
  scale_fill_discrete(name="Feedback", labels=c("Tangible","Verbal"))+
  geom_segment(aes(x=1,xend=2,y=2.25,yend=2.25))+annotate("text",x=1.5,y=2.3,label="z=-3.71, p<0.0001")+
  geom_segment(aes(x=1,xend=1,y=2.21,yend=2.25))+geom_segment(aes(x=2,xend=2,y=2.25,yend=2.21))+
  geom_segment(aes(x=1,xend=3,y=2.75,yend=2.75))+annotate("text",x=2,y=2.81,label="z=1.14, p=0.49")+
  geom_segment(aes(x=1,xend=1,y=2.75,yend=2.71))+geom_segment(aes(x=3,xend=3,y=2.75,yend=2.71))+
  geom_segment(aes(x=2,xend=3,y=2.5,yend=2.5))+annotate("text",x=2.5,y=2.56,label="z=4.85, p<0.0001")+
  geom_segment(aes(x=2,xend=2,y=2.46,yend=2.5))+geom_segment(aes(x=3,xend=3,y=2.46,yend=2.5))
  

---
qplot(data=melt_score,x=variable,y=value,geom="boxplot")

qplot(data=only_score,x=Group,y=RLG,geom="boxplot",fill=Group)+labs(x="Feedback",y="Relative Learning Gain")+scale_x_discrete( labels=c("Tangible","Verbal"))+ guides(fill=FALSE)

-------------------------
  
DF_Experts$AOI=as.character(DF_Experts$AOI)
  
for(r in row(DF_Experts)){
  l=unlist(strsplit(DF_Experts$AOI[r],':'))
  if(length(l)==2)
    DF_Experts[r,"primary.AOI"]=l[2]
  else{
     candidate=""      
     for(i in c(2:length(l)))
        if(grepl(l[i],pattern = "Joint")){
            candidate=l[i]
            break
        }else{
          candidate=l[i]
        }
     DF_Experts[r,"primary.AOI"]=candidate
  }
}

unlist(strsplit(DF_Experts$AOI[30],':'))[2]
  
DF_Experts=read.csv("DF_Experts.csv",sep=",")
DF_Fixations=read.csv("DF_Fixations",sep=",")

DF_Fixations_Full=DF_Fixations
colnames(DF_Fixations_Full)=colnames(DF_Experts[,c(1,8,10,3,5,4,11,9)])
DF_Fixations_Full=rbind(DF_Experts[,c(1,8,10,3,5,4,11,9)],DF_Fixations_Full)
DF_Fixations_Full$Trial=as.factor(DF_Fixations_Full$Trial)

tmp=aggregate(Duration  ~ Participant+Trial+Condition,data=DF_Fixations_Full[grep("Joint",DF_Fixations_Full$primary.AOI),],FUN=sum)
tmp2=aggregate(Duration  ~ Participant+Trial+Condition,data=DF_Fixations_Full,FUN=sum)
JoinFocus=merge(tmp,tmp2,by=c("Participant","Trial","Condition"))
JoinFocus$prop=JoinFocus$Duration.x/JoinFocus$Duration.y

qplot(data = JoinFocus,x=as.factor(Trial),y=prop,fill=Condition,geom="boxplot")
m=lmer(data=JoinFocus,prop~Condition+(1|Trial))
drop1(m,test="Chi")
summary(multcomp::glht(m,mcp(Condition="Tukey")))

JointFocus_plot=JoinFocus%>%group_by(Trial,Condition)%>%summarise(meanP=mean(prop)*100,ci=100*sd(prop)*1.96/sqrt(n()),participants=n())

ggplot(JointFocus_plot, aes(x=as.numeric(Trial), y=meanP, colour=Condition)) + 
  geom_errorbar(aes(ymin=meanP-ci, ymax=meanP+ci), width=.5, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1),size=1) +
  geom_point(position=position_dodge(0.1),size=2)+labs(x="Trial",y="% Fixation Time")+
  scale_x_continuous(breaks = c(1:8))+
  scale_color_discrete(name="Feedback", labels=c("Expert","Verbal","Tangible"))

#LEarner non learner part

tmp=aggregate(Event.Duration..ms.  ~ Participant+Trial+Learner,data=DF_Fixations[grep("Joint",DF_Fixations$primary.AOI),],FUN=sum)
tmp2=aggregate(Event.Duration..ms.  ~ Participant+Trial+Learner,data=DF_Fixations,FUN=sum)
Learn=merge(tmp,tmp2,by=c("Participant","Trial","Learner"))
Learn$Perc=Learn$Event.Duration..ms..x/Learn$Event.Duration..ms..y
qplot(data = Learn,x=as.factor(Trial),y=Perc,fill=Learner,geom="boxplot")

colnames(Learn)=colnames(JoinFocus)

Learn=rbind(Learn,subset(JoinFocus,Condition=="Expert"))

qplot(data = Learn,x=as.factor(Trial),y=prop,fill=Condition,geom="boxplot")
m=lmer(data=Learn,prop~Condition+(1|Trial))
drop1(m,test="Chi")
summary(multcomp::glht(m,mcp(Condition="Tukey")))

LearnFocus_plot=Learn%>%group_by(Trial,Condition)%>%summarise(meanP=mean(prop)*100,ci=100*sd(prop)*1.96/sqrt(n()),participants=n())

ggplot(LearnFocus_plot, aes(x=as.numeric(Trial), y=meanP, colour=Condition)) + 
  geom_errorbar(aes(ymin=meanP-ci, ymax=meanP+ci), width=.5, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1),size=1) +
  geom_point(position=position_dodge(0.1),size=2)+labs(x="Trial",y="% Fixation Time")+
  scale_x_continuous(breaks = c(1:8))+
  scale_color_discrete(name="RLG", labels=c("Expert","Zero or Negative","Positive"))





------------
#KL divergence
  tmp=aggregate(Duration~Participant+Trial+Condition,data=DF_Fixations_Full,FUN=length)
  Saliency=aggregate(Duration~Participant+Trial+Condition+primary.AOI,data=DF_Fixations_Full,FUN=length)
  Saliency=merge(Saliency,tmp,by=c("Participant","Trial","Condition"))
  colnames(Saliency)[5] <- "Fix"
  colnames(Saliency)[6] <- "Tot"
  
  
  Saliency$prop=Saliency$Fix/Saliency$Tot;
  
  
  #participants=Saliency[!duplicated(Saliency[,c("Trial","Participant","Condition")]),c("Trial","Participant","Condition")]
  #participant_trial=merge(participants,AOI[,c(1,2)],all = T,by="Trial")
  #Saliency=merge(Saliency,participant_trial,by.x=c("Participant","Trial","Condition","primary.AOI"),by.y=c("Participant","Trial","Condition","AOI"),all=T)
  
  #Saliency[is.na(Saliency)]=0.0000001
  
  Saliency_Expert=Saliency%>%filter(Condition=="Expert")%>%group_by(Trial,primary.AOI)%>%summarise(meanFixProp=sum(prop)/6)
  Saliency_Expert=Saliency_Expert%>%group_by(Trial)%>%mutate(meanFixProp=meanFixProp/sum(meanFixProp))
  
  Saliency=merge(Saliency,Saliency_Expert,by = c("Trial","primary.AOI"), all = T)
  
  Saliency[is.na(Saliency)]=0.0000001
  
  KL=Saliency%>%group_by(Trial,Condition,Participant)%>%summarise(KL=sum(prop*log(prop/meanFixProp)))
  
  qplot(data = KL, x=Trial,y=log(KL),fill=Condition,geom="boxplot")
  KL_plot=KL%>%filter(KL<1)%>%group_by(Trial,Condition)%>%summarise(meanP=mean(KL),ci=sd(KL)*1.96/sqrt(n()),participants=n())
  
  ggplot(subset(KL_plot,Condition!="Expert"), aes(x=as.numeric(Trial), y=meanP, colour=Condition)) + 
    geom_errorbar(aes(ymin=meanP-ci, ymax=meanP+ci), width=.5, position=position_dodge(0.1)) +
    geom_line(position=position_dodge(0.1),size=1) +
    geom_point(position=position_dodge(0.1),size=2)+labs(x="Trial",y="KL Divergence")+
    scale_x_continuous(breaks = c(1:8))+
    scale_color_discrete(name="Feedback", labels=c("Verbal","Tangible"))
  
  KL=merge(KL,DF_Learner, by="Participant")
  
  KL$Learner=ifelse(KL$Condition=="Expert","Expert",ifelse(KL$Learner>0,"Yes","No"))
 
   KLLearner_plot=KL%>%filter(KL<1)%>%group_by(Trial,Learner)%>%summarise(meanP=mean(KL),ci=sd(KL)*1.96/sqrt(n()),participants=n())
   
   ggplot(subset(KLLearner_plot,Learner!="Expert"), aes(x=as.numeric(Trial), y=meanP, colour=Learner)) + 
     geom_errorbar(aes(ymin=meanP-ci, ymax=meanP+ci), width=.5, position=position_dodge(0.1)) +
     geom_line(position=position_dodge(0.1),size=1) +
     geom_point(position=position_dodge(0.1),size=2)+labs(x="Trial",y="KL Divergence")+
     scale_x_continuous(breaks = c(1:8))+
     scale_color_discrete(name="RLG", labels=c("Zero or Negative","Positive"))
  
  
  #Saliency=dcast(data=Saliency, Trial+primary.AOI~Condition)
  
  #Saliency[is.na(Saliency)]=0.0000001
  
  #Saliency=Saliency%>%group_by(Trial)%>%mutate(Expert=Expert/sum(Expert),static=static/sum(static),tangible=tangible/sum(tangible))
  #KL=Saliency%>%group_by(Trial)%>%summarise(KL_static=sum(static*log(static/Expert)),KL_tangible=sum(tangible*log(tangible/Expert)))
  
  
-----  
experiment_score=only_score[,c(1,2,35:42)]
experiment_score=melt(experiment_score,id.vars = c(1,2))
experiment_score$Trial=ifelse(experiment_score$variable=="Exp_1_Score",1,ifelse(experiment_score$variable=="Exp_2_Score",2,ifelse(experiment_score$variable=="Exp_3_Score",3,ifelse(experiment_score$variable=="Exp_4_Score",4,ifelse(experiment_score$variable=="Exp_5_Score",5,ifelse(experiment_score$variable=="Exp_6_Score",6,ifelse(experiment_score$variable=="Exp_7_Score",7,8)))))))

ggplot(data=experiment_score,aes(x=as.factor(value)))+
  geom_bar(position=position_dodge())+facet_wrap(~variable,ncol=4)

qplot(data=experiment_score,x=variable,y=value/3,fill=Group,geom="boxplot")

ggplot(data=experiment_score,aes(fill=as.factor(value),x=variable))+
  geom_bar(position=position_dodge())

m=glm(data=subset(experiment_score,variable!="Exp_5_Score"&variable!="Exp_6_Score"&variable!="Exp_7_Score"),value~as.numeric(variable),family=poisson())
m_null=glm(data=experiment_score,value~1,family=poisson())

summary(multcomp::glht(m,mcp(variable="Tukey")))

m=glm(data=experiment_score,value~variable,family=quasipoisson())

m=glmer(data=experiment_score,value~variable*Group+(1|ID),family=poisson())

ggplot(data = experiment_score,aes(x=as.numeric(variable),y=exp(predict(m))))+geom_line()

experiment_score_summary=experiment_score%>%group_by(Group,variable)%>%summarise(meanD=mean(value), ci=1.96*sd(value)/(sqrt(n())))

ggplot(experiment_score_summary, aes(x=as.numeric(variable), y=meanD, colour=Group)) + 
  geom_errorbar(aes(ymin=meanD-ci, ymax=meanD+ci), width=.5, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1),size=1) +
  geom_point(position=position_dodge(0.1),size=2)+labs(x="Trial",y="Score")+
  scale_x_continuous(breaks = c(1:8))+
  scale_color_discrete(name="Feedback", labels=c("Tangible","Verbal"))+
  geom_segment(aes(x=4,xend=4,y=3,yend=2.95),color="black",size=0.1)+annotate("text",x=4,y=3.08,label="t=2.99, p=0.003")+
  geom_segment(aes(x=8,xend=8,y=3,yend=2.95),color="black",size=0.1)+annotate("text",x=8,y=3.08,label="t=4.45, p<0.0001")
  


------------------------

raw_data=read.spss(file = "raw data with scores.sav",to.data.frame = T)

raw_data$Pre_zero_TP=raw_data$Pre_zero
raw_data$Post_zero_TP=raw_data$Post_zero
raw_data$Exp_zero_TP=raw_data$Exp_zero

raw_data$Pre_comp_TP=raw_data$Pre_comp
raw_data$Post_comp_TP=raw_data$Post_comp
raw_data$Exp_comp_TP=raw_data$Exp_comp

raw_data$Pre_tens_TP=raw_data$Pre_tension
raw_data$Post_tens_TP=raw_data$Post_tension
raw_data$Exp_tens_TP=raw_data$Exp_tension

raw_data$Pre_zero_FP=0
raw_data$Post_zero_FP=0
raw_data$Exp_zero_FP=0

raw_data$Pre_comp_FP=0
raw_data$Post_comp_FP=0
raw_data$Exp_comp_FP=0

raw_data$Pre_tens_FP=0
raw_data$Post_tens_FP=0
raw_data$Exp_tens_FP=0


for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(11,52,2)){
    if(grepl("zero",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Pre_zero_FP"]=sum
}
for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(11,52,2)){
    if(grepl("comp",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Pre_comp_FP"]=sum
}
for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(11,52,2)){
    if(grepl("tens",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Pre_tens_FP"]=sum
}




for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(174,215,2)){
    if(grepl("zero",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Post_zero_FP"]=sum
}
for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(174,215,2)){
    if(grepl("comp",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Post_comp_FP"]=sum
}
for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(174,215,2)){
    if(grepl("tens",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Post_tens_FP"]=sum
}





for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(53,100,2)){
    if(grepl("zero",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Exp_zero_FP"]=sum
}
for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(53,100,2)){
    if(grepl("comp",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Exp_comp_FP"]=sum
}
for(i in c(1:nrow(raw_data))){
  sum=0
  for(j in seq(53,100,2)){
    if(grepl("tens",raw_data[i,j],ignore.case = T) & grepl("wro",raw_data[i,j+1],ignore.case = T))
      sum=sum+1
  }
  raw_data[i,"Exp_tens_FP"]=sum
}

qplot(data=raw_data,x=Group,y=Post_zero_TP/7,geom="boxplot")
qplot(data=raw_data,x=Group,y=Post_zero_TP/(Post_zero_TP+Post_zero_FP),geom="boxplot")

qplot(data=raw_data,x=Group,y=Post_comp_TP/7,geom="boxplot")
qplot(data=raw_data,x=Group,y=Post_comp_TP/(Post_comp_TP+Post_comp_FP),geom="boxplot")

qplot(data=raw_data,x=Group,y=Post_tens_TP/7,geom="boxplot")
qplot(data=raw_data,x=Group,y=Post_tens_TP/(Post_tens_TP+Post_tens_FP),geom="boxplot")

qplot(data=raw_data,x=Group,y=Exp_zero_TP/7,geom="boxplot")
qplot(data=raw_data,x=Group,y=Exp_zero_TP/(Exp_zero_TP+Exp_zero_FP),geom="boxplot")

qplot(data=raw_data,x=Group,y=Exp_comp_TP/7,geom="boxplot")
qplot(data=raw_data,x=Group,y=Exp_comp_TP/(Exp_comp_TP+Exp_comp_FP),geom="boxplot")

qplot(data=raw_data,x=Group,y=Exp_tens_TP/7,geom="boxplot")
qplot(data=raw_data,x=Group,y=Exp_tens_TP/(Exp_tens_TP+Exp_tens_FP),geom="boxplot")


recog_data=raw_data[,c(1,2,364:381)]
recog_data=melt(recog_data,id.vars = c(1,2))
recog_data$phase=ifelse(grepl("Pre",recog_data$variable),"Pre",ifelse(grepl("Post",recog_data$variable),"Post","Exp"))
recog_data$force=ifelse(grepl("tens",recog_data$variable),"tens",ifelse(grepl("comp",recog_data$variable),"comp","zero"))
recog_data$measure=ifelse(grepl("TP",recog_data$variable),"TP","FP")

recall_data=subset(recog_data,measure=="TP")
recall_data$recall=ifelse(grepl("Exp",recall_data$phase),recall_data$value/8,recall_data$value/7)
levels(recall_data$force)<-c("Compression","Tension","Zero-Force")
qplot(data=recall_data,x = phase,y=recall,geom="boxplot")+facet_wrap(~as.factor(force))+
  scale_x_discrete( labels=c("Pre-Test", "Intervention", "Post-Test"))+labs(x="Phase",y="True Positive Rate")

qplot(data=recall_data,x = force,y=recall,geom="boxplot")+facet_wrap(~as.factor(phase))

recall_data$phase=as.factor(recall_data$phase)
recall_data$force=as.factor(recall_data$force)


summary(multcomp::glht(m,mcp(force="Tukey")))

m=lm(data=recall_data, recall~force+phase+force:phase)
lsmeans(m, list(pairwise ~ force|phase))


precision_data=dcast(recog_data, ID+Group+phase+force~measure,value.var = "value")
precision_data$precision=precision_data$TP/(precision_data$TP+precision_data$FP)
qplot(data=precision_data,x = phase,y=precision,fill=Group,geom="boxplot")+facet_wrap(~as.factor(force))
qplot(data=precision_data,x = force,y=precision,fill=Group,geom="boxplot")+facet_wrap(~as.factor(phase))


precision_recall=merge(recall_data[,c(1,2,5,6,8)],precision_data[,c(1:4,7)],by = c("ID","Group","phase","force"))

qplot(data=precision_recall,x=precision,y=recall,color=Group)+facet_grid(phase~as.factor(force))

recall_data=recall_data%>%group_by(ID,phase)%>%mutate(TN=sum(value)-value)
recall_data$specificity=ifelse(grepl("Exp",recall_data$phase),recall_data$TN/16,recall_data$TN/14)
recall_data=merge(recall_data,DF_Learner,by.x="ID",by.y="Participant")

qplot(data=recall_data,x=1-specificity,y=recall,color=Group)+facet_grid(phase~as.factor(force))+geom_text(aes(label=ID),check_overlap = T)

qplot(data=recall_data,y=1-specificity,x=phase,fill=Group,geom="boxplot")+facet_wrap(~as.factor(force))+
  


change_of_mind=raw_data[,c(1,2,seq(11,52,2),seq(174,215,2))]
change_of_mind=melt(change_of_mind,id.vars = c(1,2))
change_of_mind$phase=ifelse(grepl("Pre",change_of_mind$variable),"Pre",ifelse(grepl("Post",change_of_mind$variable),"Post","Exp"))
change_of_mind$Q=substring(change_of_mind$variable,5)
change_of_mind=change_of_mind%>%group_by(ID,Group,Q)%>%summarise(changing=first(value)!=last(value))%>%summarise(changing=sum(changing))



raw_data_copy=raw_data
for(j in seq(11,52,2)){
  for(i in c(1:nrow(raw_data_copy))){
    if( grepl("right",raw_data_copy[i,j+1],ignore.case = T)){
      raw_data_copy[,j+1]=raw_data_copy[i,j]
    }
      
    }
  
  
}
for(j in seq(174,215,2)){
  for(i in c(1:nrow(raw_data_copy))){
    if( grepl("right",raw_data_copy[i,j+1],ignore.case = T)){
      raw_data_copy[,j+1]=raw_data_copy[i,j]
    }
    
  }
  
  
}
for(j in seq(53,100,2)){
  for(i in c(1:nrow(raw_data_copy))){
    if( grepl("right",raw_data_copy[i,j+1],ignore.case = T)){
      raw_data_copy[,j+1]=raw_data_copy[i,j]
    }
  }
}

confusion_pre_tangible=matrix(data = 0,nrow = 3,ncol = 3)
confusion_pre_verbal=matrix(data=0,nrow = 3,ncol = 3)

for(j in seq(11,52,2)){
  for(i in c(1:nrow(raw_data_copy))){
    if(grepl("tens",raw_data_copy[i,j+1],ignore.case = T))
    {
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[1,1]=confusion_pre_verbal[1,1]+1
        else
          confusion_pre_tangible[1,1]=confusion_pre_tangible[1,1]+1
        
      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[2,1]=confusion_pre_verbal[2,1]+1
        else
          confusion_pre_tangible[2,1]=confusion_pre_tangible[2,1]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[3,1]=confusion_pre_verbal[3,1]+1
        else
          confusion_pre_tangible[3,1]=confusion_pre_tangible[3,1]+1        
      }
    }
    else if(grepl("comp",raw_data_copy[i,j+1],ignore.case = T))
    {
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[1,2]=confusion_pre_verbal[1,2]+1
        else
          confusion_pre_tangible[1,2]=confusion_pre_tangible[1,2]+1
      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[2,2]=confusion_pre_verbal[2,2]+1
        else
          confusion_pre_tangible[2,2]=confusion_pre_tangible[2,2]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[3,2]=confusion_pre_verbal[3,2]+1
        else
          confusion_pre_tangible[3,2]=confusion_pre_tangible[3,2]+1        
      }
    }else{
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[1,3]=confusion_pre_verbal[1,3]+1
        else
          confusion_pre_tangible[1,3]=confusion_pre_tangible[1,3]+1      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[2,3]=confusion_pre_verbal[2,3]+1
        else
          confusion_pre_tangible[2,3]=confusion_pre_tangible[2,3]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_pre_verbal[3,3]=confusion_pre_verbal[3,3]+1
        else
          confusion_pre_tangible[3,3]=confusion_pre_tangible[3,3]+1        
      }
      
    }
  }
}

confusion_post_tangible=matrix(data = 0,nrow = 3,ncol = 3)
confusion_post_verbal=matrix(data=0,nrow = 3,ncol = 3)

for(j in seq(174,215,2)){
  for(i in c(1:nrow(raw_data_copy))){
    if(grepl("tens",raw_data_copy[i,j+1],ignore.case = T))
    {
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[1,1]=confusion_post_verbal[1,1]+1
        else
          confusion_post_tangible[1,1]=confusion_post_tangible[1,1]+1
        
      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[2,1]=confusion_post_verbal[2,1]+1
        else
          confusion_post_tangible[2,1]=confusion_post_tangible[2,1]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[3,1]=confusion_post_verbal[3,1]+1
        else
          confusion_post_tangible[3,1]=confusion_post_tangible[3,1]+1        
      }
    }
    else if(grepl("comp",raw_data_copy[i,j+1],ignore.case = T))
    {
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[1,2]=confusion_post_verbal[1,2]+1
        else
          confusion_post_tangible[1,2]=confusion_post_tangible[1,2]+1
      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[2,2]=confusion_post_verbal[2,2]+1
        else
          confusion_post_tangible[2,2]=confusion_post_tangible[2,2]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[3,2]=confusion_post_verbal[3,2]+1
        else
          confusion_post_tangible[3,2]=confusion_post_tangible[3,2]+1        
      }
    }else{
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[1,3]=confusion_post_verbal[1,3]+1
        else
          confusion_post_tangible[1,3]=confusion_post_tangible[1,3]+1      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[2,3]=confusion_post_verbal[2,3]+1
        else
          confusion_post_tangible[2,3]=confusion_post_tangible[2,3]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_post_verbal[3,3]=confusion_post_verbal[3,3]+1
        else
          confusion_post_tangible[3,3]=confusion_post_tangible[3,3]+1        
      }
      
    }
  }
}


confusion_exp_tangible=matrix(data = 0,nrow = 3,ncol = 3)
confusion_exp_verbal=matrix(data=0,nrow = 3,ncol = 3)

for(j in seq(53,100,2)){
  for(i in c(1:nrow(raw_data_copy))){
    if(grepl("tens",raw_data_copy[i,j+1],ignore.case = T))
    {
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[1,1]=confusion_exp_verbal[1,1]+1
        else
          confusion_exp_tangible[1,1]=confusion_exp_tangible[1,1]+1
        
      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[2,1]=confusion_exp_verbal[2,1]+1
        else
          confusion_exp_tangible[2,1]=confusion_exp_tangible[2,1]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[3,1]=confusion_exp_verbal[3,1]+1
        else
          confusion_exp_tangible[3,1]=confusion_exp_tangible[3,1]+1        
      }
    }
    else if(grepl("comp",raw_data_copy[i,j+1],ignore.case = T))
    {
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[1,2]=confusion_exp_verbal[1,2]+1
        else
          confusion_exp_tangible[1,2]=confusion_exp_tangible[1,2]+1
      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[2,2]=confusion_exp_verbal[2,2]+1
        else
          confusion_exp_tangible[2,2]=confusion_exp_tangible[2,2]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[3,2]=confusion_exp_verbal[3,2]+1
        else
          confusion_exp_tangible[3,2]=confusion_exp_tangible[3,2]+1        
      }
    }else{
      if(grepl("tens",raw_data_copy[i,j],ignore.case = T)){
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[1,3]=confusion_exp_verbal[1,3]+1
        else
          confusion_exp_tangible[1,3]=confusion_exp_tangible[1,3]+1      }
      else     if(grepl("comp",raw_data_copy[i,j],ignore.case = T))
      {
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[2,3]=confusion_exp_verbal[2,3]+1
        else
          confusion_exp_tangible[2,3]=confusion_exp_tangible[2,3]+1        
      }
      else{
        if(grepl("verba",raw_data_copy[i,2],ignore.case = T))
          confusion_exp_verbal[3,3]=confusion_exp_verbal[3,3]+1
        else
          confusion_exp_tangible[3,3]=confusion_exp_tangible[3,3]+1        
      }
      
    }
  }
}

confusion_pre=confusion_pre_tangible+confusion_pre_verbal
confusion_pre=melt(as.data.frame(confusion_pre))
confusion_pre$Correct_answer=as.factor(c("Tension","Compression","Zero-Force","Tension","Compression","Zero-Force","Tension","Compression","Zero-Force"))
confusion_pre$Correct_answer=factor(confusion_pre$Correct_answer, levels = c("Tension","Compression","Zero-Force"))
colnames(confusion_pre)[1]="Given_Answer"
confusion_pre$Given_Answer=factor(c("Tension","Tension","Tension","Compression","Compression","Compression","Zero-Force","Zero-Force","Zero-Force"), levels = c("Tension","Compression","Zero-Force"))
confusion_pre$perc=round(100*confusion_pre$value/168)
qplot(data=confusion_pre,x=Given_Answer,y=Correct_answer,geom="tile",fill=perc)+
  geom_text(aes(label=paste(perc,"%","(",value,")")))+scale_fill_gradient2(low="white",high = "red")+
  theme(axis.text.y= element_text(angle = 90,hjust = 0.5))+theme(legend.position="none")+labs(x="Given Answer",y="Correct Answer")

confusion_post=confusion_post_tangible+confusion_post_verbal
confusion_post=melt(as.data.frame(confusion_post))
confusion_post$Correct_answer=as.factor(c("Tension","Compression","Zero-Force","Tension","Compression","Zero-Force","Tension","Compression","Zero-Force"))
confusion_post$Correct_answer=factor(confusion_post$Correct_answer, levels = c("Tension","Compression","Zero-Force"))
colnames(confusion_post)[1]="Given_Answer"
confusion_post$Given_Answer=factor(c("Tension","Tension","Tension","Compression","Compression","Compression","Zero-Force","Zero-Force","Zero-Force"), levels = c("Tension","Compression","Zero-Force"))
confusion_post$perc=round(100*confusion_post$value/168)
qplot(data=confusion_post,x=Given_Answer,y=Correct_answer,geom="tile",fill=perc)+
  geom_text(aes(label=paste(perc,"%","(",value,")")))+scale_fill_gradient2(low="white",high = "red")+
  theme(axis.text.y= element_text(angle = 90,hjust = 0.5))+theme(legend.position="none")+labs(x="Given Answer",y="Correct Answer")

confusion_exp=confusion_exp_tangible+confusion_exp_verbal
confusion_exp=melt(as.data.frame(confusion_exp))
confusion_exp$Correct_answer=as.factor(c("Tension","Compression","Zero-Force","Tension","Compression","Zero-Force","Tension","Compression","Zero-Force"))
confusion_exp$Correct_answer=factor(confusion_exp$Correct_answer, levels = c("Tension","Compression","Zero-Force"))
colnames(confusion_exp)[1]="Given_Answer"
confusion_exp$Given_Answer=factor(c("Tension","Tension","Tension","Compression","Compression","Compression","Zero-Force","Zero-Force","Zero-Force"), levels = c("Tension","Compression","Zero-Force"))
confusion_exp$perc=round(100*confusion_exp$value/168)
qplot(data=confusion_exp,x=Given_Answer,y=Correct_answer,geom="tile",fill=perc)+
  geom_text(aes(label=paste(perc,"%","(",value,")")))+scale_fill_gradient2(low="white",high = "red")+
  theme(axis.text.y= element_text(angle = 90,hjust = 0.5))+theme(legend.position="none")+labs(x="Given Answer",y="Correct Answer")

----------------------

  
  library(bmp)
  AOI_Codes=read.csv("Models/AOI_codes.csv",header = F,na.strings = "qqqq")
  colnames(AOI_Codes)=c("AOI","r","b")
  img=read.bmp("Models/5.AOI.res.BMP");
  width=dim(img)[1]
  height=dim(img)[2]
  
  test=merge(Saliency_Expert,AOI_Codes,by.x = "primary.AOI",by.y = "AOI" )
  
  saliency_map=matrix(nrow = width,ncol = height,data = 0)
  
  test1=subset(test,Trial==5)
  for(r in c(1:nrow(test1))){
    red=test1[r,"r"]
    blue=test1[r,"b"]
    saliency_map[which(img[,,1]==red & img[,,3]==blue,arr.ind = T)]= test1[r,"meanFixProp"]
  }
  
  colnames(saliency_map)=c(1:height)
  rownames(saliency_map)=c(1:width)
  
  d=melt(saliency_map)
  kde2d(d$Var1,d$Var2,1000*d$value+0.0001)
  library(RColorBrewer)
  rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
  r <- rf(32)
  image(kde2d(d$Var2,d$Var1,1000*d$value+1))
  
  qplot(data=d,x=Var2,y=Var1,color=value)+
    scale_color_gradient(low="white", high="red")
  
  ggplot(d,aes(x=Var2,y=Var1))+
    stat_bin2d(aes(fill=value),geom="identity'") +
    scale_fill_gradient(low="blue", high="green")
  
  library("imager")
  ser=as.cimg(saliency_map)
  d=melt(as.matrix(isoblur(ser,sigma = 10)))
  
  qplot(data=d,x=Var2,y=Var1,fill=value)+ annotation_custom(rasterGrob(readPNG("Models/5.PNG"), interpolate=TRUE,width = unit(1,"npc"), 
                                                                        height = unit(1,"npc")) , xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    scale_fill_gradient2(low="white",mid="yellow", high="red")+scale_y_reverse(limits = c(width,0))+theme_void()+geom_tile(alpha=0.5)+scale_x_continuous(limits = c(0, height))
    
  
  which(img[,,1]<255,arr.ind = T)
  
  
  tmp=aggregate(Event.Duration..ms.~Participant+Trial+Condition+Learner,data=DF_Dwell,FUN=length)
  Saliency=aggregate(Event.Duration..ms.~Participant+Trial+Condition+Learner+primary.AOI,data=DF_Dwell,FUN=length)
  Saliency=merge(Saliency,tmp,by=c("Participant","Trial","Condition","Learner"))
  colnames(Saliency)[6] <- "Dwell"
  colnames(Saliency)[7] <- "Tot"
  Saliency$prop=Saliency$Dwell/Saliency$Tot;
  
  
  Saliency_Condition=aggregate(prop~Trial+Condition+primary.AOI,data=Saliency,FUN=mean)
  
---------------  
  
  

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
