# clear all
rm(list=ls())

# loading tools
library(ggplot2) 
library("gridExtra")
library("lme4")
library(plyr) # for collapse-and-mean functions like ddply
library(psych)
library(GPArotation)
library(paran)
library(reshape)
library(polycor)
library(nFactors)
library(R.matlab)
library(reshape)
options(scipen = 999)

# set directory 
setwd("D:/Radicalism_Change_of_Mind/Replication/Questionnaire Data")
#loading data
mdata = readMat("CombinedData_Gorilla.mat")

mydata=mdata$Questionnaire.extract
colnames(mydata, do.NULL = FALSE)
heading<-matrix(0,1,length(mdata$Item.labels2))
for (i in 1:length(mdata$Item.labels2))
{
heading[i]=mdata$Item.labels2[[i]][[1]][,1]
}

colnames(mydata)=heading

CNG=nCng(cor(mydata[1:344,]), cor=TRUE, model="factors", details=TRUE)
number_factors=CNG$nFactors 
CNG$details

fa.results= fa(mydata[1:344,], nfactors =number_factors,rotate = "oblimin", fm="ml")
loadings <- abs(fa.results$loadings)

factorscores=fa.results$scores
factorscores_f1=factorscores[,1]
factorscores_f2=factorscores[,2]
factorscores_f3=factorscores[,3]
hist(factorscores_f1, 
     xlab="Left  -  Political Orientation  -  Right", 
     border="black", 
     col="blue",
     breaks=20)
hist(factorscores_f2)
hist(factorscores_f3)



z_factorscores_f1=scale(factorscores_f1)
z_factorscores_f2=scale(factorscores_f2)
z_factorscores_f3=scale(factorscores_f3)
quadratic_factor1=z_factorscores_f1^2
absolute_factor1=abs(z_factorscores_f1)


fit<-rlm(z_factorscores_f2 ~ z_factorscores_f1+ I(z_factorscores_f1^2))
BIC(fit)
summary(fit)
fit<-rlm(z_factorscores_f2 ~I(z_factorscores_f1^2))
BIC(fit)
summary(fit)
fit<-rlm(z_factorscores_f2 ~ z_factorscores_f1)
BIC(fit)
summary(fit)


fit<-rlm(z_factorscores_f3 ~ z_factorscores_f1+ I(z_factorscores_f1^2))
BIC(fit)
summary(fit)
fit<-rlm(z_factorscores_f3 ~ z_factorscores_f1)
BIC(fit)
summary(fit)
fit<-rlm(z_factorscores_f3 ~  I(z_factorscores_f1^2))
BIC(fit)
summary(fit)

fit<-rlm(z_factorscores_f3 ~ z_factorscores_f2)
summary(fit)



Radicalism=z_factorscores_f3+z_factorscores_f2
fit<-lm(Radicalism ~ z_factorscores_f1+ I(z_factorscores_f1^2))
summary(fit)


colNames=c("PoliticalBelief", "Dogmatism", "Openness", "ID")
compCode<-matrix(0,length(mdata$CompletionCode.save),1)
for (i in 1:length(mdata$CompletionCode.save))
{
compCode[i]=mdata$CompletionCode.save[[i]][[1]][,1]
}

Factorscores_output=matrix(c(z_factorscores_f1[1:1401,1], z_factorscores_f2[1:1401,1],z_factorscores_f3[1:1401,1], mdata$SubjectID[1:1401,1]), nrow=1401, ncol=4)
colnames(Factorscores_output)=colNames
write.table(Factorscores_output, file="D:/Radicalism_Change_of_Mind/Replication/Questionnaire Data/factorscores_combined.csv", sep=",", row.names = FALSE)


loadingsplot<- 0
loadingsplot = data.frame(loadings[,1],loadings[,2],loadings[,3])
loadingsplot$x <- factor(rownames(loadingsplot), levels = rownames(loadingsplot))
Questionnaires=c(rep("Self report",3), rep("Voting",3), rep("SECS",12), rep("RWA",12), rep("LWA",8), rep("Political Issues",9), rep("Belief Superiority",9), rep("Dog",22))
values=c(rep("#999999",3), rep("#E69F00",3), rep("#56B4E9",12), rep("#009E73",12), rep("#F0E442",8), rep("#0072B2",9), rep("#D55E00",9), rep("#CC79A7",22))


a<-ggplot(loadingsplot[1], aes(x = loadingsplot$x, y = loadingsplot$loadings...1, fill=Questionnaires)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9", "#009E73","#F0E442","#0072B2","#D55E00", "#CC79A7"), name="Questionnaires", breaks=c("Self report", "Voting", "SECS", "RWA", "LWA", "Political Issues", "Belief Superiority", "Dog"),labels=c("Political Orientation", "Voting", "SECS", "RWA", "LWA", "Political Issues", "Belief Superiority", "Dogmatism"))+

labs(title="Factor 1: Political Orientation", x="", y = "Loadings") +  theme_classic(base_size = 17) +
  theme(axis.ticks = element_blank() ,axis.text.x = element_blank(),axis.line = element_blank())+
expand_limits(y=c(-.9,.9))


b<-ggplot(loadingsplot[2], aes(x = loadingsplot$x, y = loadingsplot$loadings...2, fill=Questionnaires)) + 
  geom_bar(stat = "identity") + 
scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9", "#009E73","#F0E442","#0072B2","#D55E00", "#CC79A7"), name="Questionnaires", breaks=c("Self report", "Voting", "SECS", "RWA", "LWA", "Political Issues", "Belief Superiority", "Dog"),labels=c("Political Orientation", "Voting", "SECS", "RWA", "LWA", "Political Issues", "Belief Superiority", "Dogmatism"))+
labs(title="Factor 2: Political Radicalism", x="", y = "Loadings") +  theme_classic(base_size = 17)+
  theme(axis.ticks = element_blank() ,axis.text.x = element_blank(),axis.line = element_blank(),legend.title=element_blank())+
expand_limits(y=c(-.9,.9))



c<-ggplot(loadingsplot[3], aes(x = loadingsplot$x, y = loadingsplot$loadings...3, fill=Questionnaires)) + 
  geom_bar(stat = "identity") + 
scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9", "#009E73","#F0E442","#0072B2","#D55E00", "#CC79A7"), name="Questionnaires", breaks=c("Self report", "Voting", "SECS", "RWA", "LWA", "Political Issues", "Belief Superiority", "Dog"),labels=c("Political Orientation", "Voting", "SECS", "RWA", "LWA", "Political Issues", "Belief Superiority", "Dogmatism"))+
labs(title="Factor 3: Open-mindedness/Individualism", x="Questions", y = "Loadings") +  theme_classic(base_size = 17) +
  theme(axis.ticks = element_blank() ,axis.text.x = element_blank(),axis.line = element_blank(),legend.title=element_blank())+
expand_limits(y=c(-.9,.9))

grid.arrange(a,b,c, ncol=1,nrow=3)
