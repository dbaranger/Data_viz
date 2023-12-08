library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(ggpubr)

# Function to make measures unreliable
add.noise = function(x,rel){
  x2 = base::scale(x = (x+stats::rnorm(length(x), 0, sqrt((1-rel)/rel))),
                   center = TRUE, scale = TRUE)
  return(x2)
}
#########################

N=10 # sample size
r1 = .8 # correlation between variables
mean1 = 0 # variable means
mean2 = 2
rel1 = .4 # reliability of the two variables
rel2 = .4

#########################
#Simulate some data
set.seed(9353)
dat = MASS::mvrnorm(n=N, mu=c(0,0), Sigma=matrix(data = c(1,r1,r1,1),nrow=2,byrow = T), empirical=TRUE) %>% as.data.frame()
dat$ID = c(1:N)

# if the data were perfectly reliable, we would have the exact same observation at each visit
dat1 = dat
dat2 = dat

# make less reliable and adjust variable mean
dat1$V1 = add.noise(dat1$V1,rel = rel1) + mean1
dat1$V2 = add.noise(dat1$V2,rel = rel1) + mean2

dat2$V1 = add.noise(dat2$V1,rel = rel1) + mean1
dat2$V2 = add.noise(dat2$V2,rel = rel1) + mean2

# compute difference score
dat1$diff = dat1$V2 - dat1$V1
dat2$diff = dat2$V2 - dat2$V1


########################################################
# Reshape for plotting

dat1a = dat1 %>% dplyr::select(c("ID","V1","V2")) %>% pivot_longer(cols = starts_with("V"),names_to = "Condition",values_to = "V")
dat1a$visit = "Visit 1"

dat2a = dat2 %>% dplyr::select(c("ID","V1","V2")) %>% pivot_longer(cols = starts_with("V"),names_to = "Condition",values_to = "V")
dat2a$visit = "Visit 2"

dat3a = rbind(dat1a,dat2a)
dat3a$Condition = as.factor(dat3a$Condition)
levels(dat3a$Condition) = c("Control","Manipulation")

##########

dat1b = dat1 %>% dplyr::select(c("ID","diff")) 
dat1b$Visit = "Visit 1"

dat2b = dat2 %>% dplyr::select(c("ID","diff"))
dat2b$Visit = "Visit 2"

dat3b = rbind(dat1b,dat2b)

# reliability. yields equivalent results too ICC(3,1)
cor(dat1$V1,dat2$V1)
cor(dat1$V2,dat2$V2)
cor(dat1$diff,dat2$diff)

#ICC(3,1)
# cor(dat1$V1,dat2$V1)
# rptR::rpt(V ~ 1 + (1|ID),grname = "ID",datatype = "Gaussian",data = dat3a %>% dplyr::filter(Condition == "Control"),nboot = 0,npermut = 0)
# cor(dat1$V2,dat2$V2)
# rptR::rpt(V ~ 1 + (1|ID),grname = "ID",datatype = "Gaussian",data = dat3a %>% dplyr::filter(Condition == "Manipulation"),nboot = 0,npermut = 0)
# cor(dat1$diff,dat2$diff)
# rptR::rpt(diff ~ 1 + (1|ID),grname = "ID",datatype = "Gaussian",data = dat3b ,nboot = 0,npermut = 0)



# plot
###############################################
plot1 = ggplot(data = dat3a,aes(x = Condition,y =V,fill =  Condition,color = Condition))+
  scale_color_viridis_d(option = "H",begin = .2,end = .8)+
  scale_fill_viridis_d(option = "H",begin = .2,end = .8)+
  geom_boxplot(color="darkgrey",alpha=.25,show.legend = F)+
  geom_line(inherit.aes = F,aes(x = Condition,y =V, group=ID),size=.75)+
  geom_point(shape=21,position = position_dodge(width = 1),size=2,show.legend = F)+
  facet_wrap(~visit,ncol = 2)+
  ggtitle(label = "Replicable within-subject effects")+
  ylab(label = "Outcome variable")+
  theme_classic()


plot2 = ggplot(data = dat3b,aes(x = Visit,y =diff,fill =  Visit,color = Visit))+
  scale_color_viridis_d(option = "H",begin = .3,end = .7)+
  scale_fill_viridis_d(option = "H",begin = .3,end = .7)+
  geom_boxplot(color="darkgrey",alpha=.25,show.legend = F)+
 # scale_y_continuous(limits = c(-2,5))+
  geom_line(inherit.aes = F,aes(x = Visit,y =diff, group=ID),size=.75)+
  geom_point(shape=21,position = position_dodge(width = 1),size=2,show.legend = F)+
  ggtitle(label = "Do not necessarily yield\nreliable difference scores",subtitle = paste("ICC = ",round(cor(dat1$diff,dat2$diff),2),sep="") )+
  ylab(label = "Difference score (Manipulation - Control)")+
  theme_classic()
plot2


plot3 = ggplot(data = dat3a,aes(x = visit,y =V,fill =  Condition,color = Condition))+
  scale_color_viridis_d(option = "H",begin = .2,end = .8)+
  scale_fill_viridis_d(option = "H",begin = .2,end = .8)+
  geom_boxplot(color="darkgrey",alpha=.25,show.legend = F)+
  geom_line(inherit.aes = F,aes(x = visit,y =V, group=ID),size=.75)+
  geom_point(shape=21,position = position_dodge(width = 1),size=2,show.legend = F)+
  ggtitle(label = "Because the original variable\nmay not be very reliable",subtitle = paste("ICC = ",round((cor(dat1$V1,dat2$V1) + cor(dat1$V2,dat2$V2))/2,2),sep=""))+
  ylab(label = "Outcome variable")+
  xlab(label = "Visit")+
  theme_classic()+
  facet_wrap(~Condition,ncol = 2)
plot3



plot4 = ggarrange(plot1,plot2,plot3,ncol = 3,widths = c(1.5,1,1.5))
plot4


ggsave(plot = plot4,
       filename = "difference_scores.jpeg",dpi=500,width = 10,height = 4) 
