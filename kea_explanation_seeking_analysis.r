# load packages needed:
library(lme4)
library(tidyverse)
library(summarytools)
#read in data

raw.data<-read.csv("data/ALL SUBJECTS Explanation Seeking Coding 17.1.2023 csv.csv")
cb.data<-read.csv("data/counter balancing trials.csv")%>%
  select(-Date)%>%
  filter(Condition != "Refresher")

cb.data2<-expand.grid(Subject=levels(as.factor(cb.data$Subject)), Trial=c(4,5), Session=c(1:8), Behavior=c("Right side peek", "Peek on top", "Peephole peek"))

#calculate duration of each trial
trialdurationdata<-raw.data %>%
  filter(Behavior == "Food to end of trial") %>%
  rename(Condition = Subject.1, Trial_Duration = Duration..s.) %>%
  mutate(Trial = as.numeric(ifelse(Trial.Order == 1 & Condition == "Functional", 4, 
                                   ifelse(Trial.Order == 2 & Condition == "Functional", 5,
                                          ifelse(Trial.Order == 1 & Condition == "Non-functional", 5,
                                                 ifelse(Trial.Order == 2 & Condition == "Non-functional", 4,""))))))%>%
  select(Subject, Condition, Session, Trial,  Trial_Duration)

table(trialdurationdata$Subject,trialdurationdata$Condition)
table(cb.data$Subject,cb.data$Condition)

#data preparation (summed up peeks, trial duration, added 0s, proportion peeking)
xdata<-raw.data %>%
  filter(Behavioral.category == "Peeking" | (Behavioral.category == "Proximity" & Behavior == "Right box (side)")) %>%
  rename(Condition = Subject.1) %>%
  mutate(Trial = as.numeric(ifelse(Trial.Order == 1 & Condition == "Functional", 4, 
                        ifelse(Trial.Order == 2 & Condition == "Functional", 5,
                               ifelse(Trial.Order == 1 & Condition == "Non-functional", 5,
                                      ifelse(Trial.Order == 2 & Condition == "Non-functional", 4,""))))))%>%
  group_by(Subject, Session, Trial, Condition)%>% #Subject.1 means trial type (func or non func)
  summarise(sum_peeking=sum(Duration..s.))%>% #aggregating durations per behavioral category within each session
  ungroup()%>%
  full_join(cb.data)%>%
  complete(Subject, Session, Trial,  fill=list(sum_peeking=0))%>% #fill in 0s
  full_join(trialdurationdata)%>%
  mutate(Proportionpeeking = sum_peeking / Trial_Duration,
         peeking_binary= as.numeric(ifelse(sum_peeking > 0, 1, 0)))



#data frame (types of peeking split up)
peekingdata<-raw.data %>%
  filter(Behavioral.category == "Peeking" | (Behavioral.category == "Proximity" & Behavior == "Right box (side)")) %>%
 # mutate(Behavior = fct_recode(as.factor(Behavior), "Right side peek"= "Right box (side)"))%>%
  rename(Condition = Subject.1) %>%
  mutate(Trial = as.numeric(ifelse(Trial.Order == 1 & Condition == "Functional", 4,
                                   ifelse(Trial.Order == 2 & Condition == "Functional", 5,
                                          ifelse(Trial.Order == 1 & Condition == "Non-functional", 5,
                                                 ifelse(Trial.Order == 2 & Condition == "Non-functional", 4,""))))))%>%
  group_by(Subject, Session, Trial, Condition, Behavior)%>%
  summarise(sum_peeking=sum(Duration..s.))%>% #aggregating durations per behavioral category within each session
  ungroup()%>%
  full_join(cb.data2)%>%
  complete(Subject, Session, Trial, Behavior,  fill=list(sum_peeking=0))%>% #fill in 0s
  select(-Condition)%>%
  full_join(cb.data)%>%
  full_join(trialdurationdata)%>%
  mutate(Proportionpeeking = sum_peeking / Trial_Duration,
         peeking_binary= as.numeric(ifelse(sum_peeking > 0, 1, 0)))

view(dfSummary(peekingdata)) #data fram summary of peeking data


xdata%>%filter(is.na(Proportionpeeking))
view(dfSummary(xdata)) #data fram summary of xdata

#Plotting of mean peeking

xdata.agg2<-xdata %>%
  group_by(Subject, Condition) %>%
  summarise(mean.peeking = mean(Proportionpeeking), count = sum(Proportionpeeking > 0))

write.csv(xdata.agg2, file = "Saves/Overall Peeking Table.csv")


xdata.agg<-xdata %>%
  group_by(Subject, Condition) %>%
  summarise(mean.peeking = mean(Proportionpeeking), count = sum(Proportionpeeking > 0))

xdata.agg%>%
  group_by(Condition)%>%
  summarise(sum(count>0), sum(count==0))


xdata.agg$condition2 <- jitter(as.numeric(as.factor(xdata.agg$Condition), amount = .0001))

ggplot(data = xdata.agg, aes(x = Condition, y = mean.peeking)) +
  geom_boxplot(outlier.colour = "white")+ 
  geom_line(aes(x = condition2, group = Subject), color = "darkgray", lty = 1, alpha = .3) +
  geom_point(aes(x = condition2))+
  theme_classic()#+


#plotting of peeking type
peekingdata.agg<-peekingdata %>%
  group_by(Subject, Condition, Behavior) %>%
  summarise(mean.peeking = mean(Proportionpeeking))%>%
  mutate(Behavior = fct_recode(Behavior, "Time Spent on Right Side" = "Right box (side)"))


peekingdataplot<-ggplot(data = peekingdata.agg, aes(x = Condition, y = mean.peeking)) +
  geom_boxplot(outlier.colour = "white")+ 
  geom_jitter(width=0.3)+
  theme_bw()+
  facet_wrap(~Behavior, scales = "free_y")+
  ylab("Average Peeking Behavior")+
  xlab("")

peekingdataplot
ggsave(peekingdataplot, file="Graphics/Overall Peeking Data Plot.png", width = 10, height = 10, scale = 0.6)
#overall peeking analysis

# z-transformation of covariates

xdata$z.trial <- as.vector(scale(as.numeric(xdata$Trial)))
xdata$z.session <- as.vector(scale(as.numeric(xdata$Session)))

# dummy code factors
xdata$Condition <- as.factor(xdata$Condition)
xdata$condition.dummy <- as.numeric(xdata$Condition == levels(xdata$Condition)[2])

# center condition for random slopes:
xdata$condition.c <- as.numeric(xdata$Condition) - mean(as.numeric(xdata$Condition))

# define control structure to make convergence more likely:
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))


#analysis of binary peeking response
peekingmodel<-glmer(
  peeking_binary ~ Condition + z.trial + z.session + (1 + condition.c + z.trial + z.session | Subject),
  data = xdata,
  family = binomial,
  control = contr
)

drop1(peekingmodel, test = "Chisq")
summary(peekingmodel)

# right side peek analysis
rightsidepeekdata <- peekingdata%>%
  filter(Behavior == "Right side peek")

xdata.agg3<-peekingdata %>%
  filter(Behavior == "Right box (side)")%>%
  group_by(Subject, Condition) %>%
  summarise(mean.peeking = mean(Proportionpeeking), count = sum(Proportionpeeking > 0))

xdata.agg4<-peekingdata %>%
  filter(Behavior == "Right box (side)")%>%
  group_by(Subject, Condition) %>%
  summarise(mean.peeking = mean(Proportionpeeking), count = sum(Proportionpeeking > 0))

xdata.agg4%>%
  group_by(Condition)%>%
  summarise(sum(count>0), sum(count==0))

write.csv(xdata.agg3, file = "Saves/Time Spent Right Side Table.csv")

  
# z-transformation of covariates

rightsidepeekdata$z.trial <- as.vector(scale(as.numeric(rightsidepeekdata$Trial)))
rightsidepeekdata$z.session <- as.vector(scale(as.numeric(rightsidepeekdata$Session)))

# dummy code factors
rightsidepeekdata$Condition <- as.factor(rightsidepeekdata$Condition)
rightsidepeekdata$condition.dummy <- as.numeric(rightsidepeekdata$Condition == levels(rightsidepeekdata$Condition)[2])

# center condition for random slopes:
rightsidepeekdata$condition.c <- as.numeric(rightsidepeekdata$Condition) - mean(as.numeric(rightsidepeekdata$Condition))

# define control structure to make convergence more likely:
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))


# analysis of binary peeking response
peekingmodel<-glmer(
  peeking_binary ~ Condition + z.trial + z.session + (1 + condition.c + z.trial + z.session | Subject),
  data = rightsidepeekdata,
  family = binomial,
  control = contr
)

drop1(peekingmodel, test = "Chisq")
summary(peekingmodel)


# analysis of proportional peeking response
library(glmmTMB)
library(car)

source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
#source("./functions/boot_glmm2.r")
source("./functions/boot_glmmTMB.r")
source("./functions/glmmTMB_stability.r")
source("./functions/drop1_para_glmmtmb.r")
source("./functions/extract_ranef_gmmTMB.r")

## code to run the model

contr<-glmmTMBControl(optCtrl=list(iter.max=100000000, eval.max=100000000))
xdata$Proportionpeeking_scaled <- (xdata$Proportionpeeking*(length(xdata$Proportionpeeking) - 1) + 0.5)/length(xdata$Proportionpeeking) 
min(xdata$Proportionpeeking_scaled)
max(xdata$Proportionpeeking_scaled)
max(xdata$Proportionpeeking)

# analysis of binary peeking response
peekingmodel_beta<-glmmTMB(
  Proportionpeeking_scaled ~ Condition + z.trial + z.session + 
    (1| Subject)+
    (0 + condition.c | Subject)+(0 +z.trial | Subject)+(0 +  z.session | Subject),
  data = xdata,
  family = beta_family,
  control = contr
)



overdisp.test(peekingmodel_beta)

summary(peekingmodel_beta)

drop1(peekingmodel_beta, test="Chisq")


# analysis of right side peek proportion

rightsidepeekdata$Proportionpeeking_scaled <- (rightsidepeekdata$Proportionpeeking*(length(rightsidepeekdata$Proportionpeeking) - 1) + 0.5)/length(rightsidepeekdata$Proportionpeeking) 
min(rightsidepeekdata$Proportionpeeking_scaled)
max(rightsidepeekdata$Proportionpeeking_scaled)
max(rightsidepeekdata$Proportionpeeking)

# analysis of binary peeking response
rightsidepeekmodel_beta<-glmmTMB(
  Proportionpeeking_scaled ~ Condition + z.trial + z.session + 
    (1| Subject)+
    (0 + condition.c | Subject)+(0 +z.trial | Subject)+(0 +  z.session | Subject),
  data = rightsidepeekdata,
  family = beta_family,
  control = contr
)



overdisp.test(rightsidepeekmodel_beta)

summary(rightsidepeekmodel_beta)

drop1(rightsidepeekmodel_beta, test="Chisq")

