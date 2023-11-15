# load packages needed:
library(lme4)
library(tidyverse)
#data preparation (needs to be adjusted to the data set)
xdata<-raw.data %>%
  group_by(subject, condition, session, trial, Behaviour)%>%
  summarise(sum_duration=sum(duration))%>% #aggregating durations per behavioral category within each trial
  ungroup()%>%
  complete(subject, condition, session, trial, Behaviour, fill=list(sum.duration=0))%>% #fill in 0s



# z-transformation of covariates
xdata$z.age <- as.vector(scale(xdata$age))
xdata$z.trial <- as.vector(scale(as.numeric(xdata$trial_w_session)))
xdata$z.session <- as.vector(scale(as.numeric(xdata$session)))

# dummy code factors
xdata$condition <- as.factor(xdata$condition)
xdata$condition.dummy <- as.numeric(xdata$condition == levels(xdata$condition)[2])

# center condition for random slopes:
xdata$condition.c <- as.numeric(xdata$condition) - mean(as.numeric(xdata$condition))

# define control structure to make convergence more likely:
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))


#analysis of binary peeking response
model1<-glmer(
  peeking ~ condition + z.trial + z.session + (1 + condition.c + z.trial + z.session |
                                                 subj.id),
  data = xdata,
  family = binomial,
  control = contr
)