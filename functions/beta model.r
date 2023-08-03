library(glmmTMB)
library(car)

source("./functions/diagnostic_fcns.r")
source("./functions/glmm_stability.r")
source("./functions/boot_glmm2.r")
source("./functions/boot_glmmTMB.r")
source("./functions/glmmTMB_stability.r")
source("./functions/drop1_para_glmmtmb.r")
source("./functions/extract_ranef_gmmTMB.r")

## code to run the model

contr<-glmmTMBControl(optCtrl=list(iter.max=100000000, eval.max=100000000))

model.data$caregiver.scaled <- (model.data$caregiver*(length(model.data$caregiver) - 1) + 0.5)/length(model.data$caregiver) 


mm1_duration_owner=glmmTMB(caregiver.scaled ~owner_presence*stranger_presence+z.trial+z.age+sex+
                             (1|subject),
                           data=model.data, family=beta_family, control=contr)

mm1_duration_owner_null=glmmTMB(caregiver.scaled ~z.trial+z.age+sex+
                                  (1|subject),
                                data=model.data, family=beta_family, control=contr)

anova(mm1_duration_owner, mm1_duration_owner_null, test="Chisq")
summary(mm1_duration_owner)

overdisp.test(mm1_duration_owner)

summary(mm1_duration_owner)

drop1(mm1_duration_owner, test="Chisq")
```