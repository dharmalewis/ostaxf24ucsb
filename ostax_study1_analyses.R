
# STUDY 1 ANALYSIS SCRIPT
library(pacman)
p_load(tidyr, dplyr, jmv, afex, psych, stats, emmeans, effectsize, tidyverse)

study1 <- read.csv("ostax1clean_07-27-2025_with_timing.csv")


### diffs bt groups validation #####
#dems
chi_gender_tbl <- table(study1$Condition, study1$dem_gen)
chisq.test(chi_gender_tbl)

study1 <- study1 %>%
  mutate(dem_race_chi = ifelse(grepl(",", dem_race), "99", dem_race))
chi_race_tbl <- table(study1$Condition, study1$dem_race_chi)
chisq.test(chi_race_tbl)

# prior knowledge
ttestIS(study1,var="likert_skill", group = "Condition", effectSize = T, meanDiff = T, ci = T)
ttestIS(study1,var="chemExpTotal", group = "Condition", effectSize = T, meanDiff = T, ci = T)
ttestIS(study1,var="pretestScore", group = "Condition", effectSize = T, meanDiff = T, ci = T)

#age
study1$dem_age <- as.numeric(study1$dem_age)
ttestIS(study1,var="dem_age", group = "Condition", effectSize = T, meanDiff = T, ci = T)

# total time
ttestIS(study1,var="totLessonTime", group = "Condition", effectSize = T, meanDiff = T, ci = T)
welch_time <- t.test(totLessonTime ~ Condition, data = study1, var.equal = FALSE)
welch_time$p.value

## check scores + timing
jmv::descriptives(
  data    = study1,
  vars    = vars(LessonTimeSummary, LessonTimePQ, LessonTime),
  splitBy = c("Condition"),
  sd      = TRUE,
  min     = T,
  max     = T
)

summary(aov(LessonTime ~ Condition, data = study1))
summary(aov(LessonTimeTotal ~ Condition, data = study1))

leveneTest(LessonTime ~ Condition, data = study1)
welch_time <- t.test(LessonTime ~ Condition, data = study1, var.equal = FALSE)
welch_time$p.value

#### ANOVA Retention ####
summary(aov(postRet ~ Condition, data = study1))
summary(aov(postRet ~ Condition + LessonTimeTotal, data = study1))

eta_squared(aov(postRet ~ Condition, data = study1))

#### ANOVA Retention ####
summary(aov(postTrans~ Condition, data = study1))
summary(aov(postTrans ~ Condition + LessonTimeTotal, data = study1))

eta_squared(aov(postTrans ~ Condition, data = study1))
