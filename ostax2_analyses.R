# ostax study 2 analyses

library(pacman)
p_load(dplyr, tidyverse, emmeans, car, aov)

study2 <- read.csv("ostax2_163_clean_07-27-2025_with_timing.csv")

### diffs bt groups validation #####

chi_gender_tbl <- table(study2$Condition, study2$dem_gen)
chisq.test(chi_gender_tbl)

# did groups differ on race
study2 <- study2 %>%
  mutate(dem_race_chi = ifelse(grepl(",", dem_race), "99", dem_race))
study2$dem_race_chi[study2$dem_race_chi == ""] <- NA
chi_race_tbl <- table(study2$Condition, study2$dem_race_chi)
chisq.test(chi_race_tbl)

# did groups differ on age

aov_age_o2 <- aov(dem_age~ Condition, data = study2)
summary(aov_age_o2)

# did groups differ on prior knowledge Chem Classes ANOVA 
aov_pk <- aov(prior_knowledge ~ Condition, data = study2)
summary(aov_pk)

# did groups differ on prior knowledge self-report ANOVA and Dunnett's test
aov_pk_selfreport <- aov(likert_skill ~ Condition, data = study2)
summary(aov_pk_selfreport)

# did groups differ on prior knowledge pretest ANOVA and Dunnett's test
aov_pk_pretest <- aov(pretestScore ~ Condition, data = study2)
summary(aov_pk_pretest)

study2 <- ostax2tdf

study2$Condition <- factor(
  study2$Condition,
  levels = c("ControlLesson", "ExptLesson", "ActiveControl"),
  labels = c("Original", "Revised", "Summarizing")
)

jmv::descriptives(
  data    = ostax2tdf,
  vars    = vars(lessonTimeTotal, lessonTime),
  splitBy = c("Condition"),
  sd      = TRUE,
  min     = T,
  max     = T
)

### did groups differ summ time ###

o2_aov_timingtotal <- aov(lessonTimeTotal ~ Condition, data = ostax2tdf)
summary(o2_aov_timingtotal)

TukeyHSD(o2_aov_timingtotal)

### retention AOV 2

summary(aov_ret <- aov(postRet ~ Condition, data = study2))

eta_squared(aov_ret)

summary(aov_ret_time <- aov(postRet ~ Condition + lessonTimeTotal, data = study2))

eta_squared(aov_ret_time)
emmeans_ret <- emmeans(aov_ret_time, ~ Condition)

contrast(emmeans_ret, method = "dunnett", ref = "Original")
contrast(emmeans_ret, method = "dunnett", ref = "Summarizing")

### TRANSFER AOV 2

summary(aov_trans <- aov(postTrans ~ Condition, data = study2))

eta_squared(aov_trans)

summary(aov_trans_time <- aov(postTrans ~ Condition, data = study2))

eta_squared(aov_ret_time)
emmeans_ret <- emmeans(aov_ret_time, ~ Condition)

contrast(emmeans_ret, method = "dunnett ", ref = "Original")
contrast(emmeans_ret, method = "dunnett", ref = "Summarizing")


