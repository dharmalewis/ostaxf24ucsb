#OSTAX STUDY 1  cleaning script
# updated 27 july 2025; working
library(pacman)
p_load(psych, tidyr, jmv, dplyr, emmeans, afex, stats, tidyverse)

ostax1 <- read.csv("ostax_raw_F24.csv")

# view(ostax1$FL_13_DO)
# colnames(ostax1)
# Recode chemExpTotal, totLessonTime
ostax1 <- ostax1 %>%
  mutate(Condition = case_when(
    FL_13_DO %in% c("ControlLesson") ~ "Original",
    FL_13_DO %in% c("ExptLesson") ~ "Revised",
    TRUE ~ NA_character_  # Handle unexpected values if necessary
  ))
#troubleshoot mismatch lengths
# nrow(ostax1)  # Number of rows in the data frame
# length(ostax1$FL_13_DO)  # Length of the column


#remove participant w/o consent and/or did not follow instruction (see pt log)
# first useless qualtrics rows
# then pt/test trial data: 39, 50 where 999 was used in consent signature
# total recruited was 192
# finally remove pt with initials: ywl, ES, VS, NP(where control-cond was actually exptl)
# shouldve used by by the ResponseId column
  ##  initials: LK, R.E.P, JQ, SC, ME, RAC, M.S.
## 6 pts total in construction
ostax1 <- ostax1[-(1:2),]
ostax1 <- ostax1[!grepl("^-?\\d+$", ostax1$consent), ] # removes pos AND neg digit vals
ostax1 <- ostax1[!(ostax1$consent %in% c('ywl', 'EAS', 'VS', 'NP')), ]

#remove useless col
ostax1 <- ostax1 %>% select(-c(StartDate:consent))

# Col: pretest final raw score
ostax1 <- ostax1 %>%
  mutate(across(pretest1:pretest10, ~ as.numeric(.)))

# causes error idk why
# ostax1 %>% 
#   mutate(across(pretest1:pretest10, ~ replace_na(., 0))) 

ostax1$pretestScore <- rowSums(ostax1[, c("pretest1", "pretest2", "pretest3", "pretest4", "pretest5", 
                                              "pretest6", "pretest7", "pretest8", "pretest9", "pretest10")], 
                                 na.rm = TRUE)

# Col: posttest transfer, retention, total
ostax1 <- ostax1 %>%
  mutate(across(transfer1:transfer10, ~ as.numeric(.)))

ostax1 %>% 
  mutate(across(transfer1:transfer10, ~ replace_na(., 0))) 

ostax1 <- ostax1 %>%
  mutate(across(retention1:retention15, ~ as.numeric(.)))

ostax1 %>% 
  mutate(across(retention1:retention15, ~ replace_na(., 0))) 


ostax1$postTrans<- rowSums(ostax1[, c("transfer1", "transfer2", "transfer3", "transfer4", "transfer5", 
                                              "transfer6", "transfer7", "transfer8", "transfer9", "transfer10")], 
                                 na.rm = TRUE)

ostax1$postRet <- rowSums(ostax1[, c("retention1", "retention2.", "retention3", "retention4", "retention5", 
                                          "retention6", "retention7", "retention8", "retention9", "retention10",
                                        "retention11","retention12", "retention13","retention14","retention15")], 
                             na.rm = TRUE)
ostax1$postTotal<- rowSums(ostax1[, c("postRet", "postTrans")])

#Col: chem experience in qtrs

ostax1 <- ostax1 %>%
  mutate(across(chemexp_hs:chem_experience_ucsb, ~ as.numeric(.)))

ostax1$chemExpTotal<- rowSums(ostax1[,c("chemexp_hs", "chem_experience_ucsb")],  na.rm = TRUE)

sort(ostax1$chemExpTotal, decreasing = T)

#### change 2 factors ####

ostax1 <- ostax1 %>%
  mutate(across(pretestScore:postTotal, ~ as.numeric(.)))

# across() needs conditional logic
ostax1 <- ostax1 %>%
  mutate(across(
    all_of(setdiff(
      names(select(., timer1_First.Click:timer_compcheck_Page.Submit)),
      c("expt_summary_pg1", "expt_summary_pg2", "expt_summary_pg3", "expt_summary_pg4", 
        "expt_summary_pg5", "expt_summary_pg6")
    )),
    ~ as.numeric(.)
  ))

# change str() types ####
ostax1$Condition <- as.factor(ostax1$Condition)
ostax1$likert_skill <- as.numeric(ostax1$likert_skill)


timer_cols <-
    c("timer_pg1_Page.Submit", "timer_pg2_Page.Submit", "timer_pg3_Page.Submit",
      "timer_pg4_Page.Submit", "timer_pg5_Page.Submit", 
      "timer_pg6_Page.Submit", "timer_pg7_Page.Submit",
      "timer1_Page.Submit", "timer2_Page.Submit", 
      "timer3_Page.Submit", "timer4_Page.Submit",
      "timer_summ_pg1_Page.Submit", "timer_summ_pg2_Page.Submit",
      "timer_summ_pg4_Page.Submit", # pg 3 summary glitch- no timer
      "timer_summ_pg5_Page.Submit", "timer_summ_pg6_Page.Submit",
      "timer_compcheck_Page.Submit")

ostax1 <- ostax1 %>%
  mutate(across(all_of(timer_cols), ~ as.numeric(.) %>% replace_na(0)))


# total time spent on lesson
ostax1$LessonTime<-rowSums(ostax1[, c("timer_pg1_Page.Submit", "timer_pg2_Page.Submit", "timer_pg3_Page.Submit",
                                           "timer_pg4_Page.Submit", "timer_pg5_Page.Submit", 
                                           "timer_pg6_Page.Submit", "timer_pg7_Page.Submit",
                                           "timer1_Page.Submit", "timer2_Page.Submit", 
                                           "timer3_Page.Submit", 
                                           "timer4_Page.Submit"
                                           
)], 
na.rm = TRUE)


# total time spent on SUMMARY
ostax1$LessonTimeSummary<-rowSums(ostax1[, c(
                                    "timer_summ_pg1_Page.Submit", "timer_summ_pg2_Page.Submit",
                                    "timer_summ_pg4_Page.Submit", # pg 3 summary glitch- no timer
                                    "timer_summ_pg5_Page.Submit", "timer_summ_pg6_Page.Submit",
                                    "timer_compcheck_Page.Submit"
                                  )], 
                       na.rm = TRUE)


ostax1$LessonTimeTotal <- ostax1$LessonTimeSummary + ostax1$LessonTime


write.csv(ostax1,"~/Desktop/MAYER LAB/OSTAX study/all-data/ostax1clean_07-27-2025_with_timing.csv", row.names = FALSE)
# 
