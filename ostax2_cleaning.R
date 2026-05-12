# OSTAX 2 Cleaning
# updated 27 july 2025

library(pacman)
p_load(psych, tidyr, jmv, dplyr, tidyverse)

ostax2raw <-read.csv("ostax2_163_raw.csv")
# remove unnecc rows
ostax2raw <- ostax2raw[-(1:2),]

# select columns we need
ostax2raw <- ostax2raw %>% select(-c(StartDate:Progress))
ostax2raw <- ostax2raw %>% select(-c(Finished:consent))
ostax2raw <- ostax2raw %>% select(-c(timer_pg1_First.Click:ac_timer1_Last.Click))
ostax2raw <- ostax2raw %>% select(-c(ac_timer1_Click.Count))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ1_First.Click:ac_timer_summ1_Last.Click))
ostax2raw <- ostax2raw %>% select(-c(ac_timer2_Click.Count))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ1_Click.Count))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ2_First.Click:ac_timer_summ2_Last.Click))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ2_Click.Count:ac_timer3_Page.Submit))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ3_First.Click:ac_timer_summ3_Last.Click))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ3_Click.Count:ac_timer4_Click.Count))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ4_First.Click:ac_timer_summ4_Last.Click))
ostax2raw <- ostax2raw %>% select(-c(ac_timer_summ4_Click.Count))
ostax2raw <- ostax2raw %>% select(-c(Q145_1:Q149_4))


# mark conditions ####

ostax2raw <- ostax2raw %>%
  mutate(Condition = case_when(FL_4_DO %in% c("ControlLesson") ~ "Original",
                              FL_4_DO %in% c("ExptLesson") ~ "Revised",
                             FL_4_DO %in% c("ActiveControl") ~ "ActiveControl",
                              TRUE ~ NA_character_)) 

# summary timing sums #####

ostax2raw$ac_timer_summ1_Page.Submit <- as.numeric(ostax2raw$ac_timer_summ1_Page.Submit)
ostax2raw$ac_timer_summ2_Page.Submit <- as.numeric(ostax2raw$ac_timer_summ2_Page.Submit)
ostax2raw$ac_timer_summ3_Page.Submit <- as.numeric(ostax2raw$ac_timer_summ3_Page.Submit)
ostax2raw$ac_timer_summ4_Page.Submit <- as.numeric(ostax2raw$ac_timer_summ4_Page.Submit)


## timing #### 
table(ostax2raw$Condition)

timer_cols_ostax2 <- c(
  "ac_timer_summ1_Page.Submit",
  "ac_timer_summ2_Page.Submit",
  "ac_timer_summ3_Page.Submit",
  "ac_timer_summ4_Page.Submit"
)

# convert numeric + NA minutes to zero
ostax2raw <- ostax2raw %>%
  mutate(across(all_of(timer_cols_ostax2), ~ as.numeric(.) %>% replace_na(0)))


ostax2raw$ac_summ_time <- rowMeans(
  ostax2raw[, c(
    "ac_timer_summ1_Page.Submit",
    "ac_timer_summ2_Page.Submit",
    "ac_timer_summ3_Page.Submit",
    "ac_timer_summ4_Page.Submit"
  )],
  na.rm = TRUE
)

ostax2raw$lessonTimeTotal <- ostax2raw$lessonTime + ostax2raw$ac_summ_time

# scoring: pretest #####
ostax2raw <- ostax2raw %>%
  mutate(across(pretest1:pretest10, ~ as.numeric(.)))

ostax2raw$pretestScore <- rowSums(ostax2raw[, c("pretest1", "pretest2", "pretest3", "pretest4", "pretest5", 
                                              "pretest6", "pretest7", "pretest8", "pretest9", "pretest10")], 
                                 na.rm = TRUE)

# scoring: postTest
ostax2raw <- ostax2raw %>%
  mutate(across(transfer1:transfer10, ~ as.numeric(.)))

ostax2raw <- ostax2raw %>%
  mutate(across(retention1:retention15, ~ as.numeric(.)))

ostax2raw$postTrans<- rowSums(ostax2raw[, c("transfer1", "transfer2", "transfer3", "transfer4", "transfer5", 
                                          "transfer6", "transfer7", "transfer8", "transfer9", "transfer10")], 
                             na.rm = TRUE)


ostax2raw$postRet <- rowSums(ostax2raw[, c("retention1", "retention2.", "retention3", "retention4", "retention5", 
                                         "retention6", "retention7", "retention8", "retention9", "retention10",
                                         "retention11","retention12", "retention13","retention14","retention15")], 
                            na.rm = TRUE)
ostax2raw$postTotal<- rowSums(ostax2raw[, c("postRet", "postTrans")])


ostax2raw <- ostax2raw %>%
  mutate(across(pretestScore:postTotal, ~ as.numeric(.)))

#take chr values into numeric sum of checked boxes
ostax2raw <- ostax2raw %>%
  mutate(chemexp_ucsb_count = ifelse(
    is.na(chemexp_ucsb) | chemexp_ucsb == "",
    0,
    sapply(strsplit(chemexp_ucsb, ","), length)
  ))

ostax2raw <- ostax2raw %>% 
  mutate(across(
    c(chemexp_hs, chemexp_coll, chemexp_ucsb_count),
    ~ coalesce(as.numeric(.), 0) # convert to numeric otherwise NA = 0
  ))


ostax2raw$chemExpTotal<- rowSums(ostax2raw[,c("chemexp_hs", "chemexp_coll",
                                              "chemexp_ucsb_count")],  na.rm = TRUE)
# API scoring ####

#fac_learn_total MIE_total
ostax2raw <- ostax2raw %>%
  mutate(across(faclearn1:external3_rev, ~ as.numeric(.)))

#delete the two duplicates
ostax2raw <- ostax2raw %>% select(-faclearn9_duplicate)
ostax2raw <- ostax2raw %>% select(-intrinsic5_duplicate)


ostax2raw$credible_total<- rowSums(ostax2raw[, c("cred1", "cred2", "cred3", "cred4")], 
                                    na.rm = TRUE)

ostax2raw$engagement_total<- rowSums(ostax2raw[, c("eng1", "eng2", "eng3")], 
                                         na.rm = TRUE)

#do not have "faclearn9"
ostax2raw$faclearn_total<- rowSums(ostax2raw[, c("faclearn1", "faclearn2", "faclearn3", "faclearn4", 
                                                             "faclearn5", "faclearn6", "faclearn7", "faclearn8",
                                                             "faclearn10", "faclearn11")])

#make interest_total (just 1 interest)
ostax2raw$interest_total <- ostax2raw$interest1

#API total
ostax2raw$API_total<- rowSums(ostax2raw[, c("credible_total", "engagement_total", 
                                                             "interest_total", "faclearn_total")])

ostax2raw$API<- rowMeans(ostax2raw[, c("cred1", "cred2", "cred3", "cred4", "eng1", "eng2", "eng3",
                                       "faclearn1", "faclearn2", "faclearn3", "faclearn4", 
                                       "faclearn5", "faclearn6", "faclearn7", "faclearn8",
                                       "faclearn10", "faclearn11", "interest1")])

# IMI scoring ####

#reverse code for external_rev and intrinsic_rev
ostax2raw$external1_rev <- 6 - ostax2raw$external1_rev
ostax2raw$external2_rev <- 6 - ostax2raw$external2_rev
ostax2raw$external3_rev <- 6 - ostax2raw$external3_rev
ostax2raw$intrinsic2_rev <- 6 - ostax2raw$intrinsic2_rev
ostax2raw$intrinsic6_rev <- 6 - ostax2raw$intrinsic6_rev

ostax2raw$external_total<- rowSums(ostax2raw[, c("external1_rev", "external2_rev", "external3_rev")], 
                                   na.rm = TRUE)

#do not have "intrinsic5"
ostax2raw$intrinsic_total<- rowSums(ostax2raw[, c("intrinsic1", "intrinsic2_rev", "intrinsic3", 
                                                  "intrinsic4", "intrinsic6_rev")], 
                                    na.rm = TRUE)

ostax2raw$rawsumIMI <- ostax2raw$intrinsic_total + ostax2raw$external_total
# total IMI = 8 items; IMI is means
ostax2raw$IMI <- rowMeans(ostax2raw[, c("external1_rev", "external2_rev", "external3_rev",
                                        "intrinsic1", "intrinsic2_rev", "intrinsic3", 
                                        "intrinsic4", "intrinsic6_rev")], 
                                                 na.rm = TRUE)


# renaming and restructuring for cleanliness ####

ostax2raw$Condition <- ostax2raw$FL_4_DO

ostax2raw$prior_knowledge <- ostax2raw$chemExpTotal

### remove ppl who did not complete lesson
ostax2raw <- ostax2raw %>%
  filter(!is.na(Condition), Condition != "") %>%
  droplevels()


### save all above as new df csv ####

write_csv(ostax2raw, "/Users/dharmalewis/Desktop/MAYER LAB/OSTAX study/all-data/ostax2_163_clean_07-27-2025_with_timing.csv")


