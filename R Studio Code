#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Mental Healthcare use of adult Australian women with police contact for intimate partner abuse perpetration: a case-linkage study  

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### I) Table of Contents 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# I: Table of Contents 
#
# 1: Packages
#
# 2: Data cleaning and wrangling 
#     2.1: Data
#     2.2: Set exclusion criteria and separate missing cases 
#     2.3: Recode relevant cases 
#     2.4: Sample characteristics/descriptives
#
#     MENTAL HEALTH SERVICE USE (MHSU) OF WOMEN EXPOSED TO IPA COMPARED TO WOMEN IN THE GENERAL POPULATION 
#
# 3:  Acute Public Mental Health Service Use Comparisons 
#     3.1: Clean VAHI data (general population dataset)  
#     3.2: Covert count variables to numeric 
#     3.3: Summarise dataset to aggregate number from all programs for each demographic
#     3.4: Understand data distribution, identify & remove outliers 
#     3.5: Create equivalent IPA victims dataframe (i.e. dataset containing same variables as VAHI data)
#     3.5: Insert population numbers into VAHI dataset 
#     3.7: Bind IPA victims dataset with VAHI dataset & ensure variable names are identical between both datasets)
#     3.8: Calculate rates of acute MHSU for both VAHI and IPA victims datasets
#
# 4:  Outpatient Public Mental Health Service Use Comparisons
#     4.1: Clean VAHI data   
#     4.2: Covert count variables to numeric 
#     4.3: Summarise dataset to aggregate number from all programs for each demographic
#     4.4: Understand data distribution, identify & remove outliers 
#     4.5: Create equivalent IPA victims dataframe (i.e. dataset containing same variables as VAHI data)
#     4.5: Insert population numbers into VAHI dataset 
#     4.7: Bind IPA victims dataset with VAHI dataset & ensure variable names are identical between both datasets
#     4.8: Calculate rates of acute MHSU for both VAHI and IPA victims datasets
#
#     LOGISTIC REGRESSION ANALYSES 
#
# 5:  Within Group Analysis - Acute Mental Health Service Use 
#     5.1: Set data distribution ready for regression
#     5.2: Q1: Controlling for index age, what is the odds of acute MHSU in women who have perpetrated IPA?
#     5.3: Q2: Controlling for index age, how does prior FV perpetration predict acute MHSU in sample?
#     5.4: Q3: Controlling for index age & prior general violence perpetration, how does historical FV perpetration predict acute MHSU? 
#     5.5: Q4: Controlling for index age, how does prior FV victimisation predict acute MHSU?
#     5.6: Q5: Controlling for index age and general crime victimisation, how does prior FV victimisation predict acute MHSU?  
#
# 6:  Within Group Analysis - Outpatient Mental Health Service Use
#     6.1: Q6: Controlling for index age, what is the odds of outpatient MHSU in women who have perpetrated IPA?
#     6.2: Q7: Controlling for index age, how does prior FV perpetration predict outpatient MHSU in sample?
#     6.3: Q8: Controlling for index age & prior general violence perpetration, how does historical FV perpetration predict outpatient MHSU? 
#     6.4: Q9: Controlling for index age, how does prior FV victimisation predict outpatient MHSU?
#     6.5: Q10: Controlling for index age and general crime victimisation, how does prior FV victimisation predict outpatient MHSU?  
#
# 7:  Within Group Analysis - Combined Mental Health Service Use 
#     7.1: Q11: Controlling for index age, what is the odds of combined MHSU in women who have perpetrated IPA?
#     7.2: Q12: Controlling for index age, how does prior FV perpetration predict combined MHSU in sample?
#     7.3: Q13: Controlling for index age & prior general violence perpetration, how does historical FV perpetration predict combined MHSU? 
#     7.4: Q14: Controlling for index age, how does prior FV victimisation predict combined MHSU?
#     7.5: Q15: Controlling for index age and general crime victimisation, how does prior FV victimisation predict combined MHSU?  

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 1) Packages ----------------------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

library(tidyverse)
library(Hmisc)
library(psych)
library(readxl)
library(MASS)
library(AER)
library(rms)
library(tidyselect)
library(dplyr)
library(sjPlot)
library(sjstats)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2) Data cleaning and wrangling ---------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.1) Data ------------------------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Import data 

setwd("G:/Projects/Maddie B analysis")
sp<-read.csv("SUPPRESSED.csv")
rels<-read.csv("G:/Projects/Maddie B analysis/SUPPRESSED.csv")
str(rels)

rels<-rels%>%dplyr::select(RQ494_CLUSTER_ID1,index_relation_recode)

sp<-left_join(sp,rels)

# Read csv file 
analysis_data<-read.csv("G:/Projects/Maddie B analysis/SUPPRESSED.csv")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.2) Set exclusion criteria and separate missing cases ---------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


sp_fems<-sp%>%filter(index_sex=="F")
sp_fems_IPA<-sp_fems%>%filter(index_relation_recode=="IPA")
sp_fems_IPAperps<-sp_fems_IPA%>%filter(Cluster_id_type=="Respondent")

# Clean data to remove constant & victim only variables
sp_fems_IPAperps<-sp_fems_IPAperps%>%dplyr::select(-MHSU_ODSA_outpatient_public_community_cat_Before.index, -MHSU_ODSA_outpatient_public_community_cat_post.index, -MHSU_ODSA_outpatient_public_community_cat, -ODS_D_Dissoc_somato_other_neuro_disors, -ODS_D_eating_disors, 
                                                   -ODS_D_non_organic_sleep_disors, -ODS_D_Paranoid_person_disor, -ODS_D_Schizoid_person_disor, -ODS_D_Anankastic_person_disor, -ODS_D_Axious_avoid_person_disor, -ODS_D_Mixed_person_disor, -ODS_D_Enduring_person_changes, 
                                                   -ODS_D_Habit_impulse_disors, -ODS_D_Mental_retardation, -ODS_D_Disordered_psych_develop, -ODS_D_Pervasive_develop_disors,
                                                   -ODS_D_Hyperkinetic_disors, -ODS_D_Opposit_defian_disor, -ODS_D_Childhood_anxiet_disors, -hist_sev_iafm_resp_fvi, -hist_not_sev_iafm_resp_fvi, -hist_general_perp_count_iafm, -hist_general_perp_iafm, -hist_any_perp_iafm,
                                                   -num_fvi_charge_vic, -num_fvi_nocharge_vic)

sp_fems_IPAperps<-sp_fems_IPAperps%>%dplyr::select(-contains("iafm"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.3) Recode relevant variables ---------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Recode 'acute MHSU' to binary 
sp_fems_IPAperps$bin_acute_MHSU<-ifelse(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_lifetime>0,"Used MHS","Did not use MHS")
sp_fems_IPAperps$bin_acute_MHSU<-ifelse(sp_fems_IPAperps$bin_acute_MHSU=="Used MHS",1,0)

# Recode 'outpatient/community MHSU' to binary 
sp_fems_IPAperps$bin_outpatient_MHSU<-ifelse(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_lifetime>0,"Used MHS","Did not use MHS")
sp_fems_IPAperps$bin_outpatient_MHSU<-ifelse(sp_fems_IPAperps$bin_outpatient_MHSU=="Used MHS",1,0)

# Create variable for combined MHSU 
sp_fems_IPAperps <- sp_fems_IPAperps %>%
  mutate(combined_MHSU_lifetime = MHSU_Acute_intensive_cat_total_lifetime + MHSU_outpatient_public_community_cat_total_lifetime)

# Confirm this is the sum of acute and outpatient 
sum(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_lifetime) # total number of contacts - acute
sum(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_lifetime) # total number of contacts - community
sum(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_lifetime, sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_lifetime) # total number of contacts - community and acute
sum(sp_fems_IPAperps$combined_MHSU_lifetime) # total number of contacts- combined variable

# Recode 'combined MHSU' binary 
sp_fems_IPAperps$bin_combined_MHSU<-ifelse(sp_fems_IPAperps$combined_MHSU_lifetime>0,"Used MHS","Did not use MHS")
sp_fems_IPAperps$bin_combined_MHSU<-ifelse(sp_fems_IPAperps$combined_MHSU_lifetime=="Used MHS",1,0)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 2.4) Sample Characteristics/descriptives -----------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Provide the mean, maximum, minimum, SD of age at time of index for female IPA perpetration  
table(sp_fems_IPAperps$index_age, useNA = "always")
describe(sp_fems_IPAperps$index_age)
summary(sp_fems_IPAperps$index_age)
sd <- sd(sp_fems_IPAperps$index_age, na.rm = TRUE)
print(sd)
ggplot(sp_fems_IPAperps, aes(x=index_age)) + geom_histogram(bins = 100) + xlab("Index Age") + ylab("Number of Perpetrators") +
  scale_y_continuous(breaks =c(0,2,5,8))

# Provide the prevalence of lifetime acute, outpatient & combined lifetime MHSU in female IPA users
describe(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_lifetime)
table(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_lifetime>0, useNA = "always")

describe(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_lifetime)
table(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_lifetime>0, useNA = "always")
describe(sp_fems_IPAperps$combined_MHSU_lifetime)
table(sp_fems_IPAperps$combined_MHSU_lifetime>0, useNA = "always")

# Provide the prevalence of post-index acute & outpatient MHSU IN female IPA users 
describe(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index)
table(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index>0, useNA = "always")
describe(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index)
table(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index>0, useNA = "always")

## Explore victimisation history prior to index IPA perpetration 

# Police-recorded general crime victimisation   
table(sp_fems_IPAperps$hist_general_vic_iresp, useNA = "always")
prop.table(table(sp_fems_IPAperps$hist_general_vic_iresp))*100
describe(sp_fems_IPAperps$hist_general_vic_iresp)
table(sp_fems_IPAperps$hist_general_vic_iresp>0)
ggplot(sp_fems_IPAperps, aes(x=hist_general_vic_iresp)) + geom_histogram()

# Police-recorded FV victimisation 
table(sp_fems_IPAperps$num_hist_vic, useNA = "always")
prop.table(table(sp_fems_IPAperps$num_hist_vic, useNA = "always"))*100
describe(sp_fems_IPAperps$num_hist_vic)
table(sp_fems_IPAperps$num_hist_vic>0)
ggplot(sp_fems_IPAperps, aes(x=num_hist_vic)) + geom_histogram()


## Explore perpetration history prior to index IPA perpetration 

# Police-recorded general crime perpetration   
table(sp_fems_IPAperps$hist_general_perp_iresp, useNA = "always")
prop.table(table(sp_fems_IPAperps$hist_general_perp_iresp))*100
describe(sp_fems_IPAperps$hist_general_perp_iresp)
table(sp_fems_IPAperps$hist_general_perp_iresp>0)
ggplot(sp_fems_IPAperps, aes(x=hist_general_perp_iresp)) + geom_histogram()

# Police-recorded family Violence perpetration 
table(sp_fems_IPAperps$hist_FVI_as_resp, useNA = "always")
prop.table(table(sp_fems_IPAperps$hist_FVI_as_resp))*100
describe(sp_fems_IPAperps$hist_FVI_as_resp)
table(sp_fems_IPAperps$hist_FVI_as_resp>0)
ggplot(sp_fems_IPAperps, aes(x=hist_FVI_as_resp)) + geom_histogram()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 3) General Population Comparisons - Acute Public Mental Health Services ----

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-####################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-####################

VAHI_adm<-read_excel("G:/raw_data/SUPPRESSED.xlsx", sheet = x, skip = x)

### 3.1) Clean General Population (VAHI) Data  ---------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

glimpse(VAHI_adm)
str(VAHI_adm)
table(VAHI_adm$Program)
table(VAHI_adm$Sex)
table(VAHI_adm$Admissions)

# Check missing data 
sum(is.na(VAHI_adm))
table(VAHI_adm$Program, useNA = 'always')
table(VAHI_adm$Sex, useNA = 'always')
table(VAHI_adm$`Age Group`, useNA = 'always')
table(VAHI_adm$Admissions, useNA = 'always')

# Remove irrelevant values (remove males)
VAHI_adm<-VAHI_adm%>%filter(Sex=="Female")

# Remove 'other' programs
VAHI_adm<-VAHI_adm%>%filter(Program!="Other")

### 3.2) Convert count variables to numeric  ----------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Recode <5 values to either 0 or 4
VAHI_adm$Admissions_0<-as.numeric(ifelse(VAHI_adm$Admissions=="<5",0,VAHI_adm$Admissions))
VAHI_adm$Admissions_4<-as.numeric(ifelse(VAHI_adm$Admissions=="<5",4,VAHI_adm$Admissions))

### 3.3) Summarise dataset to aggregate number from all programs for each demographic ---------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_collapsed_0<-VAHI_adm%>%group_by(Sex, Program)%>% summarise(Admissions = sum(Admissions_0))
VAHI_collapsed_4<-VAHI_adm%>%group_by(Sex, Program)%>% summarise(Admissions = sum(Admissions_4))

### 3.4) Understand data distribution, identify & remove outliers --------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index
ggplot(sp_fems_IPAperps, aes(x=MHSU_Acute_intensive_cat_total_post_index)) + geom_histogram(bins = 8)
prop.table(table(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index))
table(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index>26)
psych::describe(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index, quant = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99))

# Exclude 99th quantile 
quantile_099 <- quantile(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index, probs  = 0.99)
sp_fems_IPAperps_agg<-sp_fems_IPAperps%>%dplyr::select(index_age,index_sex,MHSU_Acute_intensive_cat_total_post_index)%>%
  filter(sp_fems_IPAperps$MHSU_Acute_intensive_cat_total_post_index < quantile_099)%>%
  pivot_longer(cols = c(MHSU_Acute_intensive_cat_total_post_index), values_to = "Admissions_0", names_to = "Program")


### 3.5) Create equivalent IPA perps dataframe (i.e. dataset containing same variables as VAHI data) --------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Remove all rows where index age is missing 
sp_fems_IPAperps_agg<-sp_fems_IPAperps_agg[!is.na(sp_fems_IPAperps_agg$index_age),]

# Recode to equivalent ages
sp_fems_IPAperps_agg$`Age Group`<-ifelse(sp_fems_IPAperps_agg$index_age<18, "0-17",
                                         ifelse(sp_fems_IPAperps_agg$index_age>17 & sp_fems_IPAperps_agg$index_age<25, "18-24",
                                                ifelse(sp_fems_IPAperps_agg$index_age>24 & sp_fems_IPAperps_agg$index_age<35, "25-34",
                                                       ifelse(sp_fems_IPAperps_agg$index_age>34 & sp_fems_IPAperps_agg$index_age<45, "35-44",
                                                              ifelse(sp_fems_IPAperps_agg$index_age>44, "45+", NA_character_)))))
# Check for missing data 
table(sp_fems_IPAperps_agg$`Age Group`, useNA = "always")
table(sp_fems_IPAperps_agg$`Age Group`, sp_fems_IPAperps_agg$index_age)

# Assign sp_fems_IPAperps_agg to spfemsperps_ppl
# Group spfemsperps_ppl by age & program and then count observations in each group, stored in column 'n' 
spfemsperps_ppl<-sp_fems_IPAperps_agg%>%group_by(Program)%>%summarise(n=n())

# Group spfemsperps_ppl by age & program and then calculate sum of 0 admissions in each group, stored in Admissions_0 column 
sp_fems_IPAperps_agg<-sp_fems_IPAperps_agg%>%group_by(Program)%>%summarise(Admissions_0=sum(Admissions_0))%>%ungroup()

# Make new column Admissions_4 with same values as Admissions_0 
sp_fems_IPAperps_agg$Admissions_4<-sp_fems_IPAperps_agg$Admissions_0

# Merge sp_fems_IPAperps_agg and spfemsperps_ppl 
sp_fems_IPAperps_agg<-left_join(sp_fems_IPAperps_agg, spfemsperps_ppl)

# Assign female to sex, specify by perps
sp_fems_IPAperps_agg$Sex<-"F"
sp_fems_IPAperps_agg$Group<-"Female perps"

### 3.6) Insert population numbers into VAHI dataset ---------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

vicpop<-read_excel("G:/raw_data/Victoria population counts.xlsx")
vicpop_f<-vicpop%>%filter(Sex=="Female")
names(vicpop_f)<-c("Sex","Age Group", "n")

### 3.7) Bind perps dataset with VAHI dataset & ensure variable names are identical between both datasets -----------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_adm<-left_join(VAHI_adm, vicpop_f)
VAHI_adm$Group<-"Gen pop"
VAHI_adm<-VAHI_adm%>%select(-Admissions)
sp_fems_IPAperps_agg

# Exclude non-acute data; 'Other Program' and 'Adult Rehabilitation, Extended Care & Residential' 
adm_compare_perps<-bind_rows(VAHI_adm, sp_fems_IPAperps_agg)%>%filter(Program!="Other Program", Program!="Adult Rehabilitation, Extended Care & Residential")
adm_compare_perps_overall<-adm_compare_perps%>%group_by(Group, Program)%>%summarise(Admissions_0=sum(Admissions_0), Admissions_4=sum(Admissions_4), `n`=sum(`n`))%>%ungroup()

### 3.8) Calculate rates of acute MHSU for both VAHI and IPA perp datasets -----------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Calculate rate of adm per 1000 by dividing by number of admissions divided by n(pop size) and multiplied by 1000 
adm_compare_perps$ratepp_0<-(adm_compare_perps$Admissions_0/adm_compare_perps$n)*1000
adm_compare_perps$ratepp_4<-(adm_compare_perps$Admissions_4/adm_compare_perps$n)*1000

# Store new column 'ratepp_0' in adm_compare_perps_overall dataframe
adm_compare_perps_overall$ratepp_0<-(adm_compare_perps_overall$Admissions_0/adm_compare_perps_overall$n)*1000

# Store new column 'ratepp_4' in adm_compare_perps_overall dataframe
adm_compare_perps_overall$ratepp_4<-(adm_compare_perps_overall$Admissions_4/adm_compare_perps_overall$n)*1000

adm_compare_perps_overall
View(adm_compare_perps_overall)

VAHI_outpatient<-read_excel("G:/raw_data/Data Request REDACTED.xlsx", sheet = x, skip = x)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 4) General Population Comparisons - Outpatient Public Mental Health Services ----

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_outpatient<-read_excel("G:/raw_data/Data Request REDACTED.xlsx", sheet = x, skip = x)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 4.1) Clean General VAHI data -------------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

glimpse(VAHI_outpatient)
str(VAHI_outpatient)
table(VAHI_outpatient$Program)
table(VAHI_outpatient$Sex)
table(VAHI_outpatient$Contacts)

# Check missing data 
sum(is.na(VAHI_outpatient))
table(VAHI_outpatient$Program, useNA = 'always')
table(VAHI_outpatient$Sex, useNA = 'always')
table(VAHI_outpatient$`Age Group`, useNA = 'always')
table(VAHI_outpatient$Contacts, useNA = 'always')

# Remove irrelevant values (remove males)
VAHI_outpatient<-VAHI_outpatient%>%filter(Sex=="Female")

# Remove 'other' programs
VAHI_outpatient<-VAHI_outpatient%>%filter(Program!="Other")

### 4.2) Convert count variables to numeric ------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Recode <5 values to either 0 or 4
VAHI_outpatient$Contacts_0<-as.numeric(ifelse(VAHI_outpatient$Contacts=="<5",0,VAHI_outpatient$Contacts))
VAHI_outpatient$Contacts_4<-as.numeric(ifelse(VAHI_outpatient$Contacts=="<5",4,VAHI_outpatient$Contacts))

### 4.3) Summarise dataset to aggregate number from all programs, for each demographic ---------- 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_collapsed_0<-VAHI_outpatient%>%group_by(Sex, Program)%>% summarise(Contacts = sum(Contacts_0))
VAHI_collapsed_4<-VAHI_outpatient%>%group_by(Sex, Program)%>% summarise(Contacts = sum(Contacts_4))

### 4.4) Understand data distribution, identify & remove outliers 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index
ggplot(sp_fems_IPAperps, aes(x=MHSU_outpatient_public_community_cat_total_post_index)) + geom_histogram(bins = 8)
prop.table(table(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index))
table(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index>0)
prop.table(table(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index))
table(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index>260)
psych::describe(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index, quant = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99))

# Exclude 99th quantile
sp_fems_IPAperps_agg<-sp_fems_IPAperps%>%dplyr::select(index_age,index_sex,MHSU_outpatient_public_community_cat_total_post_index)%>%
  filter(sp_fems_IPAperps$MHSU_outpatient_public_community_cat_total_post_index < quantile_0.99)%>%
  pivot_longer(cols = c(MHSU_outpatient_public_community_cat_total_post_index), values_to = "Contacts_0", names_to = "Program")

### 4.5) Create equivalent IPA perps dataframe (i.e. dataset containing same variables as VAHI data) --------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Remove all rows where index age is missing 
sp_fems_IPAperps_agg<-sp_fems_IPAperps_agg[!is.na(sp_fems_IPAperps_agg$index_age),]

# Recode to equivalent ages
sp_fems_IPAperps_agg$`Age Group`<-ifelse(sp_fems_IPAperps_agg$index_age<18, "0-17",
                                         ifelse(sp_fems_IPAperps_agg$index_age>17 & sp_fems_IPAperps_agg$index_age<25, "18-24",
                                                ifelse(sp_fems_IPAperps_agg$index_age>24 & sp_fems_IPAperps_agg$index_age<35, "25-34",
                                                       ifelse(sp_fems_IPAperps_agg$index_age>34 & sp_fems_IPAperps_agg$index_age<45, "35-44",
                                                              ifelse(sp_fems_IPAperps_agg$index_age>44, "45+", NA_character_)))))
# Check for missing data 
table(sp_fems_IPAperps_agg$`Age Group`, useNA = "always")
table(sp_fems_IPAperps_agg$`Age Group`, sp_fems_IPAperps_agg$index_age)


# Assign sp_fems_IPAperps_agg to spfemsperps_ppl
# Group spfemsperps_ppl by age & program and then count observations in each group, stored in column 'n' 
spfemsperps_ppl<-sp_fems_IPAperps_agg%>%group_by(Program)%>%summarise(n=n())

# Group spfemsperps_ppl by age & program and then calculate sum of 0 Contacts in each group, stored in Contacts_0 column 
sp_fems_IPAperps_agg<-sp_fems_IPAperps_agg%>%group_by(Program)%>%summarise(Contacts_0=sum(Contacts_0))%>%ungroup()

# Make new column Contacts_4 with same values as Contacts_0 
sp_fems_IPAperps_agg$Contacts_4<-sp_fems_IPAperps_agg$Contacts_0

# Merge sp_fems_IPAperps_agg and spfemsperps_ppl 
sp_fems_IPAperps_agg<-left_join(sp_fems_IPAperps_agg, spfemsperps_ppl)

# Assign female to sex, specify by perps
sp_fems_IPAperps_agg$Sex<-"F"
sp_fems_IPAperps_agg$Group<-"Female perps"

### 4.6) Insert population numbers into VAHI dataset ---------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

vicpop<-read_excel("G:/raw_data/Victoria population counts.xlsx")
vicpop_f<-vicpop%>%filter(Sex=="Female")
names(vicpop_f)<-c("Sex","Age Group", "n")

### 4.7) Bind perps dataset with VAHI dataset & ensure variable names are identtical between both 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

VAHI_outpatient<-left_join(VAHI_outpatient, vicpop_f)
VAHI_outpatient$Group<-"Gen pop"
VAHI_outpatient<-VAHI_outpatient%>%select(-Contacts)
VAHI_outpatient
sp_fems_IPAperps_agg

# Exclude non-outpatient data; 'Other Program' and 'Adult Rehabilitation, Extended Care & Residential' 
contacts_compare_perps<-bind_rows(VAHI_outpatient, sp_fems_IPAperps_agg)%>%filter(Program!="Other Program", Program!="Adult Rehabilitation, Extended Care & Residential")
contacts_compare_perps_overall<-contacts_compare_perps%>%group_by(Group, Program)%>%summarise(Contacts_0=sum(Contacts_0), Contacts_4=sum(Contacts_4), `n`=sum(`n`))%>%ungroup()

### 4.8) Calculate rates of outpatient MHSU for both VAHI and IPA perp datasets---------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Calculate rate of contacts per 1000 by dividing by number of Contacts divided by n(pop size) and multiplied by 1000 
contacts_compare_perps$ratepp_0<-(contacts_compare_perps$Contacts_0/contacts_compare_perps$n)*1000
contacts_compare_perps$ratepp_4<-(contacts_compare_perps$Contacts_4/contacts_compare_perps$n)*1000

# Store new column 'ratepp_0' in contacts_compare_perps_overall dataframe
contacts_compare_perps_overall$ratepp_0<-(contacts_compare_perps_overall$Contacts_0/contacts_compare_perps_overall$n)*1000

# Store new column 'ratepp_4' in contacts_compare_perps_overall dataframe
contacts_compare_perps_overall$ratepp_4<-(contacts_compare_perps_overall$Contacts_4/contacts_compare_perps_overall$n)*1000

contacts_compare_perps_overall
View(contacts_compare_perps_overall)

# Ensure all population sizes equivalent 

contacts_compare_perps_overall[7,5]<-3035206
contacts_compare_perps_overall[7,5]<-(2421/3035206)*1000
contacts_compare_perps_overall[7,7]<-(2421/3035206)*1000
options(scipen = 999)

contacts_compare_perps_overall
View(contacts_compare_perps_overall)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 5) Regression Analyses - Acute mental health service use ---------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 5.1) Set data distribution ready for regression  ---------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

dd<-datadist(sp_fems_IPAperps)
options(datadist = "dd")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#- PERPETRATION FACTORS 

### 5.2) Q1: Controlling for index age, what is the odds of acute MHSU in women who have perpetrated IPA? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 1

perp_logreg_model_1<-lrm(bin_acute_MHSU ~ rcs(index_age, 3), data = sp_fems_IPAperps)
perp_logreg_model_1
summary(perp_logreg_model_1)
anova(perp_logreg_model_1)
exp(coef(perp_logreg_model_1))

# Diagnostics

vif_values<-vif(perp_logreg_model_1)
print(vif_values)
library(lmtest)
dw_test<-dwtest(perp_logreg_model_1)
print(dw_test)

# Model 1 Summary & Visualisation 

library(modelsummary)

modelsummary(perp_logreg_model_1e, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_1.docx")

ggplot(Predict(perp_logreg_model_1,'index_age', fun = plogis)) + ylab("Probability of Acute MHSU") +
  xlab("Index Age") + theme_classic()


### 5.3) Q2: Controlling for index age, how does prior FV perpetration predict acute MHSU in sample? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 2

#relevel variable "hist_FVI_as_resp" 
sp_fems_IPAperps$hist_FVI_as_resp<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_resp), ref = "No")

perp_logreg_model_2<-lrm(bin_acute_MHSU ~ rcs(index_age, 3) + hist_FVI_as_resp, data = sp_fems_IPAperps)

# Diagnostics

vif_values<-vif(perp_logreg_model_2)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_2)
print(dw_test)

# Model 2 Summary & ANOVA

summary(perp_logreg_model_2, hist_FVI_as_resp="No")
anova(perp_logreg_model_2)
exp(coef(perp_logreg_model_2))
exp(confint.default(perp_logreg_model_2))


### 5.4) Q3: Controlling for index age & prior general violence perpetration, how does historical FV perpetration predict acute MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 3

#relevel variable "hist_general_perp_iresp" 
sp_fems_IPAperps$hist_general_perp_iresp<-relevel(as.factor(sp_fems_IPAperps$hist_general_perp_iresp), ref = "No")

perp_logreg_model_3<-lrm(bin_acute_MHSU ~ index_age + hist_FVI_as_resp + hist_general_perp_iresp, data = sp_fems_IPAperps) 

summary(perp_logreg_model_3, hist_FVI_as_resp="No", hist_general_perp_iresp="No")

# Diagnostics 

vif_values<-vif(perp_logreg_model_3)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_3)
print(dw_test)

# Model 3 Summary & ANOVA

summary(perp_logreg_model_3, hist_FVI_as_resp="No", hist_general_perp_iresp="No")
anova(perp_logreg_model_3)
exp(coef(perp_logreg_model_3))
exp_lower <- exp(perp_logreg_model_3)
exp_upper <- exp(perp_logreg_model_3)
print(exp_lower)
print(exp_upper)

#-#- VICTIMISATION FACTORS IN WOMEN WITH POLICE-RECORDED IPA PERPETRATION

### 5.5) Q4: Controlling for index age, how does prior FV victimisation predict acute MHSU?  ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 4

#relevel hist_FVI_as_AFM
sp_fems_IPAperps$hist_FVI_as_AFM<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_AFM), ref ="No")

perp_logreg_model_4<-lrm(bin_acute_MHSU ~ index_age + hist_FVI_as_AFM, data = sp_fems_IPAperps)

# Diagnostics

vif_values<-vif(perp_logreg_model_4)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_4)
print(dw_test)

# Model 4 Summary & ANOVA

summary(perp_logreg_model_4, hist_FVI_as_AFM="No")
exp(coef(perp_logreg_model_4))

modelsummary(perp_logreg_model_4, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "acute_model2a.docx")

anova(perp_logreg_model_4)

### 5.5) Q5: Controlling for index age and general crime victimisation, how does prior FV victimisation predict acute MHSU?  ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 5

#relevel hist_general_vic_iresp
sp_fems_IPAperps$hist_general_vic_iresp<-relevel(as.factor(sp_fems_IPAperps$hist_general_vic_iresp), ref = "No")

perp_logreg_model_5<-lrm(bin_acute_MHSU ~ rcs(index_age, 3) + hist_general_vic_iresp + hist_FVI_as_AFM, data = sp_fems_IPAperps)

# Diagnostics

vif_values<-vif(perp_logreg_model_5)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_5)
print(dw_test)

# Model 5 Summary & ANOVA

summary(perp_logreg_model_5, hist_general_vic_iresp="No", hist_FVI_as_AFM="No")
exp(coef(perp_logreg_model_5))
anova(perp_logreg_model_5)

modelsummary(perp_logreg_model_5, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "acute_model2b.docx")

plot(Predict(perp_logreg_model_5, fun = plogis))

#exponentiate CI's 
exp_lower <- exp(perp_logreg_model_5)
exp_upper <- exp(perp_logreg_model_5)
print(exp_lower)
print(exp_upper)



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 6) Regression Analyses - Outpatient mental health service use ---------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


#-#- PERPETRATION FACTORS 

### 6.2) Q6: Controlling for index age, what is the odds of outpatient MHSU in women who have perpetrated IPA? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 6

perp_logreg_model_6<-lrm(bin_outpatient_MHSU ~ rcs(index_age, 3), data = sp_fems_IPAperps)


# Diagnostics

vif_values<-vif(perp_logreg_model_6)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_6)
print(dw_test)

# Model 6 Summary & Visualisation 

summary(perp_logreg_model_6)
anova(perp_logreg_model_6)
exp(coef(perp_logreg_model_6))

ggplot(Predict(perp_logreg_model_6,'index_age', fun = plogis)) + ylab("Probability of Outpatient MHSU") +
  xlab("Index Age") + theme_classic()

modelsummary(perp_logreg_model_6, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_6.docx")


### 5.3) Q7: Controlling for index age, how does prior FV perpetration predict acute outpatient in sample? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 7

#relevel "hist_FVI_as_resp" 
sp_fems_IPAperps$hist_FVI_as_resp<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_resp), ref = "No")


# Diagnostics

vif_values<-vif(perp_logreg_model_7)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_7)
print(dw_test)

# Model 7 Summary & ANOVA

perp_logreg_model_7<-lrm(bin_outpatient_MHSU ~ rcs(index_age, 3) + hist_FVI_as_resp, data = sp_fems_IPAperps)

summary(perp_logreg_model_7, hist_FVI_as_resp="No")
anova(perp_logreg_model_7)
exp(coef(perp_logreg_model_7))

modelsummary(perp_logreg_model_7, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_7.docx")

ggplot(Predict(perp_logreg_model_7,'hist_FVI_as_resp', fun = plogis)) + ylab("History of Any Perpetration (General/FVI)") +
  xlab("Probability of Outpatient MHSU") + theme_classic()



### 5.4) Q8: Controlling for index age & prior general violence perpetration, how does historical FV perpetration predict outpatient MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 8

# Relevel variables 
sp_fems_IPAperps$hist_general_perp_iresp<-relevel(as.factor(sp_fems_IPAperps$hist_general_perp_iresp), ref = "No")
sp_fems_IPAperps$hist_FVI_as_resp<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_resp), ref = "No")

perp_logreg_model_8<-lrm(bin_outpatient_MHSU ~ index_age + hist_FVI_as_resp + hist_general_perp_iresp, data = sp_fems_IPAperps) 

# Diagnostics 

vif_values<-vif(perp_logreg_model_8)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_8)
print(dw_test) 

# Model 8 Summary & ANOVA

summary(perp_logreg_model_8, hist_FVI_as_resp="No", hist_general_perp_iresp="No")
anova(perp_logreg_model_8)
exp(coef(perp_logreg_model_8))

modelsummary(operp_logreg_model_8, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_8.docx")


# Exponentiate CI's 
exp_lower <- exp(perp_logreg_model_8)
exp_upper <- exp(perp_logreg_model_8)
print(exp_lower)
print(exp_upper)
exp_lower <- exp(lower)



#-#- VICTIMISATION FACTORS IN WOMEN WITH POLICE-RECORDED IPA PERPETRATION

### 5.5) Q9: Controlling for index age, how does prior FV victimisation predict outpatient MHSU?  ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 9

#relevel hist_FVI_as_AFM
sp_fems_IPAperps$hist_FVI_as_AFM<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_AFM), ref = "No")

perp_logreg_model_9<-lrm(bin_outpatient_MHSU ~ index_age + hist_FVI_as_AFM, data = sp_fems_IPAperps)

# Diagnostics

vif_values<-vif(perp_logreg_model_9)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_9)
print(dw_test) 

# Model 9 Summary & ANOVA

summary(perp_logreg_model_9, hist_FVI_as_AFM="No")
exp(coef(perp_logreg_model_9))
anova(perp_logreg_model_9, hist_FVI_as_AFM="No")

modelsummary(perp_logreg_model_9, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_9.docx")


### 5.5) Q10: Controlling for index age and general crime victimisation, how does prior FV victimisation predict outpatient MHSU?  ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 10

#relevel hist_general_vic_iresp
sp_fems_IPAperps$hist_general_vic_iresp<-relevel(as.factor(sp_fems_IPAperps$hist_general_vic_iresp), ref = "No")

perp_logreg_model_10<-lrm(bin_outpatient_MHSU ~ rcs(index_age, 3) + hist_general_vic_iresp + hist_FVI_as_AFM, data = sp_fems_IPAperps)


# Diagnostics

vif_values<-vif(perp_logreg_model_10)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_10)
print(dw_test) 

# Model 10 Summary & ANOVA

summary(perp_logreg_model_10, hist_general_vic_iresp="No", hist_FVI_as_AFM="No")
exp(coef(perp_logreg_model_10))
anova(perp_logreg_model_10)


modelsummary(perp_logreg_model_10, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "outpatient_model2b.docx")

plot(Predict(perp_logreg_model_10, fun = plogis))

# Exponentiate CI's 

exp_lower <- exp(perp_logreg_model_10)
exp_upper <- exp(perp_logreg_model_10)
print(exp_lower)
print(exp_upper)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

### 7) Regression Analyses - combined mental health service use ------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#-#- PERPETRATION FACTORS 

### 6.2) Q11: Controlling for index age, what is the odds of combined MHSU in women who have perpetrated IPA? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 11

perp_logreg_model_11<-lrm(bin_combined_MHSU ~ rcs(index_age, 3), data = sp_fems_IPAperps)


# Diagnostics

vif_values<-vif(perp_logreg_model_11)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_11)
print(dw_test)

# Model 11 Summary & Visualisation 

summary(perp_logreg_model_11)
anova(perp_logreg_model_11)
exp(coef(perp_logreg_model_11))

ggplot(Predict(perp_logreg_model_11,'index_age', fun = plogis)) + ylab("Probability of Outpatient MHSU") +
  xlab("Index Age") + theme_classic()

modelsummary(perp_logreg_model_11, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_11.docx")


### 5.3) Q12: Controlling for index age, how does prior FV perpetration predict combined MHSU in sample? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 12

#relevel "hist_FVI_as_resp" 
sp_fems_IPAperps$hist_FVI_as_resp<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_resp), ref = "No")


# Diagnostics

vif_values<-vif(perp_logreg_model_12)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_12)
print(dw_test)

# Model 12 Summary & ANOVA

perp_logreg_model_11<-lrm(bin_combined_MHSU ~ rcs(index_age, 3) + hist_FVI_as_resp, data = sp_fems_IPAperps)

summary(perp_logreg_model_12, hist_FVI_as_resp="No")
anova(perp_logreg_model_12)
exp(coef(perp_logreg_model_12))

modelsummary(perp_logreg_model_12, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_12.docx")

ggplot(Predict(perp_logreg_model_12,'hist_FVI_as_resp', fun = plogis)) + ylab("History of Any Perpetration (General/FVI)") +
  xlab("Probability of Outpatient MHSU") + theme_classic()



### 5.4) Q13: Controlling for index age & prior general violence perpetration, how does historical FV perpetration predict combined MHSU? ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 13

# Relevel variables 
sp_fems_IPAperps$hist_general_perp_iresp<-relevel(as.factor(sp_fems_IPAperps$hist_general_perp_iresp), ref = "No")
sp_fems_IPAperps$hist_FVI_as_resp<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_resp), ref = "No")

perp_logreg_model_13<-lrm(bin_combined_MHSU ~ index_age + hist_FVI_as_resp + hist_general_perp_iresp, data = sp_fems_IPAperps) 

# Diagnostics 

vif_values<-vif(perp_logreg_model_13)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_13)
print(dw_test) 

# Model 13 Summary & ANOVA

summary(perp_logreg_model_13, hist_FVI_as_resp="No", hist_general_perp_iresp="No")
anova(perp_logreg_model_13)
exp(coef(perp_logreg_model_13))

modelsummary(perp_logreg_model_13, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_13.docx")


# Exponentiate CI's 
exp_lower <- exp(perp_logreg_model_13)
exp_upper <- exp(perp_logreg_model_13)
print(exp_lower)
print(exp_upper)
exp_lower <- exp(lower)



#-#- VICTIMISATION FACTORS IN WOMEN WITH POLICE-RECORDED IPA PERPETRATION

### 5.5) Q14: Controlling for index age, how does prior FV victimisation predict combined MHSU?  ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 14

#relevel hist_FVI_as_AFM
sp_fems_IPAperps$hist_FVI_as_AFM<-relevel(as.factor(sp_fems_IPAperps$hist_FVI_as_AFM), ref = "No")

perp_logreg_model_14<-lrm(bin_combined_MHSU ~ index_age + hist_FVI_as_AFM, data = sp_fems_IPAperps)

# Diagnostics

vif_values<-vif(perp_logreg_model_14)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_14)
print(dw_test) 

# Model 14 Summary & ANOVA

summary(perp_logreg_model_14, hist_FVI_as_AFM="No")
exp(coef(perp_logreg_model_14))
anova(perp_logreg_model_14, hist_FVI_as_AFM="No")

modelsummary(perp_logreg_model_14, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_14.docx")


### 5.5) Q15: Controlling for index age and general crime victimisation, how does prior FV victimisation predict combined MHSU?  ------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Logistic Regression Model 15

#relevel hist_general_vic_iresp
sp_fems_IPAperps$hist_general_vic_iresp<-relevel(as.factor(sp_fems_IPAperps$hist_general_vic_iresp), ref = "No")

perp_logreg_model_15<-lrm(bin_combined_MHSU ~ rcs(index_age, 3) + hist_general_vic_iresp + hist_FVI_as_AFM, data = sp_fems_IPAperps)


# Diagnostics

vif_values<-vif(perp_logreg_model_15)
print(vif_values)
dw_test<-dwtest(perp_logreg_model_15)
print(dw_test) 

# Model 15 Summary & ANOVA

summary(perp_logreg_model_15, hist_general_vic_iresp="No", hist_FVI_as_AFM="No")
exp(coef(perp_logreg_model_15))
anova(perp_logreg_model_15)


modelsummary(perp_logreg_model_15, conf_level = .95, 
             estimate = "{estimate} [{conf.low}, {conf.high}], {statistic}, {p.value}",
             statistic = NULL, exponentiate = T, formula = "term ~ model + statistic", output = "perp_logreg_model_10.docx")

plot(Predict(perp_logreg_model_15, fun = plogis))

# Exponentiate CI's 

exp_lower <- exp(perp_logreg_model_15)
exp_upper <- exp(perp_logreg_model_15)
print(exp_lower)
print(exp_upper)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
