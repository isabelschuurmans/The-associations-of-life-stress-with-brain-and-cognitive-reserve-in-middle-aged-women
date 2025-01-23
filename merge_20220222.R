# PLS and reserve
# Isabel K Schuurmans
# 03-11-2021

# open library
library(haven)
library(magrittr)
library(dplyr)
library(lavaan)
library(mice)
library(semTools)

# read in function
source("O:\\medewerkers\\042647 Schuurmans, I\\Project_16_PLS_reserve\\Rcode\\functions_20211216.R")

#------------------------------------------------
# MERGE
#------------------------------------------------

#--- READ IN DATA

# read in pls
pls_ids <- readRDS("O:\\medewerkers\\042647 Schuurmans, I\\Project_16_PLS_reserve\\Voor Isabel\\PLS_N7612.rds")
pls_ids$pls <- 1

# read in data
cognition <- read_sav("//store/department/genr/isi-store/WERKGROEPEN/MRI/OUDERS/Afronding/Cognition/Clean/IDC/MRIparent_Cognition_limited_20210720.sav")   
cognition$oracle <- 1
general <- read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/CHILD-ALLGENERALDATA_12112020.sav")
womenage <- read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/MOTHERAGE_BIRTHCHILD_11062017.sav")
general <- merge(womenage, general, by = 'IDC', all = T)
icv <- read_sav("//store/department/genr/isi-store/WERKGROEPEN/MRI/OUDERS/Afronding/MRI/Clean/T1/ParentMRI_tbv_20210601.sav")
wml_fs <- read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/Ouder MRI/ParentMRI_aseg_stats_20210601.sav")
mri_core <- read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/Ouder MRI/MRIparent_MRIcore_data_02072021.sav")
depression <- read_sav("//store/department/genr/isi-store/WERKGROEPEN/MRI/OUDERS/Afronding/Questionnaires/Clean/IDC/MRIparent_interview_19.08.2021.sav") 
smoking_mother <- read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/GR1001-F1-10_22112016.sav")

#--- RECODE DATA

# get NAs for general
general$NAs <- apply(general, 1, function(x) sum(is.na(x)))

# dichotomize ethnicity
general$ethni <- rep(NA, nrow(general))
for (i in 1:nrow(general)){
  if (is.na(general$ETHNMv2[i])){
    general$ethni[i] <- NA}
  else{
    if (general$ETHNMv2[i] == 2|general$ETHNMv2[i] == 3|general$ETHNMv2[i] == 4|
        general$ETHNMv2[i] == 5|general$ETHNMv2[i] == 6|general$ETHNMv2[i] == 7|
        general$ETHNMv2[i] == 200|general$ETHNMv2[i] == 400|general$ETHNMv2[i] == 600){
      general$ethni[i] <- 1}
    else {
      if (general$ETHNMv2[i] == 1|general$ETHNMv2[i] == 300| general$ETHNMv2[i] == 500|
          general$ETHNMv2[i] == 700|general$ETHNMv2[i] == 800){
        general$ethni[i] <- 0}}}}

# get most complete edu measure
general$edu <- ifelse(!is.na(general$EDUCM), general$EDUCM, 
                      ifelse(!is.na(general$EDUCM3), general$EDUCM3, general$EDUCM5))
general$edu_report <- ifelse(!is.na(general$EDUCM), 'intake', 
                             ifelse(!is.na(general$EDUCM3), 'age3', 'age5'))

# dichotomize income
general$income <- ifelse(general$INCOME < 7, 1, 0)

# fix doubles (as some mothers/partners are repeated because they have multiple children)
general <- general %>% # dataframe 
  filter(!is.na(general$MOTHER)) %>% # only take the not missing and usable moms
  group_by(general$MOTHER) %>% # group by moms
  filter(NAs == min(NAs)) %>% # take by duplicated scans the not missing one
  filter(!duplicated(MOTHER)) 

# dichotomize depression
depression$depression <- ifelse(depression$CESDscorewgt>=16, 1, 0)

# smoking mother
smoking_mother <- merge(general[,c('IDC', 'IDM', 'MOTHER')], smoking_mother, by = 'IDM', all = T)
smoking_mother$smoking <- as.numeric(smoking_mother$f0700101)
smoking_mother <- smoking_mother %>% # dataframe 
  filter(!is.na(smoking_mother$MOTHER)) %>% # only take the not missing and usable moms
  group_by(MOTHER) %>% # group by moms
  filter(smoking == max(smoking)) %>% # take by duplicated scans the one with the higher quality
  filter(!duplicated(MOTHER)) 

# fix cs domain and substract edu
pls_ids$CS_domain_score <- pls_ids$CS_domain_score - pls_ids$education

# split data for parent-based datasets: COGNITION 
cognition_women <- cognition[!is.na(cognition$MOTHER),]
icv_women <- icv[!is.na(icv$MOTHER),]

#--- MERGE DATA

# merge
df_women_1 <- merge(general, cognition_women, by = 'MOTHER', all.x = T)
df_women_2 <- merge(df_women_1, icv_women, by = 'MOTHER', all.x = T)
df_women_3 <- merge(df_women_2, pls_ids, by = 'MOTHER', all.x = T)
df_women_4 <- merge(df_women_3, mri_core, by = 'MOTHER', all.x = T)
df_women_5 <- merge(df_women_4, depression[,c('MOTHER', 'depression')], by = 'MOTHER', all.x = T)
df_women_6 <- merge(df_women_5, wml_fs, by = 'MOTHER', all.x = T)
df_women_7 <- merge(df_women_6, smoking_mother, by = 'MOTHER', all.x = T)

# but for now only analyses with women
df_1 <- df_women_7

#------------------------------------------------
# CALCULATE COGNITIVE RESERVE
#------------------------------------------------

df_1$total_brain_volume_cm3 <- df_1$TotalGrayVol/1000 + df_1$CerebralWhiteMatterVol/1000
df_1$wml_cm3 <- df_1$WM_hypointensities_vol/1000
df_1$healthy_appearing <- as.numeric(scale(df_1$total_brain_volume_cm3 - df_1$wml_cm3 ))

#------------------------------------------------
# CALCULATE COGNITIVE RESERVE
#------------------------------------------------

df_1$marital <- as.numeric(df_1$MARDICH)-1
df_1$icv <- as.numeric(scale(df_1$eTIV))
df_1$brainmatter <- as.numeric(scale(df_1$total_brain_volume_cm3))
df_1$age_at_scan <- as.numeric(scale(df_1$AGEPARENT.x))
df_1$sex <- as.numeric(ifelse(df_1$Type == 'M', 0, 1))
df_1$wml <- log(df_1$WM_hypointensities_vol)
df_1$wml <- as.numeric(scale(ifelse(df_1$wml == -Inf, -6, df_1$wml)))
df_1$edu_low <- as.numeric(ifelse(df_1$edu < 3, 1, 0))
df_1$edu_intermediate <- as.numeric(ifelse(df_1$edu == 3 | df_1$edu == 4, 1, 0))
df_1$edu_high <- as.numeric(ifelse(df_1$edu > 4, 1, 0))
df_1$LDS <- as.numeric(scale(df_1$Total_correct_LDST))
df_1$WFT <- as.numeric(scale(df_1$typical_correct))
df_1$WLT <- as.numeric(scale(df_1$del_correct))
df_1$PPB <- as.numeric(scale(df_1$Sum_PPB))
df_1$STR <- as.numeric(scale(df_1$S3_time_total_cor_S1))
df_1$DOT <- as.numeric(scale(df_1$Total_correct_DOT))
df_1$missingscogn <- apply(df_1[,c('LDS', 'WFT', 'WLT', 'PPB', 'STR', 'DOT')], 1, function(x) sum(is.na(x)))

# flowchart
step1 <- df_1[which(df_1$oracle==1),]
step2 <- step1[which(step1$pls == 1),]
step3 <- step2[which(step2$t1_available==1),]
step4 <- step3[which(step3$exclude_incidental==0),]
step5 <- step4[which(step4$exclude_quality_t1==0),]
step6 <- step5[which(step5$missingscogn<3),]
step7 <- step6[!is.na(step6$edu),]

flowchart <- c(nrow(step1), nrow(step2)-nrow(step1),
               nrow(step2), nrow(step3)-nrow(step2),
               nrow(step3), nrow(step4)-nrow(step3),
               nrow(step4), nrow(step5)-nrow(step4),
               nrow(step5), nrow(step6)-nrow(step5),
               nrow(step6), nrow(step7)-nrow(step6),
               nrow(step7))

measurement_oracle <- round(c(mean(step1$AGEPARENT-step1$agemother_birthchild), sd(step1$AGEPARENT-step1$agemother_birthchild)),2)

# get needed vars
df_to_impute <- step7[,c('MOTHER',
                         
                         # extra aux and predictor
                         names(pls_ids)[4:46],'BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',
                         
                         # predictor
                         'icv', 'brainmatter', "healthy_appearing",'age_at_scan',"AGEPARENT.x", 'wml',
                         'edu_low','edu_intermediate', 'edu_high', 'LDS', 'WFT', 'WLT', 'PPB', 'STR', 'DOT', 
                         
                         # sensitivity analyses and table 1
                         'ethni', 'depression','income','marital',
                         
                         # domain scores
                         names(pls_ids)[48:51])]


# imputation
imp0 <- mice(df_to_impute, maxit = 0, defaultMethod = c('pmm', 'pmm', 'pmm', 'pmm'))

#Save the imputation method
meth <- imp0$method

# change method for passive imputation
meth['LE_domain_score'] <- "~I( partner_or_child_died + smbd_else_died + smbd_else_ill + 
infertility + abortion + miscarriage_stillborn + victim_crime + 
discrimination + childhood_deprivation + childhood_abuse +
childhood_neglect + parents_divorced + immigration )"

meth['CS_domain_score'] <- "~I( unemployment_chronic + trouble_paying + financial_difficulties + 
income_once + income_chronic + work_study_stress + material_deprivation + 
neighbourhood_problems + arrested) "

meth['PS_domain_score'] <- "~I(parenting_stress_18months + parenting_stress_8years + pregnancy_planned +
pregnancy_worried + victimization_child + 
general_health_child + child_hospital_accidents + affected_physical_health + 
child_conflict + callous_unemotional + affected_behavior + 
cbcl + bullying_child)"

meth['IS_domain_score'] <- "~I( no_partner_once + divorce + difficulties_partner + family_distress +
family_conflict + difficulties_others + social_support)"

### Change the predictormatrix ####
predictormatrix <- imp0$predictorMatrix

## set the whole matrix to 0
predictormatrix[,] <- 0

# needs to be imputed: ELS items, aux vars, outcomes?

## life events
predictormatrix[c("partner_or_child_died", "smbd_else_died", "smbd_else_ill",
                "infertility", "abortion", "miscarriage_stillborn", 
                "victim_crime", "discrimination",   
                "childhood_deprivation", "childhood_abuse", 
                "childhood_neglect", "parents_divorced"),
                
                # impute with: other items
                c("partner_or_child_died", "smbd_else_died", "smbd_else_ill",
                  "infertility", "abortion", "miscarriage_stillborn", 
                  "victim_crime", "discrimination",   
                  "childhood_deprivation", "childhood_abuse", 
                  "childhood_neglect", "parents_divorced", "immigration", 
                  
                # impute with: other domain scores
                'CS_domain_score', 'PS_domain_score', 'IS_domain_score',
                
                # impute with: aux
                'BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',"immigration", 'education', 'divorce', "family_distress",
                
                # impute with: outcome
                "healthy_appearing",'age_at_scan','wml','LDS')] <- 1

## contextual stress
predictormatrix[c("unemployment_chronic", "trouble_paying", "financial_difficulties",
                  "income_once", "income_chronic", "work_study_stress", "material_deprivation",
                  "neighbourhood_problems", "arrested"),c(
                    
                    # impute with: other items
                    "unemployment_chronic", "trouble_paying", "financial_difficulties",
                    "income_once", "income_chronic", "work_study_stress", "material_deprivation",
                    "neighbourhood_problems", "education", "arrested", 
                    
                    # impute with: other domain scores
                    'LE_domain_score', 'PS_domain_score', 'IS_domain_score',
                    
                    # impute with: aux
                    'BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',"immigration", 'education', 'divorce', "family_distress",
                    
                    # impute with: outcome
                    "healthy_appearing",'age_at_scan','wml','LDS')] <- 1

## parenting-related stress
predictormatrix[c("parenting_stress_18months", "parenting_stress_8years", "pregnancy_planned", 
                  "pregnancy_worried","victimization_child", "general_health_child", 
                  "child_hospital_accidents", "affected_physical_health",  "child_conflict", 
                  "callous_unemotional", "affected_behavior", "cbcl", "bullying_child"),c(
                    
                    # impute with: other items
                    "parenting_stress_18months", "parenting_stress_8years", "pregnancy_planned", 
                    "pregnancy_worried","victimization_child", "general_health_child", 
                    "child_hospital_accidents", "affected_physical_health",  "child_conflict", 
                    "callous_unemotional", "affected_behavior", "cbcl", "bullying_child", 
                    
                    # impute with: other domain scores
                    'CS_domain_score', 'LE_domain_score', 'IS_domain_score',
                    
                    # impute with: aux
                    'BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',"immigration", 'education', 'divorce', "family_distress",
                    
                    # impute with: outcome
                    "healthy_appearing",'age_at_scan','wml','LDS')] <- 1

## interpersonal stress
predictormatrix[c("no_partner_once",
                   "difficulties_partner",
                  "family_conflict",
                  "difficulties_others", "social_support"),c(
                    
                    # impute with: other items
                    "no_partner_once",
                    "divorce", "difficulties_partner", "family_distress",
                    "family_conflict",
                    "difficulties_others", "social_support", 
                    
                    # impute with: other domain scores
                    'CS_domain_score', 'PS_domain_score', 'LE_domain_score',
                    
                    # impute with: aux
                    'BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',"immigration", 'education', 'divorce', "family_distress",
                    
                    # impute with: outcome
                    "healthy_appearing",'age_at_scan','wml','LDS')] <- 1


## auxs

predictormatrix[c('BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',"immigration", 'education', 'divorce', "family_distress"),
                
                    # impute with: other domain scores
                c('CS_domain_score', 'PS_domain_score', 'LE_domain_score','IS_domain_score',
                    
                    # impute with: aux
                    'BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',"immigration", 'education', 'divorce', "family_distress",
                    
                    # impute with: outcome
                    "healthy_appearing",'age_at_scan','wml','LDS')] <- 1

## outcome
predictormatrix[c('LDS', 'WFT', 'WLT', 'PPB', 'STR', 'DOT'),
                
                # impute with: other domain scores
                c('CS_domain_score', 'PS_domain_score', 'LE_domain_score','IS_domain_score',
                  
                  # impute with: aux
                  'BMI_0', 'AGE_M_v2', 'PARITY', 'smoking',"immigration", 'education', 'divorce', "family_distress",
                  
                  # impute with: outcome
                   "healthy_appearing",'age_at_scan',
                  'LDS', 'WFT', 'WLT', 'PPB', 'STR', 'DOT')] <- 1 ## - edulow-middle-high, -brain matter


# diag to zero
diag(predictormatrix) <- 0

### visit the sequence
visiting_sequence <- imp0$visitSequence

### run
#implist <- mice(df_to_impute, m = 30, maxit = 60, method = meth, predictorMatrix = predictormatrix)
# saveRDS(implist, 'O:\\medewerkers\\042647 Schuurmans, I\\Project_16_PLS_reserve\\implist_20220316.rds')
# in sens with depr exclude participants without depr meas, now it is imputed albeit not used in imp models

## info for in paper

# methods - mothers and partners included
table(cognition$Type)

# flowchart
flowchart

# edu reported at intake, otherwise 3 years or 5 years after
table(step7$edu_report)

#----------------------------------------------------------
# TABLE S2 - NON-RESPONSE
#----------------------------------------------------------

# following variables: age, edu, ethni, marital, income, depression 
step1$edu <- ifelse(step1$edu < 3, 1, 
                    ifelse(step1$edu >2, 2, NA))

# convert to correct data types GENDER
step1[,c('edu', "ethni", "marital", "income", "depression")] <- 
  apply(step1[,c('edu', "ethni", "marital", "income", "depression")], 2, as.factor)

# run non-response
tab1 <- nonresponse(ids_study = df_to_impute$MOTHER, ids_full = step1$MOTHER, dataset = step1, 
                    variables = c('AGEPARENT.x', 'edu', "ethni", "marital", "income", "depression"))

# get n
length(df_to_impute$MOTHER)
length(df_to_impute$MOTHER) - length(step1$MOTHER)

write.xlsx(tab1, 'table_s2_non_response.xlsx')
