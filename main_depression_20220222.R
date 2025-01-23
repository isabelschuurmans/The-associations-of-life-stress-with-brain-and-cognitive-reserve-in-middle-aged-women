# script for main analyses PLS ~ CR/BR
# Isabel Schuurmans
# 2021-12-15

# open library
library(lavaan)
library(mice)
library(semTools)
library(xlsx)

# read in function
source("O:\\medewerkers\\042647 Schuurmans, I\\Project_16_PLS_reserve\\Rcode\\functions_20211216.R")

# read in data
setwd("O:\\medewerkers\\042647 Schuurmans, I\\Project_16_PLS_reserve\\Tables and paper info")
implist <- readRDS('O:\\medewerkers\\042647 Schuurmans, I\\Project_16_PLS_reserve\\implist_20220222.rds')
non_imp_df <- complete(implist, action = 0)

#----------------------------------------------------------
# STANDARDIZE DOMAINS + ADD BRAIN RESERVE
#----------------------------------------------------------

implist_c <- complete(implist, action = 'long', include = T)

pred <- matrix(nrow=nrow(non_imp_df), ncol=31)
for (i in 1:31){
  pred[,i] <- scale(complete(implist,action=i-1)[,c("LE_domain_score")])}
non_imp_df$LE_domain_score <- pred[,1]
implist_c$LE_domain_score <- as.numeric(pred)

pred <- matrix(nrow=nrow(non_imp_df), ncol=31)
for (i in 1:31){
  pred[,i] <- scale(complete(implist,action=i-1)[,c("CS_domain_score")])}
non_imp_df$CS_domain_score <- pred[,1]
implist_c$CS_domain_score <- as.numeric(pred)

pred <- matrix(nrow=nrow(non_imp_df), ncol=31)
for (i in 1:31){
  pred[,i] <- scale(complete(implist,action=i-1)[,c("PS_domain_score")])}
non_imp_df$PS_domain_score <- pred[,1]
implist_c$PS_domain_score <- as.numeric(pred)

pred <- matrix(nrow=nrow(non_imp_df), ncol=31)
for (i in 1:31){
  pred[,i] <- scale(complete(implist,action=i-1)[,c("IS_domain_score")])}
non_imp_df$IS_domain_score <- pred[,1]
implist_c$IS_domain_score <- as.numeric(pred)



#----------------------------------------------------------
# SELECT WOMEN WITHOUT DEPRESISON RISK
#----------------------------------------------------------

implist_c_women <- implist_c[implist_c$MOTHER %in% non_imp_df$MOTHER[which(non_imp_df$depression == 0)],]
implist_depr <- as.mids(implist_c_women)

#----------------------------------------------------------
# FACTOR LOADINGS
#----------------------------------------------------------

# PARENTAL LIFE STRESS

sm_factorloadings_pls <- '

# PLS score
pls =~ LE_domain_score + CS_domain_score + PS_domain_score + IS_domain_score

#variance
pls ~~ 1* pls 
'


# COGNITIVE RESERVE

sm_factorloadings_cr <- '

# regressions
brainmatter~ icv + age_at_scan 
wml~ icv + age_at_scan 

LDS ~ age_at_scan + edu_intermediate + edu_high + brainmatter + wml  
WFT ~ age_at_scan + edu_intermediate + edu_high + brainmatter + wml  
WLT ~ age_at_scan + edu_intermediate + edu_high + brainmatter + wml  
PPB ~ age_at_scan + edu_intermediate + edu_high + brainmatter + wml  
STR ~ age_at_scan + edu_intermediate + edu_high + brainmatter + wml  
DOT ~ age_at_scan + edu_intermediate + edu_high + brainmatter + wml  

# latent variables
rgeneral =~ NA*WLT+ STR + LDS + WFT+ PPB + DOT

#variance
rgeneral ~~ 1* rgeneral 
'

#----------------------------------------------------------
# PLS
#----------------------------------------------------------

## BRAIN RESERVE ~ PARENTAL LIFE STRESS 

sm_br <- paste0(sm_factorloadings_pls, 'healthy_appearing ~ pls + age_at_scan')

# get model stats imputed model
model_imp_pls_br <- runMI(sm_br, data = implist_depr, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_pls_br_sum <- summary(model_imp_pls_br, standardized=T, rsq=T))

# get the stats
row2_1_br <- get_table_sem(summary_model = model_imp_pls_br_sum, outcome = 'healthy_appearing', predictor = 'pls')

## COGNITIVE RESERVE ~ PARENTAL LIFE STRESS

sm_cr <- paste0(sm_factorloadings_pls, sm_factorloadings_cr, 'rgeneral ~ pls + age_at_scan')

# get model stats imputed model
model_imp_pls_cr <- runMI(sm_cr, data = implist_depr, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_pls_cr_sum <- summary(model_imp_pls_cr, standardized=T, rsq=T))

# get the stats
row2_1_cr <- get_table_sem(summary_model = model_imp_pls_cr_sum, outcome = 'rgeneral', predictor = 'pls')

#----------------------------------------------------------
# DOMAINS
#----------------------------------------------------------

## BRAIN RESERVE ~ LIFE EVENTS
model_le_br <- with(implist_depr, lm(healthy_appearing ~ scale(LE_domain_score) + age_at_scan))
model_imp_le_br_sum <- summary(pool(model_le_br))
# get the stats
row2_2_br <- get_table_lm(summary_model = model_imp_le_br_sum, outcome = 'healthy_appearing', predictor = 'scale(LE_domain_score)')

## BRAIN RESERVE ~ CONTEXTUAL STRESS
model_cs_br <- with(implist_depr, lm(healthy_appearing ~ scale(CS_domain_score) + age_at_scan))
model_imp_cs_br_sum <- summary(pool(model_cs_br))
# get the stats
row2_3_br <- get_table_lm(summary_model = model_imp_cs_br_sum, outcome = 'healthy_appearing', predictor = 'scale(CS_domain_score)')

## BRAIN RESERVE ~ PARENTING-RELATED STRESS
model_ps_br <- with(implist_depr, lm(healthy_appearing ~ scale(PS_domain_score) + age_at_scan))
model_imp_ps_br_sum <- summary(pool(model_ps_br))
# get the stats
row2_4_br <- get_table_lm(summary_model = model_imp_ps_br_sum, outcome = 'healthy_appearing', predictor = 'scale(PS_domain_score)')

## BRAIN RESERVE ~ INTERPERSONAL STRESS
model_is_br <- with(implist_depr, lm(healthy_appearing ~ scale(IS_domain_score) + age_at_scan))
model_imp_is_br_sum <- summary(pool(model_is_br))
# get the stats
row2_5_br <- get_table_lm(summary_model = model_imp_is_br_sum, outcome = 'healthy_appearing', predictor = 'scale(IS_domain_score)')

## COMBINE ALL BR
table2_br <- rbind(row2_1_br, row2_2_br, row2_3_br, row2_4_br, row2_5_br)


## COGNITIVE RESERVE ~ LIFE EVENTS
sm_cr_le <- paste0(sm_factorloadings_cr, 'rgeneral ~ LE_domain_score + age_at_scan')
# get model stats imputed model
model_imp_le_cr <- runMI(sm_cr_le, data = implist_depr, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_le_cr_sum <- summary(model_imp_le_cr, standardized=T, rsq=T))
# get the stats
row2_2_cr <- get_table_sem(summary_model = model_imp_le_cr_sum, outcome = 'rgeneral', predictor = 'LE_domain_score')

## COGNITIVE RESERVE ~ CONTEXTUAL STRESS
sm_cr_ce <- paste0(sm_factorloadings_cr, 'rgeneral ~ CS_domain_score + age_at_scan')
# get model stats imputed model
model_imp_ce_cr <- runMI(sm_cr_ce, data = implist_depr, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_ce_cr_sum <- summary(model_imp_ce_cr, standardized=T, rsq=T))
# get the stats
row2_3_cr <- get_table_sem(summary_model = model_imp_ce_cr_sum, outcome = 'rgeneral', predictor = 'CS_domain_score')

## COGNITIVE RESERVE ~ PARENTING-RELATED STRESS
sm_cr_ps <- paste0(sm_factorloadings_cr, 'rgeneral ~ PS_domain_score + age_at_scan')
# get model stats imputed model
model_imp_ps_cr <- runMI(sm_cr_ps, data = implist_depr, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_ps_cr_sum <- summary(model_imp_ps_cr, standardized=T, rsq=T))
# get the stats
row2_4_cr <- get_table_sem(summary_model = model_imp_ps_cr_sum, outcome = 'rgeneral', predictor = 'PS_domain_score')

## COGNITIVE RESERVE ~ INTERPERSONAL STRESS
sm_cr_is <- paste0(sm_factorloadings_cr, 'rgeneral ~ IS_domain_score + age_at_scan')
# get model stats imputed model
model_imp_is_cr <- runMI(sm_cr_is, data = implist_depr, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_is_cr_sum <- summary(model_imp_is_cr, standardized=T, rsq=T))
# get the stats
row2_5_cr <- get_table_sem(summary_model = model_imp_is_cr_sum, outcome = 'rgeneral', predictor = 'IS_domain_score')

## COMBINE ALL CR
table2_cr <- rbind(row2_1_cr, row2_2_cr, row2_3_cr, row2_4_cr, row2_5_cr)

## COMBINE ALL
table2 <- cbind(table2_br, table2_cr)

#----------------------------------------------------------
# DOMAINS SIMULTANEOUS MODEL
#----------------------------------------------------------

## BRAIN RESERVE ~ ALL DOMAINS
model_all_br <- with(implist_depr, lm(healthy_appearing ~ scale(LE_domain_score) + scale(CS_domain_score) + scale(PS_domain_score) + scale(IS_domain_score) + age_at_scan))
model_imp_all_br_sum <- summary(pool(model_all_br))

## get the stats
table3_br <- get_table_lm(summary_model = model_imp_all_br_sum, outcome = 'healthy_appearing', 
                          predictor = c('scale(LE_domain_score)','scale(CS_domain_score)','scale(PS_domain_score)','scale(IS_domain_score)'))

## COGNITIVE RESERVE ~ ALL DOMAINS

sm_all_cr <- paste0(sm_factorloadings_cr, 'rgeneral ~ LE_domain_score + CS_domain_score + PS_domain_score + IS_domain_score')

# get model stats imputed model
model_imp_all_cr <- runMI(sm_all_cr, data = implist_depr, fun = "sem", estimator = "MLR", std.lv = T)
model_imp_all_cr_sum <- summary(model_imp_all_cr, standardized=T, rsq=T)

# get the stats
row3_1_cr <- get_table_sem(summary_model = model_imp_all_cr_sum, outcome = 'rgeneral', predictor = 'LE_domain_score')
row3_2_cr <- get_table_sem(summary_model = model_imp_all_cr_sum, outcome = 'rgeneral', predictor = 'CS_domain_score')
row3_3_cr <- get_table_sem(summary_model = model_imp_all_cr_sum, outcome = 'rgeneral', predictor = 'PS_domain_score')
row3_4_cr <- get_table_sem(summary_model = model_imp_all_cr_sum, outcome = 'rgeneral', predictor = 'IS_domain_score')

### COMBINE INTO TABLE
table3_cr <- rbind(row3_1_cr, row3_2_cr, row3_3_cr, row3_4_cr)

### COMBINE ALL
table3 <- cbind(table3_br, table3_cr)


#----------------------------------------------------------
# TABLE S1 
#----------------------------------------------------------

table_depr <- rbind(table2, table3)
write.xlsx(table_depr, 'table_sens_depression.xlsx')

