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
implist <- readRDS('O:\\medewerkers\\042647 Schuurmans, I\\Project_16_PLS_reserve\\implist_20220316.rds')
non_imp_df <- complete(implist, action = 0)

#----------------------------------------------------------
# STANDARDIZE DOMAINS + ADD BRAIN RESERVE
#----------------------------------------------------------

implist_c <- complete(implist, action = 'long', include = T)

## this as br measure instead of hap?
pred <- matrix(nrow=nrow(non_imp_df), ncol=31)
for (i in 1:31){
  pred[,i] <- as.numeric(scale(resid(lm(healthy_appearing ~ age_at_scan, data = complete(implist,action=i-1)))))}
non_imp_df$br <- pred[,1]
implist_c$br <- as.numeric(pred)

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

implist <- as.mids(implist_c)

#----------------------------------------------------------
# INFO METHODS
#----------------------------------------------------------

# missing information PPB/DOT
round(100-sum(is.na(non_imp_df$PPB))/nrow(non_imp_df)*100,1)
round(100-sum(is.na(non_imp_df$DOT))/nrow(non_imp_df)*100,1)

# participants with missings ELS
table(!is.na(non_imp_df$LE_domain_score & non_imp_df$CS_domain_score & non_imp_df$PS_domain_score & non_imp_df$IS_domain_score))
prop.table(table(!is.na(non_imp_df$LE_domain_score & non_imp_df$CS_domain_score & non_imp_df$PS_domain_score & non_imp_df$IS_domain_score)))
missings <- sort(as.data.frame(apply(non_imp_df[,2:44], 2, function(x) round((sum(is.na(x))/length(x)*100),1)))[,1])
min(missings[missings != 0])
max(missings)
mean(missings)
sd(missings)

# participants with missings cognition
table(!is.na(non_imp_df$WLT & non_imp_df$LDS & non_imp_df$WFT & non_imp_df$PPB& non_imp_df$STR & non_imp_df$DOT))
prop.table(table(!is.na(non_imp_df$WLT & non_imp_df$LDS & non_imp_df$WFT & non_imp_df$PPB& non_imp_df$STR & non_imp_df$DOT)))
missings <- sort(as.data.frame(apply(non_imp_df[,c('DOT', 'WLT', 'LDS', 'WFT', 'PPB', 'STR')], 2, function(x) round((sum(is.na(x))/length(x)*100),1)))[,1])
min(missings[missings != 0])
max(missings)
mean(missings)
sd(missings)

# difference parenting-related stress western vs non-western
mod <- with(implist, lm(PS_domain_score ~ ethni))
get_table_lm(summary_model = summary(pool(mod)), outcome = 'parentins-related stress', predictor = 'ethni')

# different ethnicities
general <- read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/CHILD-ALLGENERALDATA_25052022.sav")
general <- general[!duplicated(general$MOTHER),]
ethnicities <- merge(general[,c("MOTHER", "ETHNMv2")], non_imp_df, by = "MOTHER")
table(ethnicities$ETHNMv2)
round(prop.table(table(ethnicities$ETHNMv2))*100,1)

#----------------------------------------------------------
# TABLE 1
#----------------------------------------------------------

# get extra vars
non_imp_df$ethni_1 <- non_imp_df$ethni*-1+1
non_imp_df$marital_1 <- non_imp_df$marital*-1+1
non_imp_df$income_1 <- non_imp_df$income*-1+1
non_imp_df$depression_1 <- non_imp_df$depression*-1+1

# get NAs per row
non_imp_df$ethni_na <- ifelse(is.na(non_imp_df$ethni), 1, 0)
non_imp_df$marital_na <- ifelse(is.na(non_imp_df$marital), 1, 0)
non_imp_df$income_na <- ifelse(is.na(non_imp_df$income), 1, 0)
non_imp_df$depression_na <- ifelse(is.na(non_imp_df$depression), 1, 0)

# PAS AAN AANTAL IMPTUATIES

# stress items present
pred <- matrix(nrow=nrow(non_imp_df), ncol=30)
for (i in 1:30){
  pred[,i] <- apply(complete(implist,action=i)[,c("LE_domain_score", "CS_domain_score","PS_domain_score","IS_domain_score")], 1, sum)}
non_imp_df$total_items <- apply(pred, 1, mean)

# prepare df
df_table1 <- non_imp_df[,c("AGEPARENT.x", 'edu_low', 'edu_intermediate', 'edu_high','ethni', 'ethni_1', 'ethni_na','marital', 'marital_1','marital_na','income', 'income_1','income_na', 'depression', 'depression_1', 'depression_na', 'total_items')]
df_table1[,c('edu_low', 'edu_intermediate', 'edu_high','ethni', 'ethni_1', 'ethni_na','marital', 'marital_1','marital_na','income', 'income_1','income_na', 'depression', 'depression_1', 'depression_na')] <- 
  apply(df_table1[,c('edu_low', 'edu_intermediate', 'edu_high','ethni', 'ethni_1', 'ethni_na','marital', 'marital_1','marital_na','income', 'income_1','income_na', 'depression', 'depression_1', 'depression_na')], 2, as.factor)

# get info
table1 <- get_table1(df_table1)
rownames(table1) <- c('N',colnames(df_table1))

# write out
write.xlsx(table1, 'table1.xlsx')   

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

# get model stats imputed model
model_imp_factorloadings_pls <- runMI(sm_factorloadings_pls, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_factorloadings_pls_sum <- summary(model_imp_factorloadings_pls, standardized=T, rsq=T))

# get factorloadings pls
factorloadings_pls <- data.frame(loading = round(model_imp_factorloadings_pls_sum[model_imp_factorloadings_pls_sum$lhs == 'pls',][model_imp_factorloadings_pls_sum[model_imp_factorloadings_pls_sum$lhs == 'pls',]$op== "=~",'std.all'],2),
                                 name = model_imp_factorloadings_pls_sum[model_imp_factorloadings_pls_sum$lhs == 'pls',][model_imp_factorloadings_pls_sum[model_imp_factorloadings_pls_sum$lhs == 'pls',]$op== "=~",'rhs'])
sink('factorloadings_pls.txt'); print(factorloadings_pls); sink()
 
# get fitmeasures pls
fitmeasures_model_pls <- fitmeasures(model_imp_factorloadings_pls)[c('chisq', 'df', 'pvalue', 'srmr', 'cfi')]
fitmeasures_model_pls <- round(fitmeasures_model_pls,3)
sink('fitmeasures_model_pls.txt'); print(fitmeasures_model_pls); sink()

# get imputed pls
n <- nrow(non_imp_df)
m <- 30

# get pls
pred <- matrix(nrow=n, ncol=m)
for (i in 1:m){
  model_pred <- sem(sm_factorloadings_pls, fixed.x = F, missing='fiml', data = complete(implist,action=i), std.lv=T)
  pred[,i] <- predict(model_pred)
}
non_imp_df$pls <- apply(pred, 1, mean)

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

# get model stats imputed model
model_imp_factorloadings_cr <- runMI(sm_factorloadings_cr, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_factorloadings_cr_sum <- summary(model_imp_factorloadings_cr, standardized=T, rsq=T))

# get factorloadings cr
factorloadings_cr <- data.frame(loading = round(model_imp_factorloadings_cr_sum[model_imp_factorloadings_cr_sum$lhs == 'rgeneral',][model_imp_factorloadings_cr_sum[model_imp_factorloadings_cr_sum$lhs == 'rgeneral',]$op== "=~",'std.all'],2),
                                name = model_imp_factorloadings_cr_sum[model_imp_factorloadings_cr_sum$lhs == 'rgeneral',][model_imp_factorloadings_cr_sum[model_imp_factorloadings_cr_sum$lhs == 'rgeneral',]$op== "=~",'rhs'])
sink('factorloadings_cr.txt'); print(factorloadings_cr); sink()

# get fitmeasures
fitmeasures_model_cr <- fitmeasures(model_imp_factorloadings_cr)[c('chisq', 'df', 'pvalue', 'srmr', 'cfi')]
fitmeasures_model_cr <- round(fitmeasures_model_cr,3)
sink('fitmeasures_model_cr.txt'); print(fitmeasures_model_cr); sink()

# get cr
pred <- matrix(nrow=n, ncol=m)
for (i in 1:m){
  model_pred <- sem(sm_factorloadings_cr, fixed.x = F, missing='fiml', data = complete(implist,action=i), std.lv=T)
  pred[,i] <- predict(model_pred)
}
non_imp_df$cr <- apply(pred, 1, mean)

###---- correlation br cr
cor.test(non_imp_df$cr, non_imp_df$healthy_appearing)

#----------------------------------------------------------
# TABLE 2 - PLS
#----------------------------------------------------------

## BRAIN RESERVE ~ PARENTAL LIFE STRESS 

sm_br <- paste0(sm_factorloadings_pls, 'healthy_appearing ~ pls + age_at_scan')

# get model stats imputed model
model_imp_pls_br <- runMI(sm_br, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_pls_br_sum <- summary(model_imp_pls_br, standardized=T, rsq=T))

# get fitmeasures
fitmeasures_pls_br <- fitmeasures(model_imp_pls_br)[c('chisq', 'df', 'pvalue', 'srmr', 'cfi')]
fitmeasures_pls_br <- round(fitmeasures_pls_br,3)
sink('fitmeasures_pls_br.txt'); print(fitmeasures_pls_br); sink()

# get the stats
row2_1_br <- get_table_sem(summary_model = model_imp_pls_br_sum, outcome = 'healthy_appearing', predictor = 'pls')

## COGNITIVE RESERVE ~ PARENTAL LIFE STRESS

sm_cr <- paste0(sm_factorloadings_pls, sm_factorloadings_cr, 'rgeneral ~ pls')

# get model stats imputed model
model_imp_pls_cr <- runMI(sm_cr, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_pls_cr_sum <- summary(model_imp_pls_cr, standardized=T, rsq=T))

# get fitmeasures
fitmeasures_pls_cr <- fitmeasures(model_imp_pls_cr)[c('chisq', 'df', 'pvalue', 'srmr', 'cfi')]
fitmeasures_pls_cr <- round(fitmeasures_pls_cr,3)
sink('fitmeasures_pls_cr.txt'); print(fitmeasures_pls_cr); sink()

# get the stats
row2_1_cr <- get_table_sem(summary_model = model_imp_pls_cr_sum, outcome = 'rgeneral', predictor = 'pls')

#----------------------------------------------------------
# TABLE 2 - DOMAINS
#----------------------------------------------------------

## BRAIN RESERVE ~ LIFE EVENTS
model_le_br <- with(implist, lm(healthy_appearing ~ scale(LE_domain_score) + age_at_scan))
model_imp_le_br_sum <- summary(pool(model_le_br))
# get the stats
row2_2_br <- get_table_lm(summary_model = model_imp_le_br_sum, outcome = 'healthy_appearing', predictor = 'scale(LE_domain_score)')

## BRAIN RESERVE ~ CONTEXTUAL STRESS
model_cs_br <- with(implist, lm(healthy_appearing ~ scale(CS_domain_score) + age_at_scan))
model_imp_cs_br_sum <- summary(pool(model_cs_br))
# get the stats
row2_3_br <- get_table_lm(summary_model = model_imp_cs_br_sum, outcome = 'healthy_appearing', predictor = 'scale(CS_domain_score)')

## BRAIN RESERVE ~ PARENTING-RELATED STRESS
model_ps_br <- with(implist, lm(healthy_appearing ~ scale(PS_domain_score) + age_at_scan))
model_imp_ps_br_sum <- summary(pool(model_ps_br))
# get the stats
row2_4_br <- get_table_lm(summary_model = model_imp_ps_br_sum, outcome = 'healthy_appearing', predictor = 'scale(PS_domain_score)')

## BRAIN RESERVE ~ INTERPERSONAL STRESS
model_is_br <- with(implist, lm(healthy_appearing ~ scale(IS_domain_score) + age_at_scan))
model_imp_is_br_sum <- summary(pool(model_is_br))
# get the stats
row2_5_br <- get_table_lm(summary_model = model_imp_is_br_sum, outcome = 'healthy_appearing', predictor = 'scale(IS_domain_score)')

## COMBINE ALL BR
table2_br <- rbind(row2_1_br, row2_2_br, row2_3_br, row2_4_br, row2_5_br)


## COGNITIVE RESERVE ~ LIFE EVENTS
sm_cr_le <- paste0(sm_factorloadings_cr, 'rgeneral ~ LE_domain_score + age_at_scan')
# get model stats imputed model
model_imp_le_cr <- runMI(sm_cr_le, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_le_cr_sum <- summary(model_imp_le_cr, standardized=T, rsq=T))
# get the stats
row2_2_cr <- get_table_sem(summary_model = model_imp_le_cr_sum, outcome = 'rgeneral', predictor = 'LE_domain_score')

## COGNITIVE RESERVE ~ CONTEXTUAL STRESS
sm_cr_ce <- paste0(sm_factorloadings_cr, 'rgeneral ~ CS_domain_score + age_at_scan')
# get model stats imputed model
model_imp_ce_cr <- runMI(sm_cr_ce, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_ce_cr_sum <- summary(model_imp_ce_cr, standardized=T, rsq=T))
# get the stats
row2_3_cr <- get_table_sem(summary_model = model_imp_ce_cr_sum, outcome = 'rgeneral', predictor = 'CS_domain_score')

## COGNITIVE RESERVE ~ PARENTING-RELATED STRESS
sm_cr_ps <- paste0(sm_factorloadings_cr, 'rgeneral ~ PS_domain_score + age_at_scan')
# get model stats imputed model
model_imp_ps_cr <- runMI(sm_cr_ps, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_ps_cr_sum <- summary(model_imp_ps_cr, standardized=T, rsq=T))
# get the stats
row2_4_cr <- get_table_sem(summary_model = model_imp_ps_cr_sum, outcome = 'rgeneral', predictor = 'PS_domain_score')

## COGNITIVE RESERVE ~ INTERPERSONAL STRESS
sm_cr_is <- paste0(sm_factorloadings_cr, 'rgeneral ~ IS_domain_score + age_at_scan')
# get model stats imputed model
model_imp_is_cr <- runMI(sm_cr_is, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(model_imp_is_cr_sum <- summary(model_imp_is_cr, standardized=T, rsq=T))
# get the stats
row2_5_cr <- get_table_sem(summary_model = model_imp_is_cr_sum, outcome = 'rgeneral', predictor = 'IS_domain_score')

## COMBINE ALL CR
table2_cr <- rbind(row2_1_cr, row2_2_cr, row2_3_cr, row2_4_cr, row2_5_cr)

## COMBINE ALL
table2 <- cbind(table2_br, table2_cr)
write.xlsx(table2, 'table2.xlsx')

#----------------------------------------------------------
# TABLE 3 - DOMAINS SIMULTANEOUS MODEL
#----------------------------------------------------------

## BRAIN RESERVE ~ ALL DOMAINS
model_all_br <- with(implist, lm(healthy_appearing ~ scale(LE_domain_score) + scale(CS_domain_score) + scale(PS_domain_score) + scale(IS_domain_score) + age_at_scan))
model_imp_all_br_sum <- summary(pool(model_all_br))

## get the stats
table3_br <- get_table_lm(summary_model = model_imp_all_br_sum, outcome = 'healthy_appearing', 
                          predictor = c('scale(LE_domain_score)','scale(CS_domain_score)','scale(PS_domain_score)','scale(IS_domain_score)'))

## COGNITIVE RESERVE ~ ALL DOMAINS

sm_all_cr <- paste0(sm_factorloadings_cr, 'rgeneral ~ LE_domain_score + CS_domain_score + PS_domain_score + IS_domain_score')

# get model stats imputed model
model_imp_all_cr <- runMI(sm_all_cr, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
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
write.xlsx(table3, 'table3.xlsx')

#----------------------------------------------------------
# TABLE S1 - NOTE
#----------------------------------------------------------

cor.test(non_imp_df$income_chronic, non_imp_df$income)

#----------------------------------------------------------
# TABLE S3 - CORRELATION MATRIX
#----------------------------------------------------------

# define vars
cor_df <- non_imp_df[,c('LE_domain_score', 'CS_domain_score', 'PS_domain_score', 'IS_domain_score',
                        'healthy_appearing','brainmatter','wml', 'icv', "cr",
                        'LDS', 'WFT', 'WLT', 'PPB', 'STR', 'DOT', 'age_at_scan', 'edu_intermediate', 'edu_high')]

# correlation
cor_s1_est <- round(cor(cor_df, use = 'pairwise.complete.obs'),2)

# get the corresponding p values
cor_s1_p <- round(pvalue_cor(cor_df, cor_s1),4)

# make asterisk thingy
cor_s1 <- cor_s1_est
for (i in 1:nrow(cor_s1)){
  for (j in 1:ncol(cor_s1)){
    if (cor_s1_p[i,j] < .05) cor_s1[i,j] <- paste0(cor_s1[i,j], '*')
    if (cor_s1_p[i,j] < .01) cor_s1[i,j] <- paste0(cor_s1[i,j], '*')
    if (cor_s1_p[i,j] < .001) cor_s1[i,j] <- paste0(cor_s1[i,j], '*')
  }
}

# remove uper triangle
cor_s1[upper.tri(cor_s1)] <- NA

# write out
write.xlsx(cor_s1, 'table_s1.xlsx')
