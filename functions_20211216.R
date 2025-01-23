# functions

# NON RESPONSE ANALYSES
nonresponse <- function(ids_study, ids_full, dataset, variables){
  
  # convert dataset to dataframe
  dataset <- as.data.frame(dataset)[]
  
  # make indicator variable for included and excluded participants
  dataset$indicator <- ids_full %in% ids_study
  
  # get empty table
  table_stats <- matrix(NA, nrow = (length(variables)), ncol = 5)
  
  # compare on the variables
  
  for (i in 1:length(variables)){
    
    # t-test continuous outcomes
    if (is.numeric(dataset[,variables[i]])){
      
      table_stats[i,1] <- paste(round(mean(dataset[dataset$indicator == T,variables[i]], na.rm = T),2), round(sd(dataset[dataset$indicator == T,variables[i]], na.rm = T),2), sep = ' ± ')
      table_stats[i,2] <- paste(round(mean(dataset[dataset$indicator == F,variables[i]], na.rm = T),2), round(sd(dataset[dataset$indicator == F,variables[i]], na.rm = T),2), sep = ' ± ')
      table_stats[i,3] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$statistic, 2)
      table_stats[i,4] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$parameter, 2)
      table_stats[i,5] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$p.value, 3)
      
    } 
    
    # chi-square for categorical outcomes 
    else {
      
      table_stats[i,1] <- paste(table(dataset[dataset$indicator == T,variables[i]])[1], ' (', round(prop.table(table(dataset[dataset$indicator == T,variables[i]]))[1]*100,1), ')', sep = '')
      table_stats[i,2] <- paste(table(dataset[dataset$indicator == F,variables[i]])[1], ' (', round(prop.table(table(dataset[dataset$indicator == F,variables[i]]))[1]*100,1), ')', sep = '') 
      table_stats[i,3] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$statistic, 2)
      table_stats[i,4] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$parameter, 2)
      table_stats[i,5] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$p.value, 3)
      
    }
    
  }
  
  # clean up stats
  colnames(table_stats) <- c('mean study', 'mean remaining','t/X', 'df_t', 'p_t')
  rownames(table_stats) <- variables
  
  return(table_stats)
  
}


#---------------------------------------------------------------------
# get sociodemographics

get_table1 <- function(df){
  
  table <- data.frame(rep(NA, ncol(df)+1),
                      rep(NA, ncol(df)+1)) 
  
  table[1,2] <- nrow(df)
  
  for (i in 1:ncol(df)){
    
    # get n & % for factor data
    if (is.factor(df[,i]) | is.character(df[,i])){
      
      n <- length(df[which(df[,i]==1),i])
      pt <- round(n/nrow(df)*100,1)
      table[(i+1),2] <- paste0(n, ' (', pt, ')', collapse = '')
      
    }
    
    else {
      
      m <- round(mean(df[,i], na.rm=T),2)
      sd <- round(sd(df[,i], na.rm=T),2)
      table[(i+1),1] <- paste0(m, ' (', sd, ')', collapse = '')
      
    }
    
  }
  
  return(table)
  
}

#---------------------------------------------------------------------
# get table with statistics (sem)

get_table_sem <- function(summary_model, outcome, predictor){
  
  # get regression
  mod <- summary_model[summary_model$op=='~',]
  
  info <- matrix(nrow = length(predictor), ncol = length(outcome)*3)
  
  for (i in 1:length(outcome)){
    
    # select outcome
    mod1 <- mod[mod$lhs == outcome[i],]
    
    # get info
    beta <- round(mod1[mod1$rhs == predictor,'est'],2)
    ci <- paste0(round(mod1[mod1$rhs == predictor,'est']-1.96*mod1[mod1$rhs == predictor,'se'],2), ', ',
                 round(mod1[mod1$rhs == predictor,'est']+1.96*mod1[mod1$rhs == predictor,'se'],2))
    p <- round(mod1[mod1$rhs == predictor,'pvalue'],3)
    
    # combine
    position <- i*3-2
    info[1,position:(position+2)] <- c(beta, ci, p)
  }
 
  row.names(info) <- predictor
  
  return(info)
}

#---------------------------------------------------------------------
#get table with statistics (lm)

get_table_lm <- function(summary_model, outcome, predictor){
  
  # get regression
  mod <- summary_model
  
  info <- matrix(nrow = length(predictor), ncol = length(outcome)*3)
  
  for (i in 1:length(predictor)){
    
    # get info
    beta <- round(mod[mod$term == predictor[i],'estimate'],2)
    ci <- paste0(round(mod[mod$term == predictor[i],'estimate']-1.96*mod[mod$term == predictor[i],'std.error'],2), ', ',
                 round(mod[mod$term == predictor[i],'estimate']+1.96*mod[mod$term == predictor[i],'std.error'],2))
    p <- round(mod[mod$term == predictor[i],'p.value'],3)
    
    # combine
    info[i,] <- c(beta, ci, p)
  }
  
  row.names(info) <- predictor
  
  return(info)
}

#---------------------------------------------------------------------
#get p values correlation matrix

pvalue_cor <- function(dataset, cortable){
  varsx <- varsy <- rownames(cortable)
  pmatrix <- matrix(NA, length(varsx), length(varsx))
  for (i in 1:length(varsx)){
    for (j in 1:length(varsy)){
      pmatrix[i,j] <- cor.test(dataset[,varsx[i]], dataset[,varsy[j]])$p.value
    }
  }
  return(pmatrix)
}
