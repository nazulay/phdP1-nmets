#Imputation by the "impute, then transform" method
# for the analysis in the main hypothesis

library(mice)
library(tidyverse)
#library(naomiphd)

#The range of all variables are unchanged by the imputation
#when using pmm and log/polyreg, but not norm


load("Data/RData/mets_clean.RData")



# -- start missing status
na.table.orig <- missing.colwise(mets)  #function from naomiphd


keep.vars <- c("PERS_KEY"
               , "AGE_T6"
               , "SEX_T6"
               , "HBA1C_T6"
               , hrv.var
               , day2.var
               , attended)

mets <- dplyr::select(mets, all_of(keep.vars))


# --type changes
#str(mets)
mets$PERS_KEY <- as.character(mets$PERS_KEY)
mets$SEX_T6 <- as.factor(mets$SEX_T6)



# -- initiate
ini <- mice(mets, maxit=0, print=F)  
#ini$loggedEvents  # pers_key out

# special case variables:
# -- day 2: only impute for people that attended, 
#           variables only impute themselves
# -- hrv: outcome - impute to impute others, then remove



#predictor matrix:
predict.with <- ini$pred
predict.with[, c(attended, day2.var)] <- 0 


#Want to let day2 vars predict each other, but not themselves
predict.with[day2.var, day2.var] <- 1
for (var in day2.var) {
  predict.with[var, var] <- 0
}


where.imp <- ini$where
where.imp[!mets$attended_day2, day2.var] <- FALSE


imp.mets <- mice(mets, m=30, seed=691, printFlag = FALSE,
                 predictorMatrix = predict.with, where = where.imp)


long <- mice::complete(imp.mets, "long", include = TRUE)
                  


# remove imputed values of hrv.var
for (var in hrv.var) {
  long[where.imp[, var], var] <- NA
}

all.equal(mets$PT_POST_CPT_SDNN_T6, long[long$.imp==1, "PT_POST_CPT_SDNN_T6"], 
          check.attributes = F)
# obs doesnt work with whole hrv.var at the same time

na.table.imp <- missing.colwise(long[long$.imp == 1,], conditionals = F)


# save(imp.mets, long, attended, hrv.var, day2.var, file="Data/RData/mets_imp_alb.RData")



