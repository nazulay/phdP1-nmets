#Imputation by the "impute, then transform" method
# for the analysis in the main hypothesis

library(mice)
library(tidyverse)
#library(naomiphd)

#The range of all variables are unchanged by the imputation
#when using pmm and log/polyreg, but not norm


load("Data/RData/mets_clean.RData")

#hba_IFCC = (hba_DCCT - 2.15) x 10.929

# -- start missing status
na.table.orig <- missing.colwise(mets)  #function from naomiphd


keep.vars <- c("PERS_KEY"
               , "AGE_T6"
               , "SEX_T6"
               , "HBA1C_T6"
               , hrv.var
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
# -- hrv: outcome - impute to impute others, then remove


#predictor matrix:
predict.with <- ini$pred
predict.with[, attended] <- 0 


imp.mets <- mice(mets, m=30, seed=691, printFlag = FALSE, 
                 predictorMatrix = predict.with)


long <- mice::complete(imp.mets, "long", include = TRUE)


# remove imputed values of hrv.var
for (var in hrv.var) {
  long[imp.mets$where[, var], var] <- NA
}

all.equal(mets$PT_POST_CPT_SDNN_T6, long[long$.imp==1, "PT_POST_CPT_SDNN_T6"], 
          check.attributes = F)
# obs doesnt work with whole hrv.var at the same time

na.table.imp <- missing.colwise(long[long$.imp == 1,], conditionals = F)


# save(imp.mets, long, hrv.var, attended, file="Data/RData/mets_imp_hba.RData")


