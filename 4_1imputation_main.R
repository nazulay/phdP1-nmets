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


remove.vars <- c("DIABETES_AGE_T6"
                 , "HEIGHT_T6"
                 , "WEIGHT_T6"
                 , "TIME_LAST_MEAL_T6"
                 , day2.var
                 , cvd.var
                 , ep.reg) 

mets <- dplyr::select(mets, !all_of(remove.vars))


# --type changes
#str(mets)
mets$PERS_KEY <- as.character(mets$PERS_KEY)
factor.var <- c("DIABETES_T6", "SEX_T6", "LIPID_LOWERING_DRUGS_T6", 
                "BP_TREATMENT_T6", "INSULIN_T6", "DIABETES_TABLETS_T6")

mets[, factor.var] <- lapply(mets[, factor.var], as.factor)
#pmm works for count (tslm)



# -- initiate
ini <- mice(mets, maxit=0, print=F)  
#ini$loggedEvents  # pers_key out

# special case variables:
# -- hrv: outcome - impute to impute others, then remove
# -- AF: dont impute var from ep reg, impute self report


#predictor matrix:
predict.with <- ini$pred
predict.with[, attended] <- 0 



imp.mets <- mice(mets, m=30, seed=691, printFlag = FALSE,
                 predictorMatrix = predict.with)


#data <- complete(imp.mets, 2)
long <- mice::complete(imp.mets, "long", include = TRUE)


# remove imputed values of hrv.var
for (var in hrv.var) {
  long[imp.mets$where[, var], var] <- NA
}

all.equal(mets$PT_POST_CPT_SDNN_T6, long[long$.imp==1, "PT_POST_CPT_SDNN_T6"], 
          check.attributes = F)
# obs doesnt work with whole hrv.var at the same time, different class

na.table.imp <- missing.colwise(long[long$.imp == 1,], conditionals = F)


# save(imp.mets, long, attended, hrv.var, file="Data/RData/mets_imp_main.RData")
