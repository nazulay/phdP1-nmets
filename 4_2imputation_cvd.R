#Imputation by the "impute, then transform" method

library(mice)
library(tidyverse)
#library(naomiphd)

#The range of all variables are unchanged by the imputation
#when using pmm and log/polyreg, but not norm


load("Data/RData/mets_clean.RData")


# -- start missing status
na.table.orig <- missing.colwise(mets, conditionals = F)  #function from naomiphd


remove.vars <- c("DIABETES_AGE_T6"
                 , "HEIGHT_T6"
                 , "WEIGHT_T6"
                 , "TIME_LAST_MEAL_T6"
                 , day2.var)

mets <- dplyr::select(mets, !all_of(remove.vars))


# --type changes
#str(mets)
mets$PERS_KEY <- as.character(mets$PERS_KEY)
factor.var <- c("DIABETES_T6", "SEX_T6", "LIPID_LOWERING_DRUGS_T6", 
                "BP_TREATMENT_T6", "INSULIN_T6", "DIABETES_TABLETS_T6",
                "HEART_ATTACK_T6", "ANGINA_T6", "STROKE_T6")
mets[, factor.var] <- lapply(mets[, factor.var], as.factor)
#pmm works for count (tslm)



# -- initiate
ini <- mice(mets, maxit=0, print=F)  
#ini$loggedEvents  # pers_key out

# special case variables:
# -- hrv: outcome - impute to impute others, then remove
# -- end point reg: blindpassasjer, do not impute or let impute others



#predictor matrix:
predict.with <- ini$pred
predict.with[, c(attended, ep.reg)] <- 0


#where:
where.imp <- ini$where
where.imp[, ep.reg] <- FALSE


imp.mets <- mice(mets, m=30, seed=691, printFlag = FALSE, 
                 predictorMatrix = predict.with, where = where.imp)


long <- mice::complete(imp.mets, "long", include = TRUE)


# remove imputed values of hrv.var
for (var in hrv.var) {
  long[where.imp[, var], var] <- NA
}

all.equal(mets$PT_POST_CPT_SDNN_T6, long[long$.imp==1, "PT_POST_CPT_SDNN_T6"], 
          check.attributes = F)
# obs doesnt work with whole hrv.var at the same time, different class

na.table.imp <- missing.colwise(long[long$.imp == 1,], conditionals = F)


# save(imp.mets, long, attended, hrv.var, cvd.var, ep.reg, file="Data/RData/mets_imp_cvd.RData")

