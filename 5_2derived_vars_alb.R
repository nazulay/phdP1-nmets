
library(mice)

load("Data/RData/mets_imp_alb.RData")

# --Albuminuria 

#change unit from micromol/l to millimol/l  (mu-mol to mmol)
long[c("CREATININE_URINE_DAY1_T62", "CREATININE_URINE_DAY2_T62", 
       "CREATININE_URINE_DAY3_T62")] <- long[c("CREATININE_URINE_DAY1_T62", 
                                               "CREATININE_URINE_DAY2_T62", 
                                               "CREATININE_URINE_DAY3_T62")]/1000


#microalbuminuria in mg/l -> acr in mg/mmol
long$acr.mean <- (long$MICROALBUMINURIA_DAY1_T62/long$CREATININE_URINE_DAY1_T62 +
                    long$MICROALBUMINURIA_DAY2_T62/long$CREATININE_URINE_DAY2_T62 +
                    long$MICROALBUMINURIA_DAY3_T62/long$CREATININE_URINE_DAY3_T62)/3


long$albuminuria <- (long$acr.mean > 1.92 & long$SEX_T6 == 1) |  # men
  (long$acr.mean > 2.83 & long$SEX_T6 == 0)  # women


# ---- save data ---- 

mets_na <- long[long$.imp == 0, ]  # original dataset
mets <- as.mids(long)  # imputed dataset as mids object

#save(long, mets, mets_na, attended, hrv.var, day2.var, file="Data/RData/derived_vars_alb.RData")
