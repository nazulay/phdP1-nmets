

# main & cvd


library(mice)

load("Data/RData/mets_imp_main.RData")  # OR

load("Data/RData/mets_imp_cvd.RData")


long$LIPID_LOWERING_DRUGS_T6 <- long$LIPID_LOWERING_DRUGS_T6 == 1
long$BP_TREATMENT_T6 <- long$BP_TREATMENT_T6 == 1



# ---- mets criteria ----

#1) central obesity
long$central_obesity <- (long$SEX_T6 == 1 & long$WAIST_T6 >= 94) | # men
  (long$SEX_T6 == 0 & long$WAIST_T6 >= 80)  # women


#2) triglycerides
long$high_triglyc <- long$TRIGLYCERIDES_T6 > 1.7 | long$LIPID_LOWERING_DRUGS_T6  
# works as intended - is true if one is true, even when the other is NA


#3) HDL
long$low_hdl <- (long$SEX_T6 == 1 & long$HDL_T6 < 1) | #men
  (long$SEX_T6 == 0 & long$HDL_T6 < 1.3) |  #women
  long$LIPID_LOWERING_DRUGS_T6


#4) blood pressure
long$high_BP <- long$MEAN_SYSBP_T6 >= 130 | long$MEAN_DIABP_T6 >= 85 | long$BP_TREATMENT_T6


#5) glucose
long$gluc_low_meds <- long$INSULIN_T6 == 1 | long$DIABETES_TABLETS_T6 == 1


long$diabetic <- long$DIABETES_T6 == 1 | long$HBA1C_T6 >= 6.5 | long$gluc_low_meds

long$high_glucose <- long$GLUCOSE_T6 >= 5.6 & !long$diabetic


mets.comp <- c("central_obesity", "high_triglyc", "low_hdl", "high_BP", "high_glucose")
long$n_mets = rowSums(long[mets.comp])


#add diabetes as nr 6
long$n_mets6 <- long$n_mets
long$n_mets6[which(long$diabetic)] <- 6
long$n_mets6_cat <- as.factor(long$n_mets6)


# ---- save main hyp data ---- 

mets_na <- long[long$.imp == 0, ]  # original dataset
mets <- as.mids(long)  # imputed dataset as mids object

#save(long, mets, mets_na, attended, hrv.var, mets.comp, file="Data/RData/derived_vars_main.RData")


# ---- cvd variables ----

# -- heart attack
#table(mets_na$DINFARKT, useNA = "ifany")  # 11273 NAs
#mets_na$heart_attack <- mets_na$DINFARKT %in% 1:3
#table(mets_na$HEART_ATTACK_T6, mets_na$heart_attack, useNA = "ifany") 

long$heart_attack <- long$DINFARKT %in% 1:3 | long$HEART_ATTACK_T6 == 1


# --stroke
#ends up with TRUE or NA, no FALSE:
#long$stroke <- long$DIAGUKLAS == 1 | long$DIAGISCH %in% c(1, 4) | long$DIAGHEMO == 1 | long$DIAGSAB == 1 | long$STROKE_T6 == 1
# %in%: NAs and FALSE -> FALSE
# ==: NAs stay NA, other codes FALSE

# This also gives FALSE
long$stroke <- long$DIAGUKLAS == 1 & !is.na(long$DIAGUKLAS) |
               long$DIAGISCH %in% c(1, 4) |
               long$DIAGHEMO == 1 & !is.na(long$DIAGHEMO)|
               long$DIAGSAB == 1 & !is.na(long$DIAGSAB)|
               long$STROKE_T6 == 1 & !is.na(long$STROKE_T6)

# if checking this definition, use either
# sum(long$heart_attack & long$stroke, na.rm=T) 
# or 
# dim(long[which(long$heart_attack & long$stroke), ])


# MI = 25 236
# A = 20 889
# S = 16 147


# A n MI = 9948
# S n MI = 3838
# A n S = 3641

# A n MI n S = 2267

# A u MI u S = MI + A + S 
#            - A n MI - S n A
#            + A n MI n S
# = 47 112


#table(long[long$.imp==15, "stroke"], useNA = "ifany")


# --angina
long$angina <- long$ANGINA_T6 == 1


# --af
#long$af <- (long$TYPEAF %in% 1:2 & long$DIAGAF %in% 1:4) | long$ATRIAL_FIBRILLATION_T6 == 1


long$cvd <- long$heart_attack | long$stroke | long$angina



# ---- save cvd data ---- 
mets_na <- long[long$.imp == 0, ]  # original dataset
mets <- as.mids(long)  # imputed dataset as mids object

save(long, mets, mets_na, attended, hrv.var, mets.comp, 
     file="Data/RData/derived_vars_cvd.RData")

