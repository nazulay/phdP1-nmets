# First inspection of the data, make table 1
library(tidyverse)

load("Data/RData/raw_new_hrv.RData")
keep <- scan("Data/variables_keep.txt", character(), quote = "", quiet = TRUE)
mets <- subset(raw, select = keep)

rm(raw)
# ------------------------------------------------------------------------------
mets$attended_day2 <- !is.na(mets$PERS_KEY_T62)
mets$attended_cpt <- !is.na(mets$CPT_ATTENDED_T6)  # 8380 attended


# ---- add ACR ----
#change unit from micromol/l to millimol/l  (mu-mol to mmol)
mets[c("CREATININE_URINE_DAY1_T62", "CREATININE_URINE_DAY2_T62", 
       "CREATININE_URINE_DAY3_T62")] <- mets[c("CREATININE_URINE_DAY1_T62", 
                                               "CREATININE_URINE_DAY2_T62", 
                                               "CREATININE_URINE_DAY3_T62")]/1000


#microalbuminuria in mg/l -> acr in mg/mmol
mets$acr.mean <- (mets$MICROALBUMINURIA_DAY1_T62/mets$CREATININE_URINE_DAY1_T62 +
                    mets$MICROALBUMINURIA_DAY2_T62/mets$CREATININE_URINE_DAY2_T62 +
                    mets$MICROALBUMINURIA_DAY3_T62/mets$CREATININE_URINE_DAY3_T62)/3



# ---- N excluded ----
#12981 participants total

# -- AF
mets[which(mets$DATOAF > mets$PT_DATE_T6), c("DATOAF", "DIAGAF")] <- NA

mets$af <- mets$TYPEAF %in% 1:2 & mets$DIAGAF %in% 1:4 | 
  mets$ATRIAL_FIBRILLATION_T6 == 1  # + 17 by adding 3&4 

table(mets$af, useNA = "ifany")  # 964 af
table(mets$attended_cpt[which(mets$af)])  # 471 attended

mets$af.ep <- mets$TYPEAF %in% 1:2 & mets$DIAGAF %in% 1:4

# appendix?
table(mets$ATRIAL_FIBRILLATION_T6, mets$af.ep, useNA = "ifany")


# -- pregnancy
table(mets$PREGNANT_T6, useNA = "ifany")


# -- DMT1
dmt1 <- mets[which(mets$DIABETES_T6 & (mets$AGE_T6<=30 | mets$DIABETES_AGE_T6<=30)),]  #only diabetes_age applies in T6
# 33



# ---- TABLE 1 (uses mets_raw) ----
mets$hba_IFCC = (mets$HBA1C_T6 - 2.15) * 10.929

# mean
summary(mets) 
#sd
round(apply(mets, 2, sd, na.rm=T), 2)  
#not used for categorical percent


median(mets$acr.mean, na.rm=T)
IQR(mets$acr.mean, na.rm=T)


# ---- % missing diab age ----
# only count among those with diabetes
table(is.na(mets$DIABETES_AGE_T6[which(mets$DIABETES_T6 == 1)]), useNA = "ifany")
#170

table(mets$DIABETES_T6, useNA = "ifany")  # correct

170/(464+170)

# ---- % missing CPT ----
cond_pre_sdnn = sum(is.na(mets$PT_PRE_CPT_SDNN_T6[mets$attended_cpt]))
cond_post_sdnn = sum(is.na(mets$PT_POST_CPT_SDNN_T6[mets$attended_cpt]))
cond_pre_rmssd = sum(is.na(mets$PT_PRE_CPT_RMSSD_T6[mets$attended_cpt]))
cond_post_rmssd = sum(is.na(mets$PT_POST_CPT_RMSSD_T6[mets$attended_cpt]))


cond_pre_sdnn/sum(mets$attended_cpt)
cond_post_sdnn/sum(mets$attended_cpt)
cond_pre_rmssd/sum(mets$attended_cpt)
cond_post_rmssd/sum(mets$attended_cpt)


# ---- inspect surprising things from the table ----
#glucose had a high max value
hist(mets$GLUCOSE_T6)
hist(mets$GLUCOSE_T6[mets$GLUCOSE_T6 > 10])
# looks ok, gradual decline


#why does the microalbuminuria varibles have such a high sd?
hist(mets$MICROALBUMINURIA_DAY1_T62)
hist(mets$MICROALBUMINURIA_DAY1_T62[mets$MICROALBUMINURIA_DAY1_T62 > 250])

hist(mets$MICROALBUMINURIA_DAY2_T62)
hist(mets$MICROALBUMINURIA_DAY2_T62[mets$MICROALBUMINURIA_DAY2_T62 > 50])

hist(mets$MICROALBUMINURIA_DAY3_T62)
hist(mets$MICROALBUMINURIA_DAY3_T62[mets$MICROALBUMINURIA_DAY3_T62 > 50])

#apparantly those who get proteins in the urine get a lot (bimodal?)



# -- % positive (including NAs)
prop.table(table(mets$SEX_T6, useNA = "ifany"))*100  # 53.4 % women

prop.table(table(mets$DIABETES_T6, useNA = "ifany"))*100  # 4.88 % with self
# reported diabetes

prop.table(table(mets$PREGNANT_T6, useNA = "ifany"))*100  # 0.22 % of the whole sample
prop.table(table(mets$PREGNANT_T6[mets$SEX_T6 == 0], useNA = "ifany"))*100  # 0.4 % of the women


prop.table(table(mets$LIPID_LOWERING_DRUGS_T6, useNA = "ifany"))*100  # 14.4 %

prop.table(table(mets$BP_TREATMENT_T6, useNA = "ifany"))*100  # 22.3 %

prop.table(table(mets$INSULIN_T6, useNA = "ifany"))*100  # 1.63 %

prop.table(table(mets$DIABETES_TABLETS_T6, useNA = "ifany"))*100  # 3.37 %

prop.table(table(mets$HEART_ATTACK_T6, useNA = "ifany"))*100  # 5.25 %

prop.table(table(mets$ANGINA_T6, useNA = "ifany"))*100  # 4.84 %

prop.table(table(mets$STROKE_T6, useNA = "ifany"))*100  # 2.79 %

prop.table(table(mets$ATRIAL_FIBRILLATION_T6, useNA = "ifany"))*100  # 5.81 %



# ---- menopause ----
sum(mets$AGE_T6[which(mets$SEX_T6 == 0)] > 58)  # 3555
sum(mets$SEX_T6 == 0)  # 6928
sum(mets$AGE_T6[which(mets$SEX_T6 == 0)] > 58)/sum(mets$SEX_T6 == 0)  
# 51.3 % of the women are over 58




# ---- attended cpt ----

# attended cpt + missing values
table(mets$CPT_ATTENDED_T6, is.na(mets$PT_PRE_CPT_SDNN_T6))  # 141
table(mets$CPT_ATTENDED_T6, is.na(mets$PT_POST_CPT_SDNN_T6))  # 1315
table(mets$CPT_ATTENDED_T6, is.na(mets$PT_PRE_CPT_RMSSD_T6))  # 141
table(mets$CPT_ATTENDED_T6, is.na(mets$PT_POST_CPT_RMSSD_T6))  # 1315
#Can not use pre-cpt as an indication for attendance


table(is.na(mets$PT_PRE_CPT_SDNN_T6), is.na(mets$PT_POST_CPT_SDNN_T6))
# There are some people that attended the cpt but have missing on pre-cpt values and
# not on post cpt values

length(which(mets$CPT_ATTENDED_T6 & is.na(mets$PT_PRE_CPT_SDNN_T6) & is.na(mets$PT_POST_CPT_SDNN_T6)))
#noone attended the cpt and have missing on both pre and post values


# ---- diabetes T1 ----
sum(mets$DIABETES_T6[!is.na(mets$DIABETES_AGE_T6)])  # 464 diabetics with age
length(which(mets$DIABETES_T6 & (mets$AGE_T6<=30 | mets$DIABETES_AGE_T6<=30)))
#33, 7% of 460


# ---- nonsensical values ----
table(mets$DIABETES_T6, is.na(mets$DIABETES_AGE_T6), useNA = "ifany")
#  Two people have diabetes_age even though they say they dont have diabetes
tmp <- mets[which(mets$DIABETES_T6 == 0 & !is.na(mets$DIABETES_AGE_T6)), ]
#hba1c < 6.5
#glucose < 5.6
#never used insulin/tablets
# conclusion: they dont have diabetes, remove the ages


#7 people have higher diabetes_age than age
tmp <- mets[which(mets$DIABETES_AGE_T6 > mets$AGE_T6), ]
#diabetes age should be set to age (or missing?)




