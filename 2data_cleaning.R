
# 1) pick variables from raw data
# 2) add an indicator for attendance on day 2
# 3) exclude bad hrv variables, others
# OBS exclusion code has to be adjusted if more hrv vars are included, now manual
# OBS import dataset with height and weight for imputation, calc bmi after imp
# (20 participants have missing height and weight, noone is missing only one)



# ---- pick variables ----
# load("Data/RData/raw_new_hrv.RData")
# keep <- scan("Data/variables_keep.txt", character(), quote = "", quiet = TRUE)
# mets <- subset(raw, select = keep)

# save(mets, file="Data/RData/mets_raw.RData")

# --------------------------------------------------------

load("Data/RData/mets_raw.RData")

# ---- attendance flags ----
mets$attended_day2 <- !is.na(mets$PERS_KEY_T62)  # 7306
mets <- subset(mets, select= -PERS_KEY_T62)

mets$attended_cpt <- !is.na(mets$CPT_ATTENDED_T6)  # 8380 attended
mets <- subset(mets, select= -CPT_ATTENDED_T6)

nrow(hrv[is.na(hrv$PT_PRE_CPT_SDNN_T6) & is.na(hrv$PT_PRE_CPT_RMSSD_T6) & 
         is.na(hrv$PT_POST_CPT_SDNN_T6) & is.na(hrv$PT_POST_CPT_RMSSD_T6), ])
# 0


# ---- exclude AF ----
mets[which(mets$DATOAF > mets$PT_DATE_T6), c("DATOAF", "DIAGAF")] <- NA


mets$af <- mets$TYPEAF %in% 1:2 & mets$DIAGAF %in% 1:4 | 
           mets$ATRIAL_FIBRILLATION_T6 == 1  # + 17 by adding 3&4 


mets <- mets[-which(mets$af), ]



# ---- limit end point registry in time ----
# Need to remove the data that has come in after the participant's visit at T6


# -- CPT date
# We use date of CPT to limit data from the end point registry in time,
# but not every participant has that date registered:
table(is.na(mets$PT_DATE_T6))  # 1543 missing

# We are only interested in participants with CPT data, do any of those
# lack CPT date?
table(mets$attended_cpt, is.na(mets$PT_DATE_T6))
# 1 person lacks a CPT date even though they attended

View(mets[mets$attended_cpt & is.na(mets$PT_DATE_T6), ])  # PK 15304720 
# This person does not have any end point registry data
# Proceed with PT_DATE_T6

# OBS if these cvd variables are to be used for all participants, the code
# has to be altered, e.g. use the latest CPT date for those who don't have
# a date:
#latest.cpt <- max(mets$PT_DATE_T6, na.rm = T)  # 2008-12-19

# -- myocardial infarction
#max(mets$DATOINF[!is.na(mets$PT_DATE_T6)], na.rm = T)  # "2018-10-09"
mets[which(mets$DATOINF > mets$PT_DATE_T6), c("DATOINF", "DINFARKT")] <- NA
#max(mets$DATOINF[!is.na(mets$PT_DATE_T6)], na.rm = T)  # "2008-09-29"


# --stroke
mets[which(mets$DATOUKLAS > mets$PT_DATE_T6), c("DATOUKLAS", "DIAGUKLAS")] <- NA
mets[which(mets$DATOISCH > mets$PT_DATE_T6), c("DATOISCH", "DIAGISCH")] <- NA
mets[which(mets$DATOHEMO > mets$PT_DATE_T6), c("DATOHEMO", "DIAGHEMO")] <- NA
mets[which(mets$DATOSAB > mets$PT_DATE_T6), c("DATOSAB", "DIAGSAB")] <- NA


mets <- mets[, -grep("DAT", names(mets))]
mets <- subset(mets, select=-c(ATRIAL_FIBRILLATION_T6, DIAGAF, TYPEAF, af))


# ---- variable groups ----
day2.var <- names(mets)[grepl("T62", names(mets))]
attended <- names(mets)[grepl("attended", names(mets))]
hrv.var <- names(mets)[grepl("CPT", names(mets))]
hrv.pre <- names(mets)[grepl("PRE_CPT", names(mets))]
hrv.post <- names(mets)[grepl("POST_CPT", names(mets))]
cvd.var <- c("HEART_ATTACK_T6", "ANGINA_T6", "STROKE_T6")
ep.reg <- c(names(mets)[grepl("DIAG", names(mets))],
            "DINFARKT")




# ---- manual exclusion of hrv obs ----
# OBS we keep the row but force the cpt recording to be NA

#exclude HRV measurements based on visual inspection:
exclude.pre <- as.numeric(unlist(read.csv2("IBI inspection/pk_exclude_pre.csv", header = FALSE)))
exclude.post <- as.numeric(unlist(read.csv2("IBI inspection/pk_exclude_post.csv", header = FALSE)))


mets[mets$PERS_KEY  %in% exclude.pre, hrv.pre] <- NA 
mets[mets$PERS_KEY  %in% exclude.post, hrv.post] <- NA
#Exclude both sdnn and rmssd? -> technical mistake would affect both



# ---- exclude observations ----
# --Pregnant women
mets <- mets[-which(mets$PREGNANT_T6 == 1), ]  #removes 28 particiants ('not sure' would prob not affect waistline?)
mets <- subset(mets, select=-PREGNANT_T6)


# -- DMT1
# save them away first
dmt1 <- mets[which(mets$DIABETES_T6 & (mets$AGE_T6<=30 | mets$DIABETES_AGE_T6<=30)),]  #only diabetes_age applies in T6

#remove diabetes type 1 from the dataset:
mets <- mets[-which(mets$PERS_KEY %in% dmt1$PERS_KEY),] #- 30 rows (33 in total)


# ---- remove nonsensical values ----
# Two people have diabetes_age even though they say they dont have diabetes
# and do not seem to have diabetes based on their values (ok glucose & hba1c, no meds)
mets[(mets$DIABETES_T6==0 & !is.na(mets$DIABETES_AGE_T6)), "DIABETES_AGE_T6"] <- NA 


age <- which(mets$DIABETES_AGE_T6 > mets$AGE_T6)
#7 people have higher diabetes_age than age
#set diabetes age to age
mets[age, "DIABETES_AGE_T6"] <- mets[age, "AGE_T6"]


table(mets$attended_cpt)  # 7880
sum(!is.na(mets$PT_PRE_CPT_SDNN_T6) & !is.na(mets$PT_POST_CPT_SDNN_T6))  # 7124
sum(!is.na(mets$PT_PRE_CPT_SDNN_T6))  # 7704 pre cpt
sum(!is.na(mets$PT_POST_CPT_SDNN_T6))  # 7293 post cpt

nrow(hrv[is.na(hrv$PT_PRE_CPT_SDNN_T6) & is.na(hrv$PT_PRE_CPT_RMSSD_T6) & 
         is.na(hrv$PT_POST_CPT_SDNN_T6) & is.na(hrv$PT_POST_CPT_RMSSD_T6), ])
# 7 


sum(mets$attended_day2)  # 6635

# ---- save data ----
# save(dmt1, file="Data/RData/dmt1.RData")
# save(mets, attended, hrv.var, day2.var, cvd.var, ep.reg, file="Data/RData/mets_clean.RData")
