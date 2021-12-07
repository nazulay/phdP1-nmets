

library(haven)
library(readxl)

# Background: We found that one of the old exclusion criteria for the recordings
# unintentionally were a bit too strict and ended up excluding too many recordings.
# This code adds the "new" recordings to the existing dataset and removes a few 
# that are now excluded.

# The hrv.pre and hrv.post datasets are the results of the scripts in 
# P1/Add HRV data feb21/check_dataset2021_pre and _post, where exclusions
# and other adjustments were made.

# Now the new HRV data has to be swapped with the old.

# In addition, there are 4 (pre) and 54 (post) participants with hrv data in 
# the old data that need to be taken out

hrv.pre <- read_excel("Data/Excel/hrv_pre_cpt_variables_rounded.xlsx")
hrv.post <- read_excel("Data/Excel/hrv_post_cpt_variables_rounded.xlsx")

raw <- read_sav("Data/SPSS/kopi_uttrekk09042021_prno803000170_1.sav")


# starting point
table(raw$CPT_ATTENDED_T6, useNA = "ifany")  # 8343 F

table(is.na(raw$PT_PRE_CPT_SDNN_T6))  # 8202
table(is.na(raw$PT_POST_CPT_SDNN_T6))  # 7028

nrow(raw[!is.na(raw$CPT_ATTENDED_T6) & is.na(raw$PT_POST_CPT_SDNN_T6) & 
           is.na(raw$PT_PRE_CPT_SDNN_T6), ])  # 0


# ---- swap hrv data ----
vars.pre <- colnames(hrv.pre)[colnames(hrv.pre) %in% colnames(raw)][-1]
vars.post <- colnames(hrv.post)[colnames(hrv.post) %in% colnames(raw)][-1]

#safety checks
summary(raw[raw$PERS_KEY %in% hrv.pre$PERS_KEY, vars.pre])  # 27 missing, good
summary(raw[raw$PERS_KEY %in% hrv.post$PERS_KEY, vars.post])  # 837 missing, good

# are the pers_keys in the same order?
sum(raw[raw$PERS_KEY %in% hrv.pre$PERS_KEY, "PERS_KEY"] == hrv.pre$PERS_KEY)  # 8225, yes
sum(raw[raw$PERS_KEY %in% hrv.post$PERS_KEY, "PERS_KEY"] == hrv.post$PERS_KEY)  # 7811, yes



# swap
raw[raw$PERS_KEY %in% hrv.pre$PERS_KEY, vars.pre] <- hrv.pre[, vars.pre]
raw[raw$PERS_KEY %in% hrv.post$PERS_KEY, vars.post] <- hrv.post[, vars.post]
#no more missing

#update cpt_attended status if needed
table(raw[raw$PERS_KEY %in% hrv.pre$PERS_KEY, "CPT_ATTENDED_T6"], useNA = "ifany")  # 16
table(raw[raw$PERS_KEY %in% hrv.post$PERS_KEY, "CPT_ATTENDED_T6"], useNA = "ifany")  # 38
# not all since most had pre or post

raw[raw$PERS_KEY %in% hrv.pre$PERS_KEY, "CPT_ATTENDED_T6"] <- 1 
raw[raw$PERS_KEY %in% hrv.post$PERS_KEY, "CPT_ATTENDED_T6"] <- 1

table(raw$CPT_ATTENDED_T6, useNA = "ifany")  # 8382 (39 more)
table(is.na(raw$PT_PRE_CPT_SDNN_T6))  # 8229 (27 more)
table(is.na(raw$PT_POST_CPT_SDNN_T6))  # 7865 (837 more)

# also need to remove hrv values that are not in the new dataset
raw$attended_pre <- apply(raw[, vars.pre], 1, function(x) sum(is.na(x)) < length(vars.pre))
raw$attended_post <- apply(raw[, vars.post], 1, function(x) sum(is.na(x)) < length(vars.post))


View(raw[!raw$PERS_KEY %in% hrv.pre$PERS_KEY & raw$attended_pre, c(vars.pre, "attended_pre")])  # 4, correct
View(raw[!raw$PERS_KEY %in% hrv.post$PERS_KEY & raw$attended_post, c(vars.post, "attended_pre")])  # 54, correct


# set to missing
raw[!raw$PERS_KEY %in% hrv.pre$PERS_KEY & raw$attended_pre, vars.pre] <- NA
raw[!raw$PERS_KEY %in% hrv.post$PERS_KEY & raw$attended_post, vars.post] <- NA

#fix attended
raw[!raw$PERS_KEY %in% hrv.pre$PERS_KEY & raw$attended_pre, "attended_pre"] <- FALSE
raw[!raw$PERS_KEY %in% hrv.post$PERS_KEY & raw$attended_post, "attended_post"] <- FALSE

raw$CPT_ATTENDED_T6 <- ifelse(raw$attended_post | raw$attended_pre, 1, NA)
#checked that CPT_ATTENDED and attended_pre/post are the same except for those
#fixed above

table(raw$CPT_ATTENDED_T6)  # 8380
table(is.na(raw$PT_PRE_CPT_SDNN_T6))  # 8225 (4 less)
table(is.na(raw$PT_POST_CPT_SDNN_T6))  # 7811 (54 less)

#save(raw, file="Data/RData/raw_new_hrv.RData")


