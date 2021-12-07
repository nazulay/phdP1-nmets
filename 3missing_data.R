
library(naomiphd)
library(mice)

# ---- RAWDATA ----
# cleaned data further down
load("Data/RData/mets_raw.RData")


mets$attended_cpt <- !is.na(mets$CPT_ATTENDED_T6)  # 8380 attended
mets$attended_day2 <- !is.na(mets$PERS_KEY_T62)
mets <- subset(mets, select= -PERS_KEY_T62)

hrv.var <- names(mets)[grepl("CPT", names(mets))]
day2.var <- names(mets)[grepl("T62", names(mets))]


mets$acr_na <- rowSums(mets[day2.var])
na.cols.raw <- missing.colwise(mets)  # dont have attended vars for conditional missings



# -- missing ALL columns
sum(is.na(mets))/(ncol(mets)*nrow(mets))  # 0.372143


# -- missing among attended cpt
ep.reg <- c(names(mets)[grepl("DIAG", names(mets)) | grepl("DATO", names(mets))],
            "DINFARKT", "TYPEAF")


mis <- mets[, !names(mets) %in% c(ep.reg, "PERS_KEY", "HEIGHT_T6", "WEIGHT_T6", "PT_DATE_T6",
                                  "CPT_ATTENDED_T6", "PREGNANT_T6", "attended_day2", 
                                  "acr_na")]
mis_cpt <- mis[mis$attended_cpt, ]
mis_cpt <- subset(mis_cpt, select= -c(attended_cpt))

sum(is.na(mis_cpt))/(ncol(mis_cpt)*nrow(mis_cpt))  # 0.129642



# -- w/ cond diab age
# doesnt make sense when we include day2 variables..
mis_cpt$diab_age_na <- ifelse(is.na(mis_cpt$DIABETES_T6), 99, NA)
mis_cpt$diab_age_na <- ifelse(!is.na(mis_cpt$DIABETES_T6) & mis_cpt$DIABETES_T6 == 1 , mis_cpt$DIABETES_AGE_T6, mis_cpt$diab_age_na)
mis_cpt$diab_age_na <- ifelse(!is.na(mis_cpt$DIABETES_T6) & mis_cpt$DIABETES_T6 != 1 , 99, mis_cpt$diab_age_na)

mis_cpt <- subset(mis_cpt, select= -c(DIABETES_AGE_T6))
sum(is.na(mis_cpt))/(ncol(mis_cpt)*nrow(mis_cpt))  # 0.09780827



# -- w/o diab age
mis_cpt <- subset(mis_cpt, select= -c(diab_age_na))
sum(is.na(mis_cpt))/(ncol(mis_cpt)*nrow(mis_cpt))  # 0.1007325



# -- total missing
mis <- subset(mis, select= -c(attended_cpt))

sum(is.na(mis))/(ncol(mis)*nrow(mis))  # 0.1847726

# -- total w/o diab age
mis <- subset(mis, select= -c(DIABETES_AGE_T6))
sum(is.na(mis))/(ncol(mis)*nrow(mis))  # 0.1578992


where <- as.data.frame(is.na(mis))
where[mets$attended_day2, day2.var] <- FALSE
where[mets$attended_cpt, hrv.var] <- FALSE  # removed CPT_ATTENDED_T6 from hrv.var
#where[which(mets$DIABETES_T6 == 0), "DIABETES_AGE_T6"] <- FALSE
#where[is.na(mets$DIABETES_T6), "DIABETES_AGE_T6"] <- FALSE  #riktig valg?
# removed diab age from mis



# ---- CLEANED DATA ----
load("Data/RData/mets_clean.RData")

na.cols.clean <- missing.colwise(mets)

# -- total missing
mis <- subset(mets, select= -c(PERS_KEY, attended_cpt, attended_day2))
sum(is.na(mis))/(ncol(mis)*nrow(mis))  # 0.1923081


where <- as.data.frame(is.na(mets))
where[is.na(mets$PERS_KEY_T62), day2.var] <- FALSE
where[is.na(mets$CPT_ATTENDED_T6), hrv.var] <- FALSE
where[which(mets$DIABETES_T6 == 0), "DIABETES_AGE_T6"] <- FALSE
where[is.na(mets$DIABETES_T6), "DIABETES_AGE_T6"] <- FALSE  #riktig valg?

where <- subset(where, select= -c(PERS_KEY, CPT_ATTENDED_T6))  # PERS_KEY_T62, 



# ---- row wise ----
#numbers for raw data

#doesnt work with too many cols, but good visualisation for a subset
md.pattern(mets[mets$attended_cpt, c("DIABETES_T6", "GLUCOSE_T6")])  # from mice
#--

ncol(mis)  # 29


#row.na.pc <-  apply(mis, 1, function(x) sum(is.na(x))/ncol(mis))*100
#row.na <-  apply(mis, 1, function(x) sum(is.na(x)))

row.na <- rowSums(is.na(mis))
row.na.pc <- row.na/ncol(mis)*100

range(row.na)  # 0 27
range(row.na.pc)  # 0 93.1
sum(row.na == 0)  # 3858 rows without any missing
sum(row.na == 0)/nrow(mis)  # 0.2972036
hist(row.na.pc)
tab <- table(cut(row.na.pc, breaks = seq(0, 100, 10)))
sum(tab[5:10])  # 230 have 40 % or more missing


# -- only CPT attended
row.na <- rowSums(is.na(mis[mets$attended_cpt,]))
row.na.pc <- row.na/ncol(mis)*100
sum(row.na == 0)  # 3858 rows without any missing
# (everyone who was not missing any data, could also not be missing cpt data)
sum(row.na == 0)/nrow(mis[mets$attended_cpt,])  # 0.4603819

# -- conditional missing
row.na.cond <- rowSums(where)/ncol(where)*100
hist(row.na.cond)
range(row.na.cond)  # 0 93.1 %
sum(row.na.cond == 0)  # 4496 rows without any missing 
sum(row.na.cond == 0)/length(row.na.cond)  # 34.63524 %

# -- conditional and among cpt attended
sum(row.na.cond[mets$attended_cpt] == 0)  # 4496
sum(row.na.cond == 0)/length(row.na.cond[mets$attended_cpt]) # 0.5365155

# -- pregnant
preg <- sum(is.na(mis$PREGNANT_T6[which(mis$SEX_T6==0)]))  #528
wmn <- length(which(mis$SEX_T6==0))  #6928
na.preg <- preg/wmn  #0.07621247 %






# ---- patterns ----
library(lattice)
r <- is.na(mets$TIME_LAST_MEAL_T6)
histogram(~GLUCOSE_T6|r, data=mets)  # no pattern

r <- is.na(mets$WAIST_T6)
histogram(~AGE_T6|r, data=mets)  # not exactly the same, but not worrysome


