
library(ggplot2)
library(lattice)
library(plyr)
library(dplyr)
library(gmodels)  # ci
library(mgcv)

load("Data/RData/derived_vars_cvd.RData")
hrv_na <- mets_na[mets_na$attended_cpt,]


# General descriptives
boxplot(hrv_na$PT_PRE_CPT_SDNN_T6 ~ hrv_na$n_mets6, xlab="Number of MetS factors, diabetes")
ggplot(data=hrv_na, aes(AGE_T6)) + geom_histogram(bins=58, col='black', fill='light blue')


# ---- age ----
hist(mets_na$AGE_T6)
histogram(~AGE_T6|SEX_T6, data=mets_na)  # age looks the same between the sexes
mets_na$age_group <- cut(mets_na$AGE_T6, c(30, 40, 45, 60, 65, 75, 87), include.lowest = T)


# ---- waist by age ----
# by age group
histogram(~WAIST_T6|age_group, data=mets_na) # looks very similar over age

for (gr in levels(mets_na$age_group)) {
  print(c(gr, mean(mets_na$central_obesity[mets_na$age_group == gr], na.rm=T)))
}
#gets somewhat worse with age


# by age
waist.age <- ddply(mets_na, .(AGE_T6), summarise, avg_co = mean(central_obesity, na.rm=T)*100)

ggplot(waist.age, aes(x=AGE_T6, y=avg_co)) + geom_point() + ylab("% centrally obese")


# ---- waist by sex ----
histogram(~WAIST_T6|SEX_T6, data=mets_na) # some differences over sex

#women
waist_fm <- hist(mets_na$WAIST_T6[mets_na$SEX_T6 == 0])
abline(v=80, col="red")
mean(mets_na$central_obesity[mets_na$SEX_T6 == 0], na.rm=T)  # 0.8176574

#men
waist_m <- hist(mets_na$WAIST_T6[mets_na$SEX_T6 == 1])
abline(v=94, col="red")
mean(mets_na$central_obesity[mets_na$SEX_T6 == 1], na.rm=T)  # 0.7007162

#all
mean(mets_na$central_obesity, na.rm=T)  # 0.7639214


# ---- waist by sex and age ----

#average for each age group and sex
for (gr in levels(mets_na$age_group)) {
  for (sex in c(0, 1)) {
    print(c(gr, sex, mean(mets_na$central_obesity[mets_na$age_group == gr & mets_na$SEX_T6 == sex], na.rm=T))) 
  }
}
#women higher percentage in every age group


waist.age.sex <- ddply(mets_na, .(SEX_T6, AGE_T6), summarise, avg_waist=mean(WAIST_T6, na.rm=T),
                       avg_co = mean(central_obesity, na.rm=T)*100)
# -- waist
#women
ggplot(waist.age.sex[waist.age.sex$SEX_T6==0,], aes(x=AGE_T6, y=avg_waist)) + geom_point() +
  ggtitle("Women")
#men
ggplot(waist.age.sex[waist.age.sex$SEX_T6==1,], aes(x=AGE_T6, y=avg_waist)) + geom_point() +
  ggtitle("Men")


#both
ggplot(waist.age.sex, aes(x=AGE_T6, y=avg_waist)) + geom_point(aes(colour=factor(SEX_T6))) +
  geom_hline(yintercept=80, color='#F8766D') + geom_hline(yintercept = 94, color='#00BFC4')

write.xlsx(waist.age.sex, file='plotdata_gp.xlsx', sheetName = 'waist_avg', append=TRUE)


# -- co
# women
ggplot(waist.age.sex[waist.age.sex$SEX_T6==0,], aes(x=AGE_T6, y=avg_co)) + geom_point() +
  ggtitle("Women")

#men
ggplot(waist.age.sex[waist.age.sex$SEX_T6==1,], aes(x=AGE_T6, y=avg_co)) + geom_point() +
  ggtitle("Men")

# both
ggplot(waist.age.sex, aes(x=AGE_T6, y=avg_co)) + geom_point(aes(colour=factor(SEX_T6))) +
  ylab("% centrally obese")


# -- non linear stuff

# gam waist
gam_avg_waist <- gam(avg_waist ~ s(AGE_T6, by=SEX_T6) + SEX_T6, data=waist.age.sex, method="REML")
visreg(gam_avg_waist, "AGE_T6", "SEX_T6")
plot(gam_avg_waist, pages=1, all.terms=TRUE)

# gam co
gam_avg_co <- gam(avg_co ~ s(AGE_T6, by=SEX_T6) + SEX_T6, data=waist.age.sex, method="REML")
visreg(gam_avg_co, "AGE_T6", "SEX_T6")
plot(gam_avg_co, pages=1, all.terms=TRUE)


# add fitted line qudratic term
p = ggplot(waist.age.sex, aes(x=AGE_T6, y=avg_waist)) + geom_point(aes(colour=factor(SEX_T6)))

mets_na$AGE_2 <- mets_na$AGE_T6**2
fit <- lm(WAIST_T6 ~ AGE_T6 + factor(SEX_T6) + AGE_2, data=mets_na)
summary(fit)  # all sign
x0 <- seq(min(mets_na$AGE_T6), max(mets_na$AGE_T6))
#fm
y0_fm <- predict.lm(fit, newdata = list(AGE_T6 = x0, SEX_T6=rep(0, length(x0)), AGE_2=x0**2))
line_fm <- data.frame(x0, y0_fm)
#m
y0_m <- predict.lm(fit, newdata = list(AGE_T6 = x0, SEX_T6=rep(1, length(x0)), AGE_2=x0**2))
line_m <- data.frame(x0, y0_m)
p + geom_line(data=line_fm, aes(x0, y0_fm), color="red")+ geom_line(data=line_m, aes(x0, y0_m), color="blue")
#obs line not fitted directly to the plotted data, fitted to individual data


#individual points
#women
ggplot(mets_na[mets_na$SEX_T6==0,], aes(x=AGE_T6, y=WAIST_T6)) + geom_point(alpha = 0.3)+
  geom_hline(yintercept=80, color="red") 

library(ggpointdensity)
ggplot(mets_na[mets_na$SEX_T6==0,], aes(AGE_T6, WAIST_T6)) + geom_pointdensity() +
  scale_color_viridis_c()+ geom_hline(yintercept=80, color='red')

#men
ggplot(mets_na[mets_na$SEX_T6==1,], aes(x=AGE_T6, y=WAIST_T6)) + geom_point(alpha = 0.3)+
  geom_hline(yintercept=94, color="red") 

ggplot(mets_na[mets_na$SEX_T6==1,], aes(AGE_T6, WAIST_T6)) + geom_pointdensity() +
  scale_color_viridis_c() + geom_hline(yintercept=94, color='red')



#gam 
library(mgcv)
library(visreg)
gam_waist <- gam(WAIST_T6 ~ s(AGE_T6, by=SEX_T6, sp=3) + SEX_T6, data=mets_na, method="REML")

visreg(gam_waist)
visreg(gam_waist, "AGE_T6", "SEX_T6")
plot(gam_waist, pages=1, all.terms=TRUE)

summary(lm(WAIST_T6 ~ AGE_T6, data=mets_na))
#sign when age is only factor, but weak connection
#(for every one year you get older, waist increases with 0.11 cm)

summary(lm(WAIST_T6 ~ AGE_T6 + SEX_T6, data=mets_na))
#basically same


# ---- glucose ----


# ---- glucose vs HRV ----

plot(hrv_na$GLUCOSE_T6, hrv_na$PT_PRE_CPT_SDNN_T6)
#not linear, but what is it?


# plot grouped by diabetes T/F
ggplot(hrv_na[!is.na(hrv_na$diabetic), ], aes(x=GLUCOSE_T6, y=PT_PRE_CPT_SDNN_T6)) + 
  geom_point() + facet_wrap(~diabetic, ncol=2)




# ---- glucose vs TSLM ----

plot(hrv_na$TIME_LAST_MEAL_T6, hrv_na$GLUCOSE_T6)

ggplot(mets_na, aes(factor(TIME_LAST_MEAL_T6), GLUCOSE_T6)) + geom_boxplot()
table(mets_na$TIME_LAST_MEAL_T6, useNA = "ifany") 
# prob more to do with number of people in the groups than actual trend


# boxplot with more even groups (random sample)
tmp <- mets_na[mets_na$TIME_LAST_MEAL_T6 %in% c(0, 7, 8, NA),]

for (hour in c(1:6, 9)) {
  tmp <- rbind(tmp, sample_n(mets_na[which(mets_na$TIME_LAST_MEAL_T6 == hour), ],
                             size=200, replace=F))
}

table(tmp$TIME_LAST_MEAL_T6, useNA = "ifany") 
ggplot(tmp, aes(factor(TIME_LAST_MEAL_T6), GLUCOSE_T6)) + geom_boxplot()
#Cant see any association



#plot cinfidence intervals
glucose.ci <- data.frame(Estimate=NA, lower=NA, upper=NA, SE=NA)
for (i in 1:9) {
  glucose.ci[i,] <- ci(x=mets_na$GLUCOSE[mets_na$TIME_LAST_MEAL_T6 == i], na.rm=T)
}

glucose.ci$tslm <- row.names(glucose.ci)
qplot(x=tslm, y=Estimate, data=glucose.ci) + geom_errorbar(aes(ymin=lower, ymax=upper, width=0.15))

#looks like a U shape, few people with 7 or 8 hours



summary(lm(GLUCOSE_T6 ~ TIME_LAST_MEAL_T6, data=mets_na))
#significant neg ass, but very small effect (-0.16 after 8h fasting)




# ---- TSLM vs HRV ----
#
plot(hrv_na$TIME_LAST_MEAL_T6, hrv_na$PT_PRE_CPT_SDNN_T6)
boxplot(hrv_na$PT_PRE_CPT_SDNN_T6 ~ hrv_na$TIME_LAST_MEAL_T6)
#only a difference of n?
table(hrv_na$TIME_LAST_MEAL_T6)


#plot cinfidence intervals
hrv.ci <- data.frame(Estimate=NA, lower=NA, upper=NA, SE=NA)

for (i in 1:9) {
  hrv.ci[i,] <- ci(x=hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$TIME_LAST_MEAL_T6 == i], na.rm=T)
}

hrv.ci$tslm <- row.names(hrv.ci)
qplot(x=tslm, y=Estimate, data=hrv.ci) + geom_errorbar(aes(ymin=lower, ymax=upper, width=0.15))
#Random?
#(C sees a non-linear relationship for <=4h, real?)


summary(lm(PT_PRE_CPT_SDNN_T6 ~ SEX_T6+AGE_T6+TIME_LAST_MEAL_T6, data=hrv_na))  # pos *
summary(lm(PT_PRE_CPT_SDNN_T6 ~ TIME_LAST_MEAL_T6, data=hrv_na))  # not sign
summary(lm(PT_PRE_CPT_SDNN_T6 ~ SEX_T6+AGE_T6+TIME_LAST_MEAL_T6+GLUCOSE_T6, data=hrv_na))
#tslm *, glucose ***
summary(lm(PT_PRE_CPT_SDNN_T6 ~ SEX_T6+AGE_T6+TIME_LAST_MEAL_T6*GLUCOSE_T6, data=hrv_na))
# tslm*glucose not sign

hrv_na$TIME_LAST_MEAL_T6_sq <- hrv_na$TIME_LAST_MEAL_T6**2
summary(lm(PT_PRE_CPT_SDNN_T6~TIME_LAST_MEAL_T6+TIME_LAST_MEAL_T6_sq, data=hrv_na)) # not sign

#summary:
# alone: not sign
# with age+sex: sign
# with age+sex+glucose: sign
# interaction tslm*glucose not sign





# ---- HRV by glucose and TSLM ----

# glucose and hrv | tslm >= 4
plot(hrv_na$GLUCOSE_T6[hrv_na$TIME_LAST_MEAL_T6>=4], hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$TIME_LAST_MEAL_T6>=4], xlim=c(3, 18), ylim=c(0, 300))

# glucose and hrv | tslm < 4
plot(hrv_na$GLUCOSE_T6[hrv_na$TIME_LAST_MEAL_T6<4], hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$TIME_LAST_MEAL_T6<4], xlim=c(3, 18), ylim=c(0, 300))
#similar pattern just fewer obs


# make a variable with 3 levels: diab, gluc, normal
hrv_na$glucgroup <- NA
hrv_na$glucgroup[hrv_na$diabetic] <- "diabetic"
hrv_na$glucgroup[hrv_na$high_glucose] <- "glucose"
hrv_na[is.na(hrv_na$glucgroup), "glucgroup"] <- "normal"
hrv_na$glucgroup <- as.factor(hrv_na$glucgroup)

boxplot(hrv_na$PT_PRE_CPT_SDNN_T6~hrv_na$glucgroup)

fit <- lm(PT_PRE_CPT_SDNN_T6~AGE_T6 + SEX_T6 + glucgroup, data=hrv_na)
summary(fit)
an <- aov(fit)
summary(an)

ggplot(hrv_na[!is.na(hrv_na$glucgroup), ], aes(x=GLUCOSE_T6, y=PT_PRE_CPT_SDNN_T6)) + 
  geom_point() + facet_wrap(~glucgroup, ncol=3)


# HRV differences for glucose groups
#diabetes
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$diabetic], na.rm=T)  # 32.29409

#high glucose
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$high_glucose], na.rm=T)  # 36.34496

#ref
mean(hrv_na$PT_PRE_CPT_SDNN_T6[!hrv_na$high_glucose4 & !hrv_na$diabetic], na.rm=T)  # 40.33565


mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6<6.4], na.rm=T)  # 39.56263
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6>=6.4], na.rm=T)  # 33.44964
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6>=6.4 & hrv_na$GLUCOSE_T6 < 7], na.rm=T)  # 35.28588
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6>=7], na.rm=T)  # 32.35231


mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$TIME_LAST_MEAL_T6 <4], na.rm=T)  # 38.36944
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$TIME_LAST_MEAL_T6 >=4], na.rm=T)  # 40.55109


#glucose and tslm
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6<6.4 & hrv_na$TIME_LAST_MEAL_T6 <4], na.rm=T)  # 38.82403
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6<6.4 & hrv_na$TIME_LAST_MEAL_T6 >=4], na.rm=T)  # 40.83409

mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6>=6.4 & hrv_na$GLUCOSE_T6 < 7  & hrv_na$TIME_LAST_MEAL_T6 <4], na.rm=T)  # 34.10062
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6>=6.4 & hrv_na$GLUCOSE_T6 < 7  & hrv_na$TIME_LAST_MEAL_T6 >=4], na.rm=T)  # 38.55094

mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6>=7  & hrv_na$TIME_LAST_MEAL_T6 <4], na.rm=T)  # 31.54086
mean(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$GLUCOSE_T6>=7  & hrv_na$TIME_LAST_MEAL_T6 >=4], na.rm=T)  # 34.8957



t.test(hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$glucgroup=="normal"], hrv_na$PT_PRE_CPT_SDNN_T6[hrv_na$glucgroup=="glucose"])
# p 1.097e-06

summary(lm(PT_PRE_CPT_SDNN_T6~glucgroup, data=hrv_na))  # global test p 1.604e-15
an <- aov(PT_PRE_CPT_SDNN_T6~glucgroup, data=hrv_na)
summary(an)  # glucgroup sign
TukeyHSD(an)  # every level sign

summary(lm(PT_PRE_CPT_SDNN_T6~AGE_T6+SEX_T6+glucgroup, data=hrv_na))  # global test p 2.2e-16
an <- aov(PT_PRE_CPT_SDNN_T6~AGE_T6+SEX_T6+glucgroup, data=hrv_na)
summary(an)  # glucgroup sign
TukeyHSD(an, 'glucgroup')  # gluc-diab not sign, rest sign

summary(lm(PT_PRE_CPT_SDNN_T6~AGE_T6+SEX_T6+glucgroup+TIME_LAST_MEAL_T6, data=hrv_na))  # global test p 2.2e-16,
# tslm sign (0.0462)
an <- aov(PT_PRE_CPT_SDNN_T6~AGE_T6+SEX_T6+glucgroup, data=hrv_na)
summary(an)  # glucgroup sign
TukeyHSD(an, 'glucgroup')  # gluc-diab not sign, rest sign



#diff gluc >/< 4h
mean(hrv_na$GLUCOSE_T6[hrv_na$glucgroup=="normal" & hrv_na$TIME_LAST_MEAL_T6 <4], na.rm=T)  # 4.866222
mean(hrv_na$GLUCOSE_T6[hrv_na$glucgroup=="normal" & hrv_na$TIME_LAST_MEAL_T6 >=4], na.rm=T)  # 4.894071
#sign diff

mean(hrv_na$GLUCOSE_T6[hrv_na$glucgroup=="glucose" & hrv_na$TIME_LAST_MEAL_T6 <4], na.rm=T)  # 6.139027
mean(hrv_na$GLUCOSE_T6[hrv_na$glucgroup=="glucose" & hrv_na$TIME_LAST_MEAL_T6 >=4], na.rm=T)  # 6.011236
#sign diff

mean(hrv_na$GLUCOSE_T6[hrv_na$glucgroup=="diabetic" & hrv_na$TIME_LAST_MEAL_T6 <4], na.rm=T)  # 7.595763
mean(hrv_na$GLUCOSE_T6[hrv_na$glucgroup=="diabetic" & hrv_na$TIME_LAST_MEAL_T6 >=4], na.rm=T)  # 6.926064
#sign diff




# ---- HRV by sex and age ----

hrv.age.sex <- ddply(mets_na, .(SEX_T6, AGE_T6), summarise, avg_sdnn=mean(PT_PRE_CPT_SDNN_T6, na.rm=T))

#women
ggplot(hrv.age.sex[hrv.age.sex$SEX_T6==0,], aes(x=AGE_T6, y=avg_sdnn)) + geom_point() +
  ggtitle("Women")
#men
ggplot(hrv.age.sex[hrv.age.sex$SEX_T6==1,], aes(x=AGE_T6, y=avg_sdnn)) + geom_point() +
  ggtitle("Men")


#both
ggplot(hrv.age.sex, aes(x=AGE_T6, y=avg_sdnn)) + geom_point(aes(colour=factor(SEX_T6)))

write.xlsx(hrv.age.sex, file='plotdata_gp.xlsx', sheetName = 'hrv_avg', append=TRUE)

summary(lm(PT_PRE_CPT_SDNN_T6~SEX_T6+AGE_T6, data=hrv_na))


# all points, a mess
ggplot(hrv_na, aes(x=AGE_T6, y=PT_PRE_CPT_SDNN_T6)) + geom_point(aes(colour=factor(SEX_T6)))


# ---- diabetes t1 vs 2 ----
load("Data/RData/derived_dmt1.RData")
dmt2 <- mets_na[which(mets_na$diabetic),]
table(dmt1$SEX_T6)  # 15 women, 18 men
table(dmt2$SEX_T6)  # 423 w, 487 m

table(dmt1$AGE_T6)
hist(dmt1$AGE_T6)
hist(dmt2$AGE_T6)

mean(dmt1$gluc_low_meds, na.rm=T)  # 0.8125
mean(dmt2$gluc_low_meds, na.rm=T)  # 0.5415282
sum(dmt1$gluc_low_meds, na.rm=T)  # 26
sum(dmt2$gluc_low_meds, na.rm=T)  # 489

#chi square test
# (same p-values, different estimates? diff parameters being estimated?)
prop.test(c(26, 489), c(33, 931))
dmt_meds<-matrix(c(26,489,7,442),ncol=2,byrow=T)
rownames(dmt_meds) <- c("meds", "no meds")
colnames(dmt_meds) <- c("t1", "t2")
prop.test(dmt_meds)

mean(dmt1$GLUCOSE_T6, na.rm=T)  # 10.14687
mean(dmt2$GLUCOSE_T6, na.rm=T)  # 5.728635
t.test(dmt1$GLUCOSE_T6, dmt2$GLUCOSE_T6)  # sign diff, p=0.0003282
boxplot(dmt1$GLUCOSE_T6, dmt2$GLUCOSE_T6)
mean(dmt1$HBA1C_T6, na.rm=T)  # 7.853125
mean(dmt2$HBA1C_T6, na.rm=T)  # 5.914062
t.test(dmt1$HBA1C_T6, dmt2$HBA1C_T6)  # sign diff, p=5.554e-08
boxplot(dmt1$HBA1C_T6, dmt2$HBA1C_T6)


par(mfrow=c(2,2))
barplot(c(table(dmt1$n_mets), "5" = 0), col=rgb(1,0,0,0.5))
title("dmt1 n_mets")
barplot(table(dmt2$n_mets), col=rgb(0,0,1,0.5))
title("dmt2 n_mets")
hist(dmt1$AGE_T6, col=rgb(1,0,0,0.5))
hist(dmt2$AGE_T6, col=rgb(0,0,1,0.5))

mean(dmt1$AGE_T6)  # 49.12121
sd(dmt1$AGE_T6)  # 12.15462
median(dmt1$AGE_T6)  # 43


#sapply(mets_na, class)
#factor_vars <- c("SEX_T6", "LIPID_LOWERING_DRUGS_T6", "BP_TREATMENT_T6", "DEVIATIONS_BP_T6",
#                 "INSULIN_T6", "DIABETES_TABLETS_T6")
#mets_na[factor_vars] <- lapply(mets_na[factor_vars], as.factor)


# oversikt MetS
table(mets_na$n_mets)
prop.table(table(mets_na$n_mets))*100
mean(mets_na$n_mets >= 3, na.rm = TRUE)  # 0.339111 with metS


table(mets_na$diabetic, useNA = "ifany") 

diab <- mets_na[which(mets_na$diabetic == 1),]

#NA removed:
mean(diab$gluc_low_meds, na.rm=T)
# 54.43322% of the diabs use medications

sum(diab$DIABETES_T6 == 1 & diab$gluc_low_meds, na.rm = T)/sum(diab$DIABETES_T6 == 1)
#76.34069 of self reported diabetes uses medication

sum(diab$DIABETES_T6 == 0 & diab$gluc_low_meds, na.rm = T)
# only 1 person of the glucose-diabetics uses medications

sum(diab$DIABETES_T6 == 1 & diab$GLUCOSE_T6 >=7, na.rm = T)/sum(diab$DIABETES_T6 == 1)
# 45.11041% of the self reported diabs has glucose >= 7

sum(diab$DIABETES_T6 == 1 & diab$DIABETES_AGE_T6 <=30, na.rm = T)/sum(diab$DIABETES_T6 == 1)
# 5.205047% of the self reported diabs got diabetes at age <= 30
mean(diab$DIABETES_T6 == 1 & diab$DIABETES_AGE_T6 <=30, na.rm = T)


# ---- compare gluc ----
#CRITERIA 1
mean(diab$high_glucose, na.rm=T)
#55.90551% of diabetics meets glucose criterion 1
table(diab$high_glucose, useNA = "ifany")
sum(diab$high_glucose & diab$DIABETES_AGE_T6 <= 30, na.rm=T)
table(diab$high_glucose, diab$DIABETES_T6, useNA="ifany")
sum(diab$DIABETES_T6 == 0 & diab$high_glucose, na.rm=T)
hist(diab$DIABETES_AGE_T6[diab$high_glucose]) #doesnt work
sum(diab$DIABETES_T6 == 1 & diab$high_glucose, na.rm=T)/sum(diab$DIABETES_T6 == 1)

 
sum(diab$DIABETES_T6 == 0 & diab$high_glucose, na.rm=T)/sum(diab$DIABETES_T6==0)
sum(diab$DIABETES_T6 == 1 & diab$high_glucose, na.rm=T)/sum(diab$DIABETES_T6==1)

table(diab$DIABETES_T6, diab$high_glucose, useNA = "ifany")
table(diab$DIABETES_AGE_T6 <= 30, useNA = "ifany")
table(diab$DIABETES_T6, diab$DIABETES_AGE_T6 <= 30, useNA = "ifany")
sum(diab$DIABETES_T6==1, diab$DIABETES_AGE_T6 <= 30, na.rm=T)
#rowSums/colSums for margins


#Use of medications
table(diab$DIABETES_T6, diab$INSULIN_T6)
prop.table(table(diab$DIABETES_T6, diab$INSULIN_T6==1), margin=1)
# 32.75% of self rep uses insulin, 35% uses/have used

table(diab$INSULIN_T6, diab$DIABETES_AGE_T6<=30)
prop.table(table(diab$INSULIN_T6, diab$DIABETES_AGE_T6<=30), margin=1)


#NA incl:
sum(mets_na$diabetic & mets_na$gluc_low_meds, na.rm = T)/sum(mets_na$diabetic, na.rm=T)
# 53.12158% of diabs use medicine

#same as above
#sum(mets_na$DIABETES_T6 == 1 & mets_na$gluc_low_meds, na.rm = T)/sum(mets_na$DIABETES_T6 == 1, na.rm=T)
# 76.34069% of self reported diabs uses medicine

sum(mets_na$diabetic & !mets_na$DIABETES_T6 == 1 & mets_na$gluc_low_meds, na.rm=T)/sum(mets_na$diabetic & !mets_na$DIABETES_T6 == 1, na.rm=T)


sum(mets_na$diabetic == 1 & mets_na$DIABETES_T6 == 1, na.rm = T)/sum(mets_na$diabetic == 1, na.rm=T)
# 69.4414% of those that we define as diabs have reported having diab

sum(mets_na$diabetic == 1 & mets_na$GLUCOSE_T6 >= 7, na.rm = T)/sum(mets_na$diabetic == 1, na.rm=T)
# 61.8839% of those we define as diabs have glucose >= 7

sum(mets_na$DIABETES_T6 == 1 & mets_na$GLUCOSE_T6 >= 7, na.rm = T)/sum(mets_na$DIABETES_T6 == 1, na.rm=T)
# 45.11041% of those that self reported diabetes have glucose >=7

sum(mets_na$diabetic == 1 & mets_na$high_glucose == 1, na.rm = T)/sum(mets_na$diabetic == 1, na.rm=T)
# 54.43593% of the diabs meet the glucose criterion

tab <- table(diab$DIABETES_T6, diab$gluc_low_meds, useNA = "ifany")
addmargins(tab, FUN=list(Total = sum), quiet=TRUE)


#---- MetS facts ----
table(mets_na$n_mets6_cat, useNA = "ifany")


sum(mets_na$central_obesity, na.rm = TRUE)  # 8821
mean(mets_na$central_obesity, na.rm=TRUE)  # 0.7639214

#sum(mets_na$central_obesity2, na.rm = TRUE)  # 5806
#mean(mets_na$central_obesity2, na.rm=TRUE)  # 0.4641087

table(mets_na$SEX_T6[mets_na$central_obesity])  # 0.4214942 men
# age adjusted in the paper


mean(mets_na$LIPID_LOWERING_DRUGS_T6 == 1, na.rm=TRUE)  # 0.1462497

sum(mets_na$high_triglyc, na.rm=TRUE)  # 4840
mean(mets_na$high_triglyc, na.rm=TRUE)  # 0.3810124

sum(mets_na$low_hdl, na.rm=TRUE)  # 3412
mean(mets_na$low_hdl, na.rm=TRUE)  # 0.2692126

sum(mets_na$high_BP, na.rm = TRUE)  # 0.6269132
mean(mets_na$high_BP, na.rm = TRUE)  # 0.6269132

mean(mets_na$high_glucose, na.rm=TRUE)  # 0.07934869
sum(mets_na$high_glucose, na.rm=TRUE)  # 1319
mean(mets_na$gluc_low_meds, na.rm=TRUE)  # 0.04068252

sum(mets_na$DIABETES_T6, na.rm=TRUE)  # 634
mean(mets_na$DIABETES_T6, na.rm=TRUE)  # 0.0499134
sum(mets_na$diabetic, na.rm=TRUE)  # 913
mean(mets_na$diabetic, na.rm=TRUE)  # 0.07258129

# ---- histogram nmets diab ----

# Figur "nmets_non_diab_vs_diab"

par(
  mfrow=c(2,2),
  mar=c(4,4,1,0),
  oma=c(2,2,2,4)
)


barplot(table(mets_na$n_mets[mets_na$diabetic == 0]), col=rgb(1,0,0,0.5))
barplot(table(mets_na$n_mets[mets_na$diabetic == 1]), col=rgb(0,0,1,0.5))
#mtext("Title", outer=TRUE, cex=1.5)

mtext("Nr of metabolic syndrome factors", side=1, outer=TRUE, cex=1.3, line=-1)
mtext("Frequency", side=2, outer=TRUE, las=0, cex=1.3, line = 0) 
#line = -1 tar y-aksen n?rmere plottet
#cex = skriftst?rrelse

hist(mets_na$AGE_T6[mets_na$diabetic == 0], main="", xlab="", col=rgb(1,0,0,0.5))
hist(mets_na$AGE_T6[mets_na$diabetic == 1], main="", xlab="", col=rgb(0,0,1,0.5))

#Lage figur utenfor figuren for aa sette legend riktig sted
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

#ypos var 0.87
legend(x=0.45, y=-0.9, legend=c("Non-diabetics", "Diabetics"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), 
       pt.cex=1.5, pch=15, xpd=TRUE, horiz = TRUE, cex=0.8)




# histogram av samme, ser rart ut pga x-aksen har med 6
# hist(mets_na$n_mets[mets_na$diabetic == 1], breaks=0:6, col=rgb(1,0,0,0.5) , 
#      xlab="" , ylab="" , main="", right=FALSE, xaxt="n" )  #xaxt fjerner hele x-aksen
# axis(1, at=0:5) # legger p? ny men er fortsatt venstrejustert
# hist(mets_na$n_mets[mets_na$diabetic == 0], breaks=0:6, col=rgb(0,0,1,0.5) , 
#      xlab="" , ylab="" , main="", right=FALSE )




table(mets_na$n_mets[mets_na$diabetic == 1])

table(CO = mets_na$central_obesity, BP = mets_na$high_BP)
prop.table(table(CO = mets_na$central_obesity, BP = mets_na$high_BP))

table(CO = mets_na$central_obesity, tri = mets_na$high_triglyc)
prop.table(table(CO = mets_na$central_obesity, tri = mets_na$high_triglyc))




# ---- n_mets ----
table(mets_na$n_mets)
prop.table(table(mets_na$n_mets))*100
mean(mets_na$n_mets >= 3, na.rm = TRUE)  # 0.339111 with metS




# ---- HRV ----
# correlation between pre and post:
hrv.sdnn <- hrv_na[!is.na(hrv_na$PT_POST_CPT_SDNN_T6) & !is.na(hrv_na$PT_PRE_CPT_SDNN_T6),]
hrv.rmssd <- hrv_na[!is.na(hrv_na$PT_POST_CPT_RMSSD_T6) & !is.na(hrv_na$PT_PRE_CPT_RMSSD_T6),]


cor(hrv.sdnn$PT_PRE_CPT_SDNN_T6, hrv.sdnn$PT_POST_CPT_SDNN_T6)  # 0.5080703
cor(hrv.rmssd$PT_PRE_CPT_RMSSD_T6, hrv.rmssd$PT_POST_CPT_RMSSD_T6)  # 0.6628908

cor(log(hrv.sdnn$PT_PRE_CPT_SDNN_T6), log(hrv.sdnn$PT_POST_CPT_SDNN_T6))  # 0.5695864
cor(log(hrv.rmssd$PT_PRE_CPT_RMSSD_T6), log(hrv.rmssd$PT_POST_CPT_RMSSD_T6))  # 0.727406


sum(hrv_na$central_obesity, na.rm = TRUE)  # 6269
mean(hrv_na$central_obesity, na.rm=TRUE)  # 0.784017


mean(hrv_na$central_obesity2 & hrv_na$SEX_T6 == 0, na.rm=TRUE)  # 0.2879454 women
mean(hrv_na$central_obesity2 & hrv_na$SEX_T6 == 1, na.rm=TRUE)  # 0.167621 men
# age adjusted i artikkelen


mean(hrv_na$LIPID_LOWERING_DRUGS_T6 == 1, na.rm=TRUE)  # 0.1408676

sum(hrv_na$high_triglyc, na.rm=TRUE)  # 3097
mean(hrv_na$high_triglyc, na.rm=TRUE)  # 0.383387

sum(hrv_na$low_hdl, na.rm=TRUE)  # 2128
mean(hrv_na$low_hdl, na.rm=TRUE)  # 0.2638562

sum(hrv_na$high_BP, na.rm = TRUE)  # 5111
mean(hrv_na$high_BP, na.rm = TRUE)  # 0.6267321

mean(hrv_na$high_glucose, na.rm=TRUE)  # 0.0753493
sum(hrv_na$high_glucose, na.rm=TRUE)  # 809
mean(hrv_na$gluc_low_meds, na.rm=TRUE)  # 0.03780154

sum(hrv_na$DIABETES_T6, na.rm=TRUE)  # 371
mean(hrv_na$DIABETES_T6, na.rm=TRUE)  # 0.04596704
sum(hrv_na$diabetic, na.rm=TRUE)  # 550
mean(hrv_na$diabetic, na.rm=TRUE)  # 0.0686556


n_mets.hrv <- table(hrv_na$n_mets)
prop.table(table(hrv_na$n_mets))*100
mean(hrv_na$n_mets >= 3, na.rm = TRUE)  # 0.3479639 with metS




# --- plot n_mets ----
counts <- data.frame(matrix(c(table(mets_na$n_mets), table(hrv_na$n_mets)), ncol=2))
names(counts) <- c("all", "hrv")
counts <- stack(counts)
counts$nr <- rep(0:5, 2)

#Figur "Bar_nmets_all_vs_hrv"

p <- ggplot(data=counts, aes(x=nr, y=values, fill=ind)) + geom_bar(stat="identity", position=position_dodge()) + 
            geom_text(aes(label=values), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) + theme_minimal()
p + scale_fill_brewer(palette="Paired") + scale_x_discrete("Number of metabolic syndrome factors", 0:5, 0:5, 0:5) +
    labs(fill = "Dataset", y = "Participants")

dev.off()


# ---- plot n_mets gluc2 ----
# counts <- data.frame(matrix(c(table(mets_na$n_mets), table(mets_na$n_mets)), ncol=2))
# names(counts) <- c("1", "2")
# counts <- stack(counts)
# counts$nr <- rep(0:5, 2)
# 
# png("N:/data/durable/B/T6new2019/Naomi/P1/Figures/MetS/n_mets_criteria_1_2.png")
# 
# p <- ggplot(data=counts, aes(x=nr, y=values, fill=ind)) + geom_bar(stat="identity", position=position_dodge()) + 
#   geom_text(aes(label=values), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) + theme_minimal()
# p + scale_fill_brewer(palette="Paired") + scale_x_discrete("Number of metabolic syndrome factors", 0:5, 0:5, 0:5) +
#   labs(fill = "Criteria", y = "Participants")
# 
# dev.off()
# 
# ---- plot factors ----
counts.mets <- sapply(mets_na[c(mets.comp, "diabetic")], sum, na.rm=TRUE)
counts.hrv <- sapply(hrv_na[c(mets.comp, "diabetic")], sum, na.rm=TRUE)
counts <-data.frame(matrix(c(counts.mets, counts.hrv), ncol=2))
names(counts) <- c("all", "hrv")
counts <- stack(counts)
counts$comp <- c(mets.comp, "diabetic")

#Figur "Dist_mets_factors"

p <- ggplot(data=counts, aes(x=comp, y=values, fill=ind)) + geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=values), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5) + theme_minimal()
p + scale_fill_brewer(palette="Paired") + labs(fill = "Dataset", y = "Participants", x = "Metabolic syndrome factor") +
  scale_x_discrete(limits=c(mets.comp, "diabetic"))

dev.off()

# ---- plot hba1c ----
sum(!is.na(mets_na$HBA1C_T6))  # 12768
sum(!is.na(mets_na$PT_PRE_CPT_SDNN_T6))  # 8202
sum(!is.na(mets_na$HBA1C_T6) & !is.na(mets_na$PT_PRE_CPT_SDNN_T6), na.rm=TRUE)  # 8097

png("N:/data/durable/B/T6new2019/Naomi/P1/Figures/MetS/sdnn_hba1c.png")
plot(mets_na$PT_PRE_CPT_SDNN_T6, mets_na$HBA1C_T6, ylab="Hba1c", xlab="SDNN pre CPT")
#plot(hrv_na$HBA1C_T6, hrv_na$PT_PRE_CPT_SDNN_T6, xlab="Hba1c", ylab="SDNN pre CPT")  # helt likt
dev.off()


# ---- albuminuria ----
#OBS b?r ekskludere de med sykdom som gir lav hba1c

sum(!is.na(mets_na$albuminuria))  #  6899 i mets
sum(!is.na(hrv_na$albuminuria))  #  4646 i hrv
sum(mets_na$HBA1C_T6 & mets_na$PT_PRE_CPT_SDNN_T6 & mets_na$acr.mean, na.rm = TRUE)  # 4599 samme med og uten is.na
sum(hrv_na$albuminuria, na.rm=T)  # 333

#Figur "sdnn_hba1c_albuminuria"
plot(HBA1C_T6~PT_PRE_CPT_SDNN_T6, data=hrv_na[which(!hrv_na$albuminuria),], 
     ylab="Hba1c", xlab="SDNN pre CPT", col="red")
points(HBA1C_T6~PT_PRE_CPT_SDNN_T6, data=hrv_na[which(hrv_na$albuminuria),], col="green")

legend("topright", legend=c("albuminuria", "not albuminuria"), col=c("green", "red"), cex=1, pch=1)


alb = hrv_na[which(hrv_na$albuminuria & !is.na(hrv_na$HBA1C_T6)),]
no_alb = hrv_na[which(!hrv_na$albuminuria & !is.na(hrv_na$HBA1C_T6)),]

ggplot(alb, aes(PT_PRE_CPT_SDNN_T6, HBA1C_T6)) + geom_point() + geom_smooth()

library(lattice)
xyplot(HBA1C_T6 ~ PT_PRE_CPT_SDNN_T6, type=c("smooth", "p"), data=alb)
xyplot(HBA1C_T6 ~ PT_PRE_CPT_SDNN_T6, type=c("smooth", "p"), data=no_alb)

mod <- nls(HBA1C_T6~a +b/PT_PRE_CPT_SDNN_T6^(-c), data=alb,
                        start = list(a = 8, b = 1, c=0))

lines(alb$PT_PRE_CPT_SDNN_T6, predict(reg, list(x = alb$PT_PRE_CPT_SDNN_T6)))

reg <- lm(HBA1C_T6~1/PT_PRE_CPT_SDNN_T6, data=alb)
abline(reg$coefficients, col="red")

reg <- lm(HBA1C_T6~PT_PRE_CPT_SDNN_T6, data=hrv_na[which(!hrv_na$albuminuria),])
abline(reg$coefficients, col="green")

#loess_fit <- loess(HBA1C_T6 ~ PT_PRE_CPT_SDNN_T6, alb)
#lines(alb$PT_PRE_CPT_SDNN_T6, predict(loess_fit), col = "blue")



# sum(!is.na(mets_na$CREATININ_T6))  #  12827
# sum(!is.na(mets_na$creatinine.mean))  # 6895
# sum(mets_na$HBA1C_T6 & mets_na$PT_PRE_CPT_SDNN_T6 & mets_na$CREATININ_T6, na.rm = TRUE)  # 8095
# sum(mets_na$HBA1C_T6 & mets_na$PT_PRE_CPT_SDNN_T6 & mets_na$creatinine.mean, na.rm = TRUE)  # 4595
# 
# 
# sum(!is.na(mets_na$albuminuria.mean))
#sum(mets_na$HBA1C_T6 & mets_na$PT_PRE_CPT_SDNN_T6 & mets_na$albuminuria.mean, na.rm = TRUE)  # 4599



# hrv corr in separate script
#chi square test:

tbl <- table(hrv_na$HBA1C_T6 > 7, hrv_na$PT_PRE_CPT_SDNN_T6 > 100)
chisq.test(tbl)  # p-value = 0.4667


# ---- plot hrv mets group ----
boxplot(PT_PRE_CPT_SDNN_T6~n_mets, data=hrv_na, xlab="Number of metabolic syndrome factors", 
        ylab="SDNN pre CPT", ylim=c(0, 300))


boxplot(PT_PRE_CPT_SDNN_T6~n_mets, data=hrv_na)


# ---- DM vs nmets 5 hba ----
table(hrv_na$HBA1C_T6[which(hrv_na$diabetic)] >= 6.5, useNA = "ifany")
round(prop.table(table(hrv_na$HBA1C_T6[which(hrv_na$diabetic)] >= 6.5, useNA = "ifany"))*100, 1)
# 23 % below 6.5

table(hrv_na$gluc_low_meds[which(hrv_na$diabetic)], useNA = "ifany")
# app 50/50


# --CVD all types
round(prop.table(table(hrv_na$cvd[which(hrv_na$diabetic)], useNA = "ifany"))*100, 1)
# 19 % w known CVD

round(prop.table(table(hrv_na$cvd[which(hrv_na$n_mets6 == 5)], useNA = "ifany"))*100, 1)
# 32 % w known CVD!


round(prop.table(table(hrv_na$cvd[which(hrv_na$n_mets6 == 3)], useNA = "ifany"))*100, 1)
# 10 %

round(prop.table(table(hrv_na$cvd[which(hrv_na$n_mets6 == 4)], useNA = "ifany"))*100, 1)
# 25 %

round(prop.table(table(hrv_na$cvd[which(hrv_na$n_mets6 == 2)], useNA = "ifany"))*100, 1)
# 3 %

round(prop.table(table(hrv_na$cvd[which(hrv_na$n_mets6 %in% 3:5)], useNA = "ifany"))*100, 1)
# 17 %


# --angina
round(prop.table(table(hrv_na$angina[which(hrv_na$diabetic)], useNA = "ifany"))*100, 1)
# 8 %

round(prop.table(table(hrv_na$angina[which(hrv_na$n_mets6 == 5)], useNA = "ifany"))*100, 1)
# 15 %


# --heart attack
round(prop.table(table(hrv_na$heart_attack[which(hrv_na$diabetic)], useNA = "ifany"))*100, 1)
# 11 %

round(prop.table(table(hrv_na$heart_attack[which(hrv_na$n_mets6 == 5)], useNA = "ifany"))*100, 1)
# 21 %


# --stroke
round(prop.table(table(hrv_na$stroke[which(hrv_na$diabetic)], useNA = "ifany"))*100, 1)
# 6 %

round(prop.table(table(hrv_na$heart_attack[which(hrv_na$n_mets6 == 5)], useNA = "ifany"))*100, 1)
# 21 %




# ---- distribution of MetS factors  ----
sum(hrv_na$high_BP, na.rm=T)  # 4840
sum(hrv_na$high_triglyc, na.rm=T)  # 2903
sum(hrv_na$low_hdl, na.rm=T)  # 1966
sum(hrv_na$central_obesity, na.rm=T)  # 5992
sum(hrv_na$high_glucose, na.rm=T)  # 1068
sum(hrv_na$diabetic, na.rm=T)  # 504
sum(hrv_na$HBA1C_T6 >= 6.5, na.rm=T)  # 384
sum(hrv_na$GLUCOSE_T6 >= 5.6, na.rm=T)  # 1461
sum(hrv_na$diabetic[which(hrv_na$GLUCOSE_T6 >= 5.6)], na.rm=T)  # 347
sum(hrv_na$high_glucose[which(hrv_na$GLUCOSE_T6 >= 5.6)], na.rm=T)  # 1068

View(hrv_na[which(hrv_na$GLUCOSE_T6 >= 5.6 & (is.na(hrv_na$diabetic) | is.na(hrv_na$high_glucose))), ])
# 46 people get missing glucose criteria because they miss diabetes status
id <- hrv_na[which(hrv_na$GLUCOSE_T6 >= 5.6 & (is.na(hrv_na$diabetic) | is.na(hrv_na$high_glucose))), ".id"]
View(long[long$.imp == 2 & long$.id %in% id, ])
# basically all get imputed to nmets 5


table(hrv_na$high_triglyc[which(hrv_na$n_mets6 == 1)], useNA = "ifany")  # 86
table(hrv_na$high_BP[which(hrv_na$n_mets6 == 1)], useNA = "ifany")  # 472
table(hrv_na$high_glucose[which(hrv_na$n_mets6 == 1)], useNA = "ifany")  # 55
table(hrv_na$low_hdl[which(hrv_na$n_mets6 == 1)], useNA = "ifany")  # 17
table(hrv_na$central_obesity[which(hrv_na$n_mets6 == 1)], useNA = "ifany")  # 1127




# ---- compare MetS levels ----

# -- HDL

#MetS
mean(hrv_na$HDL_T6[which(hrv_na$n_mets6 %in% 3:5)], na.rm = T)  # 1.328405
#healthy
mean(hrv_na$HDL_T6[which(hrv_na$n_mets6 %in% 0:2)], na.rm = T)  # 1.63049
#DM
mean(hrv_na$HDL_T6[which(hrv_na$n_mets6 %in% 6)], na.rm = T)  # 1.300402


t.test(hrv_na$HDL_T6[which(hrv_na$n_mets6 %in% 3:5)], 
       hrv_na$HDL_T6[which(hrv_na$n_mets6 %in% 0:2)])
# significant



cor(hrv_na$WAIST_T6, hrv_na$TRIGLYCERIDES_T6, use="complete.obs")
cor(hrv_na$WAIST_T6, hrv_na$HDL_T6, use="complete.obs")
cor(hrv_na$WAIST_T6, hrv_na$MEAN_SYSBP_T6, use="complete.obs")
cor(hrv_na$WAIST_T6, hrv_na$MEAN_DIABP_T6, use="complete.obs")
cor(hrv_na$WAIST_T6, hrv_na$HBA1C_T6, use="complete.obs")
cor(hrv_na$WAIST_T6, hrv_na$GLUCOSE_T6, use="complete.obs")


cor(hrv_na$TRIGLYCERIDES_T6, hrv_na$HDL_T6, use="complete.obs")  # - 0.4730903
cor(hrv_na$TRIGLYCERIDES_T6, hrv_na$MEAN_SYSBP_T6, use="complete.obs")
cor(hrv_na$TRIGLYCERIDES_T6, hrv_na$MEAN_DIABP_T6, use="complete.obs")
cor(hrv_na$TRIGLYCERIDES_T6, hrv_na$HBA1C_T6, use="complete.obs")
cor(hrv_na$TRIGLYCERIDES_T6, hrv_na$GLUCOSE_T6, use="complete.obs")


cor(hrv_na$HDL_T6, hrv_na$MEAN_SYSBP_T6, use="complete.obs")
cor(hrv_na$HDL_T6, hrv_na$MEAN_DIABP_T6, use="complete.obs")
cor(hrv_na$HDL_T6, hrv_na$HBA1C_T6, use="complete.obs")
cor(hrv_na$HDL_T6, hrv_na$GLUCOSE_T6, use="complete.obs")


cor(hrv_na$MEAN_SYSBP_T6, hrv_na$MEAN_DIABP_T6, use="complete.obs")
cor(hrv_na$MEAN_SYSBP_T6, hrv_na$HBA1C_T6, use="complete.obs")
cor(hrv_na$MEAN_SYSBP_T6, hrv_na$GLUCOSE_T6, use="complete.obs")


cor(hrv_na$MEAN_DIABP_T6, hrv_na$HBA1C_T6, use="complete.obs")
cor(hrv_na$MEAN_DIABP_T6, hrv_na$GLUCOSE_T6, use="complete.obs")


cor(hrv_na$HBA1C_T6, hrv_na$GLUCOSE_T6, use="complete.obs")


# ---- corr HRV to MetS factors ----
# -- pre SDNN
cor(hrv_na$PT_PRE_CPT_SDNN_T6, hrv_na$GLUCOSE_T6, use="complete.obs")
cor(hrv_na$PT_PRE_CPT_SDNN_T6, hrv_na$HBA1C_T6, use="complete.obs")
cor(hrv_na$PT_PRE_CPT_SDNN_T6, hrv_na$TRIGLYCERIDES_T6, use="complete.obs")
cor(hrv_na$PT_PRE_CPT_SDNN_T6, hrv_na$HDL_T6, use="complete.obs")
cor(hrv_na$PT_PRE_CPT_SDNN_T6, hrv_na$WAIST_T6, use="complete.obs")
cor(hrv_na$PT_PRE_CPT_SDNN_T6, hrv_na$GLUCOSE_T6, use="complete.obs")



# ---- DM vs MetS ----
table(hrv_na$n_mets[which(hrv_na$diabetic)])

hrv_na$glucose_all <- hrv_na$GLUCOSE_T6 >= 5.6
hrv_na$n_mets_all <- rowSums(hrv_na[c("central_obesity", "high_triglyc", "low_hdl", "high_BP", "glucose_all")])

table(hrv_na$n_mets_all[which(hrv_na$diabetic)])
prop.table(table(hrv_na$n_mets_all[which(hrv_na$diabetic)]))*100


mean(hrv_na$HBA1C_T6[which(hrv_na$diabetic)], na.rm = T)  # 7.151004 
mean(hrv_na$HBA1C_T6[which(hrv_na$n_mets6 %in% 3:5)], na.rm = T)  # 5.651191

mean(hrv_na$HBA1C_T6[which(hrv_na$n_mets6 == 5)], na.rm = T)  # 5.857303
mean(hrv_na$HBA1C_T6[which(hrv_na$n_mets6 == 4)], na.rm = T)  # 5.684252
mean(hrv_na$HBA1C_T6[which(hrv_na$n_mets6 == 3)], na.rm = T)  # 5.601357



