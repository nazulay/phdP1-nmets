

library(mice)
library(data.table)
library(Rmisc)  # ci
library(ggplot2)
library(miceadds) 
library(multcomp)  # contrasts
library(multcompView)  # Tukey
library(mitml)  # post hoc/contr for imputed data
library(car)  # Anova
library(broom)  # model diagnostics
library(xlsx)

load("Data/RData/derived_vars_main.RData")
#imp <- as.mids(long)

hrv <- long[!is.na(long$PT_POST_CPT_SDNN_T6),] 
hrv_na <- hrv[hrv$.imp==0,]
hrv <- as.mids(hrv)  # warning because attended_cpt is now a constant, out



# ---- MAIN HYPOTHESIS ----
# ---- model ----

fit_cat_nmets <- with(hrv, lm(PT_POST_CPT_SDNN_T6~SEX_T6+AGE_T6+n_mets6_cat))
nmets_ci <- summary(pool(fit_cat_nmets), conf.int = TRUE)
nmets_ci
pool.r.squared(fit_cat_nmets, adjusted = TRUE) # 0.04077587 a lot lower than sdnn!


#plot estimates w CIs
nmets_ci <- nmets_ci[-(2:3), ]  #remove sex, age
nmets_ci$term <- c(0:5, 'Diabetes')
names(nmets_ci)[7:8] <- c('lower', 'upper')

# change estimates from difference to actual value
nmets_ci[-1, "estimate"] <- nmets_ci[1, "estimate"] + nmets_ci[-1, "estimate"]

# change CIs from diff to value 
nmets_ci[, "ci_length"] <- (nmets_ci[, "upper"] - nmets_ci[, "lower"])
nmets_ci[-1, "lower"] <- nmets_ci[-1, "estimate"] - nmets_ci[-1, "ci_length"]/2
nmets_ci[-1, "upper"] <- nmets_ci[-1, "estimate"] + nmets_ci[-1, "ci_length"]/2


qplot(x=term, y=estimate, data=nmets_ci) + geom_errorbar(aes(ymin=lower, ymax=upper, width=0.15)) +
  labs(y="SDNN post CPT", x="Number of MetS factors or diabetes")

table(hrv_na$n_mets6_cat, useNA = "ifany")
table(hrv_na$n_mets[hrv_na$n_mets6 == 6])

write.xlsx(nmets_ci, file='plotdata_gp_post_sdnn.xlsx', sheetName = 'nmets_ci')


# ---- contrasts ----
levels(hrv$data$n_mets6_cat) <- c("zero", "one", "two", "three", "four", "five", "diabetes")

implist1 <- mids2mitml.list(hrv)

# have to fit model again with implist
fit.mitml <- with(implist1, lm(PT_POST_CPT_SDNN_T6~SEX_T6+AGE_T6+n_mets6_cat))

testEstimates(fit.mitml)  # same results just rounded 


# healthy vs mets
fit.con.vs.mets <- lapply(fit.mitml, glht, linfct = mcp(n_mets6_cat = "(zero+one+two)/3-(three+four+five)/3 = 0"))
fit.con.vs.mets <- as.mitml.result(fit.con.vs.mets)
testEstimates(fit.con.vs.mets)
# significant, p-value 0.000


# diabetes vs mets
fit.diab.vs.mets <- lapply(fit.mitml, glht, linfct = mcp(n_mets6_cat = "(three+four+five)/3 - diabetes = 0"))
fit.diab.vs.mets <- as.mitml.result(fit.diab.vs.mets)
testEstimates(fit.diab.vs.mets)
# not significant, p value 0.922


#healthy vs diabetes
fit.con.vs.diab <- lapply(fit.mitml, glht, linfct = mcp(n_mets6_cat = "(zero+one+two)/3 - diabetes = 0"))
fit.con.vs.diab <- as.mitml.result(fit.con.vs.diab)
testEstimates(fit.con.vs.diab)
# significant, p value 0.000


# ---- tukey ----
fit.tukey <- lapply(fit.mitml, glht, linfct = mcp(n_mets6_cat = "Tukey"))
fit.tukey <- as.mitml.result(fit.tukey)


# a bit different p-values if we use df, but same conclusion
df <- as.numeric(table(hrv_na$n_mets6))
tuk <- testEstimates(fit.tukey, df.com = df)
tuk
ci.tuk <- confint(tuk)
tuk <- tuk$estimates
tuk <- cbind(tuk, ci.tuk)

#write.xlsx(tuk, file='plotdata_gp_post_sdnn.xlsx', sheetName = 'tukey', append=TRUE)



# ---- model w/ unimputed data----
f <- lm(PT_POST_CPT_SDNN_T6~SEX_T6+AGE_T6+n_mets6_cat, data=hrv_na)
summary(f)  # same as imputed (almost on the decimal)
Anova(f)
anova(f) # why different numbers?
Anova(f, lm(PT_POST_CPT_SDNN_T6~SEX_T6+AGE_T6+n_mets6_cat, data=hrv_na), type=3)
aov(f)


# ---- contrasts ----
#same order as the levels

#healthy vs mets
fit.gh <- glht(f, linfct = mcp(n_mets6_cat = c(-1/3, -1/3, -1/3, 1/3, 1/3, 1/3, 0))) 
summary(fit.gh)
# significant
confint(fit.gh)


#healthy vs diabetic
fit.gh <- glht(f, linfct = mcp(n_mets6_cat = c(-1, -1, -1, 0, 0, 0, 3))) 
#same order as the levels? d>g>n
summary(fit.gh)  
# significant



#diabetic vs mets
fit.gh <- glht(f, linfct = mcp(n_mets6_cat = c(0, 0, 0, -1, -1, -1, 3))) 
summary(fit.gh)  
# not significant, p value 0.201



# ---- tukey ----
an <- aov(f)
tukey <- TukeyHSD(x=an, which='n_mets6_cat')
tukey
# sign diff:
# 0 against all
# 1 against 3, 4, 6
# 2 against 6

plot(tukey, las=1)
table(hrv_na$n_mets6_cat)  # few with 5


# ---- model assumptions ----
# Can't use mids object, so either unimputed, or specific imputed dataset


#not imputed
f_mod_diag <- augment(f)
par(mfrow=c(2, 2))
plot(f)
#cooks distance looks very weird

library(ggfortify)
autoplot(f)


# ---- SPECIFIC FACTORS ----
# ---- singles ----
fit_singles <- with(hrv, lm(PT_POST_CPT_SDNN_T6~SEX_T6+AGE_T6+central_obesity+high_triglyc+
                              low_hdl+high_BP+high_glucose + diabetic))

summary(pool(fit_singles))  # all significant

pool.r.squared(fit_singles, adjusted = TRUE)  # 0.1168166  
aic1 <- sapply(fit_singles$analyses, AIC)
bic1 <- sapply(fit_singles$analyses, BIC)


#save model output
metsfac <- summary(pool(fit_singles), conf.int = TRUE)

write.xlsx(metsfac, file='plotdata_gp_post_sdnn.xlsx', sheetName = 'mets_factors', append=TRUE)

table(hrv_na$central_obesity, useNA = "ifany")
table(hrv_na$high_triglyc, useNA = "ifany")
table(hrv_na$low_hdl, useNA = "ifany")
table(hrv_na$high_BP, useNA = "ifany")
table(hrv_na$high_glucose, useNA = "ifany")
table(hrv_na$diabetic, useNA = "ifany")



# ---- model assumptions ----
# -- no imp
noimp_singles <- lm(PT_POST_CPT_SDNN_T6~SEX_T6+AGE_T6+central_obesity+high_triglyc+
                      low_hdl+high_BP+high_glucose + diabetic, data=hrv_na)
summary(noimp_singles)
#all sign
# R^2 0.116


f_mod_diag <- augment(noimp_singles)
par(mfrow=c(2, 2))
plot(noimp_singles)





# ---- pairs ----
fit_pairs <- with(hrv, lm(PT_POST_CPT_SDNN_T6~central_obesity*high_triglyc+
                            central_obesity*low_hdl + central_obesity*high_BP +
                            central_obesity*high_glucose + high_triglyc*low_hdl + 
                            high_triglyc*high_BP + high_triglyc*high_glucose +
                            low_hdl*high_BP + low_hdl*high_glucose + high_BP*high_glucose +
                            diabetic + AGE_T6 + SEX_T6))

summary(pool(fit_pairs))
#singles: hdl, glucose, co not sign
#pairs: no significance

pool.r.squared(fit_pairs, adjusted = TRUE)  # 0.1163242   
aic2 <- sapply(fit_pairs$analyses, AIC)
bic2 <- sapply(fit_pairs$analyses, BIC)



# ---- unimputed ----
noimp_pairs <- lm(PT_POST_CPT_SDNN_T6~central_obesity*high_triglyc+
                    central_obesity*low_hdl + central_obesity*high_BP +
                    central_obesity*high_glucose + high_triglyc*low_hdl + 
                    high_triglyc*high_BP + high_triglyc*high_glucose +
                    low_hdl*high_BP + low_hdl*high_glucose + high_BP*high_glucose +
                    diabetic + AGE_T6 + SEX_T6, data=hrv_na)
summary(noimp_pairs)
#hdl and glucose not sign, no significant pairs
#R^2 0.117





# ---- triplets ----
fit_triplets <- with(hrv, lm(PT_POST_CPT_SDNN_T6~
                               central_obesity*high_triglyc*low_hdl +
                               central_obesity*high_triglyc*high_BP +
                               central_obesity*low_hdl*high_BP +
                               high_triglyc*low_hdl*high_BP +
                               central_obesity*high_triglyc*high_glucose +
                               central_obesity*low_hdl*high_glucose +
                               high_triglyc*low_hdl*high_glucose +
                               central_obesity*high_BP*high_glucose + 
                               high_triglyc*high_BP*high_glucose +
                               diabetic + AGE_T6 + SEX_T6))

summary(pool(fit_triplets))
#singles: co, tri, hdl, glucose not sign, 
# BP, diabetic sign
#pairs: none significant
#triplets: none significant

pool.r.squared(fit_triplets, adjusted = TRUE)  # 0.1161961  
aic3 <- sapply(fit_triplets$analyses, AIC)
bic3 <- sapply(fit_triplets$analyses, BIC)




# ---- AIC/BIC CI plot ----
# -- plot confidence intervals
hist(aic1)
hist(aic2)
hist(aic3)
#-aic
aic <- data.frame(singles=aic1, pairs=aic2, triplets=aic3)

ci_a <- data.frame(t(apply(aic, 2, CI)))
ci_a$model <- rownames(ci_a)
qplot(x=model, y=mean, data=ci_a) + geom_errorbar(aes(ymin=lower, ymax=upper, width=0.15)) +
  ggtitle("AIC")

#-bic
bic <- data.frame(singles=bic1, pairs=bic2, triplets=bic3)

ci_b <- data.frame(t(apply(bic, 2, CI)))
ci_b$model <- rownames(ci_b)
qplot(x=model, y=mean, data=ci_b) + geom_errorbar(aes(ymin=lower, ymax=upper, width=0.15)) + 
  ggtitle("BIC")


