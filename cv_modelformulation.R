library(readxl)
library(dplyr)
library(magrittr)
library(car)
library(data.table)
library(Metrics)
library(caret)
wd = 'C:/Users/bpham/Documents/FMPH221/Project'
setwd(wd)
getwd()
#construct candidate models and find the best few models
bigdf_final=read_excel("bigdf_final.xlsx")

#filtering out states - optional

#bigdf_final=filter(bigdf_final,state != 'VT')


#group1 = bigdf_final[1:5,]
#group2 = bigdf_final[6:10,]
#group3 = bigdf_final[11:15,]
bigdf_final$deathIncrease_sd <- as.numeric(bigdf_final$deathIncrease_sd)

#dI_pI_av_m5 <- avPlots(m5)[1]
#plot(dI_pI_av_m5$pIper100k,dI_pI_av_m5$dIper100k)





#par(mfrow=c(2,2))




#avPlots(m1,data=newdata)

#hist(bigdf_final$pIper100k)
#hist(bigdf_final$adult_icu_bed_utilization_mean)
#hist(bigdf_final$hosp_shortage_mean)
#hist(bigdf_final$dIper100k)





#Adapted from http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  #cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1)
}

pairs(dIper100k ~ pIper100k + adult_icu_bed_utilization_mean + GDPperCapita + prop65 + hosp_shortage_mean + hosp_est_per100k + Health_Score + Healthcare_Affordability_Rating + critical_care_doctors_and_nurses_per_100k + first_and_second_line_workers_per_100k + population_density + uninsured_percent_65, lower.panel = panel.cor, data = bigdf_final_train)
#major correlations with response: cases, icu, gdp, shortage mean, health score

pairs(dIper100k ~ pIper100k + adult_icu_bed_utilization_mean + GDPperCapita + prop65 + hosp_shortage_mean + hosp_est_per100k + Health_Score + Healthcare_Affordability_Rating + critical_care_doctors_and_nurses_per_100k + first_and_second_line_workers_per_100k + log(population_density), lower.panel = panel.cor, data = bigdf_final_train)


par(mfrow=c(3,3))
hist(bigdf_final_train$pIper100k)
hist(bigdf_final_train$hosp_shortage_mean)
hist(bigdf_final_train$hosp_est_per100k)
hist(bigdf_final_train$GDPperCapita)
hist(bigdf_final_train$Health_Score)
pairs(dIper100k ~ pIper100k + hosp_shortage_mean + hosp_est_per100k + GDPperCapita + Health_Score, lower.panel = panel.cor, data = bigdf_final_train)
#big correlation between hospitals per 100k and cases per 100k. remove hospitals per 100k.
#population density correlated with GDP per Capita
#icu correlated with GDP per capita
#big correlation between GDP per capita and health score. remove health score, since it's harder to get


bigdf_final$hosp_shortage_in1week_mean <- as.numeric(bigdf_final$hosp_shortage_in1week_mean)

m5=step(lm(dIper100k ~ 1, data = bigdf_final), ~ inpatient_beds_utilization_mean + adult_icu_bed_utilization_mean + hosp_shortage_in1week_mean + prop65 + population_density + GDPperCapita + uninsured_percent_65 + Health_Score + critical_care_doctors_and_nurses_per_100k + first_and_second_line_workers_per_100k + pIper100k + hosp_est_per100k + hosp_shortage_mean, direction="both")
AIC(m5)
par(mfrow=c(2,2))
plot(m5)
m5_fit_val <- fitted.values(m5)
which.max(m5_fit_val)
newdata <- bigdf_final[-c(which.max(m5_fit_val)), ]
bigdf_final[c(which.max(m5_fit_val)),]$state # North Dakota

#figure validation
#par(mfrow=c(1,1))
#plot(m5,which = 1)
#fitted.values(m5) #41 is SD and 28 is ND
#bigdf_final[41,]$state
#bigdf_final[28,]$state
m6 <- lm(formula = dIper100k ~ pIper100k + Health_Score + hosp_shortage_mean + 
           inpatient_beds_utilization_mean + uninsured_percent_65 + 
           prop65, data = newdata)
plot(m6,which = 1) # problem still exists at the max fitted value.
m6_fit_val <- fitted.values(m6)
which.max(m6_fit_val)
newdata[c(which.max(m6_fit_val)),]$state #South Dakota
newdata <- newdata[-c(which.max(m6_fit_val)), ]

m6 <- lm(formula = dIper100k ~ pIper100k + Health_Score + hosp_shortage_mean + 
           inpatient_beds_utilization_mean + uninsured_percent_65 + 
           prop65, data = newdata)
par(mfrow=c(1,1))
plot(m6,which = 1) #residual line is now flat but not good fit.

summary(m6)


m6 <- lm(formula = dIper100k ~ pIper100k + Health_Score + hosp_shortage_mean +  #remove inpatient beds and replace with adult icu bed utilization because prior knowledge that ppl with COVID19 that die have a serious case that make them take adult icu beds
           adult_icu_bed_utilization_mean + uninsured_percent_65 + 
           prop65, data = newdata)

m7 <- lm(formula = dIper100k ~ pIper100k + Health_Score + hosp_shortage_mean +  #remove insignificant predictors from m6
           adult_icu_bed_utilization_mean, data = newdata)
avPlots(m7,data = newdata)

m8 <- lm(formula = dIper100k ~ pIper100k + hosp_shortage_mean +  # remove Health Score because realized that this metric is hard for non-technical people to get.
           adult_icu_bed_utilization_mean, data = newdata)

# also went back and did stepwise regression on the data without the problematic points (ND, SD)

m5=step(lm(dIper100k ~ 1, data = newdata), ~ inpatient_beds_utilization_mean + adult_icu_bed_utilization_mean + hosp_shortage_in1week_mean + prop65 + population_density + GDPperCapita + uninsured_percent_65 + Health_Score + critical_care_doctors_and_nurses_per_100k + first_and_second_line_workers_per_100k + pIper100k + hosp_est_per100k + hosp_shortage_mean, direction="both")
AIC(m5)

anova(m8,m5)
#we get m7. Remove health score since it 1. confuses policy makers and 2. confounds with adult_icu_bed_utilization_means and we get m8.
m8 <- lm(formula = dIper100k ~ pIper100k + hosp_shortage_mean +  # remove Health Score because realized that this metric is hard for non-technical people to get.
           adult_icu_bed_utilization_mean, data = newdata)


#Cross Validation
train_control <- trainControl(method="cv", number=10)
model <- train(dIper100k ~ pIper100k + adult_icu_bed_utilization_mean + hosp_shortage_mean, data=newdata, trControl=train_control, method="lm")
print(model)

# Gut check
model2 <- train(dIper100k ~ pIper100k + adult_icu_bed_utilization_mean + hosp_shortage_mean + GDPperCapita, data=bigdf_final, trControl=train_control, method="lm")
print(model2)
model3 <- train(dIper100k ~ pIper100k + adult_icu_bed_utilization_mean + hosp_shortage_mean + GDPperCapita + prop65, data=bigdf_final, trControl=train_control, method="lm")
print(model3)
model4 <- train(dIper100k ~ pIper100k + adult_icu_bed_utilization_mean + hosp_shortage_mean + GDPperCapita + prop65 + Health_Score + critical_care_doctors_and_nurses_per_100k, data=bigdf_final, trControl=train_control, method="lm")
print(model4)
model5 <- train(log(dIper100k) ~ pIper100k + adult_icu_bed_utilization_mean + hosp_shortage_mean + GDPperCapita + prop65 + Health_Score + critical_care_doctors_and_nurses_per_100k, data=bigdf_final, trControl=train_control, method="lm")
print(model5)


