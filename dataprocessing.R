library(dplyr)
library(magrittr)
library(car)
library(data.table)


#wd <- '/Users/ravisharma/Desktop/fmph_221_project/'

wd = 'C:/Users/bpham/Documents/FMPH221/Project'
setwd(wd)
getwd()


cases_death = read.csv('all-states-history.csv')
#View(cases_death)

FIP = read.csv('States_Name_Symbol_FIP.csv')
colnames(FIP)[1] = 'State'
recent_states = FIP$Symbol

cases_death$date = as.Date(cases_death$date, "%Y-%m-%d")
target_cases_death = cases_death[cases_death$date >= "2020-7-1",]

target_cases_death = target_cases_death[target_cases_death$date <= "2020-12-1",]

hosp_util = read.csv('reported_hospital_utilization_timeseries_20201129_2141.csv')

colnames(hosp_util)[1] = 'state'

#View(hosp_util)

target_cases_death_bed = merge(target_cases_death,hosp_util,by=c('date','state'))
#View(target_cases_death_bed)

start_date <- "2020-8-1"
end_date <- "2020-11-29"
target_cases_death_bed = target_cases_death_bed[target_cases_death_bed$date >= start_date,]
target_cases_death_bed = target_cases_death_bed[target_cases_death_bed$date <= end_date,]

#View(target_cases_death_bed)



#View(target_cases_death_bed)
state_unique = unique(target_cases_death_bed$state)



target_cases_death_bed <- mutate(target_cases_death_bed, inpatient_beds_utilization = ifelse(inpatient_beds_utilization > 1, NA,inpatient_beds_utilization))

target_cases_death_bed <- mutate(target_cases_death_bed, adult_icu_bed_utilization = ifelse(adult_icu_bed_utilization > 1, NA,adult_icu_bed_utilization))

target_cases_death_bed <- mutate(target_cases_death_bed, deathIncrease = ifelse(deathIncrease < 0, NA,deathIncrease))

target_cases_death_bed <- mutate(target_cases_death_bed, positiveIncrease = ifelse(positiveIncrease < 0, NA,positiveIncrease))

#View(target_cases_death_bed)

state_avail = unique(target_cases_death_bed$state)
hosp_gen = read.csv('Hospital_General_Information.csv')
colnames(hosp_gen)[5] = 'state'
#View(filter(hosp_gen,state == 'CA'))
states_nhosp_gen = data.frame()
for (z in 1:length(state_avail)){
  state_interest = state_avail[z]
  hosp_gen_state = filter(hosp_gen,state == state_interest)
  nhosp = as.numeric(nrow(hosp_gen_state))
  state_nhosp = c(state_interest,nhosp)
  states_nhosp_gen = rbind(states_nhosp_gen,state_nhosp)
}

x != 3

rownames(states_nhosp_gen) = NULL
colnames(states_nhosp_gen) = c('state','nhosp_gen')

states_nhosp_gen$nhosp_gen <- as.numeric(states_nhosp_gen$nhosp_gen)


hosp_util_maxs = data.frame()
for (z in 1:length(state_unique)){
  focus = target_cases_death_bed[target_cases_death_bed$state == state_unique[z],]
  hosp_util_max = max(select(focus,contains('coverage')),na.rm=TRUE)
  hosp_util_state_max = c(state_unique[z],hosp_util_max)
  hosp_util_maxs = rbind(hosp_util_maxs,hosp_util_state_max)
}

rownames(hosp_util_maxs) = NULL
colnames(hosp_util_maxs) = c('state','nhospital_util_max')

hosp_util_maxs$nhospital_util_max <- as.numeric(hosp_util_maxs$nhospital_util_max)


#hosp_util_maxs
#states_nhosp

hosp_gen_states_nhosp <- merge(hosp_util_maxs,states_nhosp_gen,by='state')
hosp_est = c()
for (z in 1:nrow(hosp_gen_states_nhosp)) {
  hosp_util_z = hosp_gen_states_nhosp$nhospital_util_max[z]
  gen_z = hosp_gen_states_nhosp$nhosp_gen[z]
  if (hosp_util_z >= gen_z) {
    hosp_est = c(hosp_est,hosp_util_z)
  } else {
    hosp_est = c(hosp_est,gen_z)
  }
}

hosp_gen_states_nhosp$hosp_est = hosp_est

hosp_gen_states_nhosp <- hosp_gen_states_nhosp[c('state','hosp_est','nhospital_util_max','nhosp_gen')]

#hosp_gen_states_nhosp$perc_change <- abs((hosp_gen_states_nhosp$nhospital_util_max - hosp_gen_states_nhosp$nhosp_gen) / hosp_gen_states_nhosp$nhosp_gen)





bigdf = data.frame()


#focus = target_cases_death_bed[target_cases_death_bed$state == 'ND',]
#col_interest = c('state','date')
#col_interest = c(col_interest,colnames(select(focus,contains('shortage'))))
#focus_shortage <- focus[col_interest]
#denom1 = focus_shortage[,3] + focus_shortage[,4] + focus_shortage[,5]
#num1 = focus_shortage[,3]
#denom2 = focus_shortage[,6] + focus_shortage[,7] + focus_shortage[,8]
#num2 = focus_shortage[,6]
#hosp_shortage_mean = mean(num1/denom1)
#hosp_shortage_sd = mean(num1/denom1)
#hosp_shortage_in1week_mean = mean(num2/denom2)
#hosp_shortage_in1week_mean = sd(num2/denom2)
#hosp_shortages_mean = c(hosp_shortage_mean,hosp_shortage_in1week_mean)
#hosp_shortages_sd = c(hosp_shortage_sd,hosp_shortage_in1week_sd)
#View(focus)


for (z in 1:length(state_unique)){
  focus = target_cases_death_bed[target_cases_death_bed$state == state_unique[z],]
  focus_state = state_unique[z]
  focus_deathIncrease_mean = mean(focus$deathIncrease,na.rm=TRUE)
  focus_deathIncrease_sd = sd(focus$deathIncrease,na.rm=TRUE)
  focus_positiveIncrease_mean = mean(focus$positiveIncrease,na.rm=TRUE)
  focus_positiveIncrease_sd = sd(focus$positiveIncrease,na.rm=TRUE)
  focus_inpatient_beds_util_mean = mean(focus$inpatient_beds_utilization,na.rm=TRUE)
  focus_inpatient_beds_util_sd = sd(focus$inpatient_beds_utilization,na.rm=TRUE)
  focus_adult_icu_bed_util_mean = mean(focus$adult_icu_bed_utilization,na.rm=TRUE)
  focus_adult_icu_bed_util_sd = sd(focus$adult_icu_bed_utilization,na.rm=TRUE)
  focus_cols_mean = c(focus_state,focus_deathIncrease_mean, focus_positiveIncrease_mean,focus_inpatient_beds_util_mean,focus_adult_icu_bed_util_mean)
  focus_cols_sd = c(focus_deathIncrease_sd, focus_positiveIncrease_sd,focus_inpatient_beds_util_sd,focus_adult_icu_bed_util_sd)
  
  focus_cols_mean = focus_cols_mean
  focus_cols_sd = focus_cols_sd
  
  col_interest = c('state','date')
  col_interest = c(col_interest,colnames(select(focus,contains('shortage'))))
  focus_shortage <- focus[col_interest]
  denom1 = focus_shortage[,3] + focus_shortage[,4]
  num1 = focus_shortage[,3]
  denom2 = focus_shortage[,6] + focus_shortage[,7]
  num2 = focus_shortage[,6]
  hosp_shortage_mean = mean(num1/denom1,na.rm=TRUE)
  hosp_shortage_sd = sd(num1/denom1,na.rm=TRUE)
  hosp_shortage_in1week_mean = mean(num2/denom2,na.rm=TRUE)
  hosp_shortage_in1week_sd = sd(num2/denom2,na.rm=TRUE)
  hosp_shortages_mean = c(hosp_shortage_mean,hosp_shortage_in1week_mean)
  hosp_shortages_sd = c(hosp_shortage_sd,hosp_shortage_in1week_sd)
  
  hosp_shortages_mean = hosp_shortages_mean
  hosp_shortages_sd = hosp_shortages_sd
  
  focus_out = c(focus_cols_mean,focus_cols_sd,hosp_shortages_mean,hosp_shortages_sd)
  bigdf = rbind(bigdf,focus_out)
}

focus_mean_names <- c('deathIncrease_mean','positiveIncrease_mean','inpatient_beds_utilization_mean','adult_icu_bed_utilization_mean')
focus_sd_names <- c('deathIncrease_sd','positiveIncrease_sd','inpatient_beds_utilization_sd','adult_icu_bed_utilization_sd')
hospshort_mean_names <- c('hosp_shortage_mean','hosp_shortage_in1week_mean')
hospshort_sd_names <- c('hosp_shortage_sd','hosp_shortage_in1week_sd')

colnames(bigdf) = c('state',focus_mean_names,focus_sd_names,hospshort_mean_names,hospshort_sd_names)
rownames(bigdf) = NULL

bigdf <- merge(bigdf,hosp_gen_states_nhosp,by='state')
#View(bigdf)





GDP = read.csv('Q2_GDP.csv')
colnames(GDP) = c('State','GDP')

GDP$State = trimws(GDP$State)
FIP$State = trimws(FIP$State)


FIP_GDP = merge(FIP,GDP, by='State')

#FIP_GDP = FIP_GDP[c('Symbol','GDP')]


state_GDP = FIP_GDP[c('Symbol','GDP')]

colnames(state_GDP)[1] = 'state'
state_GDP = state_GDP[state_GDP$state %in% bigdf$state,]
bigdf_GDP = merge(bigdf,state_GDP,by='state')


pop = read.csv('Population by state.csv')
FIP_pop = merge(pop,FIP ,by='State')
FIP_pop = FIP_pop[c('Symbol','Population')]
colnames(FIP_pop)[1] = 'state'
state_pop = FIP_pop
state_pop = state_pop[state_pop$state %in% bigdf_GDP$state,]
state_pop

bigdf_GDP_pop = merge(bigdf_GDP,state_pop,by = 'state')
#View(bigdf_GDP_pop)

age = read.csv('All_Pop_tot_65.csv')
colnames(age)[1] = 'state'
age$prop65 <- age$Pop65/age$PopTotal
bigdf_GDP_pop_age = merge(bigdf_GDP_pop,age,by='state')

#View(bigdf_GDP_pop_age)


land_amount = read.csv('States_area_land_50states.csv')
drops <- c("full_state_name")
land_amount=land_amount[ , !(names(land_amount) %in% drops)]
names(land_amount)[names(land_amount) == "Area..mi.2."] <- "Area"
colnames(land_amount)[1] <- 'state' #for some reason, the 1st column name has some weird unicode symbol in front of it. Changed to just regular state so I can actually merge.

bigdf_GDP_pop_age_land = merge(bigdf_GDP_pop_age,land_amount,by = 'state')
bigdf_GDP_pop_age_land$Area <- as.numeric(bigdf_GDP_pop_age_land$Area)
bigdf_GDP_pop_age_land$Population <- as.numeric(gsub(",", "", bigdf_GDP_pop_age_land$Population))
bigdf_GDP_pop_age_land$population_density = bigdf_GDP_pop_age_land$Population/bigdf_GDP_pop_age_land$Area

bigdf_GDP_pop_age_land$GDP <- as.numeric(gsub(",", "", bigdf_GDP_pop_age_land$GDP))
bigdf_GDP_pop_age_land$GDPperCapita = bigdf_GDP_pop_age_land$GDP/bigdf_GDP_pop_age_land$Population

insurance = read.csv('uninsured_percent_65.csv')
insurance = insurance[c('state','uninsured_percent_65')]
bigdf_GDP_pop_age_land_uninsured65 = merge(bigdf_GDP_pop_age_land,insurance,by='state')


#START NEW CODE
healthcare_quality_ranking = read.csv('Hospital_Quality_Data.csv')
keeps <- c("state", "Hospital_Quality")
healthcare_quality_ranking=healthcare_quality_ranking[keeps]

public_health_metric = read.csv('Public_Health_Data.csv')
keeps <- c("state", "Health_Score")
public_health_metric=public_health_metric[keeps]

healthcare_affordability_ranking = read.csv('Healthcare_Affordability_Data.csv')
keeps <- c("state", "Healthcare_Affordability_Rating")
healthcare_affordability_ranking=healthcare_affordability_ranking[keeps]

worker_availability = read.csv('Healthcare_Worker_Numbers_Data.csv')
worker_availability$critical_care_doctors_and_nurses_per_100k=worker_availability$Critical.care.doctors.and.nurses.per.10k.capita/10
worker_availability$first_and_second_line_workers_per_100k=worker_availability$First_and_second_line_workers/10
keeps <- c("state", "critical_care_doctors_and_nurses_per_100k","first_and_second_line_workers_per_100k")
worker_availability=worker_availability[keeps]

bigdf_GDP_pop_age_land_uninsured65_others = merge(bigdf_GDP_pop_age_land_uninsured65,healthcare_quality_ranking,by='state')
bigdf_GDP_pop_age_land_uninsured65_others = merge(bigdf_GDP_pop_age_land_uninsured65_others,public_health_metric,by='state')
bigdf_GDP_pop_age_land_uninsured65_others = merge(bigdf_GDP_pop_age_land_uninsured65_others,healthcare_affordability_ranking,by='state')
bigdf_GDP_pop_age_land_uninsured65_others = merge(bigdf_GDP_pop_age_land_uninsured65_others,worker_availability,by='state')
bigdf_final = bigdf_GDP_pop_age_land_uninsured65_others


#END NEW CODE






bigdf_final$positiveIncrease_mean = as.numeric(bigdf_final$positiveIncrease_mean)
bigdf_final$deathIncrease_mean = as.numeric(bigdf_final$deathIncrease_mean)
bigdf_final$inpatient_beds_utilization_mean = as.numeric(bigdf_final$inpatient_beds_utilization_mean)
bigdf_final$adult_icu_bed_utilization_mean = as.numeric(bigdf_final$adult_icu_bed_utilization_mean)

bigdf_final$pIper100k <- (bigdf_final$positiveIncrease_mean/bigdf_final$PopTotal)*100000
bigdf_final$dIper100k <-(bigdf_final$deathIncrease_mean/bigdf_final$PopTotal)*100000

plot(bigdf_final$pIper100k,bigdf_final$dIper100k )

bigdf_final$hosp_shortage_mean <- as.numeric(bigdf_final$hosp_shortage_mean)

bigdf_final$hosp_est <- as.numeric(bigdf_final$hosp_est)
bigdf_final$hosp_est_per100k <- (bigdf_final$hosp_est/bigdf_final$PopTotal) * 100000
bigdf_final$pIper100k.c <- bigdf_final$pIper100k - mean(bigdf_final$pIper100k)


library("writexl")
write_xlsx(bigdf_final,"bigdf_final.xlsx")

attach(bigdf_final)




