# FMPH221

Our project focuses on COVID-19 mortaility by state over a time period. From our results, we aimed to:

1. Find significant factors that can affect the mortality of people due to COVID-19 across a set period of time
2. Create models and choose the best model that illustrates a linear relationship between mortality and determined important factors
3. Suggest state-specific or general "action items" based on these factors

This repository contains:
1. Data processing code
2. Model Selection code
3. Final Report

A small summary on these components is shown below:

## Data Processing (dataprocessing.R)
Metrics on COVID-19 deaths by state was retrieved from the COVID Tracking Project (https://covidtracking.com/). We collected data from this site dated from January 1st 2020 to December 4th 2020.

We aggregated data from different sources based on what we thought would contribute to COVID-19 deaths. We boiled it down to these factors:
1. Hospital quality
()
2. Hospital bed availability (ICU and regular beds)
(https://healthdata.gov/dataset/covid-19-reported-patient-impact-and-hospital-capacity-state-timeseries)
This was the link when we did the project. It seems that the link has changed to:
(https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh)

3. Spread (Population and State Area to calculate a rough estimate of population density)
(https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx)
(https://www.census.gov/geographies/reference-files/2010/geo/state-area.html)
4. GDP (State resources normalized by population)
(https://www.bea.gov/news/2020/gross-domestic-product-state-2nd-quarter-2020)
5. Percentage of uninsured adults older than 65 (People older than 65 have a higher risk of dying from COVID-19)
(http://statehealthcompare.shadac.org/table/4/health-insurance-coverage-type-by-age#2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52/5,4,1,10,86,9,8,6,3,12,13,20,25,14,21,22,23,24,11/25/7,8)
(https://www.census.gov/cps/data/cpstablecreator.html)

FIPs state mapping is done by (https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696)

Variables from this table were merged into a big table bigdf_final and inputted into model selection (cv_modelformulation.R)

## Model Selection (cv_modelformulation.R)

We did initial feature selection in both forward and backward direction. Diagnostics of the model were examined to check if the model satisfies linear regression assumptions. These assumptions are:
1. Repsonse and Factors must have a linear relationship
2. Residuals must be homescedastic (have the same variance)
3. Residuals must be distributed normally
4. Residuals must be independent

From looking at the diagnostics plot from this model, there is a skew of Residuals vs Fitted plot from two points - North Dakota and South Dakota. This violates homescedasticity of the residuals. Removing these points makes the Residuals vs Fitted plot flatter which satisfies homescasticity, but the fit was still deemed not adequate.

The final model that we deem optimal consists of three factors: increases in cases per 100,000 capita (pIper100k), mean of hospitals experiencing a shortage (hosp_shortage_mean) and mean of adult icu bed utilization (adult_icu_bed_utilization_mean).

We then used k-folds cross validation at 10 folds to determine whether the model can predict well. From multiple runs, this model has an average RMSE of 0.0885 deaths/100k capita/day). Other larger models are checked with the same k-folds cross validation methodology at 10 folds as a gut check to confirm whether our optimal model has higher predictive power.

## Final Report

Our Final Report that goes into what was discussed here in more detail can be found in the repository.
