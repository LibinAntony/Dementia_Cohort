#PMIM702
#Libin Antony

library(RODBC);
library(tcltk);

getlogin <- function(userName='antonyl'){
  wnd <- tktoplevel()
  user <- tclVar(userName)
  passvar <- tclVar('')
  
  tkgrid(tklabel(wnd,text='Username:'))
  passBox <- tkentry(wnd,textvariable = user)
  tkgrid(passBox)
  
  tkgrid(tklabel(wnd,text='Password:'))
  passBox <- tkentry(wnd,textvariable=passvar,show='*')
  tkgrid(passBox)
  
  # Hitting return will also submit password.
  tkbind(passBox, '<Return>', function() tkdestroy(wnd))
  
  # OK button.
  tkgrid(tkbutton(wnd,text='OK',command=function() tkdestroy(wnd)))
  
  # Wait for user to click OK.
  tkwait.window(wnd)
  
  password <- tclvalue(passvar)
  userName <- tclvalue(user)
  
  db <- odbcConnect('PR_SAIL', userName, password)
  return(db)
}

channel <- getlogin()

# dementia cohort summary
# 07/10/2022

library(tidyverse)

#loading dementia cohort

dem_coh <- sqlQuery(channel,"SELECT * FROM SAILW1048V.AM_DEM_COHORT adc 
")

#filtering out missing values

dem_coh <- dem_coh %>% filter(!is.na(ALF_PE))

unwntd <- c(1,6,7,9,10,11,16,17,18,19,20)

dem_coh <- dem_coh[,-unwntd]

#finding duplicate rows

dupl <- dem_coh[duplicated(dem_coh) | duplicated(dem_coh, fromLast = TRUE),]

#keeping distinct rows only

dem_coh <- distinct(dem_coh, .keep_all = TRUE)

#filtering out unwanted dates and keep it between 2010 - 2020

dem_coh$DEM_DT <- as.Date(dem_coh$DEM_DT)

dem_coh <- dem_coh[dem_coh$DEM_DT >= as.Date('2010-01-01') & 
                     dem_coh$DEM_DT <= as.Date('2020-01-01'),]


#loading diabetes cohort

diab_coh <- sqlQuery(channel,"SELECT * FROM 
             SAILW1048V.DIABETES_COHORT_20230830 dc 
             WHERE EARLIEST_DIAG_DT BETWEEN '2000-01-01' 
             AND '2020-01-01'")

#####
#merging diabetes cohort to dementia cohort

dem_diab_coh <- left_join(dem_coh,diab_coh, by = "ALF_PE")


#######

#to do the risk factor analysis

#loading depression cohort

depr_coh <- sqlQuery(channel,"WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = '1B17.'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS Depression
FROM 
 	FirstEvent fe")


#loading hypertension cohort

hyp_coh <- sqlQuery(channel,"WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = 'G2...'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS hypertension
FROM 
 	FirstEvent fe")


#loading systolic cohort

syst_coh <- sqlQuery(channel,"WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = 'G202.'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS systolic
FROM 
 	FirstEvent fe")

#loading diastolic cohort

dias_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = 'G203.'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS diastolic
FROM 
 	FirstEvent fe")

#loading heart disease cohort

hert_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = 'G3...'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS heart_disease
FROM 
 	FirstEvent fe")

#loading COPD cohort

copd_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = 'H3...'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS COPD
FROM 
 	FirstEvent fe")

#loading mental illness cohort

mntl_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = '9H6..'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS mental_illness
FROM 
 	FirstEvent fe")

#loading smoking cohort

smkng_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = '137..'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS smoking
FROM 
 	FirstEvent fe")


#loading hearing disorder cohort

hearng_dsdr_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = 'H1...'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS hearing_disorder
FROM 
 	FirstEvent fe")


#loading obesity cohort

obese_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = 'C380.'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS obesity
FROM 
 	FirstEvent fe")

#loading obese class 1 cohort

obse_cl1_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = '22KC.'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS obese_clss_1
FROM 
 	FirstEvent fe")

#loading obese class 2 cohort

obse_cls2_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = '22KD.'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS obese_clss_2
FROM 
 	FirstEvent fe")


#loading obese class 3 cohort

obese_cls3_coh <- sqlQuery(channel, "WITH FirstEvent AS (
	SELECT ALF_PE, MIN(EVENT_DT) AS ft_evnt_dt 
FROM SAIL1048V.WLGP_GP_EVENT_CLEANSED_20221001 wgec 
WHERE EVENT_DT BETWEEN '1950-01-01' AND '2020-01-01' AND 
EVENT_CD = '22KE.'
GROUP BY ALF_PE 
)

SELECT fe.ALF_PE,
		fe.ft_evnt_dt,
		1 AS obese_clss_3
FROM 
 	FirstEvent fe")

#merging Depression data with base data

dem_diab_depr <- left_join(dem_diab_coh, depr_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_depr$DEPRESSION[dem_diab_depr$FT_EVNT_DT >
                           dem_diab_depr$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_depr <- dem_diab_depr[,!names(dem_diab_depr)=="FT_EVNT_DT"]

#merging Hypertension data with base data

dem_diab_hyp <- left_join(dem_diab_depr, hyp_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_hyp$HYPERTENSION[dem_diab_hyp$FT_EVNT_DT >
                            dem_diab_hyp$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_hyp <- dem_diab_hyp[,!names(dem_diab_hyp)=="FT_EVNT_DT"]

#merging Systolic data with base data

dem_diab_syst <- left_join(dem_diab_hyp, syst_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_syst$SYSTOLIC[dem_diab_syst$FT_EVNT_DT >
                         dem_diab_syst$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_syst <- dem_diab_syst[,!names(dem_diab_syst)=="FT_EVNT_DT"]

#merging Diastolic data with base data

dem_diab_dias <- left_join(dem_diab_syst, dias_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_dias$DIASTOLIC[dem_diab_dias$FT_EVNT_DT >
                          dem_diab_dias$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_dias <- dem_diab_dias[,!names(dem_diab_dias)=="FT_EVNT_DT"]

#merging Heart Disease data with base data

dem_diab_hert <- left_join(dem_diab_dias, hert_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_hert$HEART_DISEASE[dem_diab_hert$FT_EVNT_DT >
                              dem_diab_hert$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_hert <- dem_diab_hert[,!names(dem_diab_hert)=="FT_EVNT_DT"]

#merging copd data with base data

dem_diab_copd <- left_join(dem_diab_hert, copd_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_copd$COPD[dem_diab_copd$FT_EVNT_DT >
                     dem_diab_copd$DEM_DT] <- 0

#removing column ft_event_date from data

dem_diab_copd <- dem_diab_copd[,!names(dem_diab_copd)=="FT_EVNT_DT"]

#merging mental data with base data

dem_diab_mntl <- left_join(dem_diab_copd, mntl_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_mntl$MENTAL_ILLNESS[dem_diab_mntl$FT_EVNT_DT >
                               dem_diab_mntl$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_mntl <- dem_diab_mntl[,!names(dem_diab_mntl)=="FT_EVNT_DT"]

#merging smoking data with base data

dem_diab_smkng <- left_join(dem_diab_mntl, smkng_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_smkng$SMOKING[dem_diab_smkng$FT_EVNT_DT >
                         dem_diab_smkng$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_smkng <- dem_diab_smkng[,!names(dem_diab_smkng)=="FT_EVNT_DT"]

#merging hearing data with base data

dem_diab_hearng <- left_join(dem_diab_smkng, hearng_dsdr_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_hearng$HEARING_DISORDER[dem_diab_hearng$FT_EVNT_DT >
                                   dem_diab_hearng$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_hearng <- dem_diab_hearng[,!names(dem_diab_hearng)=="FT_EVNT_DT"]

#merging obesity data with base data

dem_diab_obese <- left_join(dem_diab_hearng, obese_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_obese$OBESITY[dem_diab_obese$FT_EVNT_DT >
                         dem_diab_obese$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_obese <- dem_diab_obese[,!names(dem_diab_obese)=="FT_EVNT_DT"]

#merging obesity class 1 data with base data

dem_diab_obese_cl1 <- left_join(dem_diab_obese, obse_cl1_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_obese_cl1$OBESE_CLSS_1[dem_diab_obese_cl1$FT_EVNT_DT >
                                  dem_diab_obese_cl1$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_obese_cl1 <- dem_diab_obese_cl1[,!names(dem_diab_obese_cl1)
                                         =="FT_EVNT_DT"]

#merging obesity class 2 data with base data

dem_diab_obese_cl2 <- left_join(dem_diab_obese_cl1, obse_cls2_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_obese_cl2$OBESE_CLSS_2[dem_diab_obese_cl2$FT_EVNT_DT >
                                  dem_diab_obese_cl2$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_obese_cl2 <- dem_diab_obese_cl2[,!names(dem_diab_obese_cl2)
                                         =="FT_EVNT_DT"]

#merging obesity class 3 data with base data

dem_diab_obese_cl3 <- left_join(dem_diab_obese_cl2, obese_cls3_coh, by = "ALF_PE")

#checking whether the risk factor date is later than the dementia date

dem_diab_obese_cl3$OBESE_CLSS_3[dem_diab_obese_cl3$FT_EVNT_DT >
                                  dem_diab_obese_cl3$DEM_DT] <- 0


#removing column ft_event_date from data

dem_diab_obese_cl3 <- dem_diab_obese_cl3[,!names(dem_diab_obese_cl3)
                                         =="FT_EVNT_DT"]

#renaming to a new variable

dem_diab_rsk_fct <- dem_diab_obese_cl3

#removing unwanted table

rm(dem_diab_depr, dem_diab_copd, dem_diab_dias, dem_diab_hearng, dem_diab_hert, 
   dem_diab_hyp, dem_diab_mntl, dem_diab_obese, dem_diab_obese_cl1, 
   dem_diab_obese_cl2, dem_diab_obese_cl3, dem_diab_smkng, dem_diab_syst)

# removing missing values and preparing data for analysis

table(dem_diab_rsk_fct$TYPE_NUM)

# creating a duplicate column for type_nu  to find the number of
# type 1 and uncertain events in the dataset

dem_diab_rsk_fct <- dem_diab_rsk_fct %>%
  mutate(type_num_dupl = TYPE_NUM)

#replacing values 0 and 1 to 5 in column type_num

dem_diab_rsk_fct$TYPE_NUM <- ifelse(dem_diab_rsk_fct$TYPE_NUM %in% c(0,1), 5, 
                           dem_diab_rsk_fct$TYPE_NUM)
  
#replacing all na values to 0 in column type_num

dem_diab_rsk_fct$TYPE_NUM <- ifelse(is.na(dem_diab_rsk_fct$TYPE_NUM), 0, 
                                    dem_diab_rsk_fct$TYPE_NUM)

#removing rows containing value 5 in column type_num

dem_diab_rsk_fct <- dem_diab_rsk_fct %>%
  filter(TYPE_NUM !=5)

#converting all value 2 to 1 in type_num

dem_diab_rsk_fct$TYPE_NUM <- ifelse(dem_diab_rsk_fct$TYPE_NUM == 2, 1,
                                    dem_diab_rsk_fct$TYPE_NUM)

#converting all 0 values to 5 for risk factors

dem_diab_rsk_fct <- dem_diab_rsk_fct %>%
  mutate_at(vars(DEPRESSION, HYPERTENSION, SYSTOLIC, DIASTOLIC, HEART_DISEASE,
                 COPD, MENTAL_ILLNESS, SMOKING, HEARING_DISORDER,
                 OBESITY, OBESE_CLSS_1, OBESE_CLSS_2, OBESE_CLSS_3), 
            ~ ifelse(. == 0, 5, .))

#converting all na values to 0

dem_diab_rsk_fct <- dem_diab_rsk_fct %>%
  mutate_at(vars(DEPRESSION, HYPERTENSION, SYSTOLIC, DIASTOLIC, HEART_DISEASE,
                 COPD, MENTAL_ILLNESS, SMOKING, HEARING_DISORDER,
                 OBESITY, OBESE_CLSS_1, OBESE_CLSS_2, OBESE_CLSS_3),
            ~ ifelse(is.na(.), 0, .))


#removing rows with value 5 

dem_diab_rsk_fct <- dem_diab_rsk_fct %>%
  filter(DEPRESSION %in% c(0,1) &
           HYPERTENSION %in% c(0,1) &
           SYSTOLIC %in% c(0,1) &
           DIASTOLIC %in% c(0,1) &
           HEART_DISEASE %in% c(0,1) &
           COPD %in% c(0,1) &
           MENTAL_ILLNESS %in% c(0,1) &
           SMOKING %in% c(0,1) &
           HEARING_DISORDER %in% c(0,1) &
           OBESITY %in% c(0,1) &
           OBESE_CLSS_1 %in% c(0,1) &
           OBESE_CLSS_2 %in% c(0,1) &
           OBESE_CLSS_3 %in% c(0,1))

#summary of risk factors

count_table <- sapply(dem_diab_rsk_fct, function(x) table(x, useNA = "ifany"))

print(count_table)

#finding mean and standard deviation for dementia patients

mean(dem_diab_rsk_fct$AGE_DEM_DIAG)

sd(dem_diab_rsk_fct$AGE_DEM_DIAG)

#summary of risk factors in type 2 diabetic patients

filtrd_data <- dem_diab_rsk_fct[dem_diab_rsk_fct$TYPE_NUM == 1,]


#creating function to count for values in columns

cnt_vlues <- function(column_name) {
  table(filtrd_data[[column_name]], useNA = "ifany")
}

#list of columns to calculate count

clmns_to_cont <- c("DEPRESSION","HYPERTENSION", "SYSTOLIC",
                   "DIASTOLIC", "HEART_DISEASE", "COPD", "MENTAL_ILLNESS",
                   "SMOKING", "HEARING_DISORDER", "OBESITY",
                   "OBESE_CLSS_1", "OBESE_CLSS_2", "OBESE_CLSS_3", "SEX")

#list to store count tables

count_tables <- lapply(clmns_to_cont, cnt_vlues)

print(count_tables)

#finding mean and standard deviation for type 2 diabetic patients 
#in dementia cohort 

mean(filtrd_data$AGE_DEM_DIAG)

sd(filtrd_data$AGE_DEM_DIAG)


#summary of risk factors in non type 2 diabetic patients

filtrd_data_2 <- dem_diab_rsk_fct[dem_diab_rsk_fct$TYPE_NUM == 0, ]

#creating function to count for values in columns

cnt_vlues_2 <- function(x) {
  table(filtrd_data_2[[x]], useNA = "ifany")
}

#list of columns to calculate count

clmns_to_cont_2 <- c("DEPRESSION","HYPERTENSION", "SYSTOLIC",
                   "DIASTOLIC", "HEART_DISEASE", "COPD", "MENTAL_ILLNESS",
                   "SMOKING", "HEARING_DISORDER", "OBESITY",
                   "OBESE_CLSS_1", "OBESE_CLSS_2", "OBESE_CLSS_3", "SEX")

#list to store count tables

count_tables_2 <- lapply(clmns_to_cont_2, cnt_vlues_2)

print(count_tables_2)

#finding mean and standard deviation for non type 2 diabetic patients 
#in dementia cohort 

mean(filtrd_data_2$AGE_DEM_DIAG)

sd(filtrd_data_2$AGE_DEM_DIAG)


#Performing chi-square test for categorical variables

#Depression

obsrvd_1 <- matrix(c(852, 10385, 3541, 48624), nrow = 2, byrow = TRUE)
chi_sqr_1 <- chisq.test(obsrvd_1)
print(chi_sqr_1)

#Hypertension

obsrvd_2 <- matrix(c(4032, 7205, 14936, 37229), nrow = 2, byrow = TRUE)
chi_sqr_2 <- chisq.test(obsrvd_2)
print(chi_sqr_2)

#Systolic

obsrvd_3 <- matrix(c(67, 11170, 321, 51844), nrow = 2, byrow = TRUE)
chi_sqr_3 <- chisq.test(obsrvd_3)
print(chi_sqr_3)

#Heart Disease

obsrvd_4 <- matrix(c(2137, 9100, 7270, 44895), nrow = 2, byrow = TRUE)
chi_sqr_4 <- chisq.test(obsrvd_4)
print(chi_sqr_4)

#COPD

obsrvd_5 <- matrix(c(1091, 10146, 4185, 47980), nrow = 2, byrow = TRUE)
chi_sqr_5 <- chisq.test(obsrvd_5)
print(chi_sqr_5)

#mental Illness

obsrvd_6 <- matrix(c(45, 11192, 208, 51957), nrow = 2, byrow = TRUE)
chi_sqr_6 <- chisq.test(obsrvd_6)
print(chi_sqr_6)

#Smoking

obsrvd_7 <- matrix(c(291, 10946, 1191, 50649), nrow = 2, byrow = TRUE)
chi_sqr_7 <- chisq.test(obsrvd_7)
print(chi_sqr_7)

#Obesity

obsrvd_8 <- matrix(c(802, 10435, 1516, 50649), nrow = 2, byrow = TRUE)
chi_sqr_8 <- chisq.test(obsrvd_8)
print(chi_sqr_8)

#Sex

obsrvd_9 <- matrix(c(6178, 5059, 32607, 19558), nrow = 2, byrow = TRUE)
chi_sqr_9 <- chisq.test(obsrvd_9)
print(chi_sqr_9)

#performing independent t test on age

mean1 <- 81.5
sd1 <- 7.6
n1 <- 11237
mean2 <- 82.9
sd2 <- 7.8
n2 <- 52165

set.seed(123)

typ_2 <- rnorm(n1,mean1, sd1)
non_typ_2 <- rnorm(n2,mean2, sd2)


#performin independent t test

ttest <- t.test(typ_2, non_typ_2, alternative = "two.sided")
print(ttest)


#Plotting graphs

library(ggplot2)

#plotting frequency polygon for the age

#defining age intervals

age_intervals <- seq(60,100, by = 4)


age_hist <- hist(dem_diab_rsk_fct$
                   AGE_DEM_DIAG[dem_diab_rsk_fct$AGE_DEM_DIAG <= 100],
                 breaks = age_intervals,
                 plot = FALSE)

#to smoothen

age_density <- density(dem_diab_rsk_fct$
                         AGE_DEM_DIAG[dem_diab_rsk_fct$AGE_DEM_DIAG <= 100])

#plotting smoothened curve

plot(age_density,
     main = "Age Distribution(Frequency Polygon)",
     xlab = "Age Group",
     ylab = "Density",
     col = "lightblue",
     lwd = 2)

#### Logistic regression####

# univariate logistic regression analysis

#renaming to new variable

dem_diab_lgstc <- dem_diab_rsk_fct

#Age

lgstc_age <- glm(TYPE_NUM ~ AGE_DEM_DIAG, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_age)

exp(cbind(coef(lgstc_age), confint(lgstc_age)))

#Sex

lgstc_sex <- glm(TYPE_NUM ~ SEX, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_sex)

exp(cbind(coef(lgstc_sex), confint(lgstc_sex)))

#depression

lgstc_dep <- glm(TYPE_NUM ~ DEPRESSION, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_dep)

exp(cbind(coef(lgstc_dep), confint(lgstc_dep)))

#hypertension

lgstc_hyp <- glm(TYPE_NUM ~ HYPERTENSION, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_hyp)

exp(cbind(coef(lgstc_hyp), confint(lgstc_hyp)))

#Systolic

lgstc_sys <- glm(TYPE_NUM ~ SYSTOLIC, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_sys)

exp(cbind(coef(lgstc_sys), confint(lgstc_sys)))

#Heart Disease

lgstc_htd <- glm(TYPE_NUM ~ HEART_DISEASE, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_htd)

exp(cbind(coef(lgstc_htd), confint(lgstc_htd)))

#COPD

lgstc_copd <- glm(TYPE_NUM ~ COPD, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_copd)

exp(cbind(coef(lgstc_copd), confint(lgstc_copd)))

#Mental Illness

lgstc_mntl <- glm(TYPE_NUM ~ MENTAL_ILLNESS, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_mntl)

exp(cbind(coef(lgstc_mntl), confint(lgstc_mntl)))

#Smoking

lgstc_smkng <- glm(TYPE_NUM ~ SMOKING, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_smkng)

exp(cbind(coef(lgstc_smkng), confint(lgstc_smkng)))

#Obesity

lgstc_obs <- glm(TYPE_NUM ~ OBESITY, family = binomial(link = "logit"),
                 data = dem_diab_lgstc)
summary(lgstc_obs)

exp(cbind(coef(lgstc_obs), confint(lgstc_obs)))

#performing multivariate logistic regression analysis

logstc_mdl <- glm(TYPE_NUM ~ AGE_DEM_DIAG + DEPRESSION + HYPERTENSION +
                  HEART_DISEASE + COPD + SMOKING + 
                  OBESITY + SEX , family = binomial(link = "logit"),
                  data = dem_diab_lgstc)

summary(logstc_mdl)
exp(cbind(coef(logstc_mdl), confint(logstc_mdl)))
exp(coef(logstc_mdl))


