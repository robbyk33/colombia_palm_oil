


##########################
# Regression Models
##########################


#######################
#### Load Libraries ###
#######################
library(haven)
library(estimatr)
library(MASS)
library(lmtest)
library(AER)
library(dplyr)
library(tidyverse)
library(stargazer)
library(modelsummary)

gc()
############################
# Load processed IPUMS data
############################

ipums_f <- readRDS("processed_data/ipums_grouped.RDS")


###########################
## Continuous Treatments ##
###########################

# Continuous AG93 Employment
emp_c <- ipums_f %>%
  lm(EMPLOYMENT~m*POST+AGE+SEX,
     data=.)

# Continuous AG93 Wage/Salary Worker
wage_c <- ipums_f %>%
  lm(WAGEWORKER~m*POST+AGE+SEX,
     data=.)

# add 
edu_c <- ipums_f %>%
  lm(SCHOOL~m*POST+AGE+SEX,
     data=.)

se1 <- starprep(emp_c,se_type = 'stata',clusters = ipums_f$GEO2_CO)
se2 <- starprep(wage_c,se_type = 'stata',clusters = ipums_f$GEO2_CO)
se3 <- starprep(edu_c,se_type = 'stata',clusters = ipums_f$GEO2_CO)

con_means = ipums_f %>% group_by(TREAT) %>%
  summarize(
    edu = round(mean(SCHOOL),2),
    empl = round(mean(EMPLOYMENT),2),
    wage = round(mean(WAGEWORKER),2)
  ) %>% ungroup()

# view regression results
star1 <- stargazer(emp_c,wage_c,edu_c,
                   se = c(se1,se2,se3),
                   p = starprep(emp_c,wage_c,edu_c,stat = "p.value"),
                   type='text', digits = 2,
                   # ci=TRUE,
                   title = 'Continuous Treatment Outcomes', 
                   style = 'qje',
                   omit.table.layout = "#",
                   # star.cutoffs = NA,
                   omit.stat = c("f","adj.rsq","ser"),
                   # omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                   df=FALSE,
                   order = c(
                     "m",
                     "POST",
                     "AGE",
                     "SEX",
                     "Intercept"
                   ),
                   covariate.labels = c(
                     "AG93",
                     "AG93*D",
                     "D",
                     "Age",
                     "Male",
                     "Constant"
                   ),
                   # omit.labels = c("Birth Year Fixed-Effects",
                   #                 "BRTHLOC",
                   #                 "Department Fixed-Effects"),
                   add.lines = list(
                     c("Controls", "Yes","Yes", "Yes"),
                     c("Control Group Mean", 
                       con_means[0,]$empl,
                       con_means[0,]$wage,
                       con_means[0,]$edu),
                     c("Treated Group Mean",
                       con_means[1,]$empl,
                       con_means[1,]$wage,
                       con_means[1,]$edu)),
                   out = 'plots/continous_robust.doc'
)
star1

rm(star1)
gc()



#########################################
## Upper and lower Quartile Treatments ##
#########################################
lower = quantile(ipums_f$m)[[2]]
upper = quantile(ipums_f$m)[[4]]

qr_tr <- ipums_f %>%
  subset(m<lower | m>upper)


# Employment
emp_c <- qr_tr %>%
  lm(EMPLOYMENT~m*POST+AGE+SEX,
     data=.)

# Wage/Salary Worker
wage_c <- qr_tr %>%
  lm(WAGEWORKER~m*POST+AGE+SEX,
     data=.)

# add 
edu_c <- qr_tr %>%
  lm(SCHOOL~m*POST+AGE+SEX,
     data=.)

se1 <- starprep(emp_c,se_type = 'stata',clusters = qr_tr$GEO2_CO)
se2 <- starprep(wage_c,se_type = 'stata',clusters = qr_tr$GEO2_CO)
se3 <- starprep(edu_c,se_type = 'stata',clusters = qr_tr$GEO2_CO)

con_means = qr_tr %>% group_by(TREAT) %>%
  summarize(
    edu = round(mean(SCHOOL),2),
    empl = round(mean(EMPLOYMENT),2),
    wage = round(mean(WAGEWORKER),2)
  ) %>% ungroup()

# view regression results
star1 <- stargazer(emp_c,wage_c,edu_c,
                   se = c(se1,se2,se3),
                   p = starprep(emp_c,wage_c,edu_c,stat = "p.value"),
                   type='text', digits = 2,
                   # ci=TRUE,
                   title = 'Upper and Lower Quantile Treatment Outcomes', 
                   style = 'qje',
                   omit.table.layout = "#",
                   # star.cutoffs = NA,
                   omit.stat = c("f","adj.rsq","ser"),
                   # omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                   df=FALSE,
                   order = c(
                     "m",
                     "POST",
                     "AGE",
                     "SEX",
                     "Intercept"
                   ),
                   covariate.labels = c(
                     "AG93",
                     "AG93*D",
                     "D",
                     "Age",
                     "Male",
                     "Constant"
                   ),
                   # omit.labels = c("Birth Year Fixed-Effects",
                   #                 "BRTHLOC",
                   #                 "Department Fixed-Effects"),
                   add.lines = list(
                     c("Controls", "Yes","Yes", "Yes"),
                     c("Control Group Mean", 
                       con_means[0,]$empl,
                       con_means[0,]$wage,
                       con_means[0,]$edu),
                     c("Treated Group Mean",
                       con_means[1,]$empl,
                       con_means[1,]$wage,
                       con_means[1,]$edu)),
                   out = 'plots/quantile_robust.doc'
)
star1

rm(star1)
gc()