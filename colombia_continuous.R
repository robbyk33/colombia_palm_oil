


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

ipums_f <- readRDS("processed_data/ipums.RDS")

ipums_f <- ipums_f %>% subset(YEAR!=1985) %>%
  subset(BPLCO3!=9) %>%  # remove NA/Missing
  mutate(
    WAGEWORKER = case_when(CLASSWK==2~1,TRUE~0),
    EMPLOYMENT = case_when(EMPSTAT == 1 ~1, TRUE~0),
    POST = case_when(YEAR==2005~1,TRUE~0),
    YRBRTH = as.factor(YRBIRTH),
    PLBRTH = as.factor(BPLCO3)
  ) %>% droplevels()


###########################
## Continuous Treatments ##
###########################

# Continuous AG93 Employment
emp_c <- ipums_f %>%
  lm(EMPLOYMENT~m*POST+YRBRTH+PLBRTH+SEX+GEO1_CO,
     data=.)

# Continuous AG93 Wage/Salary Worker
wage_c <- ipums_f %>%
  lm(WAGEWORKER~m*POST+YRBRTH+PLBRTH+SEX+GEO1_CO,
     data=.)

# add 
edu_c <- ipums_f %>%
  lm(SCHOOL~m*POST+YRBRTH+PLBRTH+SEX+GEO1_CO,
     data=.)

se1 <- starprep(emp_c,se_type = 'stata',clusters = ipums_f$GEO1_CO)
se2 <- starprep(wage_c,se_type = 'stata',clusters = ipums_f$GEO1_CO)
se3 <- starprep(edu_c,se_type = 'stata',clusters = ipums_f$GEO1_CO)

# view regression results
star1 <- stargazer(emp_c,wage_c,edu_c,
                   se = c(se1,se2,se3),
                   p = starprep(emp_c,wage_c,edu_c,stat = "p.value"),
                   type='text', digits = 2,
                   ci=TRUE,
                   title = 'Continuous Treatment Outcomes', 
                   style = 'qje',
                   omit.table.layout = "#",
                   # star.cutoffs = NA,
                   omit.stat = c("f","adj.rsq","ser"),
                   omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                   df=FALSE,
                   order = c(
                     "m",
                     "POST",
                     "SEX",
                     "Intercept"
                   ),
                   covariate.labels = c(
                     "AG93",
                     "AG93*D",
                     "D",
                     "Male",
                     "Constant"
                   ),
                   # omit.labels = c("Birth Year Fixed-Effects",
                   #                 "BRTHLOC",
                   #                 "Department Fixed-Effects"),
                   add.lines = list(
                     c("Birth Year FE","Yes","Yes","Yes"),
                     c("Birthplace FE", "Yes","Yes","Yes"),
                     c("Department FE", "Yes","Yes","Yes")),
                   out = 'plots/continous_emp_bpl.doc'
)
star1

rm(star1)
gc()

