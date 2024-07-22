#############################
# created date: 6.27.24
# created by: krobby



##########################
# Regression Models
##########################

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
  select(-c(COUNTRY,SERIAL,EMPSTATD,
            SAMPLE, HHWT,LABFORCE, INDGEN,
            REGNCO,BPLCO1, BPLCO2,CLASSWKD,
            CO2005A_EMPSTAT)) %>%
  mutate(
    WAGEWORKER = case_when(CLASSWK==2~1,TRUE~0),
    EMPLOYMENT = case_when(EMPSTAT == 1 ~1, TRUE~0),
    POST = case_when(YEAR==2005~1,TRUE~0),
    YRBRTH = as.factor(YRBIRTH),
    PLBRTH = as.factor(BPLCO3)
  ) %>% droplevels()



################
## Employment ##
################

# first regression - no fixed effects
emp1 <- ipums_f %>%
  lm(EMPLOYMENT~TREAT*POST+SEX,
     data=.)

emp1_se = starprep(emp1,se_type = 'stata',clusters = ipums_f$GEO1_CO)


# add Birth Year and Birthplace fixed effects
emp2 <- ipums_f %>% subset(PLBRTH!=9) %>%  # drop NA
  lm(EMPLOYMENT~TREAT*POST+YRBRTH+PLBRTH+SEX,
     data=.)

emp2_se = starprep(emp2,
                 se_type = 'stata',
                 clusters = 
                   ipums_f %>% subset(PLBRTH!=9) %>% .$GEO1_CO)

# add department fixed effects
emp3 <- ipums_f %>% subset(PLBRTH!=9) %>%
  lm(EMPLOYMENT~TREAT*POST+YRBRTH+PLBRTH+SEX+GEO1_CO,
     data=.)

emp3_se = starprep(emp3,
                 se_type = 'stata',
                 clusters = 
                   ipums_f %>% subset(PLBRTH!=9) %>% .$GEO1_CO)

# 
# 
# # view regression results
# star1 <- stargazer(emp1,emp2,emp3,
#           se = starprep(emp1,emp2,emp3,
#                         se_type="stata",
#                         clusters = ipums_f$GEO1_CO),
#           p = starprep(emp1,emp2,emp3,stat = "p.value"),
#           type='text', digits = 2,
#           title = 'Employment', 
#           style = 'qje',
#           star.cutoffs = NA,
#           omit.stat = c("f","adj.rsq","ser"),
#           df=FALSE,
#           covariate.labels = c(
#             "AG93",
#             "D",
#             "Local",
#             "Male",
#             "AG93*D",
#             "Intercept"
#           ),
#           omit = c("YRBRTH","GEO1_CO"),
#           # omit.labels = c("Birth Year Fixed-Effects",
#           #                 "BRTHLOC",
#           #                 "Department Fixed-Effects"),
#           add.lines = list(
#             c("Birthplace FE", "No","Yes","Yes"),
#             c("Birth Year FE","No","Yes","Yes"),
#             c("Department FE", "No","No","Yes")),
#           out = 'plots/emp1.doc'
#           )
# star1
# rm(star1)

gc()

###################################################################
# Hlavac, Marek (2022). stargazer: Well-Formatted Regression
#     and Summary Statistics Tables. R package version 5.2.3.
#     https://CRAN.R-project.org/package=stargazer 
###################################################################

#############################################################
#############################################################

#################
## Wage Worker ##
#################

# first regression - no FE controls
wage1 <- ipums_f %>%
  lm(WAGEWORKER~TREAT*POST+SEX,
     data=.)
wage1_se = starprep(wage1,se_type = 'stata',clusters = ipums_f$GEO1_CO)

# add birth FE
wage2 <- ipums_f %>% subset(PLBRTH!=9) %>%  # drop NA
  lm(WAGEWORKER~TREAT*POST+YRBRTH+PLBRTH+SEX,
     data=.)

wage2_se = starprep(wage2,
                   se_type = 'stata',
                   clusters = 
                     ipums_f %>% subset(PLBRTH!=9) %>% .$GEO1_CO)


# 3rd regression
wage3 <- ipums_f %>% subset(PLBRTH!=9) %>%  # drop NA
  lm(WAGEWORKER~TREAT*POST+YRBRTH+PLBRTH+SEX+GEO1_CO,
     data=.)

wage3_se = starprep(wage3,
                    se_type = 'stata',
                    clusters = 
                      ipums_f %>% subset(PLBRTH!=9) %>% .$GEO1_CO)



#####################################################
## view combined employment results  ############
###############################################
se_list = c(emp1_se,emp2_se,emp3_se,
            wage1_se, wage2_se,wage3_se)

# view regression results
allemp <- stargazer(emp1,emp2,emp3,
                    wage1,wage2,wage3,
                    se = se_list,
                    p = starprep(emp1,emp2,emp3,
                                  wage1,wage2,wage3,stat = "p.value"),
                    type='text', digits = 2,
                    title = 'Employment Outcomes', 
                    style = 'qje',
                    omit.stat = c("f","adj.rsq","ser"),
                    omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                    df=FALSE,
                    order = c(
                      "TREAT",
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
                       c("Birthplace FE", "No","Yes","Yes","No","Yes","Yes"),
                       c("Birth Year FE","No","Yes","Yes","No","Yes","Yes"),
                       c("Department FE", "No","No","Yes","No","No","Yes")),
                    out = 'plots/emp_wage_full.doc'
)
allemp


rm(emp1,emp1_se)
rm(emp2,emp2_se)
rm(emp3,emp3_se)
rm(wage1,wage1_se)
rm(wage2,wage2_se)
rm(wage3,wage3_se)
rm(se_list)
gc()




###############
## Education ##
###############

# first regression - no FE controls
edu1 <- ipums_f %>%
  lm(SCHOOL~TREAT*POST+SEX,
     data=.)

edu1_se = starprep(edu1,
                   se_type = 'stata',
                   clusters = ipums_f$GEO1_CO)

# add birthplace/year fixed effects
edu2 <- ipums_f %>% subset(PLBRTH!=9) %>%
  lm(SCHOOL~TREAT*POST+YRBRTH+PLBRTH+SEX,
     data=.)

edu2_se = starprep(edu2,
                   se_type = 'stata',
                   clusters = 
                     ipums_f %>% subset(PLBRTH!=9) %>% .$GEO1_CO)

# add department fixed effects
edu3 <- ipums_f %>% subset(PLBRTH!=9) %>%
  lm(SCHOOL~TREAT*POST+YRBRTH+PLBRTH+SEX+GEO1_CO,
     data=.)

edu3_se = starprep(edu3,
                   se_type = 'stata',
                   clusters = 
                     ipums_f %>% subset(PLBRTH!=9) %>% .$GEO1_CO)

gc()
# store robust standard errors
edu_se_list = c(edu1_se,edu2_se,edu3_se)

# view regression results
sumedu <- stargazer(edu1,edu2,edu3,
                     se = edu_se_list,
                     p = starprep(edu1,edu2,edu3,stat = "p.value"),
                     type='text', digits = 2,
                     title = 'Education', 
                     style = 'qje',
                     omit.stat = c("f","adj.rsq","ser"),
                     omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                     # column.labels = ">4 Years School",
                     dep.var.caption = ">4 Years School",
                     dep.var.labels.include = FALSE,
                     df=FALSE,
                     order = c(
                        "TREAT",
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
                       c("Birthplace FE", "No","Yes","Yes"),
                       c("Birth Year FE","No","Yes","Yes"),
                       c("Department FE", "No","No","Yes")),
                     out = 'plots/edu_fe.doc')

sumedu

rm(sumedu)
rm(edu1)
rm(edu2)
rm(edu3)
gc()


####################################################
# counterfactual test - Valle del Cauca Sugarcane ##
# GEO1_CO == 170076
####################################################
cf_valle <- ipums_f %>% subset(GEO1_CO==170076)

cf_emp <- cf_valle %>% 
  lm(EMPLOYMENT~TREAT*POST+YRBRTH+LOCAL+SEX,
     data=.)

cf_wage <- cf_valle %>%
  lm(WAGEWORKER~TREAT*POST+YRBRTH+LOCAL+SEX,
     data=.)

cf_edu <- cf_valle %>%
  lm(SCHOOL~TREAT*POST+YRBRTH+LOCAL+SEX,
     data=.)


# view regression results for sugarcane workers (counterfactual)
cf_summary <- stargazer(cf_emp,cf_wage,cf_edu,
                    se = starprep(cf_emp,cf_wage,cf_edu,
                                  se_type="stata"),
                    p = starprep(cf_emp,cf_wage,cf_edu,stat = "p.value"),
                    type='text', digits = 2,
                    title = 'Sugarcane Industry Counterfactual', 
                    style = 'qje',
                    omit.stat = c("f","adj.rsq","ser"),
                    omit = c("YRBRTH"),
                    df=FALSE,
                    order = c(
                      "TREAT",
                      "POST",
                      "SEX",
                      "LOCAL",
                      "Intercept"
                    ),
                    covariate.labels = c(
                      "AG93",
                      "AG93*D",
                      "D",
                      "Male",
                      "Local",
                      "Constant"
                    ),
                    # omit.labels = c("Birth Year Fixed-Effects",
                    #                 "BRTHLOC",
                    #                 "Department Fixed-Effects"),
                    add.lines = list(
                      c("Controls", "Yes","Yes","Yes"),
                      c("Birth Year FE","Yes","Yes","Yes"),
                      c("Department FE", "No","No","No")),
                    out = 'plots/valle_counter.doc'
                    )
cf_summary


rm(cf_valle)
rm(cf_emp)
rm(cf_wage)
rm(cf_edu)

#################################
## verifying SEs  -- CONFIRMED ##
#################################
# 
# 
# cf_emp_robust <- cf_ipums %>% 
#   lm_robust(EMPLOYMENT~TREAT*POST+YRBRTH+LOCAL+SEX+GEO1_CO,
#      data=.,se_type = 'stata',clusters = cf_ipums$GEO1_CO)
# summary(cf_emp_robust)
# rm(cf_emp_robust)



##############################################
# Heterogenous effects                ########
##############################################
# library(lfe)   -- look into this package



###############################
## Birthplace Fixed Effects ###
###############################

# drop NA
bpl <- ipums_f %>% subset(BPLCO3!=9) %>%
  mutate(
    PLBRTH = as.factor(BPLCO3))

# birthplace fixed effects - employment
bpl_emp <- bpl  %>%
  lm(EMPLOYMENT~TREAT*POST+YRBRTH+SEX+GEO1_CO+PLBRTH,
     data=.)

# birthplace fixed effects - wage/salary worker
bpl_wage <- bpl %>% 
  lm(WAGEWORKER~TREAT*POST+YRBRTH+SEX+GEO1_CO+PLBRTH,
     data=.)

# birthplace fixed effects - education
bpl_schl <- bpl %>% 
  lm(SCHOOL~TREAT*POST+YRBRTH+SEX+GEO1_CO+PLBRTH,
     data=.)

###############################################

## generate robust standard errors
## department level clustering

bpl_se1 <- starprep(bpl_emp,
                    se_type = 'stata',
                    clusters = bpl$GEO1_CO)

bpl_se2 <- starprep(bpl_wage,
                    se_type = 'stata',
                    clusters = bpl$GEO1_CO)

bpl_se3 <- starprep(bpl_schl,
                    se_type = 'stata',
                    clusters = bpl$GEO1_CO)

# view regression results for white collar workers (countefactual)
bpl_summary <- stargazer(bpl_emp,bpl_wage, bpl_schl,
                        se = c(bpl_se1,bpl_se2,bpl_se3),
                        p = starprep(
                          bpl_emp,bpl_wage,bpl_schl,
                          stat = "p.value"),
                        type='text', digits = 2,
                        title = 'Birthplace and Birth Year Fixed Effects',
                        column.labels = c("Employment","Wage Worker",">4 Years School"),
                        dep.var.labels.include = FALSE,
                        style = 'qje',
                        omit.stat = c("f","adj.rsq","ser"),
                        omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                        df=FALSE,
                        order = c(
                          "TREAT",
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
                        out = 'plots/birthplace_fe.doc')

bpl_summary

