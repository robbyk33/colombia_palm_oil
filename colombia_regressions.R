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

df1 <- readRDS("processed_data/ipums_grouped.RDS")



################
## Employment ##
################

# first regression - no controls
emp1 <- df1 %>%
  lm(EMPLOYMENT~TREAT*POST,
     data=.)


# add muni means controls
emp2 <- df1 %>%
  lm(EMPLOYMENT~TREAT*POST+AGE+SEX,
     data=.)


# first regression - no controls
wage1 <- df1 %>%
  lm(WAGEWORKER~TREAT*POST,
     data=.)


# add muni means controls
wage2 <- df1 %>%
  lm(WAGEWORKER~TREAT*POST+AGE+SEX,
     data=.)


# generate robust standard errors w/ Municipality level clusters

emp1_se = starprep(emp1,se_type = 'stata',clusters = df1$GEO2_CO)
emp2_se = starprep(emp2,se_type = 'stata',clusters = df1$GEO2_CO)  

wage1_se = starprep(wage1,se_type = 'stata',clusters = df1$GEO2_CO)
wage2_se = starprep(wage2,se_type = 'stata',clusters = df1$GEO2_CO) 
# emp3_se = starprep(emp3,se_type = 'stata',clusters = ipums_f$GEO2_CO)

gc()  # clear unused memory

tmeans = df1 %>% group_by(TREAT) %>%
  summarise(
    emp_m = round(mean(EMPLOYMENT),2),
    wage_m = round(mean(WAGEWORKER),2),
  )


####################################
# # view regression results
###################################################################
# Hlavac, Marek (2022). stargazer: Well-Formatted Regression
#     and Summary Statistics Tables. R package version 5.2.3.
#     https://CRAN.R-project.org/package=stargazer 
###################################################################


star1 <- stargazer(emp1,emp2,wage1,wage2,
          se = c(emp1_se,emp2_se,wage1_se,wage2_se),
          p = starprep(emp1,emp2,wage1,wage2,stat = "p.value"),
          type='text', digits = 2,
          title = 'Employment Outcomes',
          style = 'qje',
          # star.cutoffs = NA,
          omit.stat = c("f","adj.rsq","ser","rsq"),
          # omit.table.layout = "#",
          # star.cutoffs = NA,
          # omit = "GEO1_CO",
          df=FALSE,
          order = c(
            "TREAT",
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
          add.lines = list(
          c("Controls", "No","Yes", "No","Yes"),
          c("Control Group Mean", tmeans[1,]$emp_m,tmeans[1,]$emp_m
            , tmeans[1,]$wage_m,tmeans[1,]$wage_m ),
          c("Treated Group Mean", tmeans[2,]$emp_m,tmeans[2,]$emp_m
            , tmeans[2,]$wage_m,tmeans[2,]$wage_m )),
          out = 'plots/empwage_groups.doc'
          )
star1
rm(star1)


rm(emp1,emp1_se)
rm(emp2,emp2_se)
rm(emp3,emp3_se)
# gc()



###############
## Education ##
###############
df_edu <- readRDS("processed_data/ipums_school.RDS")

# first regression - no FE controls
edu1 <- df_edu %>%
  lm(SCHOOL~TREAT*POST,
     data=.)

edu1_se = starprep(edu1,
                   se_type = 'stata',
                   clusters = df_edu$GEO2_CO)

# add birthplace/year fixed effects
edu2 <- df_edu %>% 
  lm(SCHOOL~TREAT*POST+AGE+SEX,
     data=.)

edu2_se = starprep(edu2,
                   se_type = 'stata',
                   clusters = df_edu$GEO2_CO)

# effects on child labor
cl1 <- df_edu %>% 
  lm(EMPLOYMENT~TREAT*POST+AGE+SEX,
     data=.)

edu3_se = starprep(cl1,
                   se_type = 'stata',
                   clusters = df_edu$GEO2_CO)

gc()
# store robust standard errors
edu_se_list = c(edu1_se,edu2_se,edu3_se)

ed_means = df_edu %>% group_by(TREAT) %>%
  summarize(
    edu = round(mean(SCHOOL),2),
    empl = round(mean(EMPLOYMENT),2)
  ) %>% ungroup()

# view regression results
sumedu <- stargazer(edu1,edu2, cl1,
                     se = edu_se_list,
                     p = starprep(edu1,edu2,cl1,stat = "p.value"),
                     type='text', digits = 2,
                     title = 'Education', 
                     style = 'qje',
                     omit.stat = c("f","adj.rsq","ser","rsq"),
                     # omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                     # column.labels = ">4 Years School",
                     # dep.var.caption = ">4 Years School",
                     dep.var.labels = "School Attendance", "Employment",
                     df=FALSE,
                     order = c(
                        "TREAT",
                        "POST",
                        "AGE",
                        "SEX",
                        "Intercept"
                      ),
                     covariate.labels = c(
                      "AG93",
                      "AG93*D",
                      "D",
                      "AGE",
                      "Male",
                      "Constant"
                      ),
                     # omit.labels = c("Birth Year Fixed-Effects",
                     #                 "BRTHLOC",
                     #                 "Department Fixed-Effects"),
                    add.lines = list(
                      c("Controls", "No","Yes", "Yes"),
                      c("Control Group Mean", 
                        ed_means[1,]$edu,
                        ed_means[1,]$edu,
                        ed_means[1,]$empl),
                      c("Treated Group Mean",
                        ed_means[2,]$edu,
                        ed_means[2,]$edu,
                        ed_means[2,]$empl)),
                     out = 'plots/edu_school2.doc')

sumedu


rm(sumedu)
rm(edu1)
rm(edu2)
rm(edu3)
gc()


##############################
## Birthplace Heterogeneity ##
##############################

# df_full <- readRDS("processed_data/ipums_bpl.RDS")
df_full <- readRDS("processed_data/ipums_bpl-child.RDS")


# local born municipality
loc1 <- df_full %>% subset(BPLCO3==1) %>%
  lm(SCHOOL~TREAT*POST+AGE+SEX,
     data=.)

loc1_se = starprep(loc1,
                   se_type = 'stata',
                   clusters = df_full %>% subset(BPLCO3==1) %>% .$GEO2_CO)


# local born department
loc2 <- df_full %>% subset(BPLCO3==2) %>%
lm(SCHOOL~TREAT*POST+AGE+SEX,
   data=.)

loc2_se = starprep(loc2,
                   se_type = 'stata',
                   clusters = df_full %>% subset(BPLCO3==2) %>% .$GEO2_CO)

# born in Colombia
loc3 <- df_full %>% subset(BPLCO3==3) %>%
lm(SCHOOL~TREAT*POST+AGE+SEX,
   data=.)

loc3_se = starprep(loc3,
                   se_type = 'stata',
                   clusters = df_full %>% subset(BPLCO3==3) %>% .$GEO2_CO)


# foreign born
loc4 <- df_full %>% filter(BPLCO3==4) %>%
lm(SCHOOL~TREAT*POST+AGE+SEX,
   data=.)

loc4_se = starprep(loc4,
                   se_type = 'stata',
                   clusters = df_full %>% subset(BPLCO3==4) %>% .$GEO2_CO)

gc()
# store robust standard errors
loc_se_list = c(loc1_se,loc2_se,loc3_se,loc4_se)

loc_means = df_full %>% group_by(TREAT,BPLCO3) %>%
  summarize(
    edu = round(mean(SCHOOL),2),
    empl = round(mean(EMPLOYMENT),2),
    wage = round(mean(WAGEWORKER),2)
  ) %>% ungroup()

ctrl_1 = loc_means[1:4,]
trt_1 = loc_means[5:8,]

# view regression results
bpl1 <- stargazer(loc1,loc2,loc3,loc4,
                    se = loc_se_list,
                    p = starprep(loc1,loc2,loc3,loc4,stat = "p.value"),
                    type='text', digits = 2,
                    title = 'Heterogenous Effects - Locals vs Migrants', 
                    style = 'qje',
                    omit.stat = c("f","adj.rsq","ser","rsq"),
                    # omit = c("PLBRTH","YRBRTH","GEO1_CO"),
                    column.labels = c("Municipality","Dept.","Other Dept.", "Foreign"),
                    # dep.var.caption = ">4 Years School",
                    dep.var.labels = "School Attendance",
                    df=FALSE,
                    order = c(
                      "TREAT",
                      "POST",
                      "AGE",
                      "SEX",
                      "Intercept"
                    ),
                    covariate.labels = c(
                      "AG93",
                      "AG93*D",
                      "D",
                      "AGE",
                      "Male",
                      "Constant"
                    ),
                    # omit.labels = c("Birth Year Fixed-Effects",
                    #                 "BRTHLOC",
                    #                 "Department Fixed-Effects"),
                    add.lines = list(
                      c("Controls", "Yes","Yes", "Yes","Yes"),
                      c("Control Group Mean", 
                        ctrl_1[1,]$edu,
                        ctrl_1[2,]$edu,
                        ctrl_1[3,]$edu,
                        ctrl_1[4,]$edu),
                      c("Treated Group Mean",
                        trt_1[1,]$edu,
                        trt_1[2,]$edu,
                        trt_1[3,]$edu,
                        trt_1[4,]$edu)),
                    out = 'plots/birthplace_schooling.doc')

bpl1


