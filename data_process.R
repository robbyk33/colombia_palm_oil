################################################

# Colombia BioFuel Reform Effects on Rural Wage/Salary Workers
# 06.27.2024
# krobby

################################################

library(tidyverse)
library(dplyr)
library(reshape2)

# import IPUMS census data
# ipums <- read.csv("data/ipums_full_1985-2005.csv")

ipums <- read.csv("data/ipumsi_rev3.csv")

# filter and mutate data as needed
ipums_f <-  mutate(ipums,
                   YRBIRTH = as.integer(YEAR)-AGE,
                   YEAR = as.factor(YEAR),
                   URBAN = as.factor(URBAN),
                   GEO1_CO = as.factor(GEO1_CO),
                   GEO2_CO = as.factor(GEO2_CO),
                   RELATE = as.factor(RELATE),
                   BPLCO3 = as.factor(BPLCO3),
                 ) %>%
  # filter(EMPSTAT == 1 | EMPSTAT == 2) %>%
  # filter(CLASSWK != 0 ) %>%
  # filter(CLASSWK != 9) %>%
  # filter(YEAR != 1985) %>%
  filter(AGE < 65) %>%  # retirement age in Colombia is 62 for Male, 57 Female
  filter(SEX == 1 | SEX == 2) %>%
  # filter(URBAN ==1) %>%
  filter(GEO2_CO != "170011099") %>%  # remove san andres island district
  # filter(BPLCO3 == 1 | BPLCO3 == 2) %>%  # filters for locals (department level)
  mutate(
    SEX = case_when(SEX==1~1,TRUE~0),
    MINOR = case_when(AGE<15 ~ 1,TRUE~0),
    LF = case_when(LABFORCE==2~1,TRUE~0),
    WAGEWORKER = case_when(CLASSWK==2~1,TRUE~0),
    EMPLOYMENT = case_when(EMPSTAT == 1 ~1, TRUE~0),
    RURAL = case_when(URBAN==1~1,TRUE~0),
    CLASSWK = as.factor(CLASSWK),
    LOCAL = case_when(BPLCO3 == 1~1,TRUE~0),  #dummy for local
    SCHOOL = case_when(SCHOOL==1~1,TRUE~0),# dummy for school attendance
    # COLLEGE = as.factor(case_when(EDUCCO>400 & EDUCCO<600~1,TRUE~0)),
    # FAM_UNPAID = as.factor(case_when(CLASSWKD==310 ~ 1,TRUE~0)),
    # DOMESTIC = as.factor(case_when(CLASSWKD==230 ~ 1,TRUE~0)),
    # MAN_LABOR = as.factor(case_when(CLASSWKD==204 ~ 1,TRUE~0)),
    # WHITE_COLLAR = as.factor(case_when(CLASSWKD==203 ~ 1,TRUE~0)),
    # SELF_EMP = as.factor(case_when(CLASSWKD==110 | CLASSWKD==120 ~ 1,TRUE~0))
    ) %>% droplevels

rm(ipums)

# assign percent agriculture industry based on entire 1993 census
ag_means <- subset(ipums_f,YEAR==1993) %>% subset(AGE>=10) %>%
  mutate(AG_IND = case_when(INDGEN==10~1,TRUE~0)) %>% 
  group_by(GEO2_CO) %>%  summarize(m=mean(AG_IND)) %>%  ungroup()

cutoff = mean(ag_means$m)

# visualize 1993 agriculture industry's share of Labor Force
library(ggstatsplot)

plt_hist <- gghistostats(ag_means,m,
             title = "1993 Share of Workforce in the Agriculture/Farming Industry",
             subtitle = "Municipal Level",
             xlab = "% Workers in Agriculture/Fishing/Farming",
             centrality.type = "nonparametric",
             results.subtitle = FALSE,

             caption = NULL,
             # type = "parametric",
             # test.value = 0,
             # bf.prior = 0.707,
             bf.message = FALSE,
             # effsize.type = "g",
             # conf.level = 0.95,
             # tr = 0.2,
             # digits = 2L,
             ggtheme = theme_light())

plt_hist



print(length(ipums_f$GEO2_CO))


# generate column for 1993 ag means (assigned to all observations)
# detach(package:plyr)    
# library(dplyr)

ip_means <- filter(ipums_f,AGE>=18) %>%
  group_by(GEO2_CO,YEAR) %>%
  summarize(
    WAGEWORKER = mean(WAGEWORKER),
    EMPLOYMENT = mean(EMPLOYMENT),
    RURAL = mean(RURAL),
    SCHOOL = mean(SCHOOL),
    SEX = mean(SEX),
    AGE = mean(AGE),
    ) %>% merge(ag_means,by="GEO2_CO",all.x=TRUE) %>%
  mutate(
    POST = case_when(YEAR==2005~1,TRUE~0),
    TREAT = case_when(m>cutoff~1,TRUE~0)
  ) %>% ungroup()

# save grouped means
saveRDS(ip_means,file="processed_data/ipums_grouped.RDS")

##########################
## school grouped means ##

school_means <- filter(ipums_f,AGE<18) %>%
  group_by(GEO2_CO,YEAR) %>%
  summarize(
    # GEO1_CO = GEO1_CO,
    WAGEWORKER = mean(WAGEWORKER),
    EMPLOYMENT = mean(EMPLOYMENT),
    RURAL = mean(RURAL),
    SCHOOL = mean(SCHOOL),
    SEX = mean(SEX),
    AGE = mean(AGE),
  ) %>% merge(ag_means,by="GEO2_CO",all.x=TRUE) %>%
  mutate(
    POST = case_when(YEAR==2005~1,TRUE~0),
    TREAT = case_when(m>cutoff~1,TRUE~0)
  ) %>% ungroup()

# save grouped means
saveRDS(school_means,file="processed_data/ipums_school.RDS")

########################################
## grouped by birthplace
##########################

ip_bpl <- filter(ipums_f,AGE>=18) %>% filter(BPLCO3!=9) %>%
  droplevels() %>%
  group_by(GEO2_CO,YEAR,BPLCO3) %>%
  summarize(
    WAGEWORKER = mean(WAGEWORKER),
    EMPLOYMENT = mean(EMPLOYMENT),
    RURAL = mean(RURAL),
    SCHOOL = mean(SCHOOL),
    SEX = mean(SEX),
    AGE = mean(AGE),
  ) %>% merge(ag_means,by="GEO2_CO",all.x=TRUE) %>%
  mutate(
    POST = case_when(YEAR==2005~1,TRUE~0),
    TREAT = case_when(m>cutoff~1,TRUE~0)
  ) %>% ungroup()

# save grouped means
saveRDS(ip_bpl,file="processed_data/ipums_bpl.RDS")

########################################
## grouped by birthplace - children subset
##########################

bpl_child <- filter(ipums_f,AGE<18) %>% filter(BPLCO3!=9) %>%
  droplevels() %>%
  group_by(GEO2_CO,YEAR,BPLCO3) %>%
  summarize(
    WAGEWORKER = mean(WAGEWORKER),
    EMPLOYMENT = mean(EMPLOYMENT),
    RURAL = mean(RURAL),
    SCHOOL = mean(SCHOOL),
    SEX = mean(SEX),
    AGE = mean(AGE),
  ) %>% merge(ag_means,by="GEO2_CO",all.x=TRUE) %>%
  mutate(
    POST = case_when(YEAR==2005~1,TRUE~0),
    TREAT = case_when(m>cutoff~1,TRUE~0)
  ) %>% ungroup()

# save grouped means
saveRDS(bpl_child,file="processed_data/ipums_bpl-child.RDS")
#################################
## ungrouped data 

ipums_f <- merge(ipums_f,ag_means,by="GEO2_CO",all.x = TRUE)
# generate treatment variable = 1 if municipality ag_mean > Median in 1993

ipums_f <- ipums_f %>%
  mutate(
    TREAT = case_when(m>cutoff~1,TRUE~0)
  )

##########################
# Store data?
##########################
saveRDS(ipums_f,file="processed_data/ipums.RDS")

############################################################
############################################################
##  Summary Statistic Analysis
###########################################################
#############################################################
library(gtsummary)

ipums_f <- readRDS("processed_data/ipums.RDS")

# compare treatment vs control means

# generate characteristics of sample
treatment_means <- ipums_f %>%
  group_by(GEO2_CO,YEAR,TREAT) %>% 
  summarize(
    Age=mean(AGE),
    Male = mean(SEX),
    Rural = mean(case_when(URBAN==1~1,TRUE~0)),
    Local = mean(LOCAL),
    LaborForce = mean(case_when(LABFORCE==2~1,TRUE~0)),
    Employed = mean(case_when(EMPSTAT == 1 ~1, TRUE~0)),
    Wage = mean(case_when(CLASSWK==2 ~ 1,TRUE~0)),
    Schooling = mean(SCHOOL)
  ) %>%  ungroup()


# generate characteristics of sample
tbl1 <- treatment_means %>%
  mutate(TREAT = factor(TREAT,labels=c("Control Group","Treated Group"))) %>%
  tbl_summary(
    include = c(Age,Male,Schooling,Rural,Local,LaborForce,Employed,Wage),
    label = list(Schooling~"School Attendance",
                 LaborForce~"Labor Force\nParticipation",
                 Wage~"Wage/Salary Employee",
                 Local~"Born in Municipality"),
    by=TREAT,
    statistic = all_continuous() ~ "{mean} ({sd})", missing = "no"
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Characteristic**") %>% # update the column header
  modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**") %>%
  bold_labels()

tbl1




##########################
# plotting trends
#########################

g2 <- treatment_means %>%
  group_by(YEAR,TREAT) %>%
  summarize(
    Age=mean(Age),
    Male = mean(Male),
    Rural = mean(Rural),
    Local = mean(Local),
    LF = mean(LaborForce),
    Employed = mean(Employed),
    Wage = mean(Wage),
    Schooling = mean(Schooling)
  )

# parallel trends Employed
plot_emp <- ggplot(g2,aes(x=YEAR,y=Employed,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Average Municipal Employment Status\n",
       x = "Year",
       y = "% Employed",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()
plot_emp

# parallel trends Wage Worker
plot_wage <- ggplot(g2,aes(x=YEAR,y=Wage,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Municipal Share of Salaried/Wage Workers\n",
       x = "Year",
       y = "% Wage Worker",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()

plot_wage


# parallel trends Schooling
plot_school <- ggplot(g2,aes(x=YEAR,y=Schooling,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Municipal Means - More than four years of school\n",
       x = "Year",
       y = "% >4yr School",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()

plot_school


# parallel trends LF participation
plot_lf <- ggplot(g2,aes(x=YEAR,y=LF,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Average Municipal Labor Force Participation Rate\n",
       x = "Year",
       y = "% LF Participation",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()

plot_lf
############################################
## generate heterogeneity regressions
############################################

rural_means <- ipums_f %>% subset(URBAN==1) %>%
  group_by(GEO2_CO,YEAR,TREAT) %>% 
  summarize(
    Age=mean(AGE),
    Male = mean(SEX),
    # Rural = mean(case_when(URBAN==1~1,TRUE~0)),
    Local = mean(LOCAL),
    LaborForce = mean(case_when(LABFORCE==2~1,TRUE~0)),
    Employed = mean(case_when(EMPSTAT == 1 ~1, TRUE~0)),
    Wage = mean(case_when(CLASSWK==2 ~ 1,TRUE~0)),
    Schooling = mean(case_when((YRSCHOOL>4 &YRSCHOOL<92)~1,TRUE~0))
  ) %>%  ungroup()

ru <- rural_means %>%
  group_by(YEAR,TREAT) %>%
  summarize(
    Age=mean(Age),
    Male = mean(Male),
    # Rural = mean(Rural),
    Local = mean(Local),
    LF = mean(LaborForce),
    Employed = mean(Employed),
    Wage = mean(Wage),
    Schooling = mean(Schooling)
  )




# parallel trends Employed
plot_emp <- ggplot(ru,aes(x=YEAR,y=Employed,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Rural Municipal Employment Means\n",
       x = "Year",
       y = "% Employed",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()
plot_emp

# parallel trends Wage Worker
plot_wage <- ggplot(ru,aes(x=YEAR,y=Wage,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Municipal Share of Salaried/Wage Workers\n",
       x = "Year",
       y = "% Wage Worker",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()

plot_wage

# parallel trends Schooling
plot_school <- ggplot(ru,aes(x=YEAR,y=Schooling,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Municipal Means - More than four years of school\n",
       x = "Year",
       y = "% >4yr School",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()

plot_school


# parallel trends LF participation
plot_lf <- ggplot(ru,aes(x=YEAR,y=LF,color=as.factor(TREAT),group=as.factor(TREAT)))+
  labs(title = "Average Municipal Labor Force Participation Rate\n",
       x = "Year",
       y = "% LF Participation",
       color = "Treatment Group\n") +
  scale_color_manual(labels = c("Control", "Treatment"),values = c("cyan","deeppink")) +
  geom_line()+
  theme_bw()

plot_lf
