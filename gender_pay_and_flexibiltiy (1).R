
library(readxl)
library(dplyr) 
library(ggplot2)
library(car)
library(stargazer)
library(broom)
library(caret)

setwd("C:/Users/afari/Desktop/dissertation/Rscript")

WERS_2011_Employee_Questionnaire_Data <- read_excel("employee_questionnaire.xlsx")

#create unique identifier column 
#run once
WERS_2011_Employee_Questionnaire_Data <- cbind(ID = 1:nrow(WERS_2011_Employee_Questionnaire_Data), WERS_2011_Employee_Questionnaire_Data) 

#New table with selected variables 
WERS_2011_Employee_Gender_flexibility_Data  <- WERS_2011_Employee_Questionnaire_Data %>%select(ID, 
                                  #main explanatory variables 
                                   qa4, #hours worked weekly, including overtime and extratime
                                   qe1, #gender 
                                   qe3, #marital_status
                                   qe5, #caregiver?
                                   qe4, #children
                                   qe2, #age
                                   qe7, #education
                                  
                                  # supporting explanatory variables 
                                  qa7a, qa7b, qa7c, qa7d, qa7e,# influence over job tasks
                                  qe13, # ethnicity
                                  qa6, # working longer hours to gain promotion
                                   
                                  #outcome variable 
                                   qe11, #pay without tax and deductions 
                                   
                                  #Moderating variables
                                   qb1a, qb1b, qb1c, qb1d, qb1e, qb1f, #availability of flexible working arrangements 
                                   )

#rename columns in new table 
colnames(WERS_2011_Employee_Gender_flexibility_Data) <- c("ID", "Hours_worked_weekly", "Gender",  "Marital_Status", 
                            "Caregiver", "No_dependent_child", "Age","Education", "influence_over_task_perfomed", "influence_over_workpace",
                            "influence_over_working_methods", "influence_over_order_of_tasks", "influence_over_start_end_of_workday",
                            "Ethnicity","Promotion_hours", 
                            "Gross_Salary", 
                            "Flexitime", "Jobsharing", "Reduced_hours", "Compressed_hours",
                            "Remote_work", "Annualized_work")

#check for NA's
sum(is.na(WERS_2011_Employee_Gender_flexibility_Data))

################################################ data restriction for explanatory variables #####################################################
##################################################################################################################################################
#################################################################################################################################################

#############################################################################
##### Age ##### - nominal factor variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Age, useNA = "always")

#multi-coded, refused values are removed because they are unable to show ones age
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Age != "Multi-coded") 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Age != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Age != "Item not applicable")



#some levels within this variable have very small smaple sizes, the influence of multicollinearity can be magnified as a result.

#remove 16-17 as a result of small sample size
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  filter(Age != "16-17") 


#assess proportion of samples
age <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Age, fill = Age)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
age
#136 variables removed 
##############################################################################

####################################################################
##### hours worked weekly including overtime ##### - numeric variable

#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Hours_worked_weekly, useNA = "always")

#multi-coded, not applicable, and refused values are removed because they are unable to show ones 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Hours_worked_weekly != "Item not applicable") 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Hours_worked_weekly != "Refusal")

#change to variable type numeric 
class(WERS_2011_Employee_Gender_flexibility_Data$Hours_worked_weekly)
WERS_2011_Employee_Gender_flexibility_Data$Hours_worked_weekly <- as.numeric(WERS_2011_Employee_Gender_flexibility_Data$Hours_worked_weekly)

#remove na's from changing to numeric process
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  filter(Hours_worked_weekly != "NA") 

#check summary statistics
summary(WERS_2011_Employee_Gender_flexibility_Data$Hours_worked_weekly)

#box plot to check for working hour spread - looking for outliers
working_hours <-ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes(x= Hours_worked_weekly))+   
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + coord_flip() +
  labs(title = "Number of hours worked spread",
       x = "Working hours", y = "Count") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )

working_hours

#outliers present, cap using UK law directive cap working hours at 48 as UK/ European law caps the working week at a maximum of 48 hours, 
#remove hours above 48
# working time directive - https://www.gov.uk/maximum-weekly-working-hours
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  filter(Hours_worked_weekly <= 60) 

#removing outliers at lower end
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  filter(Hours_worked_weekly >= 15) 

#recheck summary 
summary(WERS_2011_Employee_Gender_flexibility_Data$Hours_worked_weekly)

#2723 variables removed
####################################################################



####################################################################
##### Gender ##### -binary variable

#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Gender, useNA = "always")


#multi-coded, not applicable, and refused values are removed because they are unable to show gender 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Gender != "Item not applicable") 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Gender != "Multi-coded") 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Gender != "Refusal")


#check proportions for genders represented
gend <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Gender, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
gend

#change levels of Gender variable 
Gender <- WERS_2011_Employee_Gender_flexibility_Data$Gender <- factor(WERS_2011_Employee_Gender_flexibility_Data$Gender, levels=c("male", "female"))

#recode for descriptive statistics
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Gender_binary = ifelse( Gender == "male", 0 , 1))

#113 variables removed
##################################################################



##################################################################
##### Marital Status ##### - binary variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Marital_Status, useNA = "always")

#multi-coded, refused values are removed because they are unable to show ones marital status
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Marital_Status != "Multi-coded") 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Marital_Status != "Refusal") 

#assessing proportion per category
maritalstatus <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Marital_Status, fill = Marital_Status)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") +
  labs(title = "Demographic breakdown by marital status - male and fmale combined") +
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
maritalstatus


##recode to binary (reducing influence of multi colinnearity due to sample size)
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Marital_Stats = ifelse( Marital_Status =="married or living with a partner", "married", 
                                   ifelse(Marital_Status == "divorced/ separated" | Marital_Status == "single" | 
                                            Marital_Status == "widowed", "not married", Marital_Status )))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Marital_Stats = ifelse( Marital_Stats =="married", 1 , 0))


#79 observations removed
#############################################################################



#############################################################################
####### care giver ######### - binary variable 

#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Caregiver, useNA = "always")

#multi-coded, refused values are removed because they are unable to show ones age
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Caregiver != "Multi-coded") 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Caregiver != "Refusal")

#assess proportions per categories
careg <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Caregiver, fill = Caregiver)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
careg

##recode to make sample sizes larger to reduce influence of multicollinearity
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Caregiver = ifelse( Caregiver =="yes, 0-4 hours a week" | Caregiver =="yes, 10-19 hours a week" | Caregiver =="yes, 20-34 hours a week" | Caregiver =="yes, 35 or more hours a week" |
                                Caregiver =="yes, 5-9 hours a week" , "caregiver" , "not a caregiver"))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Caregiver = ifelse( Caregiver =="caregiver", 1 , 0))

#71 observations removed
###############################################################################



###############################################################################
##### children ###### - binary variable

#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$No_dependent_child, useNA = "always")

#refused values  are removed because they are unable to tell anything about if the employee has a child 
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(No_dependent_child != "Refusal")

###recode into binary variable
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( No_dependent_child = ifelse( No_dependent_child =="yes, i have dependent children", 1 , 0))

#assess proportion per category
child <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = No_dependent_child, fill = No_dependent_child)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
child

#728 observations removed 
###############################################################################







###############################################################################
##### educational qualification ###### - nominal fctor variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Education, useNA = "always")

#assessing variable type 
class(WERS_2011_Employee_Gender_flexibility_Data$Education)

###recode to replace numbers with educational qualification name per codebook and survey
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% 
  mutate(Education = ifelse(Education == 0, "GCSE grades D_G/CSE grades 2-5 SCE O grades D-E/SCE Standard grades 4-7", #
                            ifelse(Education == 1, "GCSE grades A-C, GCE O-level passes, CSE Grade 1 SCE O grades A-C, SCE Standard",#
                                   ifelse(Education == 2, "1 GCE 'A' level grades A-E, 1-2 SCE Higher grades A-C, AS levels",#
                                          ifelse(Education == 3, "2 or more GCE 'A' levels grades A-E, 3 or more SCE Higher grades A-C", 
                                                 ifelse(Education == 4, " First degree, eg. BSc, BA, BEd, HND, HNC, MA at first degree level", 
                                                        ifelse(Education == 5, "Higher degree, eg. MSc, MA, MBA, PGCE, PhD",
                                                               ifelse(Education == 6, "Other academic qualifications",
                                                                      ifelse(Education == 7, "No acaedmic qualification","No vocational or professional qualifications")))))))))
                                                
#education binary for desc statistics
#1-Educational Qualification gained
#0 - No educational Qualification Gained
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Education_binary = ifelse( Education =="GCSE"| Education == "GCE" | Education == "First degree, e.g. BSc" | Education == "Higher Degree e.g. MSc" |
                                       Education == "Other academic qualifications" | Education == "NVQ/SVQ" |
                                       Education == "Other Professional Qualification", 1, 0))

#assess proportion of samples 
educ <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Education, fill = Education)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + coord_flip() +
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
educ

#no observations removed
#######################################################################



#######################################################################
##### influence over task performed ###### - binary variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$influence_over_task_perfomed, useNA = "always")

#refused values and multi coded are removed because they are unable to tell anything about the level of influence the employee has over that aspect of their work
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_task_perfomed != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_task_perfomed!= "Multi-coded")

#assess proportions 
influence_over_task_perfomed <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = influence_over_task_perfomed, fill = influence_over_task_perfomed)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
influence_over_task_perfomed

### recode to combine smaller observations into larger samples
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_task_perfomed = ifelse( influence_over_task_perfomed =="a little" | influence_over_task_perfomed =="a lot" |
                                                   influence_over_task_perfomed =="some", "influence",influence_over_task_perfomed))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_task_perfomed = ifelse( influence_over_task_perfomed =="Don't know" | influence_over_task_perfomed =="none" ,
                                                 "no influence  ",influence_over_task_perfomed))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_task_perfomed = ifelse( influence_over_task_perfomed =="influence", 1 , 0))
###
#78 variables removed
#######################################################################


#######################################################################
##### influence workpace ###### - binary variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$influence_over_workpace, useNA = "always")


#refused values and multi coded are removed because they are unable to tell anything about the level of influence the employee has over that aspect of their work
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_workpace != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_workpace!= "Multi-coded")


#assess proportions 
influence_over_workpace <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = influence_over_workpace, fill = influence_over_workpace)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
influence_over_workpace


### recode to combine smaller observations into larger samples
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_workpace = ifelse( influence_over_workpace =="a little" | influence_over_workpace =="a lot" |
                                              influence_over_workpace =="some", "influence",influence_over_workpace))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_workpace = ifelse( influence_over_workpace =="Don't know" | influence_over_workpace =="none" ,
                                            "no influence  ",influence_over_workpace))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_workpace = ifelse( influence_over_workpace =="influence", 1 , 0))
#75 variables removed
#######################################################################


#######################################################################
##### influence working methods ###### - binary variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$influence_over_working_methods, useNA = "always")


#refused values and multi coded are removed because they are unable to tell anything about the level of influence the employee has over that aspect of their work
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_working_methods != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_working_methods != "Multi-coded")

#assess proportions 
influence_over_working_methods <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = influence_over_working_methods, fill = influence_over_working_methods)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
influence_over_working_methods

### recode to combine smaller observations into larger samples
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_working_methods = ifelse( influence_over_working_methods =="a little" | influence_over_working_methods =="a lot" |
                                                     influence_over_working_methods =="some", "influence",influence_over_working_methods))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_working_methods = ifelse( influence_over_working_methods =="Don't know" | influence_over_working_methods =="none" ,
                                                   "no influence  ",influence_over_working_methods))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_working_methods = ifelse( influence_over_working_methods =="influence", 1 , 0))
#42 observations removed
#######################################################################

#######################################################################
##### influence_over_order_of_tasks ###### - binary variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$influence_over_order_of_tasks, useNA = "always")

#refused values and multi coded are removed because they are unable to tell anything about the level of influence the employee has over that aspect of their work
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_order_of_tasks != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_order_of_tasks != "Multi-coded")

#assess proportions 
influence_over_working_methods <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = influence_over_working_methods, fill = influence_over_working_methods)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
influence_over_working_methods


### recode to combine smaller observations into larger samples
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_order_of_tasks = ifelse( influence_over_order_of_tasks =="a little" | influence_over_order_of_tasks =="a lot" |
                                                    influence_over_order_of_tasks =="some", "influence",influence_over_order_of_tasks))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_order_of_tasks = ifelse( influence_over_order_of_tasks =="Don't know" | influence_over_order_of_tasks =="none" ,
                                                  "no influence  ",influence_over_order_of_tasks))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_order_of_tasks = ifelse( influence_over_order_of_tasks =="influence", 1 , 0))
#32 observations removed
#######################################################################

#######################################################################
##### influence_over_start_end_of_workday ###### - binary variable 
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$influence_over_start_end_of_workday, useNA = "always")

#refused values and multi coded are removed because they are unable to tell anything about the level of influence the employee has over that aspect of their work
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_start_end_of_workday != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(influence_over_start_end_of_workday != "Multi-coded")

#assess proportions 
influence_over_start_end_of_workday <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = influence_over_start_end_of_workday, fill = influence_over_start_end_of_workday)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
influence_over_start_end_of_workday

### recode to combine smaller observations into larger samples
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_start_end_of_workday = ifelse( influence_over_start_end_of_workday =="a little" | influence_over_start_end_of_workday =="a lot" |
                                                          influence_over_start_end_of_workday =="some", "influence",influence_over_start_end_of_workday))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_start_end_of_workday = ifelse( influence_over_start_end_of_workday =="Don't know" | influence_over_start_end_of_workday =="none" ,
                                                        "no influence  ",influence_over_start_end_of_workday))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( influence_over_start_end_of_workday = ifelse( influence_over_start_end_of_workday =="influence", 1 , 0))
#23 observations removed
#############################################################################


#############################################################################
##### ethnicity #####
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Ethnicity, useNA = "always")

#Refused values are removed because they are unable to show ones ethnicity
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Ethnicity != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Ethnicity != "Multi-coded")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Ethnicity != "Item not applicable")

###### treat as factor, nominal variable


#recode per (https://www.ethnicity-facts-figures.service.gov.uk/style-guide/ethnic-groups)
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Ethnicity = ifelse( Ethnicity =="indian" |  Ethnicity =="pakistani" |  Ethnicity =="bangladeshi"  |  Ethnicity =="chinese" |  Ethnicity =="any other asian background" , "Asian or Asian British" ,
                                  ifelse(Ethnicity == "caribbean" | Ethnicity == "african" | Ethnicity == "any other black background", "Black, Black British, Caribbean or African", 
                                         ifelse(Ethnicity == "white and asian" | Ethnicity == "white and black african" | Ethnicity == "white and black caribbean" | Ethnicity == "any other mixed background", "Mixed or multiple ethnic groups", 
                                                ifelse(Ethnicity == "british" | Ethnicity == "irish" | Ethnicity == "any other white background" , " White",
                                                       ifelse(Ethnicity == "arab" | Ethnicity == "any other ethnic group",  "Other ethnic group", Ethnicity))))))


#assess proportions
Ethnicity <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Ethnicity, fill = Ethnicity)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + coord_flip() +
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Ethnicity

#585 observations removed
############################################################################


############################################################################
##### work long hours for promotion ( promotion hours) ######
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Promotion_hours, useNA = "always")

#multi-coded, not applicable, and refused values are removed because they are unable to show ones gender
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Promotion_hours != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Promotion_hours != "Multi-coded")

#assessing spread of observations per level
Promotion_hours <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Promotion_hours, fill = Promotion_hours)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Promotion_hours

###recode
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Promotion_hours = ifelse( Promotion_hours =="agree" | Promotion_hours =="strongly agree","agree",Promotion_hours))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Promotion_hours = ifelse( Promotion_hours =="disagree" | Promotion_hours =="strongly disagree","disagree",Promotion_hours))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Promotion_hours_binary = ifelse( Promotion_hours =="agree", 1 , 0))
#53 observations removed
######################################################################


###############################################################################################################################################
##################################################################################################################################################
#################################################################################################################################################


#################################################### Data restrictions for moderation variables ###################################################
##################################################################################################################################################
#################################################################################################################################################

################################################################################

##### availability of flexible working arrangements ######
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Flexitime, useNA = "always")
table(WERS_2011_Employee_Gender_flexibility_Data$Jobsharing, useNA = "always")
table(WERS_2011_Employee_Gender_flexibility_Data$Reduced_hours, useNA = "always")
table(WERS_2011_Employee_Gender_flexibility_Data$Compressed_hours, useNA = "always")
table(WERS_2011_Employee_Gender_flexibility_Data$Remote_work, useNA = "always")
table(WERS_2011_Employee_Gender_flexibility_Data$Annualized_work, useNA = "always")


#refused values and multi coded are removed because they are unable to tell anything about the level of influence the employee has over that aspect of their work
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Flexitime != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Flexitime!= "Multi-coded")

WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Jobsharing != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Jobsharing != "Multi-coded")


WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Reduced_hours != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Reduced_hours != "Multi-coded")


WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Compressed_hours != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Compressed_hours != "Multi-coded")


WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Remote_work != "Refusal")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Remote_work != "Multi-coded")

WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Annualized_work != "Refusal")

###assessing variable proportions
Flexitime <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Flexitime, fill = Flexitime)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Flexitime

Jobsharing <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Jobsharing, fill = Jobsharing)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Jobsharing

Reduced_hours <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Reduced_hours, fill = Reduced_hours)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Reduced_hours

Compressed_hours <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Compressed_hours, fill = Compressed_hours)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Compressed_hours

Remote_work <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Remote_work, fill = Remote_work)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Remote_work

Annualized_work <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Annualized_work, fill = Annualized_work)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
Annualized_work


## recode to binary
### Flexitime 
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Flexitime = ifelse( Flexitime =="available to me but i do not use" | Flexitime =="i have used this arrangement",
                                "FWA available",Flexitime))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Flexitime = ifelse( Flexitime =="Don't know" | Flexitime =="not available to me",
                              "FWA unavailable",Flexitime))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Flexitime = ifelse( Flexitime =="FWA available", 1 , 0))
###

### Jobsharing 
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Jobsharing = ifelse( Jobsharing =="available to me but i do not use" | Jobsharing =="i have used this arrangement",
                              "FWA available",Jobsharing))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Jobsharing = ifelse( Jobsharing =="Don't know" | Jobsharing =="not available to me",
                              "FWA unavailable",Jobsharing))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Jobsharing = ifelse( Jobsharing =="FWA available", 1 , 0))
###

### Reduced_hours 
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Reduced_hours = ifelse( Reduced_hours =="available to me but i do not use" | Reduced_hours =="i have used this arrangement",
                               "FWA available",Reduced_hours))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Reduced_hours = ifelse( Reduced_hours =="Don't know" | Reduced_hours =="not available to me",
                               "FWA unavailable",Reduced_hours))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Reduced_hours = ifelse( Reduced_hours =="FWA available", 1 , 0))
###

### Compressed_hours 
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Compressed_hours = ifelse( Compressed_hours =="available to me but i do not use" | Compressed_hours =="i have used this arrangement",
                                  "FWA available",Compressed_hours))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Compressed_hours = ifelse( Compressed_hours =="Don't know" | Compressed_hours =="not available to me",
                                  "FWA unavailable",Compressed_hours))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Compressed_hours = ifelse( Compressed_hours =="FWA available", 1 , 0))
###

### Remote_work
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Remote_work = ifelse( Remote_work =="available to me but i do not use" | Remote_work =="i have used this arrangement",
                                     "FWA available",Remote_work))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Remote_work = ifelse( Remote_work =="Don't know" | Remote_work =="not available to me",
                                     "FWA unavailable",Remote_work))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Remote_work = ifelse( Remote_work =="FWA available", 1 , 0))
###

### Annualized_work
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Annualized_work = ifelse( Annualized_work =="available to me but i do not use" | Annualized_work =="i have used this arrangement",
                                "FWA available",Remote_work))

WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Annualized_work = ifelse( Annualized_work =="Don't know" | Annualized_work =="not available to me",
                                "FWA unavailable",Annualized_work))


WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( Annualized_work = ifelse( Annualized_work =="FWA available", 1 , 0))
###


##aggregate responses for descriptive stats 
WERS_2011_Employee_Gender_flexibility_Data <-WERS_2011_Employee_Gender_flexibility_Data %>%
  mutate( FWA_availabilty = ifelse( Flexitime == 1 | Jobsharing == 1 | Reduced_hours == 1| Compressed_hours == 1 |
                                      Remote_work == 1| Annualized_work == 1 , 1, 0))
###treat as binary variable
#1003 variables removed 
###############################################################################################################################################
##################################################################################################################################################
#################################################################################################################################################



################################################################# Data restriction for Outcome Variable #############################################################
##################################################################################################################################################
#################################################################################################################################################

##### Gross Salary ######
#check initial response levels in variable 
table(WERS_2011_Employee_Gender_flexibility_Data$Gross_Salary, useNA = "always")

#WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Gross_Salary != "Item not applicable")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Gross_Salary != "Multi-coded")
WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% filter(Gross_Salary != "Refusal")

#Average weekly salary estimate


WERS_2011_Employee_Gender_flexibility_Data <- WERS_2011_Employee_Gender_flexibility_Data %>% 
  mutate(Average_yearly_salary_estimate = ifelse(Gross_Salary == "0 or less per week (,120 or less per year)", 120, 
                                                ifelse(Gross_Salary == "1 - 00 per week (,121 - ,200 per year)",(121+200)/2,
                                                       ifelse(Gross_Salary == "01 - 30 per week (,201 - ,760 per year)",(201 + 760)/2,
                                                              ifelse(Gross_Salary == "31 - 70 per week (,761 - ,840 per year)",(761 + 840)/2, 
                                                                     ifelse(Gross_Salary == "71 - 20 per week (,841 - 1,440 per year)", (841 + 1440)/2, 
                                                                            ifelse(Gross_Salary == "21 - 60 per week (1,441 - 3,520 per year)",(1441+3520)/2, 
                                                                                   ifelse(Gross_Salary == "61 - 10 per week (3,521 - 6,120 per year)", (3521+6120)/2, 
                                                                                          ifelse(Gross_Salary == "11 - 70 per week (6,121 - 9,240 per year)", (6121 + 9240)/2,
                                                                                                 ifelse(Gross_Salary == "71 - 30 per week (9,241 - 2,360 per year)", (2360 + 9241)/2,
                                                                                                        ifelse(Gross_Salary == "31 - 20 per week (2,361 - 7,040 per year)",(2361 + 7040)/2,
                                                                                                               ifelse(Gross_Salary == "21 - 50 per week (7,041 - 3,800 per year)",(3800 + 7041)/2,
                                                                                                                      ifelse(Gross_Salary == "51 - 20 per week (3,801 - 2,640 per year)", (2640 + 3801)/2,
                                                                                                                             ifelse(Gross_Salary == "21 - ,050 per week (2,641 - 4,600 per year)",(2641+4600)/2,4601))))))))))))))

summary(WERS_2011_Employee_Gender_flexibility_Data$Average_yearly_salary_estimate)

#box plot to check for pay spread
Pay_b <-ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes(x= Average_yearly_salary_estimate))+   
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + coord_flip() +
  labs(title = "Pay spread",
       x = "Pay", y = "Count") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )
Pay_b

#214 observations removed
##############################################################################################################################################################################
##############################################################################################################################################################################
####################################################################### Descriptive Statistics ####################################################################

#hours_worked_weekly
Hours_worked_weeklydesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Hours_worked_weekly),
                                                 minimum = min(Hours_worked_weekly), maximum =max(Hours_worked_weekly),
                                                 S.D_ = sd(Hours_worked_weekly))
Hours_worked_weeklydesc

#Gender
Genderdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Gender_binary),
                                                                                     minimum = min(Gender_binary), maximum =max(Gender_binary),
                                                                                     S.D_ = sd(Gender_binary))
Genderdesc

#Marital_Status
Marital_Statusdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Marital_Stats),
                                                                        minimum = min(Marital_Stats), maximum =max(Marital_Stats),
                                                                        S.D_ = sd(Marital_Stats))
Marital_Statusdesc

#Caregiver
Caregiverdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Caregiver),
                                                                                minimum = min(Caregiver), maximum =max(Caregiver),
                                                                                S.D_ = sd(Caregiver))
Caregiverdesc

#Education
Educationdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Education_binary),
                                                                           minimum = min(Education_binary), maximum =max(Education_binary),
                                                                           S.D_ = sd(Education_binary))
Educationdesc

#No_dependent_Children
No_dependent_childdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(No_dependent_child),
                                                                                minimum = min(No_dependent_child), maximum =max(No_dependent_child),
                                                                                S.D_ = sd(No_dependent_child))
No_dependent_childdesc

#influence_over_task_performed
influence_over_task_perfomeddesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(influence_over_task_perfomed),
                                                                                    minimum = min(influence_over_task_perfomed), maximum =max(influence_over_task_perfomed),
                                                                                    S.D_ = sd(influence_over_task_perfomed))
influence_over_task_perfomeddesc

#influnce_over_workpace
influence_over_workpacedesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(influence_over_workpace),
                                                                                              minimum = min(influence_over_workpace), maximum =max(influence_over_workpace),
                                                                                              S.D_ = sd(influence_over_workpace))
influence_over_workpacedesc

#influence_over_working_methods
influence_over_working_methodsdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(influence_over_working_methods),
                                                                                         minimum = min(influence_over_working_methods), maximum =max(influence_over_working_methods),
                                                                                         S.D_ = sd(influence_over_working_methods))
influence_over_working_methodsdesc

#influence_over_order_of_tasks
influence_over_order_of_tasksdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(influence_over_order_of_tasks),
                                                                                            minimum = min(influence_over_order_of_tasks), maximum =max(influence_over_order_of_tasks),
                                                                                            S.D_ = sd(influence_over_order_of_tasks))
influence_over_order_of_tasksdesc

#influence_over_start_end_of_workday
influence_over_start_end_of_workdaydesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(influence_over_start_end_of_workday),
                                                                                           minimum = min(influence_over_start_end_of_workday), maximum =max(influence_over_start_end_of_workday),
                                                                                           S.D_ = sd(influence_over_start_end_of_workday))

influence_over_start_end_of_workdaydesc


#Flexitime
Flexitimedesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Flexitime),
                                                                                                     minimum = min(Flexitime), maximum =max(Flexitime),
                                                                                                     S.D_ = sd(Flexitime))

Flexitimedesc


#Jobsharing
Jobsharingdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Jobsharing),
                                                                           minimum = min(Jobsharing), maximum =max(Jobsharing),
                                                                           S.D_ = sd(Jobsharing))

Jobsharingdesc

#Reduced_hours
Reduced_hoursdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Reduced_hours),
                                                                            minimum = min(Reduced_hours), maximum =max(Reduced_hours),
                                                                            S.D_ = sd(Reduced_hours))
Reduced_hoursdesc

#Compressed_hours
Compressed_hoursdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Compressed_hours),
                                                                               minimum = min(Compressed_hours), maximum =max(Compressed_hours),
                                                                               S.D_ = sd(Compressed_hours))
Compressed_hoursdesc


#Remote_work
Remote_workdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Remote_work),
                                                                                  minimum = min(Remote_work), maximum =max(Remote_work),
                                                                                  S.D_ = sd(Remote_work))
Remote_workdesc


#Annualized_work
Annualized_workdesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Annualized_work),
                                                                             minimum = min(Annualized_work), maximum =max(Annualized_work),
                                                                             S.D_ = sd(Annualized_work))
Annualized_workdesc


#FWA_availabilty
FWA_availabiltydesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(FWA_availabilty),
                                                                                      minimum = min(FWA_availabilty), maximum =max(FWA_availabilty),
                                                                                      S.D_ = sd(FWA_availabilty))
FWA_availabiltydesc

#Average_yearly_salary_estimate
Average_yearly_salary_estimatedesc <- WERS_2011_Employee_Gender_flexibility_Data %>%  summarise(average = mean(Average_yearly_salary_estimate),
                                                                                 minimum = min(Average_yearly_salary_estimate), maximum =max(Average_yearly_salary_estimate),
                                                                                 S.D_ = sd(Average_yearly_salary_estimate))

Average_yearly_salary_estimatedesc
################################################################# Demographic Statistics #############################################################
##################################################################################################################################################
#################################################################################################################################################

#gender demographic 
Genderdemo <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Gender, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  labs(title = "Demographic breakdown by Gender",
       x = "Gender", y = "Employee Count") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))

Genderdemo


#Age demographic
Agedemo <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Age, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  labs(title = "Demographic breakdown by Age",
       x = "Age", y = "Employee Count") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))

Agedemo


#marital status demographic 
maritalstatdemo <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Marital_Status, fill=Gender )) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  labs(title = "Demographic breakdown by Marital Status",
       x = "Marital Status", y = "Employee Count") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
maritalstatdemo


#Dependent children
depchild <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = No_dependent_child, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") + 
  labs(title = "Demographic breakdown by Dependent Children",
       x = "0- No Dependent Child / 1-Dependent Child ", y = "Count") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))
depchild


#Ethnicity Demographic 
ethnicitydemo <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Ethnicity, fill= Gender)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") +
  coord_flip()+
  labs(title = "Demographic breakdown by Ethnicity",
       x = "Ethnicity", y = "Employee Count") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))

ethnicitydemo

#Education demographic
educationdemo <- ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes( x = Education, fill= Education)) +
  geom_bar(stat = "count", position = "dodge" , color = "black") +
  #scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  coord_flip()+
  labs(title = "Demographic breakdown by Education",
       x = "Education", y = "Employee Count") + 
  theme(plot.title = element_text(color = "#31356e", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1),panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1))


educationdemo

################################################################# Gauss markov assumptions  #############################################################
##################################################################################################################################################
#################################################################################################################################################

#Normality :use the central limit theorem here, assuming that because the sample size is large the distribution is likely to be normal

### Multicolinearity / correlation between independent variables
#######testing correlation

model_multicolinearity <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ Gender + Marital_Stats + Caregiver +
                               No_dependent_child + Hours_worked_weekly + as.factor(Age) + as.factor(Education) + Jobsharing + Flexitime + Reduced_hours + Compressed_hours + 
                               Remote_work + Annualized_work +   as.factor(Ethnicity) + as.factor(Promotion_hours) + 
              influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks + influence_over_start_end_of_workday)


vif(model_multicolinearity)

################################################################################

########## testing for linearity and homoscedasticity
#linear model
model_for_residuals <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ Gender + Marital_Stats + Caregiver +
                           No_dependent_child + Hours_worked_weekly + as.factor(Age) + as.factor(Education) + Jobsharing + Flexitime + Reduced_hours + Compressed_hours + 
                           Remote_work + Annualized_work +  as.factor(Ethnicity) + as.factor(Promotion_hours) +
                           influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks + influence_over_start_end_of_workday)

summary(model_for_residuals)

model_for_residuals_values <- resid(model_for_residuals)

#residual and fitted plot
ggplot(model_for_residuals) +geom_point(aes(x= fitted(model_for_residuals), y = model_for_residuals_values),color = "black", size = 0.5) +
  geom_smooth(aes(x= fitted(model_for_residuals), y = model_for_residuals_values,),method = lm, se= TRUE, color = "black") + theme_minimal()

#negative linear relation with homoscedasticity present 




############################################################################ Linear Regression ##########################################################################
##############################################################################################################################################################################
#############################################################################################################################################################################


#1.
#visualisng relationship between gender and Average yearly pay
gender_pay <-ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes(x= as.factor(Gender), y = Average_yearly_salary_estimate, fill = Gender) )+   
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + 
  guides(fill = guide_legend(title = "Gender")) +
  scale_fill_manual(values=c("#d64946", "#fad8d5")) + 
  labs(title = "Gender and Average Yearly Pay",
       subtitle = "Visualising the relationship between gender and Average yearly pay",
       x = "Gender", y = "Average Yearly Salary Estimate") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )

gender_pay

#######Generating linear model
model_hypothesis_one_gender <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ Gender + Marital_Stats + Caregiver +
     No_dependent_child + Hours_worked_weekly + as.factor(Age) + as.factor(Education) + as.factor(Ethnicity) + as.factor(Promotion_hours) +
     influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks + influence_over_start_end_of_workday)

summary(model_hypothesis_one_gender)

stargazer(model_hypothesis_one_gender,
          type = "text",
          title= "Assessing the impact of Gender on pay", out = "C:/Users/afari/Desktop/dissertation/Rscript/gender.txt")


################################################################################

#2.
#visualisng relationship between education and Average yearly pay
education_pay <-ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes(x= as.factor(Education), y = Average_yearly_salary_estimate, fill = Education) )+   
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + 
  guides(fill = guide_legend(title = "Education")) +
  #scale_fill_manual(values=c("#d64946", "#fad8d5")) + 
  labs(title = "Education and Average Yearly Pay",
       subtitle = "Visualising the relationship between education and Average yearly pay",
       x = "Education", y = "Average Yearly Salary Estimate") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )



education_pay

education_pay_gender <-ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes(x= as.factor(Education), y = Average_yearly_salary_estimate, fill = Gender) )+   
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + 
  guides(fill = guide_legend(title = "Education")) +
  #scale_fill_manual(values=c("#d64946", "#fad8d5")) + 
  labs(title = "Education and Average Yearly Pay",
       subtitle = "Visualising the relationship between education, gender and Average yearly pay",
       x = "Education", y = "Average Yearly Salary Estimate") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )


education_pay_gender

#######Generating linear model
model_hypothesis_two_education <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ Marital_Stats + Caregiver +
          No_dependent_child + Hours_worked_weekly + as.factor(Age)  + 
          as.factor(Ethnicity) + as.factor(Promotion_hours) +
          influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + 
          influence_over_order_of_tasks + influence_over_start_end_of_workday + Gender + as.factor(Education))

summary(model_hypothesis_two_education)

# moderated by Gender
model_hypothesis_two_education <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ Marital_Stats + Caregiver +
                                       No_dependent_child + Hours_worked_weekly + as.factor(Age)  + 
                                       as.factor(Ethnicity) + as.factor(Promotion_hours) +
                                       influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + 
                                       influence_over_order_of_tasks + influence_over_start_end_of_workday + as.factor(Education) * Gender)

summary(model_hypothesis_two_education)


model_hypothesis_two_education_only<- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ Education)
summary(model_hypothesis_two_education_only)


################################################################################


#3
#visualisng relationship between age and Average yearly pay
age_pay <-ggplot(data = WERS_2011_Employee_Gender_flexibility_Data, aes(x= as.factor(Age), y = Average_yearly_salary_estimate, fill = Gender) )+   
  geom_boxplot(outlier.colour= "#f4cb78", outlier.shape=8,outlier.size=4) + 
  guides(fill = guide_legend(title = "Age")) +
  #scale_fill_manual(values=c("#d64946", "#fad8d5")) + 
  labs(title = "Age and Average Yearly Pay",
       subtitle = "Visualising the relationship between age and Average yearly pay",
       x = "Age", y = "Average Yearly Salary Estimate") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )


age_pay


#######Generating linear model

#re-ordering to select a different refecence category
WERS_2011_Employee_Gender_flexibility_Data$Age <- factor(WERS_2011_Employee_Gender_flexibility_Data$Age, levels=c("30-39", "18-19", "20-21", "22-29", "40-49", "50-59", "60-64", "65 and above"))


model_hypothesis_three_age_a <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~  Marital_Stats + Caregiver +
          No_dependent_child + Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + as.factor(Education) +
          influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks +
          influence_over_start_end_of_workday + Gender + as.factor(Age))

summary(model_hypothesis_three_age_a)

model_hypothesis_three_age_b <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~  Marital_Stats + Caregiver +
                                   No_dependent_child + Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + as.factor(Education)+
                                   influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks +
                                   influence_over_start_end_of_workday + Gender * as.factor(Age))

summary(model_hypothesis_three_age_b)



model_hypothesis_three_age_only<- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ as.factor(Age))
summary(model_hypothesis_three_age_only)



################################################################################

#######Generating linear model
model_hypothesis_four_child_a <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~  Marital_Stats + Caregiver +
                                      Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + Gender + as.factor(Education)+
                                    influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks +
                                    influence_over_start_end_of_workday + as.factor(Age) + Gender + No_dependent_child )

summary(model_hypothesis_four_child_a)

model_hypothesis_four_child <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~  Marital_Stats + Caregiver +
                                    Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + Gender + + as.factor(Education)+
          influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks +
          influence_over_start_end_of_workday + as.factor(Age) + Gender * No_dependent_child )

summary(model_hypothesis_four_child)

model_hypothesis_four_child_only<- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~ No_dependent_child)
summary(model_hypothesis_four_child_only)

##############################################################################
#5 Hold

#######Generating linear model
model_hypothesis_five_fwa_a <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~  Marital_Stats + Caregiver +
                                  No_dependent_child + Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + as.factor(Age) + as.factor(Education) +
                                  influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks +
                                  influence_over_start_end_of_workday + Gender + Jobsharing + Flexitime + Reduced_hours + Compressed_hours + Remote_work + Annualized_work 
)

summary(model_hypothesis_five_fwa_a)

stargazer(model_hypothesis_five_fwa_a,
          type = "text",
          title= "Assessing the impact of Gender on pay/FWA", out = "C:/Users/afari/Desktop/dissertation/Rscript/gender.txt")


model_hypothesis_five_fwa_b <- lm(data = WERS_2011_Employee_Gender_flexibility_Data, formula = Average_yearly_salary_estimate ~  Marital_Stats + Caregiver +
          No_dependent_child + Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + as.factor(Age) + as.factor(Education) +
          influence_over_task_perfomed + influence_over_workpace + influence_over_working_methods + influence_over_order_of_tasks +
          influence_over_start_end_of_workday + Gender * (Jobsharing + Flexitime + Reduced_hours + Compressed_hours + Remote_work + Annualized_work)
          )

summary(model_hypothesis_five_fwa_b)





#################################################################################################################################
#################################################################################################################


#################################################################################################################

#####Lasso

#ensures data split is reproducible
set.seed(1357)

#80% of the data 
index<-createDataPartition(WERS_2011_Employee_Gender_flexibility_Data$Average_yearly_salary_estimate,
                           p = .8, list = FALSE, times = 1)

#training data 
training_data <- WERS_2011_Employee_Gender_flexibility_Data[index, ]

#test data
test_data <- WERS_2011_Employee_Gender_flexibility_Data[-index, ]


#K-fold cross validation 
cross_val <- trainControl(method = "cv", number = 14, savePredictions = "all")

#create lambda values 
lambda_values <- 10^seq(5, -5, length = 500)

set.seed(1357)

###assess how fwa impact r^2 and rmse as predicted by lasso 

table(WERS_2011_Employee_Gender_flexibility_Data$Age)

# training model without fwa
lasso_train_model <- train(Average_yearly_salary_estimate ~ Marital_Stats + Caregiver +
                             No_dependent_child + Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + as.factor(Education) +
                             as.factor(Age) + influence_over_task_perfomed + influence_over_workpace + 
                             influence_over_working_methods + influence_over_order_of_tasks +
                             influence_over_start_end_of_workday + Gender,
                           data = training_data,
                           preProcess=c("center","scale"),
                           method = "glmnet",
                           tuneGrid = expand.grid(alpha = 1, lambda = lambda_values),
                           trControl = cross_val)


#best lambda value (best value at which the lowest RMSE is obtained)
optimal_lambda <- lasso_train_model$bestTune

#estimated coefficients 
optimal_coeff <- round(coef(lasso_train_model$finalModel, lasso_train_model$bestTune$lambda), 2)

# key variables 
important_vars <- varImp(lasso_train_model)

#graph of key variables
important_variables <-ggplot(varImp(lasso_train_model)) +
  labs(title = "Lasso - Important Variables",
       subtitle = "Visualising the important variables",
       x = "Variables" , y = "Level of Importance") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
        panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )

important_variables
####################################################################################

set.seed(1357)

#training model with fwa

lasso_train_model_fwa <- train(Average_yearly_salary_estimate ~ Marital_Stats + Caregiver +
                             No_dependent_child + Hours_worked_weekly +  as.factor(Ethnicity) + as.factor(Promotion_hours) + as.factor(Education) +
                             as.factor(Age) + influence_over_task_perfomed + influence_over_workpace + 
                             influence_over_working_methods + influence_over_order_of_tasks +
                             influence_over_start_end_of_workday + Gender + Jobsharing + Reduced_hours + 
                             Compressed_hours + Remote_work + Annualized_work,
                           data = training_data,
                           preProcess=c("center","scale"),
                           method = "glmnet",
                           tuneGrid = expand.grid(alpha = 1, lambda = lambda_values),
                           trControl = cross_val)

#best lambda value (best value at which the lowest RMSE is obtained)
optimal_lambda <- lasso_train_model_fwa$bestTune

#estimated coefficients 
optimal_coeff <- round(coef(lasso_train_model_fwa$finalModel, lasso_train_model_fwa$bestTune$lambda), 2)

# key variables 
important_vars <- varImp(lasso_train_model_fwa)

#graph of key variables
important_variables_fwa <-ggplot(varImp(lasso_train_model_fwa)) +
  labs(title = "Lasso - Important Variables",
       subtitle = "Visualising the important variables",
       x = "Variables" , y = "Level of Importance") + 
  theme(plot.title = element_text(color = "#d64946", size = 20, face = "bold", hjust = 0.5),
                      plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), 
                      panel.background = element_rect(fill = "white"),
                      panel.grid.major = element_line(color = "grey", size = 0.2, linetype = 1), 
                      panel.grid.minor = element_line(color = "grey", size = 0.025,linetype = 1) )

  
important_variables_fwa

#################################################################################

#testing model prediction capabilities without fwa
lasso_test_model <- predict(lasso_train_model, newdata = test_data)

#calculate the RMSE for the test data set prediciton model without FWA 
lasso_test_model_RMSE <- RMSE(lasso_test_model, test_data$Average_yearly_salary_estimate)

#calculate the R^2 the test data set prediciton model without FWA 
lasso_test_model_Rsq <- R2(lasso_test_model, test_data$Average_yearly_salary_estimate)

###############################################################################

#testing model prediction capabilities without fwa
lasso_test_model_FWA <- predict(lasso_train_model_fwa, newdata = test_data)

#calculate the RMSE for the test data set prediciton model without FWA 
lasso_test_model_RMSE_FWA <- RMSE(lasso_test_model_FWA, test_data$Average_yearly_salary_estimate)

#calculate the R^2 the test data set prediciton model without FWA 
lasso_test_model_Rsq_FWA <- R2(lasso_test_model_FWA, test_data$Average_yearly_salary_estimate)

################################################################################

#####matrix to compare R^2 and RMSE

Rsq_RMSE <- data.frame(lasso_test_model_RMSE,lasso_test_model_Rsq,lasso_test_model_RMSE_FWA,lasso_test_model_Rsq_FWA)

rnames <- c("No FWA", "FWA")
cnames <- c("RMSE", "R2")

Rsq_RMSE_matrix <- matrix(Rsq_RMSE, nrow = 2, byrow = TRUE, dimnames = list(rnames,cnames))

###################################################################################
