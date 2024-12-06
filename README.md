# Research on Gender pay gap and Equal pay of a Firm using R statistical package
This project analyzes the gender pay gap and equal pay within a UK firm, using statistical methods in R to uncover disparities in salaries and bonuses. Key findings include:

Gender Pay Gap in Salaries:

- Men earn higher average and median salaries than women.
- Contractual working hours show no significant pay differences, but men are more likely to work full-time, while women are more likely to work part-time.
- Recruitment is male-dominated, contributing to a gender imbalance.

Gender Pay Gap in Bonuses:

- Men receive significantly higher average and median bonuses than women.
- Even with similar contract hours, men are paid higher bonuses than women.

Equal Pay:
Men earn higher salaries on average, regardless of tenure or position.

#### File containing full details of the report and analysis is added in the repsitory.

## R codes used for ETL, EDA, Data Visualisation and Statistical Analysis
```R
library(readxl) 
library(ggplot2)
library(dplyr)  
library(broom)  
library(skimr)
library(RColorBrewer)

## set the working directory 
setwd("C:/Users/HP/Documents/Portfolio")

## Importing data into R 
firm <- read_excel("firm_05 (1).xlsx")

## Data Inspection 
str(firm)
summary(firm)
skim(firm)

table(firm$gender,useNA = "ifany")
table(firm$qualification,useNA = "ifany")
table(firm$position,useNA = "ifany")
table(firm$gender,firm$position)
table(firm$gender,firm$qualification)


## data restriction and data preparation 
firm<- firm %>% filter( position !=".")
firm<- firm %>% filter( qualification !=".")
firm$gender <- recode_factor(firm$gender,'Female' = "female")
table(firm$gender,useNA = "ifany")


## replacing  missing values with 0 
firm[is.na(firm)] <- 0
firm %>% filter(!complete.cases(.))

View(firm)

##  data manipulation / data transformation 
## changing tenure from months to years
firm <- firm %>% mutate (tenure = tenure/12)

## changing FTE from decimal to percentage of full time working hours #
firm <- firm %>% mutate(FTE = FTE*100) 

## creating a new variable 
firm <-  firm %>% mutate(contract_hour = FTE/100*40)
firm <- firm %>% mutate(contract = if_else(FTE == 100,"full_time","part_time"))
table(firm$FTE,firm$contract)
table(firm$gender,firm$contract)
View(firm)

## changing variable type 
firm$gender <- as.factor(firm$gender)
firm$qualification <- as.factor(firm$qualification)
firm$position <- as.factor(firm$position)
firm$contract <- as.factor(firm$contract)


## descriptive statistics grouped by gender 
firm %>% group_by(gender) %>% 
  summarise (Lower = min(salary),
          Average = mean(salary),
          Upper = max(salary),
          middle = median(salary),
          st_dv = sd(salary),
          quatiles = quantile(salary),
          Range = max(salary)-min(salary)) %>% 
  arrange(Average) %>% 
   View()

firm %>% group_by(gender) %>% 
  summarise (Lower = min(bonus),
             Average = mean(bonus),
             Upper = max(bonus),
             middle = median(bonus),
             st_dv = sd(bonus),
            quatiles = quantile(bonus),
            Range = max(bonus)-min(bonus)) %>% 
  arrange(Average) %>% 
  View()
  

## Data visualisation 
fig1 <- firm %>% ggplot(aes(x = salary, fill = gender)) + 
        geom_histogram(binwidth = 15,position = "identity",alpha=0.5) + 
        theme(legend.title = element_blank()) +
        ggtitle(label = "Gender pay distribution", subtitle = "by salary")+
        scale_fill_manual(values = c("red", "blue"))
fig1

fig2 <- firm %>% ggplot(aes(contract_hour,salary,fill = gender)) +
  geom_bar(position = "fill", stat = "summary", fun = "mean",alpha =0.5)+
  theme_bw()+ theme(panel.grid.major = element_blank())+
  scale_fill_manual(values = c("red", "blue")) +
  labs(y = "proportion salary pay", x = "weekly working hours")+ 
  ggtitle(label= "proportion of gender pay gap", subtitle = "for salary pay")
fig2

fig3<- firm %>% ggplot (aes(gender,salary,fill= gender)) +
  geom_boxplot(width = 0.5,alpha = 0.5,position = "identity")+
  stat_boxplot(geom ='errorbar', width = 0.5)+
  theme_bw()+ theme(panel.grid.major = element_blank())+
  scale_fill_manual(values = c("red", "blue"))+
  labs(y = "Monthly salary", x = "Gender")+
  ggtitle (label = "Average Gender pay gap", subtitle = "for monthly pay")
fig3

fig4 <- firm %>% ggplot(aes(contract_hour,salary, fill = gender)) +
  geom_bar(position = "dodge", stat = "summary", fun = "median", alpha = 0.5)+
  theme_bw()+ theme(panel.grid.major = element_blank())+
  scale_fill_manual(values = c("red", "blue"))+ 
  labs(y = "salary", x = "contractual work time", 
  title = "Median gender pay gap for monthly pay")
fig4

fig5<- firm %>% ggplot(aes(contract_hour,bonus,fill = gender))+geom_bar(position = "fill", stat = "summary", fun = "mean",alpha =0.5)+
  theme_bw()+ theme(panel.grid.major = element_blank())+
  scale_fill_manual(values = c("red", "blue")) +
  labs(y = "proportion bonus pay", x = "weekly working hours")+ 
  ggtitle(label= "proportion of gender pay gap", subtitle = "for bonus pay")
fig5

fig6 <- firm %>% ggplot(aes(gender,bonus,fill = gender)) + 
  geom_bar(position = "identity", stat = "summary", fun = "mean",alpha=0.5) +
  theme(legend.position = "right", legend.title = element_blank()) +
  labs(y = "yearly bonus in £", x = "Gender")+
  ggtitle(label = "Average Gender pay gap", subtitle = "for bonus pay")+
  scale_fill_manual(values = c("red", "blue"))
fig6

fig7 <- firm %>% ggplot(aes(contract_hour,bonus, fill = gender)) + geom_bar(position = "dodge", stat = "summary", fun = "median", alpha = 0.5)+
  theme_bw()+theme(panel.grid.major = element_blank())+
  scale_fill_manual(values = c("red", "blue"))+labs(y = "bonus pay in £", x = "contractual work time", 
       title = "Median gender pay gap for bonus pay")
fig7

fig8 <- firm %>% ggplot(aes(position,salary,fill=factor(gender))) +
  geom_boxplot(width = 0.5,alpha = 0.5,position = "dodge")+
  stat_boxplot(geom ='errorbar', width = 0.5)+
  theme_bw()+ theme(panel.grid.major = element_blank())+
  scale_fill_manual(values = c("red", "blue"))+
  ggtitle(label = "Gender pay gap quarter", subtitle = "by position")
fig8

fig9 <-firm %>% ggplot(aes(tenure,salary,colour = gender)) + 
  geom_point(size= 2,alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  facet_wrap(~ contract)+
  labs(x = "tenure(years)", y ="monthly salary", title = "Gender pay gap",
  subtitle = "by tenure and contractual working time")+
  theme_bw()+ theme(panel.grid.major = element_blank())
fig9


## Regression Analysis 
## Statistical Analysis of Equal pay 
regression1 <- lm(salary ~ gender,data  = firm)
summary(regression1)

regression2 <- lm(salary ~ gender + contract + tenure + position + qualification,data = firm)
summary(regression2)

reg1_pred <- augment(regression1, firm)

reg2_pred <- augment(regression2, firm)

fig10 <- ggplot(reg2_pred, aes(x = salary, y = .fitted, colour= gender)) +
  geom_point(size= 2,alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +   
  labs(title = "Gender Equal pay by monthly salary ") +
  theme(legend.position = "bottom", legend.title = element_blank(), panel.background = element_blank()) + 
  ylab("predicted gender pay") + xlab("Monthly salary") + 
  scale_y_continuous()
fig10```



