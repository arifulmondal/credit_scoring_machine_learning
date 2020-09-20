---
title: "Modeling Credit Scoring / Credit Rating / Consumer Risk"
author: "Ariful Mondal"
date: "20 September 2016"
output:
  word_document:
    toc: yes
  pdf_document:
    toc: yes
  html_document:
    fig_height: 8
    fig_width: 11
    toc: yes
    toc_float: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(warn=-1, width = 200, strip.white=TRUE, cache=TRUE)
```
# 1. Introduction

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

## 1.1 Setting up the environment and packages

```{r}
# Set working directory
setwd("C:/creditscoring")
# For error handling
x <- tryCatch(simpleError("eror mesid"), error = function(e) e)
```

```{r, echo = TRUE}

# List of required libraries - packages
library(lattice) # for visualization
library(knitr)  # for kable
library(gplots) # for plot
library(ggplot2) # for data visualization
library(ClustOfVar) # for variable clustering
library(ape) # for as.phylo
library(Information)
library(ROCR) # for ROC
library(caret) 
library(rpart) # for Traditional recursive Partitioning - Bayesian
library(rpart.utils)
library(rpart.plot)
library(randomForest) # for random forest
library(party) # Conditional inference Trees
library(bnlearn) # Bayesian Network
library(DAAG) #load Data Analysis And Graphics Package for R (DAAG)
library(vcd) #the `vcd' package is required for CD plots
#library(neuralnet) # Neural Network - call when you run neural network _ predition function will be different.
library(kernlab) # SVM
```

## 1.2 Reading raw data into R

```{r, echo = TRUE, cache=TRUE}

# Read data into R (tab delimitted)
cdata<-read.table("data.txt", h=T, sep="")
cdatanum<-read.table("german.data-numeric.txt", h=F, sep="") # Numeric data for Neural network

# Col names

# chk_ac_status_1	duration_month_2	credit_history_3	purpose_4	credit_amount_5	savings_ac_bond_6	p_employment_since_7	installment_pct_disp_inc_8	personal_status_9	other_debtors_or_grantors_10	present_residence_since_11	property_type_12	age_in_yrs_13	other_installment_type_14	housing_type_15	number_cards_this_bank_16	job_17	no_people_liable_for_mntnance_18	telephone_19	foreign_worker_20 good_bad_21

colnames(cdata)

y <- c(0,1) # for abline
x <- c(0,1) # for abline
```

## 1.3  Get first hand feeling of the data


```{r, echo = TRUE, cache=TRUE}

# Get first hand feeling of the data
str(cdata)
summary(cdata)

# print few observations
kable(head(cdata), format="pandoc", padding=0, caption="How may the data look like?")

# convert integers to numeric
cdata$duration_month_2  <- as.numeric(cdata$duration_month_2)             
cdata$credit_amount_5   <-  as.numeric(cdata$credit_amount_5 )            
cdata$installment_pct_disp_inc_8 <-  as.numeric(cdata$installment_pct_disp_inc_8)     
cdata$present_residence_since_11 <-  as.numeric(cdata$present_residence_since_11)     
cdata$age_in_yrs_13        <-  as.numeric(cdata$age_in_yrs_13)           
cdata$number_cards_this_bank_16    <-  as.numeric(cdata$number_cards_this_bank_16)   
cdata$no_people_liable_for_mntnance_18 <-  as.numeric(cdata$no_people_liable_for_mntnance_18)
```

***

# 2.Data analysis and variable creation

## 2.0 Create your own functions for analysis and modeling
```{r, echo = TRUE, cache=TRUE}

# Function 1: Create function to calculate percent distribution for factors
pct <- function(x){
  tbl <- table(x)
  tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(tbl_pct) <- c('Count','Percentage')
  kable(tbl_pct)
}

#pct(cdata$good_bad_21)


# Function 2: to calculate bad rates by groups - IV, WOE and Eefficiency 
gbpct <- function(x){
  mt <- as.matrix(table(as.factor(x), as.factor(cdata$good_bad_21)))
  Total <- mt[,1] + mt[,2]
  Total_Pct <- round(Total/sum(mt)*100, 2)
  Bad_pct <- round((mt[,1]/sum(mt[,1]))*100, 2)
  Good_pct <- round((mt[,2]/sum(mt[,2]))*100, 2)
  Bad_Rate <- round((mt[,1]/(mt[,1]+mt[,2]))*100, 2)
  grp_score <- round((Good_pct/(Good_pct + Bad_pct))*10, 2)
  WOE <- round(log(Good_pct/Bad_pct)*10, 2)
  g_b_comp <- ifelse(mt[,1] == mt[,2], 0, 1)
  IV <- ifelse(g_b_comp == 0, 0, (Good_pct - Bad_pct)*(WOE/10))
  Efficiency <- abs(Good_pct - Bad_pct)/2
  otb<-as.data.frame(cbind(mt, Good_pct,  Bad_pct,  Total, 
                           Total_Pct,  Bad_Rate, grp_score, 
                           WOE, IV, Efficiency ))
  otb$Names <- rownames(otb)
  rownames(otb) <- NULL
  otb[,c(12,2,1,3:11)]
}


# Function 3: Normalize using Range

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


```

***

## 2.1 Good-Bad and understand relationships between variables:

### 2.1.1 Analyse good_bad(1-good, 2-bad)

```{r, echo = TRUE, cache=TRUE}

cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 1, "Good", "Bad"))
pct(cdata$good_bad_21)

op<-par(mfrow=c(1,2), new=TRUE)

plot(as.numeric(cdata$good_bad_21), ylab="Good-Bad", xlab="n", main="Good ~ Bad")
hist(as.numeric(cdata$good_bad_21), breaks=2, 
     xlab="Good(1) and Bad(2)", col="blue", 
     main="Good Bad Distribution")

par(op)

```

***


## 2.2 Detail Analysis of variables and variable reduction:


### 2.2.1 Checking account status

```{r, echo = TRUE, cache=TRUE}

# Attribute 1:  (qualitative)
#-----------------------------------------------------------
# Checking account status

# 	       Status of existing checking account
#                A11 :      ... <    0 DM
# 	       A12 : 0 <= ... <  200 DM
# 	       A13 :      ... >= 200 DM /
# 		     salary assignments for at least 1 year
#                A14 : no checking account


A1 <- gbpct(cdata$chk_ac_status_1)

op1<-par(mfrow=c(1,2), new=TRUE)

plot(cdata$chk_ac_status_1, cdata$good_bad_21, 
     ylab="Good-Bad", xlab="category", 
     main="Checking Account Status ~ Good-Bad ")

barplot(A1$WOE, col="brown", names.arg=c(A1$Levels), 
        main="Score:Checking Account Status",
        xlab="Category",
        ylab="WOE"
)


par(op1)

kable(A1, caption = 'Checking Account Status ~ Good-Bad')
```

<font color="blue"> Information Value is  <b> `r round(sum(A1$IV),2)` </b> and Efficiency is <b> `r round(sum(A1$Efficiency),2)` </b>. </font>

***

### 2.2.2 Loan Duration

```{r, echo = TRUE}

# Attribute 2:  (numerical)
#-----------------------------------------------------------
# Loan Duration (Tenure) in Month

summary(cdata$duration_month_2)

op2<-par(mfrow=c(1,2))
boxplot(cdata$duration_month_2, ylab="Loan Duration(Month)", main="Boxplot:Loan Duration")

plot(cdata$duration_month_2, cdata$good_bad_21, 
     ylab="Good-Bad", xlab="Loan Duration(Month)",
     main="Loan Duration ~ Good-Bad ")

plot(as.factor(cdata$duration_month_2), cdata$good_bad_21, 
     ylab="Good-Bad", xlab="Loan Duration(Month)",
     main="Loan Duration(Before Groupping)")


cdata$duration_month_2 <-as.factor(ifelse(cdata$duration_month_2<=6,'00-06',
                                          ifelse(cdata$duration_month_2<=12,'06-12',
                                                 ifelse(cdata$duration_month_2<=24,'12-24', 
                                                        ifelse(cdata$duration_month_2<=30,'24-30',
                                                               ifelse(cdata$duration_month_2<=36,'30-36',
                                                                      ifelse(cdata$duration_month_2<=42,'36-42','42+')))))))
                                                                                  
                                                                           


plot(cdata$duration_month_2, cdata$good_bad_21,
      main="Loan Duration(after grouping) ",
     xlab="Loan Duration (Month)",
     ylab="Good-Bad")
par(op2)

A2<-gbpct(cdata$duration_month_2)

barplot(A2$WOE, col="brown", names.arg=c(A2$Levels),
        main="Loan Duration",
        xlab="Duration(Months)",
        ylab="WOE"
)

kable(A2, caption = 'Loan Duration ~ Good-Bad')
```

<font color="blue"> Information Value is  <b> `r round(sum(A2$IV),2)` </b> and Efficiency is <b> `r round(sum(A2$Efficiency),2)` </b>. </font>

***

### 2.2.3 Credit History

```{r, echo = TRUE}
# Attribute 3:  (qualitative)
#-----------------------------------------------------------
# Credit History

# 	      A30 : no credits taken/
# 		    all credits paid back duly
#               A31 : all credits at this bank paid back duly
# 	      A32 : existing credits paid back duly till now
#               A33 : delay in paying off in the past
# 	      A34 : critical account/
# 		    other credits existing (not at this bank)

cdata$credit_history_3<-as.factor(ifelse(cdata$credit_history_3 == "A30", "01.A30",
                                         ifelse(cdata$credit_history_3 == "A31","02.A31",
                                                ifelse(cdata$credit_history_3 == "A32","03.A32.A33",
                                                       ifelse(cdata$credit_history_3 == "A33","03.A32.A33",
                                                              "04.A34")))))

op3<-par(mfrow=c(1,2))

plot(cdata$credit_history_3, cdata$good_bad_21, 
      main = "Credit History ~ Good-Bad",
     xlab = "Credit History",
     ylab = "Good-Bad")

plot(cdata$credit_history_3, cdata$good_bad_21, 
     main = "Credit History(After Groupping) ~ Good-Bad ",
      xlab = "Credit History",
     ylab = "Good-Bad")


par(op3)

A3<-gbpct(cdata$credit_history_3)

barplot(A3$WOE, col="brown", names.arg=c(A3$Levels),
        main="Credit History",
        xlab="Credit History",
        ylab="WOE"
)

kable(A3, caption = "Credit History~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A3$IV),2)` </b> and Efficiency is <b> `r round(sum(A3$Efficiency),2)` </b>. </font>

***

### 2.2.4 Purpose of the loan


```{r, echo = TRUE}
# Attribute 4:  (qualitative)
#-----------------------------------------------------------
# Purpose of the loan

# 
# 	      A40 : car (new)
# 	      A41 : car (used)
# 	      A42 : furniture/equipment
# 	      A43 : radio/television
# 	      A44 : domestic appliances
# 	      A45 : repairs
# 	      A46 : education
# 	      A47 : (vacation - does not exist?)
# 	      A48 : retraining
# 	      A49 : business
# 	      A410 : others


A4<-gbpct(cdata$purpose_4)


op4<-par(mfrow=c(1,2))
plot(cdata$purpose_4, cdata$good_bad_21, 
     main="Purpose of Loan~ Good-Bad ",
     xlab="Purpose",
     ylab="Good-Bad")

barplot(A4$WOE, col="brown", names.arg=c(A4$Levels),
        main="Purpose of Loan",
        xlab="Category",
        ylab="WOE")

par(op4)

kable(A4, caption = "Purpose of Loan~ Good-Bad")
```

<font color="blue"> Information Value is  <b> `r round(sum(A4$IV),2)` </b> and Efficiency is <b> `r round(sum(A4$Efficiency),2)` </b>. </font>

***

### 2.2.5 Credit Amount

```{r, echo = TRUE}

# Attribute 5:  (numerical)
#-----------------------------------------------------------
# Credit (Loan) Amount

cdata$credit_amount_5 <- as.double(cdata$credit_amount_5)
summary(cdata$credit_amount_5)

boxplot(cdata$credit_amount_5)

cdata$credit_amount_5<-as.factor(ifelse(cdata$credit_amount_5<=1400,'0-1400',
                                        ifelse(cdata$credit_amount_5<=2500,'1400-2500',
                                               ifelse(cdata$credit_amount_5<=3500,'2500-3500', 
                                                      ifelse(cdata$credit_amount_5<=4500,'3500-4500',
                                                             ifelse(cdata$credit_amount_5<=5500,'4500-5500','5500+'))))))


A5<-gbpct(cdata$credit_amount_5)



plot(cdata$credit_amount_5, cdata$good_bad_21, 
      main="Credit Ammount (After Grouping) ~ Good-Bad",
      xlab="Amount",
     ylab="Good-Bad")

barplot(A5$WOE, col="brown", names.arg=c(A5$Levels),
        main="Credit Ammount",
        xlab="Amount",
        ylab="WOE")

kable(A5, caption = "Credit Ammount ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A5$IV),2)` </b> and Efficiency is <b> `r round(sum(A5$Efficiency),2)` </b>. </font>

***

### 2.2.6 Savings account/bonds

```{r, echo = TRUE}
# Attibute 6:  (qualitative)
#-----------------------------------------------------------
# Savings account/bonds

# 	      A61 :          ... <  100 DM
# 	      A62 :   100 <= ... <  500 DM
# 	      A63 :   500 <= ... < 1000 DM
# 	      A64 :          .. >= 1000 DM
#               A65 :   unknown/ no savings account

A6<-gbpct(cdata$savings_ac_bond_6)


plot(cdata$savings_ac_bond_6, cdata$good_bad_21, 
     main="Savings account/bonds ~ Good-Bad",
     xlab="Savings/Bonds",
     ylab="Good-Bad")

barplot(A6$WOE, col="brown", names.arg=c(A6$Levels),
        main="Savings account/bonds",
        xlab="Category",
        ylab="WOE")

kable(A6, caption = "Savings account/bonds ~ Good-Bad" )

```

<font color="blue"> Information Value is  <b> `r round(sum(A6$IV),2)` </b> and Efficiency is <b> `r round(sum(A6$Efficiency),2)` </b>. </font>

***

### 2.2.7 Present employment since

```{r, echo = TRUE}

# Attribute 7:  (qualitative)
#-----------------------------------------------------------
# Present employment since

# A71 : unemployed
# A72 :       ... < 1 year
# A73 : 1  <= ... < 4 years
# A74 : 4  <= ... < 7 years
# A75 :       .. >= 7 years

A7<-gbpct(cdata$p_employment_since_7)

op7<-par(mfrow=c(1,2))
plot(cdata$p_employment_since_7, cdata$good_bad_21,
     main="Present employment since ~ Good-Bad",
      xlab="Employment in Years",
     ylab="Good-Bad")

barplot(A7$WOE, col="brown", names.arg=c(A7$Levels),
        main="Present employment",
        xlab="Category",
        ylab="WOE")
par(op7)

kable(A7, caption ="Present employment since ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A7$IV),2)` </b> and Efficiency is <b> `r round(sum(A7$Efficiency),2)` </b>. </font>

***

### 2.2.8 Installment rate in percentage of disposable income

```{r, echo = TRUE}

# Attribute 8:  (numerical)
#-----------------------------------------------------------
# Installment rate in percentage of disposable income

summary(cdata$installment_pct_disp_inc_8)

op8<-par(mfrow=c(1,2))
boxplot(cdata$installment_pct_disp_inc_8)
histogram(cdata$installment_pct_disp_inc_8,
          main = "Installment rate in percentage of disposable income", 
          xlab = "installment percent",
          ylab = "Percent Population")
par(op8)

A8<-gbpct(cdata$installment_pct_disp_inc_8)

op8_1<-par(mfrow=c(1,2))
plot(as.factor(cdata$installment_pct_disp_inc_8), cdata$good_bad_21, 
     main="Installment rate in percentage of disposable income ~ Good-Bad",
     xlab="Percent",
     ylab="Good-Bad")

barplot(A8$WOE, col="brown", names.arg=c(A8$Levels),
        main="Installment rate",
        xlab="Percent",
        ylab="WOE")
par(op8_1)

kable(A8, caption = "Installment rate in percentage of disposable income ~ Good-Bad")
```

<font color="blue"> Information Value is  <b> `r round(sum(A8$IV),2)` </b> and Efficiency is <b> `r round(sum(A8$Efficiency),2)` </b>. </font>

***

### 2.2.9 Personal status and sex 

```{r, echo = TRUE}
# Attribute 9:  (qualitative)
#-----------------------------------------------------------
# Personal status and sex - you may not use for some country due to regulations

# 	      A91 : male   : divorced/separated
# 	      A92 : female : divorced/separated/married
#               A93 : male   : single
# 	      A94 : male   : married/widowed
# 	      A95 : female : single

A9<-gbpct(cdata$personal_status_9)

op9<-par(mfrow=c(1,2))
plot(cdata$personal_status_9, cdata$good_bad_21, 
          main=" Personal status",
     xlab=" Personal status",
     ylab="Good-Bad")


barplot(A9$WOE, col="brown", names.arg=c(A9$Levels),
        main="Personal status",
        xlab="Category",
        ylab="WOE")
par(op9)

kable(A9, caption =  "Personal status ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A9$IV),2)` </b> and Efficiency is <b> `r round(sum(A9$Efficiency),2)` </b>. </font>

***

### 2.2.10 Other debtors / guarantors 

```{r, echo = TRUE}
# Attribute 10: (qualitative)	   
#-----------------------------------------------------------
# Other debtors / guarantors

# 	      A101 : none
# 	      A102 : co-applicant
# 	      A103 : guarantor

A10<-gbpct(cdata$other_debtors_or_grantors_10)

op10<-par(mfrow=c(1,2))

plot(cdata$other_debtors_or_grantors_10, cdata$good_bad_21, 
      main="Other debtors / guarantors",
     xlab="Category",
     ylab="Good-Bad")

barplot(A10$WOE, col="brown", names.arg=c(A10$Levels),
        main="Other debtors / guarantors",
        xlab="Category",
        ylab="WOE")

par(op10)

kable(A10, caption = "Other debtors / guarantors ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A10$IV),2)` </b> and Efficiency is <b> `r round(sum(A10$Efficiency),2)` </b>. </font>

***

### 2.2.11 Present residence since 

```{r, echo = TRUE}
# Attribute 11: (numerical)
#-----------------------------------------------------------
# Present residence since
summary(cdata$present_residence_since_11)
A11<-gbpct(cdata$present_residence_since_11)

op11<-par(mfrow=c(1,2))
histogram(cdata$present_residence_since_11,
          main="Present Residence~ Good-Bad",
          xlab="Present residence Since", 
          ylab="Percent Population")

barplot(A11$WOE, col="brown", names.arg=c(A11$Levels),
        main="Present Residence",
        xlab="Category",
        ylab="WOE")
par(op11)

kable(A11, caption = "Present Residence~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A11$IV),2)` </b> and Efficiency is <b> `r round(sum(A11$Efficiency),2)` </b>. </font>

***

### 2.2.12 Property Type 

```{r, echo = TRUE}

# Attribute 12: (qualitative)
#-----------------------------------------------------------
# Property
# 	      A121 : real estate
# 	      A122 : if not A121 : building society savings agreement/
# 				   life insurance
#               A123 : if not A121/A122 : car or other, not in attribute 6
# 	      A124 : unknown / no property

A12 <- gbpct(cdata$property_type_12)

op12 <- par(mfrow = c(1,2))
plot(cdata$property_type_12, cdata$good_bad_21, 
     main = "Property Type",
      xlab="Type",
     ylab="Good-Bad")	      

barplot(A12$WOE, col="brown", names.arg=c(A12$Levels),
        main="Property Type",
        xlab="Category",
        ylab="WOE")
par(op12)

kable(A12,  caption = "Property Type")

```


<font color="blue"> Information Value is  <b> `r round(sum(A12$IV),2)` </b> and Efficiency is <b> `r round(sum(A12$Efficiency),2)` </b>. </font>

***

### 2.2.13 Age in Years

```{r, echo = TRUE}

# Attribute 13: (numerical)
#-----------------------------------------------------------
# Age in Years

summary(cdata$age_in_yrs_13)
op13 <- par(mfrow = c(1,2))
boxplot(cdata$age_in_yrs_13)

plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21,
     main = "Age",
     xlab = "Age in Years",
     ylab = "Good-Bad")

par(op13)

cdata$age_in_yrs_13 <- as.factor(ifelse(cdata$age_in_yrs_13<=25, '0-25',
                                      ifelse(cdata$age_in_yrs_13<=30, '25-30',
                                             ifelse(cdata$age_in_yrs_13<=35, '30-35', 
                                                    ifelse(cdata$age_in_yrs_13<=40, '35-40', 
                                                           ifelse(cdata$age_in_yrs_13<=45, '40-45', 
                                                                  ifelse(cdata$age_in_yrs_13<=50, '45-50',
                                                                         ifelse(cdata$age_in_yrs_13<=60, '50-60',
                                                                                '60+'))))))))


A13<-gbpct(cdata$age_in_yrs_13)

op13_1<-par(mfrow=c(1,2))
plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21, 
      main="Age (After Grouping)",
     xlab="Other installment plans",
     ylab="Good-Bad")


barplot(A13$WOE, col="brown", names.arg=c(A13$Levels),
        main="Age",
        xlab="Category",
        ylab="WOE")
par(op13_1)

kable(A13,  caption = "Age (After Grouping) ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A13$IV),2)` </b> and Efficiency is <b> `r round(sum(A13$Efficiency),2)` </b>. </font>

***

### 2.2.14  Other installment plans

```{r, echo = TRUE}
# Attribute 14: (qualitative)
#-----------------------------------------------------------
# 	      Other installment plans 
# 	      A141 : bank
# 	      A142 : stores
# 	      A143 : none

A14<-gbpct(cdata$other_installment_type_14)

op14<-par(mfrow=c(1,2))

plot(cdata$other_installment_type_14, cdata$good_bad_21, 
     main="Other installment plans ~ Good-Bad",
     xlab="Other installment plans",
     ylab="Good-Bad")

barplot(A14$WOE, col="brown", names.arg=c(A14$Levels),
        main="Other installment plans",
        xlab="Category",
        ylab="WOE")
par(op14)

kable(A14, caption = "Other installment plans ~ Good-Bad")

# cdata$other_installment_type_14<-as.factor(ifelse(cdata$other_installment_type_14 == "A143", "None", "banknstore"))
# 
# A14_1<-gbpct(cdata$other_installment_type_14)
# 
# plot(cdata$other_installment_type_14, cdata$good_bad_21, 
#      ylab="Good-Bad", xlab="Other installment plans",
#      main="Other installment plans (after grouping) ~ Good-Bad")	
# 
# barplot(A14_1$WOE, col="brown", names.arg=c(A14_1$Levels),
#         main="Other installment plans",
#         xlab="Category",
#         ylab="WOE")
# 
# kable(A14_1)

```


<font color="blue"> Information Value is  <b> `r round(sum(A14$IV),2)` </b> and Efficiency is <b> `r round(sum(A14$Efficiency),2)` </b>. </font>

***

### 2.2.15  Housing Type

```{r, echo = TRUE}
# Attribute 15: (qualitative)
#-----------------------------------------------------------
# 	      Housing
# 	      A151 : rent
# 	      A152 : own
# 	      A153 : for free

A15<-gbpct(cdata$housing_type_15)

op15<-par(mfrow=c(1,2))
plot(cdata$housing_type_15, cdata$good_bad_21, 
      main="Home Ownership Type",
      xlab="Type",
      ylab="Good-Bad")

barplot(A15$WOE, col="brown", names.arg=c(A15$Levels),
        main="Home Ownership Type",
        xlab="Type",
        ylab="WOE")
par(op15)

kable(A15, caption = "Home Ownership Type ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A15$IV),2)` </b> and Efficiency is <b> `r round(sum(A15$Efficiency),2)` </b>. </font>

***

### 2.2.16 Number of existing credits at this bank

```{r, echo = TRUE}

# Attribute 16: (numerical)
#-----------------------------------------------------------
#               Number of existing credits at this bank

summary(cdata$number_cards_this_bank_16)

A16<-gbpct(cdata$number_cards_this_bank_16)

op16<-par(mfrow=c(1,2))
plot(as.factor(cdata$number_cards_this_bank_16), cdata$good_bad_21,
      main="Number of credits at this bank",
      xlab="Number of Cards",
      ylab="Good-Bad")

barplot(A16$WOE, col="brown", names.arg=c(A16$Levels),
        main="Number of credits at this bank",
        xlab="Number of Cards",
        ylab="WOE")
par(op16)

kable(A16, caption = "Number of credits at this bank ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A16$IV),2)` </b> and Efficiency is <b> `r round(sum(A16$Efficiency),2)` </b>. </font>

***

### 2.2.17 Job Status

```{r, echo = TRUE}


# Attribute 17: (qualitative)
#-----------------------------------------------------------
# 	      Job
# 	      A171 : unemployed/ unskilled  - non-resident
# 	      A172 : unskilled - resident
# 	      A173 : skilled employee / official
# 	      A174 : management/ self-employed/
# 		     highly qualified employee/ officer

A17<-gbpct(cdata$job_17)

op17<-par(mfrow=c(1,2))

plot(cdata$job_17, cdata$good_bad_21, 
     main="Employment Type",
     xlab="Job",
     ylab="Good-Bad")

barplot(A17$WOE, col="brown", names.arg=c(A17$Levels),
        main="Employment Type",
        xlab="Job",
        ylab="WOE")

par(op17)

kable(A17, caption = "Employment Type ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A17$IV),2)` </b> and Efficiency is <b> `r round(sum(A17$Efficiency),2)` </b>. </font>

***

### 2.2.18 Number of people being liable to provide maintenance for

```{r, echo = TRUE}
# Attribute 18: (numerical)
#-----------------------------------------------------------
# 	      Number of people being liable to provide maintenance for

summary(cdata$no_people_liable_for_mntnance_18)

A18<-gbpct(cdata$no_people_liable_for_mntnance_18)

op18<-par(mfrow = c(1,2))

plot(as.factor(cdata$no_people_liable_for_mntnance_18), cdata$good_bad_21, 
        main = "Number of people being liable",
        xlab = "Number of People",
        ylab = "Good-Bad")

barplot(A18$WOE, col = "brown", names.arg=c(A18$Levels),
        main = " Number of people being liable",
        xlab = "Number of People",
        ylab = "WOE")

par(op18)

kable(A18, caption = "Number of people being liable ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A18$IV),2)` </b> and Efficiency is <b> `r round(sum(A18$Efficiency),2)` </b>. </font>

***


### 2.2.19 Telephone

```{r, echo = TRUE}
# Attribute 19: (qualitative)
#-----------------------------------------------------------
# 	      Telephone
# 	      A191 : none
# 	      A192 : yes, registered under the customers name

A19<-gbpct(cdata$telephone_19)

op19<-par(mfrow=c(1,2))

plot(cdata$telephone_19, cdata$good_bad_21, 
     main="Telephone",
     xlab="Telephone",
     ylab="Good-Bad")

barplot(A19$WOE, col="brown", names.arg=c(A19$Levels),
        main="Telephone",
        xlab="Telephone",
        ylab="WOE")

par(op19)

kable(A19, caption = "Telephone ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A19$IV),2)` </b> and Efficiency is <b> `r round(sum(A19$Efficiency),2)` </b>. </font>

***


### 2.2.20 foreign worker

```{r, echo = TRUE, cache=TRUE}
# Attribute 20: (qualitative)
#-----------------------------------------------------------
# 	      foreign worker
# 	      A201 : yes
# 	      A202 : no


A20<-gbpct(cdata$foreign_worker_20)

op20<-par(mfrow=c(1,2))

plot(cdata$foreign_worker_20, cdata$good_bad_21, 
     main="Foreign Worker",
     xlab="Category",
     ylab="Good-Bad")

barplot(A20$WOE, col="brown", names.arg=c(A20$Levels),
        main="Foreign Worker",
        xlab="Category",
        ylab="WOE")

par(op20)

kable(A20,  caption = "Foreign Worker ~ Good-Bad")

```

<font color="blue"> Information Value is  <b> `r round(sum(A20$IV),2)` </b> and Efficiency is <b> `r round(sum(A20$Efficiency),2)` </b>. </font>

***


### 2.2.21 IV and WOE

```{r}
cdata$good_bad_21<-as.numeric(ifelse(cdata$good_bad_21 == "Good", 0, 1))
IV <- Information::create_infotables(data=cdata, NULL, y="good_bad_21", 10)
IV$Summary$IV <- round(IV$Summary$IV*100,2)

IV$Tables
kable(IV$Summary)

cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 0, "Good", "Bad"))
```


I. Following variables do not have prediction power - very very weak predictor (IV< 2%), hence we shall exclude them from modeling

Position          Variable    IV
16	number_cards_this_bank_16	1.01 
17	job_17	0.88
19	telephone_19	0.64
11	present_residence_since_11,	0.36
18	no_people_liable_for_mntnance_18,	0.00

II. Following variables are very weak predictors (2%<=IV< 10%), hence we may or may not include  them while modeling

* Position                 Variable    IV

* 7              p_employment_since_7  8.64

* 15                  housing_type_15  8.33

* 14        other_installment_type_14  5.76

* 9                 personal_status_9  4.47

* 20                foreign_worker_20  4.39

* 10     other_debtors_or_grantors_10  3.20

* 8        installment_pct_disp_inc_8  2.63

III. Following variables have medium prediction power (10%<=IV< 30%), hence we will include them in modeling as we have less number of variables
 
* Position                 Variable    IV

* 3                  credit_history_3 29.32

* 2                  duration_month_2 27.79

* 6                 savings_ac_bond_6 19.60

* 4                         purpose_4 16.92

* 13                    age_in_yrs_13 12.12

* 12                 property_type_12 11.26

* 5                   credit_amount_5 11.18


IV. There is no strong predictor with IV between 30% to 50%


V. chk_ac_status_1 has a very high prediction power (IV > 50%), it could be suspicious and require further investigation

* Position                 Variable    IV

* 1                   chk_ac_status_1 66.60

***

#### 2.2.21.1 Subset Data - 1

```{r}
var_list_1 <- IV$Summary[IV$Summary$IV > 2,] # 15 variables
cdata_reduced_1<-cdata[, c(var_list_1$Variable,"good_bad_21")] #16 variables

```

### 2.2.22  Variable Reduction using Variable Clustering


```{r, echo=TRUE, cache=TRUE}
# ClusterOfVariables
# Step 1:
factors<-sapply(cdata_reduced_1, is.factor)
vars_quali<- cdata_reduced_1[,factors]
#vars_quali$good_bad_21<-vars_quali$good_bad_21[drop=TRUE] # remove empty factors
str(vars_quali)

vars_quanti <- cdata_reduced_1[,!factors]

str(vars_quanti)


tree <- hclustvar(X.quanti=vars_quanti,X.quali=vars_quali[,-c(12)])
plot(tree)
rect.hclust(tree, k=10,  border = 1:10)
summary(tree)

# add colors randomly
plot(as.phylo(tree), type = "fan",
     tip.color = hsv(runif(15, 0.65,  0.95), 1, 1, 0.7),
     edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
     edge.width = runif(20,  0.5, 3), use.edge.length = TRUE, col = "gray80")

summary.phylo(as.phylo(tree))

stab<-stability(tree,B=50) # Bootstrap 50 times
#plot(stab,main="Stability of the partitions")
boxplot(stab$matCR)

part<-cutreevar(tree,10)
print(part)

summary(part)
#head(part$scores)

```

We may also cross check using Kmeansvar clustering 

```{r}
kfit<-kmeansvar(X.quanti = vars_quanti, X.quali = vars_quali[,-c(12)], init=10,
          iter.max = 150, nstart = 1, matsim = TRUE)
summary(kfit)

plot(cbind(vars_quanti, vars_quali), as.factor(kfit$cluster))

kfit$E

```

We will model first ten tip labels from the varclus: 


* 1. duration_month_2

* 2. age_in_yrs_13

* 3. credit_amount_5

* 4. installment_pct_disp_inc_8

* 5. chk_ac_status_1

* 6. credit_history_3

* 7. savings_ac_bond_6

* 8. purpose_4

* 9. property_type_12

* 10. p_employment_since_7


#### 2.2.22.1 Subset data -2

```{r, cache=TRUE}
keep<- c(1:8,12,13,21)
cdata_reduced_2 <- cdata[,keep]
str(cdata_reduced_2)
```

***

## 2.3. Random Sampling (Train and Test) 

We may split the data (given population) into random samples with 50-50, 60-40 or 70-30 ratios for **Training** (Development Sample on which model will be developed or trained) and **Test** (validation/holdout sample on which model will be tested) based on population size. In this exercise we will split the sample into 70-30.


### 2.3.1 Simple Random Sampling

```{r, cache=TRUE}
div_part <- sort(sample(nrow(cdata_reduced_2), nrow(cdata_reduced_2)*.7))

#select training sample 
train<-cdata_reduced_2[div_part,] # 70% here
pct(train$good_bad_21)

# put remaining into test sample
test<-cdata_reduced_2[-div_part,] # rest of the 30% data goes here
pct(test$good_bad_21)
```

### 2.3.2 Stratified Random Sampling

```{r, cache=TRUE}
# Required "caret" package
# considering good_bad variable as strata

pct(cdata_reduced_2$good_bad_21)

div_part_1 <- createDataPartition(y = cdata_reduced_2$good_bad_21, p = 0.7, list = F)

# Training Sample
train_1 <- cdata_reduced_2[div_part_1,] # 70% here
pct(train_1$good_bad_21)

# Test Sample
test_1 <- cdata_reduced_2[-div_part_1,] # rest of the 30% data goes here
pct(test_1$good_bad_21)

# Sampling for Neural Network - It can be used for other modeling as well
div_part_2 <- createDataPartition(y = cdatanum[,25], p = 0.7, list = F)

# Training Sample for Neural Network
train_num <- cdatanum[div_part_2,] # 70% here


# Test Sample for Neural Network
test_num <- cdatanum[-div_part_2,] # rest of the 30% data goes here
```


Clearly stratified sampling is more accurate than simple random sampling.

# 3 Model Development


## 3.1 Logistic Regression

```{r}
# Logistic Regression Model
m1<-glm(good_bad_21~.,data=train_1,family=binomial())
m1<-step(m1)
summary(m1)

prob <- predict(m1, type = "response")
res <- residuals(m1, type = "deviance")

#Plot Residuals
plot(predict(m1), res,
     xlab="Fitted values", ylab = "Residuals",
     ylim = max(abs(res)) * c(-1,1))

## CIs using profiled log-likelihood
confint(m1)

## CIs using standard errors
confint.default(m1)

#
## odds ratios and 95% CI
exp(cbind(OR = coef(m1), confint(m1)))


#score test data set
test_1$m1_score<-predict(m1,type='response',test_1)
m1_pred<-prediction(test_1$m1_score, test_1$good_bad_21)
m1_perf <- performance(m1_pred,"tpr","fpr")
plot(m1_perf)


#KS
m1_KS<-max(attr(m1_perf,'y.values')[[1]]-attr(m1_perf,'x.values')[[1]])*100
m1_KS

# Cross Validatio
#load Data Analysis And Graphics Package for R (DAAG)
#library(DAAG)
#calculate accuracy over 100 random folds of data for simple logit
m1_h<-CVbinary(obj=m1, rand=NULL, nfolds=100, print.details=TRUE)

```


```{r}
m1_1<-glm(good_bad_21~chk_ac_status_1+duration_month_2
          +savings_ac_bond_6+installment_pct_disp_inc_8,
          data=train_1,family=binomial())

summary(m1_1)

test_1$m1_1_score<-predict(m1_1,type='response',test_1)
m1_1_pred<-prediction(test_1$m1_1_score,test_1$good_bad_21)
m1_1_perf <- performance(m1_1_pred,"tpr","fpr")

plot(m1_1_perf)

AUCRF=performance(m1_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCRF,"\n")

AUCRF=performance(m1_1_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCRF,"\n")

```

***

## 3.2 Using Bayesian N Using Traditional recursive Partitioning

```{r}
m2 <- rpart(good_bad_21~.,data=train_1)
plot(m2);text(m2);
prp(m2,type=2,extra=1)


#score test data
test_1$m2_score <- predict(m2,type='prob',test_1)
m2_pred <- prediction(test_1$m2_score[,2],test_1$good_bad_21)
m2_perf <- performance(m2_pred,"tpr","fpr")

#build model using 90% 10% priors
#with smaller complexity parameter to allow more complextrees
# for tuning complexity vs. pruning see Thernau 1997
m2_1<-rpart(good_bad_21~.,data=train_1,parms=list(prior=c(.9,.1)),cp=.0002)
plot(m2_1);text(m2_1);
prp(m2_1,type=2,extra=1)

test_1$m2_1_score <- predict(m2_1,type='prob',test_1)

m2_1_pred<-prediction(test_1$m2_1_score[,2],test_1$good_bad_21)
m2_1_perf<- performance(m2_1_pred,"tpr","fpr")

AUCRF=performance(m2_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCRF,"\n")

AUCRF=performance(m2_1_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCRF,"\n")

#prints complexity and out of sample error
printcp(m2)
#plots complexity vs. error
plotcp(m2)
#prints complexity and out of sample error
printcp(m2_1)
#plots complexity vs. error
plotcp(m2_1)
```


```{r}
#KS m1
m2_KS<-max(attr(m2_perf,'y.values')[[1]]-attr(m2_perf,'x.values')[[1]])*100
m2_KS

#KS m2
m2_1_KS<-max(attr(m2_1_perf,'y.values')[[1]]-attr(m2_1_perf,'x.values')[[1]])*100
m2_1_KS
```

### Print tree rules

```{r}
#print rules for all classes
#rpart.lists(m2)
#rpart.rules(m2)
#rpart.lists(m2_1)
#rpart.rules.table(m2_1)
```

## 3.3 Random Forest

### 3.3.1 General Randmon Forest

```{r}
m3 <- randomForest(good_bad_21 ~ ., data =train_1)
m3_fitForest <- predict(m3, newdata=test_1, type="prob")[,2]
m3_pred <- prediction( m3_fitForest, test_1$good_bad_21)
m3_perf <- performance(m3_pred, "tpr", "fpr")
plot(m3_perf)
#plot variable importance
varImpPlot(m3, main="Random Forest: Variable Importance")

# Model Performance
m3_AUCRF <- performance(m3_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",m3_AUCRF,"\n")

#KS m3
m3_KS<-max(attr(m3_perf,'y.values')[[1]]-attr(m3_perf,'x.values')[[1]])*100
m3_KS
```

### 3.3.2 Conditional Random Forest

```{r}
#library(party)

set.seed(42)
m3_1<-cforest(good_bad_21~.,control = cforest_unbiased(mtry = 2, ntree = 50), data=train_1)

# Variable Importance
kable(as.data.frame(varimp(m3_1)))

# Model Summary
summary(m3_1)

# Model Performance
m3_1_fitForest <- predict(m3, newdata=test_1, type="prob")[,2]
m3_1_pred <- prediction(m3_1_fitForest, test_1$good_bad_21)
m3_1_perf <- performance(m3_1_pred, "tpr", "fpr")

# Model Performance Plot
plot(m3_1_perf, main = " Conditional Random Forests")

# AUC
m3_1_AUCRF <- performance(m3_1_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",m3_1_AUCRF,"\n")

#KS m3
m3_1_KS<-max(attr(m3_perf,'y.values')[[1]]-attr(m3_perf,'x.values')[[1]])*100
m3_1_KS

```

### 3.3.3 Improve Logistic Results using Random Forest

```{r}
#model based on trial and error based on random forest variable importance
#m3_2<-glm(good_bad_21~.+credit_history_3:p_employment_since_7+ credit_history_3:installment_pct_disp_inc_8
#          +chk_ac_status_1:p_employment_since_7 +chk_ac_status_1:purpose_4
#          + duration_month_2:credit_amount_5, data=train_1,family=binomial())

m3_2<-glm(good_bad_21~.+credit_history_3:p_employment_since_7
          + credit_history_3:age_in_yrs_13
          + chk_ac_status_1:p_employment_since_7
          + chk_ac_status_1:savings_ac_bond_6
          + duration_month_2:purpose_4, data=train_1,family=binomial())


m3_2 <- step(m3_2)
summary(m3_2)

test_1$m3_2_score<-predict(m3_2,type='response',test_1)
m3_2_pred<-prediction(test_1$m3_2_score,test_1$good_bad_21)
m3_2_perf <- performance(m3_2_pred,"tpr","fpr")

# Model Performance
plot(m3_2_perf, main="Improve Logistic Results using Random Forest")


m3_2_AUCRF <- performance(m3_2_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",m3_2_AUCRF,"\n")

#KS m3
m3_2_KS<-max(attr(m3_2_perf,'y.values')[[1]]-attr(m3_2_perf,'x.values')[[1]])*100
m3_2_KS

```

## 3.4 Conditional inference Trees

```{r}
#library(party)
m4 <- ctree(good_bad_21~.,data=train_1)
plot(m4, main="Conditional inference Tree");

resultdfr <- as.data.frame(do.call("rbind", treeresponse(m4, newdata = test_1)))
test_1$m4_score <- resultdfr[,2]
m4_pred <- prediction(test_1$m4_score,test_1$good_bad_21)
m4_perf <- performance(m4_pred,"tpr","fpr")

# Model Performance
m4_AUCRF <- performance(m4_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",m4_AUCRF,"\n")

#KS m3
m4_KS<-max(attr(m4_perf,'y.values')[[1]]-attr(m4_perf,'x.values')[[1]])*100
m4_KS

#randomForest (randomForest) and cforest (party) have same results
```


## 3.5 Bayesian Network (Computation - intensive and expensive)

```{r}
#load library
#library(bnlearn)
train_2<-train_1
train_2$duration_month_2 <- as.factor(train_2$duration_month_2)
train_2$credit_amount_5 <- as.factor(train_2$credit_amount_5)
train_2$installment_pct_disp_inc_8 <- as.factor(train_2$installment_pct_disp_inc_8)
train_2$age_in_yrs_13 <- as.factor(train_2$age_in_yrs_13)
 
bn.gs <- gs(train_2)
bn.gs
bn2 <- iamb(train_2)
bn2
bn3 <- fast.iamb(train_2)
bn3
bn4 <- inter.iamb(train_2)
bn4
compare(bn.gs, bn2)
compare(bn.gs, bn3)
compare(bn.gs, bn4)

#On the other hand hill-climbing results in a completely directed network, which diers from
#the previous one because the arc between A and B is directed (A ! B instead of A  B).
bn.hc <- hc(train_2, score = "aic")
bn.hc

compare(bn.hc, bn.gs)

opm5<-par(mfrow = c(1,2))
plot(bn.gs, main = "Constraint-based algorithms")
plot(bn.hc, main = "Hill-Climbing")
par(opm5)
modelstring(bn.hc)


res2 = hc(train_2)
fitted2 = bn.fit(res2, train_2)
fitted2

# library(gRain)

```
## 3.6 Unbiased Non parametric methods-Model Based Trees (Logistic)

```{r}
#model based recursive paritioning
#library(party)
# iter 1
m6<-mob(good_bad_21~chk_ac_status_1 |
           duration_month_2
           +credit_history_3
           +purpose_4
           +credit_amount_5
           +savings_ac_bond_6
           +p_employment_since_7
           +installment_pct_disp_inc_8
           +property_type_12
           +age_in_yrs_13,
           data=train_1,
           model=glinearModel,family=binomial())

# iter 2
# m6<-mob(good_bad_21~  credit_history_3  +chk_ac_status_1 + savings_ac_bond_6 +  purpose_4  |
#            +duration_month_2          
#            +installment_pct_disp_inc_8 
#            +credit_amount_5           
#            +p_employment_since_7      
#            +property_type_12          
#            +age_in_yrs_13,
#            data=train_1,
#            model=glinearModel,family=binomial())

# iter 3
# m6<-mob(good_bad_21~  chk_ac_status_1  +  purpose_4|
#           credit_history_3
#           +duration_month_2
#            +installment_pct_disp_inc_8
#           +credit_amount_5
#           +savings_ac_bond_6
#            +p_employment_since_7
#            +property_type_12
#            +age_in_yrs_13,
#            data=train_1,
#            model=glinearModel,family=binomial())


#library(vcd) #the `vcd' package is required for CD plots
plot(m6, main="Model based Tree with GLM")

# Scoring
test_1$m6_score<-predict(m6, newdata = test_1, type =c("response"))

m6_pred <- prediction(test_1$m6_score,test_1$good_bad_21)
m6_perf <- performance(m6_pred,"tpr","fpr")
plot(m6_perf, main="ROC:m6-Model based Tree with GLM", col='blue')

# Model Performance
m6_AUCRF <- performance(m6_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",m6_AUCRF,"\n")

#KS m6
m6_KS<-max(attr(m6_perf,'y.values')[[1]]-attr(m6_perf,'x.values')[[1]])*100
m6_KS

```


## 3.7 Support Vector Machine

### 3.7.1 SVM - Vanilladot Kernel

```{r}
#library(kernlab) #for SVM

# Basic Model
m7_1 <- ksvm(good_bad_21 ~ ., data = train_1, kernel = "vanilladot")

m7_1_pred <- predict(m7_1, test_1[,1:10], type="response")
head(m7_1_pred)

# Model accuracy:
table(m7_1_pred, test_1$good_bad_21)

#agreement
m7_1_accuracy  <- (m7_1_pred == test_1$good_bad_21)
pct(m7_1_accuracy)

# Compute at the prediction scores
m7_1_score = predict(m7_1,test_1, type="decision")
m7_1_pred <- prediction(m7_1_score, test_1$good_bad_21)


# Plot ROC curve
m7_1_perf <- performance(m7_1_pred, measure = "tpr", x.measure = "fpr")
#plot(m7_1_perf, main="SVM:Plot ROC curve", col="blue")

# Plot precision/recall curve
m7_1_perf_precision <- performance(m7_1_pred, measure = "prec", x.measure = "rec")
#plot(m7_1_perf_precision, main="SVM:Plot precision/recall curve")

# Plot accuracy as function of threshold
m7_1_perf_acc <- performance(m7_1_pred, measure = "acc")
#plot(m7_1_perf_acc, main="SVM:Plot accuracy as function of threshold")

# Model Performance
m7_1_AUCRF <- performance(m7_1_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",m7_1_AUCRF,"\n")

#KS m6
m7_1_KS<-max(attr(m7_1_perf,'y.values')[[1]]-attr(m7_1_perf,'x.values')[[1]])*100
m7_1_KS

```

### 3.7.2 SVM - Gaussian RBF kernel

```{r}

# Model Improvement with  Gaussian RBF kernel
m7_2 <- ksvm(good_bad_21 ~ ., data = train_1,
                            kernel = "rbfdot")
m7_2_pred <- predict(m7_2, test_1[,1:10], type="response")
head(m7_2_pred)

# Model accuracy:
table(m7_2_pred, test_1$good_bad_21)

#agreement
m7_2_accuracy  <- (m7_2_pred == test_1$good_bad_21)
pct(m7_2_accuracy)

# Compute at the prediction scores
m7_2_score = predict(m7_2,test_1, type="decision")
m7_2_pred <- prediction(m7_2_score, test_1$good_bad_21)


# Plot ROC curve
m7_2_perf <- performance(m7_2_pred, measure = "tpr", x.measure = "fpr")
#plot(m7_2_perf, main="SVM:Plot ROC curve", col="blue")

# Plot precision/recall curve
m7_2_perf_precision <- performance(m7_2_pred, measure = "prec", x.measure = "rec")
#plot(m7_2_perf_precision, main="SVM:Plot precision/recall curve")

# Plot accuracy as function of threshold
m7_2_perf_acc <- performance(m7_2_pred, measure = "acc")
#plot(m7_2_perf_acc, main="SVM:Plot accuracy as function of threshold")

# Model Performance
m7_2_AUCRF <- performance(m7_2_pred, measure = "auc")@y.values[[1]]
cat("AUC: ",m7_2_AUCRF,"\n")

#KS m6
m7_2_KS<-max(attr(m7_2_perf,'y.values')[[1]]-attr(m7_2_perf,'x.values')[[1]])*100
m7_2_KS

#Your results may differ from those shown here due to randomness in the ksvm RBF kernel. If you'd like them to match exactly, use set.seed(12345) prior to running the ksvm() function.
```
### 3.7.3 SVM Model Performance Comparision

```{r}
# ROC Comparision
plot(m7_1_perf, col='blue', lty=1, main='SVM:Model Performance Comparision (m7 ROC)') 
plot(m7_2_perf, col='green',lty=2, add=TRUE); # simple tree
    legend(0.5,0.4,
           c("m7_1: SVM vanilladot", "m7_2: SVM RBF kernel"),
           col=c('blue', 'green'),
           lwd=3);
abline(lm(y ~x), col='red') # random line

# Precision Comparision
plot(m7_1_perf_precision, col='blue', lty=1, main='SVM:Model Performance Comparision (m7 precision/recall)') 
plot(m7_2_perf_precision, col='green',lty=2, add=TRUE); # simple tree
    legend(0.2,0.85,c("m7_1: SVM vanilladot", "m7_2: SVM RBF kernel"),
           col=c('blue', 'green'),lwd=3);

# Plot accuracy as function of threshold
plot(m7_1_perf_acc, col='blue', lty=1, main='SVM:Model accuracy as function of threshold (m7)') 
plot(m7_2_perf_acc, col='green',lty=2, add=TRUE); # simple tree
    legend(-1,0.5,c("m7_1: SVM vanilladot", "m7_2: SVM RBF kernel"),
           col=c('blue', 'green'),lwd=3);

```


## 3.8 Neural Network Modeling

```{r}
train_num_norm <- as.data.frame(lapply(train_num[,1:24], normalize ))
test_num_norm <- as.data.frame(lapply(test_num[,1:24], normalize ))

# train_num_norm <- as.data.frame(lapply(train_num[,1:24], scale )) # use scale if normal
# test_num_norm <- as.data.frame(lapply(test_num[,1:24], scale ))   # use scale if normal

set.seed(1234567890)

# build the neural network (NN) formula
a <- colnames(train_num_norm)
mformula <- as.formula(paste('V25 ~ ' ,paste(a,collapse='+')))

train_num_norm$V25 <- train_num$V25
test_num_norm$V25 <- test_num$V25

# Modeling
m8<- neuralnet(mformula,train_num_norm, 
                 hidden = lyr,
                 # lifesign = "minimal",
                 lifesign = "full",
                 threshold = thrs)
  
  # plot NN
  
plot(m8, main="Neural network", rep='best',  
             dimension = 11,
             arrow.length = 0.1,
             col.intercept = "green"
             # ,err.fct = "sse"
             )
  
summary(m8)
print(m8)
  
  gwplot1 <- gwplot(m8, selected.covariate="V1")
  gwplot2 <- gwplot(m8, selected.covariate="V2")
  gwplot3 <- gwplot(m8, selected.covariate="V3")
  
  #ci<- confidence.interval(m8)
  
  # Test Results
  m8_test_result<-compute(m8, test_num_norm[,1:24])
  m8_results <- data.frame(actual = test_num_norm$V25, prediction = m8_test_result$net.result)
  
  kable(head(m8_results))
  
  # Correlation
  corr<-round(cor(m8_results)[1,2]*100,2)
```

# 4 Model Comparision

```{r}
#Compare ROC Performance of Models

plot(m1_perf, col='blue', lty=1, main='Model Performance Comparision') # logistic regression
plot(m2_perf, col='gold',lty=2, add=TRUE); # simple tree
plot(m2_1_perf, col='dark orange',lty=3, add=TRUE); #tree with 90/10 prior
plot(m3_perf, col='green',add=TRUE,lty=4); # random forest
plot(m4_perf, col='dark gray',add=TRUE,lty=5); # Conditional Inference Tree
plot(m3_2_perf, col='dark green',add=TRUE,lty=6);
plot(m7_2_perf, col='black',add=TRUE,lty=7);
    legend(0.6,0.5,
           c('m1:logistic reg','m2:simple tree','m2_1:tree with 90/10 prior', 
                     'm3:random forest', "m4:conditional infer tree", "m3_2: Improved Logistic", "m7_2:SVM"),
           col=c('blue','gold', 'orange','green', 'dark gray', 'dark green', "black"),
           lwd=3);
abline(lm(y ~x), col='red') # random line
```

# A1: References

## Credit Scoring

1. https://sites.google.com/site/rgayler/creditscoringresources
2. http://forecastingsolutions.com/
3. http://www.rcreditscoring.com/
4. http://freakonometrics.hypotheses.org/48285

## Information Value and Weight of Evidence

1. http://www.ponssard.net/wp-content/uploads/2011/02/on-the-concept-of-the-value-of-information.pdf
2. http://research.microsoft.com/en-us/um/people/horvitz/gev.pdf
3. http://ucanalytics.com/blogs/information-value-and-weight-of-evidencebanking-case/
4. http://www.listendata.com/2015/03/weight-of-evidence-woe-and-information.html
5. https://github.com/klarsen1/gampost/blob/master/compare_models.r


## Markdown

1. https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf
2. https://rpubs.com/gallery/cache
3. http://yihui.name/knitr/options/

## ClusterOfVars

1. https://cran.r-project.org/web/packages/ClustOfVar/ClustOfVar.pdf
2. https://www.r-project.org/conferences/useR-2011/TalkSlides/Contributed/16Aug_1600_FocusII_5-DimReduction_1-Chavent.pdf
3. https://arxiv.org/pdf/1112.0295.pdf
4. https://stat.ethz.ch/R-manual/R-devel/library/stats/html/rect.hclust.html

## Bootstrap
1.https://www.r-bloggers.com/bootstrap-evaluation-of-clusters/

## Random Sampling
1.https://cran.r-project.org/web/packages/sampling/sampling.pdf

## Logistic Regression
1. https://cran.r-project.org/web/packages/HSAUR/vignettes/Ch_logistic_regression_glm.pdf
2. http://www.ats.ucla.edu/stat/r/dae/logit.htm

## rpart
1. https://cran.r-project.org/web/packages/rpart.utils/rpart.utils.pdf
2. 

## Bayesian networks
1. https://cran.r-project.org/web/packages/gRain/vignettes/gRain-intro.pdf
2. https://arxiv.org/pdf/0908.3817.pdf
3. http://www.bnlearn.com/examples/score/
4. https://stat.ethz.ch/pipermail/r-help//2012-September/336359.html
5. https://cran.r-project.org/web/packages/bnlearn/bnlearn.pdf
6. https://www.library.ln.edu.hk/eresources/etext/hkibs/hkws_0053.pdf
7. https://www.r-project.org/conferences/DSC-2003/Proceedings/BottcherDethlefsen.pdf
8. https://www.r-project.org/conferences/DSC-2003/Drafts/
9. http://www.vetepi.uzh.ch/dam/jcr:00000000-2b43-78bb-ffff-ffffb89c13f1/abn.pdf
10. http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.114.3548&rep=rep1&type=pdf
11. http://www.cs.uu.nl/research/techreps/repo/CS-2001/2001-58.pdf
12. http://bayesian-intelligence.com/publications/TR2010_1_zonneveldt_korb_nicholson_bn_class_credit_data.pdf
13. http://pure.au.dk/portal-asb-student/files/47799695/Thesis.pdf

## Plots and Visualization
1. http://www.harding.edu/fmccown/r/
2. https://rstudio.github.io/dygraphs/gallery-upper-lower-bars.html
3. https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/abline.html

## germanCredit
1. https://ocw.mit.edu/courses/sloan-school-of-management/15-062-data-mining-spring-2003/assignments/GermanCredit.pdf

## Neural Network
1. https://www.r-bloggers.com/using-neural-networks-for-credit-scoring-a-simple-example/
2. http://www.ijstm.com/images/short_pdf/1431276484_P_26-31.pdf

## Machine Learning
1. Data: http://archive.ics.uci.edu/ml/

## SVM:
1. https://www.jstatsoft.org/article/view/v011i09
2. https://escience.rpi.edu/data/DA/svmbasic_notes.pdf
3. https://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf
4. https://cran.r-project.org/web/packages/kernlab/kernlab.pdf
***

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
