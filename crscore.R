setwd("C:/creditscoring")
cdata<-read.table("data.txt", h=T, sep="")

str(cdata)

# 'data.frame':	1000 obs. of  21 variables:
# $ chk_ac_status_1                 : Factor w/ 4 levels "A11","A12","A13",..: 1 2 4 1 1 4 4 2 4 2 ...
# $ duration_month_2                : int  6 48 12 42 24 36 24 36 12 30 ...
# $ credit_history_3                : Factor w/ 5 levels "A30","A31","A32",..: 5 3 5 3 4 3 3 3 3 5 ...
# $ purpose_4                       : Factor w/ 10 levels "A40","A41","A410",..: 5 5 8 4 1 8 4 2 5 1 ...
# $ credit_amount_5                 : int  1169 5951 2096 7882 4870 9055 2835 6948 3059 5234 ...
# $ savings_ac_bond_6               : Factor w/ 5 levels "A61","A62","A63",..: 5 1 1 1 1 5 3 1 4 1 ...
# $ p_employment_since_7            : Factor w/ 5 levels "A71","A72","A73",..: 5 3 4 4 3 3 5 3 4 1 ...
# $ installment_pct_disp_inc_8      : int  4 2 2 2 3 2 3 2 2 4 ...
# $ personal_status_9               : Factor w/ 4 levels "A91","A92","A93",..: 3 2 3 3 3 3 3 3 1 4 ...
# $ other_debtors_or_grantors_10    : Factor w/ 3 levels "A101","A102",..: 1 1 1 3 1 1 1 1 1 1 ...
# $ present_residence_since_11      : int  4 2 3 4 4 4 4 2 4 2 ...
# $ property_type_12                : Factor w/ 4 levels "A121","A122",..: 1 1 1 2 4 4 2 3 1 3 ...
# $ age_in_yrs_13                   : int  67 22 49 45 53 35 53 35 61 28 ...
# $ other_installment_type_14       : Factor w/ 3 levels "A141","A142",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ housing_type_15                 : Factor w/ 3 levels "A151","A152",..: 2 2 2 3 3 3 2 1 2 2 ...
# $ number_cards_this_bank_16       : int  2 1 1 1 2 1 1 1 1 2 ...
# $ job_17                          : Factor w/ 4 levels "A171","A172",..: 3 3 2 3 3 2 3 4 2 4 ...
# $ no_people_liable_for_mntnance_18: int  1 1 2 2 2 2 1 1 1 1 ...
# $ telephone_19                    : Factor w/ 2 levels "A191","A192": 2 1 1 1 1 2 1 2 1 1 ...
# $ foreign_worker_20               : Factor w/ 2 levels "A201","A202": 1 1 1 1 1 1 1 1 1 1 ...
# $ good_bad_21                     : int  1 2 1 1 2 1 1 1 1 2 ...


#code to convert to factor
cdata$good_bad_21<-as.factor(cdata$good_bad_21)

# Check distribution
plot(cdata$chk_ac_status_1, cdata$good_bad_21)


#code to convert to decimal
cdata$credit_amount_5 <- as.double(cdata$credit_amount_5)
cdata$installment_pct_disp_inc_8  <- as.double(cdata$installment_pct_disp_inc_8)

summary(cdata$credit_amount_5)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 250    1366    2320    3271    3972   18420 

boxplot(cdata$credit_amount_5)



cdata$amount<-as.factor(ifelse(cdata$credit_amount_5<=1400,'0-1400',
                        ifelse(cdata$credit_amount_5<=2500,'1400-2500',
                        ifelse(cdata$credit_amount_5<=3500,'2500-3500', 
                        ifelse(cdata$credit_amount_5<=4500,'3500-4500',
                        ifelse(cdata$credit_amount_5<=5500,'4500-5500','5500+'))))))

plot(cdata$amount, cdata$good_bad_21)


summary(cdata$age_in_yrs_13)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 19.00   27.00   33.00   35.55   42.00   75.00 

boxplot(cdata$age_in_yrs_13)

plot(as.factor(cdata$age_in_yrs_13),  cdata$good_bad_21)

# Breaking into train and test samples (60% and 40% respectively)

num_obs <- nrow(cdata)

d <-  sort(sample(nrow(data), nrow(data)*.6))
#select training sample
train<-data[d,]
test<-data[-d,]
train<-subset(train,select=-default)



m8<-gausspr(good_bad_21~.,data=train_1, kernel="rbfdot",
            kpar="automatic", cross=0, fit=TRUE)
alpha(m8)

# predict on the training set
m8_pred<-predict(m8,test_1[,-12])
# class probabilities
m8_pred_1<-predict(m8, test_1[,-12], type="probabilities")
#plot(m8_pred_1)



