setwd("C:/creditscoring")
cdata<-read.table("data.txt", h=T, sep="")
gbpct <- function(x){
  mt <- as.matrix(table(as.factor(x), as.factor(cdata$good_bad_21)))
  Total <- mt[,1] + mt[,2]
  Total_Pct <- round(Total/sum(mt)*100, 2)
  Bad_pct <- round((mt[,1]/sum(mt[,1]))*100, 2)
  Good_pct <- round((mt[,2]/sum(mt[,2]))*100, 2)
  Bad_Rate <- round((mt[,1]/(mt[,1]+mt[,2]))*100, 2)
  grp_score <- round((Good_pct/(Good_pct + Bad_pct))*10, 2)
  grp_weight <- round(log(Good_pct/Bad_pct)*10, 2)
  g_b_comp <- ifelse(mt[,1] == mt[,2], 0, 1)
  inform_value <- ifelse(g_b_comp == 0, 0, (Good_pct - Bad_pct)*(grp_weight/10))
  efficiency <- abs(Good_pct - Bad_pct)/2
  otb<-as.data.frame(cbind(mt, Good_pct,  Bad_pct,  
                           Total, Total_Pct,  Bad_Rate,
                           grp_score, grp_weight, 
                           inform_value, efficiency))
  otb$Names <- rownames(otb)
  rownames(otb) <- NULL
  otb[,c(12,2,1,3:11)]
}



cdata$good_bad_21<-as.factor(ifelse(cdata$good_bad_21 == 1, "Good", "Bad"))

A1 <- gbpct(cdata$chk_ac_status_1)

kable(A1)
barplot(A1$grp_weight, col="brown")

library(ClustOfVar)
library(ggplot2)
library(GPArotation) 
library(ape)
# Varclus
str(cdata)

# Step 1:
vars_quali<- cdata[,c(1,3,4,6,7,9,10,12,14,15,17,19,20)]
vars_quanti <- cdata[, c(2,5,8,11,13,16,18)]

# number of  variables:  34
# number of numerical variables:  21
# number of categorical variables:  13
# number of objects:  2325

tree <- hclustvar(vars_quanti,vars_quali)


plot(tree, hang=-1, main = "Cluster Dendrogram")
rect.hclust(tree, k=10,  border = 3)

# add colors randomly
plot(as.phylo(tree), type = "fan",
     tip.color = hsv(runif(15, 0.65,  0.95), 1, 1, 0.7),
     edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), 
     edge.width = runif(20,  0.5, 3), use.edge.length = TRUE, col = "gray80")

summary.phylo(as.phylo(tree))

stab<-stability(tree,B=100) # Bootstrap 100 times
plot(stab,main="Stability of the partitions")
boxplot(stab$matCR[,1:12])

part<-cutreevar(tree,12)
print(part)

summary(part)
#head(part$scores)

part_km<-kmeansvar(vars_quanti,vars_quali,init=12,nstart=10)
summary(part_km)


# kmeans vars

kfit<-kmeansvar(X.quanti = vars_quanti, X.quali = vars_quali, init=10,
          iter.max = 150, nstart = 1, matsim = TRUE)
kfit

plot(cbind(vars_quanti, vars_quali), as.factor(kfit$cluster))

kfit$E
part$E

P3 <- cutreevar(tree, 3, matsim = TRUE)
cluster <- P3$cluster
# X <- cbind(vars_quanti, vars_quali)
# princomp(X[, which(cluster==1)], cor = TRUE)$sdev^2
# princomp(X[, which(cluster==2)], cor = TRUE)$sdev^2
# princomp(X[, which(cluster==3)], cor = TRUE)$sdev^2

varpred <- predict(part,X.quali=vars_quali, X.quanti=vars_quanti)



# Bayesian Network
library(deal)
#make copy of train
ksl<-train_1
#discrete cannot inherit from continuous so binary
#good/bad must be converted to numeric for deal package
ksl$good_bad_21<-as.numeric(ksl$good_bad_21)
#no missing values allowed so set any missing to 0
# ksl$history[is.na(ksl$history1)] <- 0 
ksl.nw<-network(ksl)
ksl.prior <- jointprior(ksl.nw)
ksl.nw <- learn(ksl.nw,ksl,ksl.prior)$nw

result <-heuristic(ksl.nw,ksl,ksl.prior,restart=1,degree=1,trace=TRUE)

thebest <- result$nw[[1]]
savenet(thebest, "ksl.net")
print(ksl.nw,condposterior=TRUE)#


train_3$chk_ac_status_1<-as.numeric(gsub("A","",train_3$chk_ac_status_1))
train_3$credit_history_3<-as.numeric(gsub("A","",train_3$credit_history_3))
train_3$purpose_4 <-as.numeric(gsub("A","",train_3$purpose_4 ))
train_3$savings_ac_bond_6<-as.numeric(gsub("A","",train_3$savings_ac_bond_6))
train_3$p_employment_since_7<-as.numeric(gsub("A","",train_3$p_employment_since_7))
train_3$property_type_12<-as.numeric(gsub("A","",train_3$property_type_12))
train_3$good_bad_21 <- as.numeric(ifelse(train_3$good_bad_21 =="Good",0,1))


### 3.3.7 Neural network

```{r}
library(neuralnet) # Neural Network
set.seed(1234567890)

# build the neural network (NN)
a <- colnames(train_1)
a <- a[a!="good_bad_21"]
mformula <- as.formula(paste('good_bad_21 ~ ' ,paste(a,collapse='+')))
#requires numeric/complex matrix/vector arguments
train_3<-train_1

train_3$chk_ac_status_1<-as.numeric(ifelse(train_3$chk_ac_status_1 == "A11",0,
                                           ifelse(train_3$chk_ac_status_1 == "A12",150,
                                                  ifelse(train_3$chk_ac_status_1 == "A13",200, -1))))



train_3$credit_history_3<-as.numeric(ifelse(train_3$credit_history_3 == "A30",0,
                                            ifelse(train_3$credit_history_3 == "A31",1,
                                                   ifelse(train_3$credit_history_3 == "A32",2,
                                                          ifelse(train_3$credit_history_3 == "A33",3, -1)))))

train_3$purpose_4 <-as.numeric(ifelse(train_3$purpose_4=="A40",0,
                                      ifelse(train_3$purpose_4=="A41",1,
                                             ifelse(train_3$purpose_4=="A42",2,
                                                    ifelse(train_3$purpose_4=="A43",3,
                                                           ifelse(train_3$purpose_4=="A44",4,
                                                                  ifelse(train_3$purpose_4=="A45",5,
                                                                         ifelse(train_3$purpose_4=="A46",6,
                                                                                ifelse(train_3$purpose_4=="A47",7,
                                                                                       ifelse(train_3$purpose_4=="A48",8,
                                                                                              ifelse(train_3$purpose_4=="A49",9,
                                                                                                     ifelse(train_3$purpose_4=="A410",10,-1))))))))))))



train_3$savings_ac_bond_6 <-as.numeric(ifelse(train_3$savings_ac_bond_6=="A61",100,
                                              ifelse(train_3$savings_ac_bond_6=="A62",500,
                                                     ifelse(train_3$savings_ac_bond_6=="A63",1000,
                                                            ifelse(train_3$savings_ac_bond_6=="A64",1500,
                                                                   ifelse(train_3$savings_ac_bond_6=="A65",0, -1))))))

train_3$p_employment_since_7 <-as.numeric(ifelse(train_3$p_employment_since_7=="A71",0,
                                                 ifelse(train_3$p_employment_since_7=="A72",1,
                                                        ifelse(train_3$p_employment_since_7=="A73",4,
                                                               ifelse(train_3$p_employment_since_7=="A74",7,
                                                                      ifelse(train_3$p_employment_since_7=="A75",10, -1))))))


train_3$property_type_12<-as.numeric(ifelse(train_3$property_type_12 == "A121",4,
                                            ifelse(train_3$property_type_12 == "A122",3,
                                                   ifelse(train_3$property_type_12 == "A123",2, 0))))

train_3$good_bad_21 <- as.numeric(ifelse(train_3$good_bad_21 =="Good",0,1)) 

str(train_3)
#--------------------


#test_3<-subset(test_1, select=-c(11))

test_3<-test_1

test_3$chk_ac_status_1<-as.numeric(ifelse(test_3$chk_ac_status_1 == "A11",0,
                                          ifelse(test_3$chk_ac_status_1 == "A12",150,
                                                 ifelse(test_3$chk_ac_status_1 == "A13",200, -1))))



test_3$credit_history_3<-as.numeric(ifelse(test_3$credit_history_3 == "A30",0,
                                           ifelse(test_3$credit_history_3 == "A31",1,
                                                  ifelse(test_3$credit_history_3 == "A32",2,
                                                         ifelse(test_3$credit_history_3 == "A33",3, -1)))))

test_3$purpose_4 <-as.numeric(ifelse(test_3$purpose_4=="A40",0,
                                     ifelse(test_3$purpose_4=="A41",1,
                                            ifelse(test_3$purpose_4=="A42",2,
                                                   ifelse(test_3$purpose_4=="A43",3,
                                                          ifelse(test_3$purpose_4=="A44",4,
                                                                 ifelse(test_3$purpose_4=="A45",5,
                                                                        ifelse(test_3$purpose_4=="A46",6,
                                                                               ifelse(test_3$purpose_4=="A47",7,
                                                                                      ifelse(test_3$purpose_4=="A48",8,
                                                                                             ifelse(test_3$purpose_4=="A49",9,
                                                                                                    ifelse(test_3$purpose_4=="A410",10,-1))))))))))))



test_3$savings_ac_bond_6 <-as.numeric(ifelse(test_3$savings_ac_bond_6=="A61",100,
                                             ifelse(test_3$savings_ac_bond_6=="A62",500,
                                                    ifelse(test_3$savings_ac_bond_6=="A63",1000,
                                                           ifelse(test_3$savings_ac_bond_6=="A64",1500,
                                                                  ifelse(test_3$savings_ac_bond_6=="A65",0, -1))))))

test_3$p_employment_since_7 <-as.numeric(ifelse(test_3$p_employment_since_7=="A71",0,
                                                ifelse(test_3$p_employment_since_7=="A72",1,
                                                       ifelse(test_3$p_employment_since_7=="A73",4,
                                                              ifelse(test_3$p_employment_since_7=="A74",7,
                                                                     ifelse(test_3$p_employment_since_7=="A75",10, -1))))))


test_3$property_type_12<-as.numeric(ifelse(test_3$property_type_12 == "A121",4,
                                           ifelse(test_3$property_type_12 == "A122",3,
                                                  ifelse(test_3$property_type_12 == "A123",2, 0))))

test_3$good_bad_21 <- as.numeric(ifelse(test_3$good_bad_21 =="Good",0,1)) 

str(test_3)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

train_3_norm <- as.data.frame(lapply(train_3[,1:10], normalize ))
train_3_norm$good_bad_21 <- train_3$good_bad_21

test_3_norm <- as.data.frame(lapply(test_3[,1:10], normalize ))
test_3_norm$good_bad_21 <- as.numeric(test_3$good_bad_21)

# m7<- neuralnet(mformula,train_3_norm, 
#                hidden = 20, 
#                lifesign = "minimal",     
#                linear.output = FALSE, 
#                threshold = 0.1)

m7<- neuralnet(mformula,train_3, 
               hidden = 20, linear.output=FALSE, likelihood=FALSE, threshold = 0.1)


# plot NN
plot(m7, main="Neural network", rep='best')
summary(m7)

print(m7)

# Test Results
m7_test_result<-compute(m7, test_3[,1:10])
m7_results <- data.frame(actual = test_3$good_bad_21 , prediction = m7_test_result$net.result)
head(m7_results, 10)

cor(m7_results  )

```

