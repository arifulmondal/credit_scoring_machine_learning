library(neuralnet) # Ref: https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf
library(caret)
setwd("C:/creditscoring")
cdatanum<-read.table("german.data-numeric.txt", h=F, sep="")
colnames(cdatanum)

# Required "caret" package
# considering good_bad variable as strata


div_part_1 <- createDataPartition(y = cdatanum[,25], p = 0.7, list = F)

# Training Sample
train_num <- cdatanum[div_part_1,] # 70% here


# Test Sample
test_num <- cdatanum[-div_part_1,] # rest of the 30% data goes here

# Normalize
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

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

# Building models
mynnetfunc<- function(lyr = 25, thrs = 0.1){
  m8<- neuralnet(mformula,train_num_norm, 
                 hidden = lyr,
                 # lifesign = "minimal",
                 lifesign = "full",
                 threshold = thrs)
  
  # plot NN
  
  pnet<-plot(m8, main="Neural network", rep='best',  
             dimension = 11,
             arrow.length = 0.1,
             col.intercept = "green"
             # ,err.fct = "sse"
             )
  
  summ<-summary(m8)
  prn <- print(m8)
  
  gwplot1 <- gwplot(m8, selected.covariate="V1")
  gwplot2 <- gwplot(m8, selected.covariate="V2")
  gwplot3 <- gwplot(m8, selected.covariate="V3")
  
  #ci<- confidence.interval(m8)
  
  # Test Results
  m8_test_result<-compute(m8, test_num_norm[,1:24])
  m8_results <- data.frame(actual =test_num_norm$V25, prediction = m8_test_result$net.result)
  
  # Correlation
  corr<-round(cor(m8_results)[1,2]*100,2)
  
  # Return
  list(Pnet = pnet, Summary = summ, Result = m8_results, 
       Correlation = corr, equation = mformula, nnmodel = m8)
}

mynnetfunc(lyr=4, thrs=0.05)



#http://scg.sdsu.edu/ann_r/
#https://en.wikipedia.org/wiki/Neural_network
#http://www.cs.stir.ac.uk/~lss/NNIntro/InvSlides.html
#https://www.youtube.com/watch?v=xbYgKoG4x2g

library(nnet)
library(ROCR)
train_num_norm$V25 <- as.factor(train_num_norm$V25)
test_num_norm$V25 <- as.factor(test_num_norm$V25)
  
a <- nnet(V25~., data=train_num_norm,size=20,maxit=10000,decay=.001)

table(test_num_norm$V25,predict(a,newdata=test_num_norm, type="class"))

pred = prediction(predict(a,newdata=test_num_norm,type="raw"),test_num_norm$V25)
perf = performance(pred,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="ROC - Neural Network")
abline(a=0,b=1)

# Model Performance
AUCRF <- performance(pred, measure = "auc")@y.values[[1]]
cat("AUC: ",AUCRF,"\n")

#KS m6
KS<-max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])*100
KS




