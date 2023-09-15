library(jsonlite)
library(dplyr) 
library(randomForestSRC)
library(glmnet)

#####################################
# Data Loading
#####################################
#filepath <- "~/Documents/capitalone/transactions.txt"
#content<-readLines(filepath)
#data <- lapply(content, function(x){
#   temp <- fromJSON(x)
#   as.data.frame(temp)
#  }) %>% bind_rows()

data <- read.csv("transaction.csv")
data <- data[,-1]
head(data)
apply(data, 2, function(x) sum(is.na(x)))
col_na <- c("customerId","echoBuffer", "merchantCity","merchantState","merchantZip","posOnPremises","recurringAuthInd")
data <- data[,!colnames(data) %in% col_na]
head(data)

cont_data <- c("creditLimit", "availableMoney","transactionAmount")
apply(data[,cont_data], 2, mean)
apply(data[,cont_data], 2, min)
apply(data[,cont_data], 2, max)



table(data$merchantCategoryCode)
table(data$isFraud)

#####################################
# Plot
#####################################

hist(data$transactionAmount, probability = TRUE, breaks = 200,main = "TransactionAmount")

hist(log(data$transactionAmount), probability = TRUE, breaks = 200,main = "TransactionAmount")

#####################################
# Data Wrangling
#####################################

temp_data <- data
ind_reversal <- which(temp_data$transactionType == "REVERSAL")
temp_data[ind_reversal, ]

# The reversal index is recordedin ind_reversal
# Test whether this index is reliable
# 1: consecutive; 2: non consecutive; 3: unique
match_rev <- rep(NA,length(ind_reversal))
ind_rec <- rep(NA,length(ind_reversal))
for (i in 1:length(ind_reversal)) {
  
  temp_current <- temp_data[ind_reversal[i], "transactionAmount"]
  temp_amount <- temp_data[ind_reversal[i]-1, "transactionAmount"]
  
  
  if(temp_current == temp_amount) {
    
      ind_rec[i] <- 1
      match_rev[i] <- ind_reversal[i]-1
    
  }else{
    
      temp_account <- temp_data[ind_reversal[i], "accountNumber"]
      temp_amount <- temp_data[ind_reversal[i], "transactionAmount"]
      temp_range <- which(temp_data$accountNumber == temp_account)
      
      temp_ind <- temp_range[which( temp_data$transactionAmount[temp_range] %in% temp_amount )]
      
      if( length(temp_ind) > 1) {
        
        temp_ind <- temp_ind[!temp_ind%in%ind_reversal[i]]
        whether_match <- FALSE
        
        for (j in temp_ind) {
          
          if(temp_data$transactionAmount[j] == temp_data$transactionAmount[ind_reversal[i]]&
             temp_data$merchantName[j] == temp_data$merchantName[ind_reversal[i]]&
             temp_data$cardLast4Digits[j] == temp_data$cardLast4Digits[ind_reversal[i]]){
            
            whether_match <- TRUE
            match_rev[i] <- j
          }
          
          
        }
        
        if(whether_match){
          
          ind_rec[i] <- 2
          
        }else{
          ind_rec[i] <- 3
        }
        
        
      }else{
        
          ind_rec[i] <- 3
        
      }
    
  }
  
}
table(ind_rec)


##################################################
# Multi-Swipe
##################################################
temp_data <- temp_data[-c(ind_reversal, match_rev[!is.na(match_rev)] ),]

time_seq <- as.POSIXct(temp_data$transactionDateTime,
                       format="%Y-%m-%dT%H:%M" ,tz="GMT")
uniq_account <- unique(temp_data$accountNumber)
multi_ind <- rep(0,nrow(temp_data))

for (i in 1:length(uniq_account)) {
  
  temp_range <- which(temp_data$accountNumber == uniq_account[i])
  table_merchant <- table(temp_data[temp_range, "merchantName"])
  
  if(sum( table_merchant > 1) > 0){
    
    temp_merchant <- unique( names(table_merchant)[table_merchant > 1] )
    
    for (j in 1:length(temp_merchant)) {
      
      temp_ind <- temp_range[temp_data$merchantName[temp_range] == temp_merchant[j]]
      
      temp_diff <- diff(time_seq[temp_ind]) 
      
      if(sum(temp_diff < 0.01) > 1) {
        
        loc_ind <- which(temp_diff < 0.01)
        loc_ind <- unique(c(loc_ind, loc_ind + 1))
        multi_ind[temp_ind[loc_ind]] <- 1
      }
      
    }
  }
}


final_data <- temp_data[ multi_ind == 0, ]


#####################################
# Model
#####################################
data_model <- final_data[, c("availableMoney", "transactionAmount",
                             "currentBalance")]
data_model[,"creditLimit"] <- as.factor(final_data[,"creditLimit"])
data_model[,"merchantCategoryCode"] <- as.factor(final_data[,"merchantCategoryCode"])
data_model[,"cardPresent"] <- as.factor(final_data[,"cardPresent"])
data_model[,"expirationDateKeyInMatch"] <- as.factor(final_data[,"expirationDateKeyInMatch"])
data_model[,"acq_merchat"] <- as.factor( final_data$acqCountry == final_data$merchantCountryCode )
data_model[,"y"] <- as.factor(as.numeric(final_data$isFraud))

# Sampling

# Generalized Linear Model
y <- data_model$y
X <- model.matrix(~.,data_model[,-ncol(data_model)])
X <- X[,-1]

cv_index <- sample(1:10, length(y), replace = TRUE)
seq_auc <- rep(NA, 10)
seq_accuracy <- rep(NA, 10)

for(i in 1:10){
  
  temp_ind <- which(cv_indx != i)
  cvfit <- cv.glmnet(X[temp_ind,], y[temp_ind,drop= FALSE], family = "binomial", type.measure = "class")
  pred1 <- predict(cvfit, newx = X[-temp_ind,], s = "lambda.min", type = "class")
  pred2 <- predict(cvfit, newx = X[-temp_ind,], s = "lambda.min", type = "response")
}

cvfit <- cv.glmnet(X, y, family = "binomial", type.measure = "class")

