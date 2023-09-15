library(jsonlite)
library(dplyr) 

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
col_na <- c("customerId","echoBuffer", "merchantCity","merchantState","merchantZip","posOnPremises","recurringAuthInd")
data <- data[,!colnames(data) %in% col_na]
head(data)

#####################################
# Plot
#####################################

plot(1:length(data$transactionAmount), data$transactionAmount)

hist(data$transactionAmount, probability = TRUE, breaks = 100)

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



# Multi-Swipe
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

table(temp_data$isFraud)

# merchantCategoryCode

table(temp_data$creditLimit)


data_model <- final_data[, c("creditLimit","posConditionCode","posEntryMode",
                       "merchantCategoryCode","cardPresent","isFraud")]

# Generalized Linear Model



# Random Forest

