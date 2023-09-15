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
head(data)
colnames(data)
summary(data)
#####################################
# Plot
#####################################

plot(1:length(data$transactionAmount), data$transactionAmount)

hist(data$transactionAmount, probability = TRUE, breaks = 100)
#####################################
# Data Wrangling
#####################################

temp_data <- data[data$transactionType != "ADDRESS_VERIFICATION", ]
ind_reversal <- which(temp_data$transactionType == "REVERSAL")
temp_data[ind_reversal, ]

# The reversal index is recordedin ind_reversal
# Test whether this index is reliable

for (i in 1:ind_reversal) {
  
  temp_account <- temp_data[ind_reversal[i], "accountNumber"]
  
  
  
}


head(temp_data[,c("accountNumber" ,"transactionAmount","transactionType")],20)

check_reverse <- cbind(temp_data[ind_reversal, c( "transactionAmount","transactionType")],
temp_data[ind_reversal-1, c( "transactionAmount","transactionType")])
head(check_reverse)

a <- temp_data[1:75,c( "transactionAmount","transactionType")]
which(a$transactionAmount == "3.87"  )
a$transactionAmount[72]
temp_data[temp_data$accountNumber == "574788567", ]
temp_data[60:75,]
test <- cbind(temp_data[ind_reversal,"transactionAmount"],
temp_data[ind_reversal-1,"transactionAmount"])

table( (test[,1] - test[,2]) == 0)
temp <- data[data$transactionType == "ADDRESS_VERIFICATION", ]

account_list <- unique(data$accountNumber)
multi <- c()
for (i in 1:length(account)) {
  
  temp <- data[data$accountNumber == account_list[i], ]
  temp <- temp[!rownames(temp) %in% rownames(data_reversal),]
  freq <- table(temp$transactionAmount)
  dup <- names(freq)[ freq > 1 ]
  for (j in 1:length(dup)) {
    temp_ind <- which(temp$transactionAmount == as.numeric(dup[j]))
    temp_ind <- temp_ind[1:length(temp_ind)]
    multi <- c(multi, as.numeric(rownames(temp)[temp_ind]))
  }
}

#####################################
# Model
#####################################


