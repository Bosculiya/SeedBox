#Importing data sets

library(readr)
install.packages("downloader")
library(downloader)
transData <- read_csv("F:/GitHub/datasciencetest/transData.csv")
View(transData)

testSamples <- read_csv("F:/GitHub/datasciencetest/testSamples.csv")
View(testSamples)

#Merging data sets and removing duplicates

total <- merge(transData,testSamples,by="sample_id")

# 1.hat is the aproximate probability distribution between the test group and the control group (binomial: (k,n)*(p^k)*(1-p)^(n-k)):

count0 <- length(which(testSamples$test_group == 0)) 
count1 <- length(which(testSamples$test_group == 1))

prop.test(c(44886, 14835), c(59721, 59721))


library(dplyr)  #to transform and summarize tabular data with rows and columns

data1 <- filter(total, test_group == 1)
data1 <- filter(data1, transaction_type =="REBILL")

data0 <- filter(total, test_group == 0)
data0 <- filter(data0, transaction_type =="REBILL")

library(ggplot2)      # plotting & data

#2.Is a user that must call-in to cancel more likely to generate at least 1 addition REBILL?

total_1_Rev <- data.frame(tapply(data1$transaction_id, data1$sample_id, FUN=length))
colnames(total_1_Rev) <- c("Num.Transaction")

total_0_Rev <- data.frame(tapply(data0$transaction_id, data0$sample_id, FUN=length))
colnames(total_0_Rev) <- c("Num.Transaction")

t.test(total_1_Rev$Num.Transaction,total_0_Rev$Num.Transaction)
ggplot(total_1_Rev, aes("sample_id_group1" , Num.Transaction)) + geom_boxplot()
ggplot(total_0_Rev, aes("sample_id_group0" , Num.Transaction)) + geom_boxplot()


#3.	Is a user that must call-in to cancel more likely to generate more revenues?

data1_rev <- filter(total, test_group == 1)
data0_rev <- filter(total, test_group == 0)

data11_rev <- data.frame(tapply(data1_rev$transaction_amount, data1_rev$sample_id, FUN=sum))
colnames(data11_rev) <- c("transaction_amount")

data00_rev <- data.frame(tapply(data0_rev$transaction_amount, data0_rev$sample_id, FUN=sum))
colnames(data00_rev) <- c("transaction_amount")

t.test(data11_rev$transaction_amount,data00_rev$transaction_amount)
ggplot(data11_rev, aes("sample_id_group1" , transaction_amount)) + geom_boxplot()



p1 <- ggplot(data1_rev, aes(transaction_amount)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ sample_id)

p2 <- ggplot(data1_rev, aes(transaction_amount)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ sample_id) + scale_x_log10()

grid.arrange(p1, p2, nrow = 2)
