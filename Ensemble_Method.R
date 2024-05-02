##Berna Oruç, Serra Kocali,Zeynep Ellialti

#Method-1-------------------------------------------------------------------------------
library(readxl)
library(ggplot2)
library(tidyverse)
library(rvest)
library(DT)
library(webshot)


setwd("C:/Users/Zeynep/Desktop/ec48e proje")
#2018 Data
Data.2018 <- read_excel("Data.xlsx", col_types = c("text",   "numeric", "date", "numeric", "numeric",  "numeric", "numeric", "numeric"))
d <- Data.2018
result <- Data.2018[20,]
Data.2018
for(i in 1:20){#Calculate candidate bias of each pollester
  for (j in 4:8) {
    Pollster <- Data.2018[i,]
    d[i,j] <- log((Pollster[,j]/100/(1 - Pollster[,j]/100))/(result[,j]/100/(1- result[,j]/100)))
    
  }
}

weights <- numeric(0)
for (i in 1:20) {#Calculate Bw of pollesters
  weights[i] <- sum(abs(d[i,4:8]))/5
}
d$Bw <- weights
d$weights <- 1/d$Bw
weights.2 <- data.frame(name = d$Pollster, value = 1/d$Bw)
d
ggplot(weights.2[1:19,], aes(x=name, y=value)) + 
  geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #Bar plot of weights
election <- numeric(0)
for (i in 4:8) {#Final vote prediction of 2018 election for each candidate using 2018 weights
  election[i-3] <- sum(Data.2018[1:19,i]*(1/d[1:19,9]))/sum((1/d[1:19,9]))
}

# Create data
election <- data.frame(
  name=c("Erdogan","Ince","Aksener","Demirtas","Other") ,  
  value=election
)

# Barplot of prediction
ggplot(election, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

election
result

#2019 Data
url <-  "https://tr.wikipedia.org/wiki/2019_T%C3%BCrkiye_yerel_se%C3%A7imleri_i%C3%A7in_yap%C4%B1lan_anketler"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T) %>% 
  lapply(., function(x) setNames(x, c("Date", "Pollster", "Number of Participants", 
                                      "AKP", "CHP", "Other", "Undecisive")))


Ankara.2019 <- data.frame(df[3])
Ankara.2019 <- Ankara.2019[2:16,]
Ankara.2019 <- Ankara.2019[-c(6,8,9,11,12),]
Ankara.result <-  c(0.4712, 0.5093)#AKP,CHP

Istanbul.2019 <- data.frame(df[4])
Istanbul.2019 <- Istanbul.2019[2:16,]
Istanbul.2019 <- Istanbul.2019[-c(9,11,13,14,15),]
Istanbul.result <- c(0.4499, 0.5421) #AKP,CHP
d <- d[-20,]
Ankara.2019
Istanbul.2019

#Ankara
A <- Ankara.2019[,1:5]
for(i in 1:nrow(A)){#Calculate candidate bias of each pollester
  for (j in 4:5) {
    Pollster <- Ankara.2019[i,]
    A[i,j] <- log((Pollster[,j]/100/(1 - Pollster[,j]/100))/(Ankara.result[j-3]/(1- Ankara.result[j-3])))
    
  }
}

weights.Ankara <- numeric(0)
for (i in 1:nrow(A)) {#Calculate Bw of pollesters
  weights.Ankara[i] <- sum(abs(A[i,4:5]))/2
}
A$Bw <- weights.Ankara
A$Pollster <- c("Avrasya"  , "AREA", "Mediar"  , "Gezici"  ,  "EMAX"   ,   "PÝAR"  ,   "PollMark", "ORC" ,    "Optimar" , "SAROS" )

weights.Ankara.2 <- data.frame(name = A$Pollster, value = 1/A$Bw)
A$weights <- 1/A$Bw
A
ggplot(weights.Ankara.2, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #Bar plot of weights

#Istanbul
I <- Istanbul.2019[,1:5]
for(i in 1:nrow(I)){#Calculate candidate bias of each pollester
  for (j in 4:5) {
    Pollster <- Istanbul.2019[i,]
    I[i,j] <- log((Pollster[,j]/100/(1 - Pollster[,j]/100))/(Istanbul.result[j-3]/(1- Istanbul.result[j-3])))
    
  }
}

weights.Istanbul <- numeric(0)
for (i in 1:nrow(I)) {#Calculate Bw of pollesters
  weights.Istanbul[i] <- sum(abs(I[i,4:5]))/2
}
I$Bw <- weights.Istanbul
I$Pollster <- c("Avrasya",   "Optimar", "AREA",      "Mediar" ,   "Themis",   "Gezici"  ,  "ADA", "EMAX",      "ORC"    ,  "PollMark")
I$weights <- 1/I$Bw
I
weights.Istanbul.2 <- data.frame(name = I$Pollster, value = 1/I$Bw)

ggplot(weights.Istanbul.2, aes(x=name, y=value)) + 
  geom_bar(stat = "identity") #Bar plot of weights

.all <- merge(A,I, by = "Pollster", all = TRUE )
.all2 <- merge(d,.all, by = "Pollster", all = TRUE)
.all2 <- .all2[,c(1,4,5,6,7,8,13, 14,19,20)]
.all2$Bw <- rowSums(abs(.all2[,2:10]), na.rm = TRUE)/9
.all2$weights <- 1/.all2$Bw
x <- .all2[,c(1,12)]

finding2018 <- merge(Data.2018[-20,], x, by = "Pollster", all.x = TRUE)
finding2018

##
election.2 <- numeric(0)
for (i in 4:8) {#Final vote prediction of 2018 election for each candidate using 2018 weights
  election.2[i-3] <- sum(finding2018[,i]*finding2018$weights)/sum(finding2018$weights)
}

weightsss <- data.frame(name = finding2018$Pollster, value = finding2018$weights)

ggplot(weightsss, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Create data
election.2 <- data.frame(
  name=c("Erdogan","Ince","Aksener","Demirtas","Other") ,  
  value=election.2
)

# Barplot of prediction
ggplot(election.2, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

election.2

#Try on 2023
polls2023 <- read_excel("2023election.xlsx")

lowest <- min(finding2018$weights)

Bw.2023 <- c(lowest, 2.05, lowest, lowest, lowest, lowest, lowest, 4.68, 4.633774, 3.796625,  lowest )
polls2023$Bw <- Bw.2023

last.weights <- data.frame(name = polls2023$Pollster, value = Bw.2023)

ggplot(last.weights, aes(x=name, y=value)) + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

election.2023 <- numeric(0)
for (i in 4:7) {#Final vote prediction of 2023 election for each candidate 
  election.2023[i-3] <- sum(polls2023[,i]*polls2023$Bw)/sum(polls2023$Bw)
}

election.2023 <- data.frame(
  name=c("Erdogan","Ince","KK","Ogan") ,  
  vote=election.2023
)

# Barplot of prediction
ggplot(election.2023, aes(x=name, y=vote)) + 
  geom_bar(stat = "identity")

election.2023

#Second round prediction
X2_tur <- read_excel("2.tur.xlsx")
round2.weights <- c(4.395114, 3.687756,  lowest, lowest, lowest, lowest,5.668701,  4.648710, lowest, lowest, lowest, 6.353835, lowest, lowest )
X2_tur$weights <- round2.weights
last.weights.2 <- data.frame(name = X2_tur$Pollster, value = X2_tur$weights)

ggplot(last.weights.2, aes(x=name, y=value)) + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

election.2023.2 <- numeric(0)
for (i in 3:4) {#Final vote prediction of 2023 election for each candidate 
  election.2023.2[i-2] <- sum(X2_tur[,i]*X2_tur$weights)/sum(X2_tur$weights)
}

election.2023.2 <- data.frame(
  name=c("Erdogan","KK") ,  
  vote=election.2023.2
)
# Barplot of prediction
ggplot(election.2023.2, aes(x=name, y=vote)) + 
  geom_bar(stat = "identity")
election.2023.2


#Method-2-------------------------------------------------------------------------------
library(dplyr)
library(readxl)
library(tidyverse)
library(knitr)

sheets <- c("2018", "2023","2023.2 tur" )
dataset <- lapply(sheets, function(sheet) {
  read_excel("secim_data.xlsx", sheet = sheet)
})
dataset

data_2018 <- dataset[[1]]
kable(data_2018)
election_result<- data_2018 %>%
  filter(Pollster == "Election Result")


data_2018 <- data_2018 %>%
  mutate(error_RTE =abs( data_2018$Erdogan - election_result$Erdogan),
         error_Ince =abs(data_2018$Ince - election_result$Ince),
         error_Aksener = abs(data_2018$Aksener - election_result$Aksener),
         error_Demirtas =abs(data_2018$Demirtas - election_result$Demirtas))%>%
  mutate(mean_error_2018 = rowMeans(.[c("error_RTE", "error_Ince", "error_Aksener", "error_Demirtas")]))

data_2018_errors <- data_2018 %>%
  select(Pollster, mean_error_2018)

kable(data_2018_errors)

data_2023 <- dataset[[2]]
data_2023$Ýnce <- as.numeric(data_2023$Ýnce)
data_2023$Ýnce[is.na(data_2023$Ýnce)] <- 0
kable(data_2023)
election_result<- data_2023 %>%
  filter(Pollster == "Election Result")


data_2023 <- data_2023 %>%
  mutate(error_RTE = abs(data_2023$Erdoðan - election_result$Erdoðan),
         error_Ince = abs(data_2023$Ýnce - election_result$Ýnce),
         error_Kýlýcdaroglu = abs(data_2023$Kýlýçdaroðlu - election_result$Kýlýçdaroðlu),
         error_Ogan = abs(data_2023$Oðan - election_result$Oðan)) %>%
  mutate(mean_error_2023 = rowMeans(.[c("error_RTE", "error_Ince", "error_Kýlýcdaroglu", "error_Ogan")]))

data_2023_errors <- data_2023 %>%
  select(Pollster, mean_error_2023)

kable(data_2023_errors)

combined_data <- full_join(data_2018_errors, data_2023_errors, by = "Pollster") %>%
  mutate(mean_error_2018 = coalesce(mean_error_2018, 0),
         mean_error_2023 = coalesce(mean_error_2023, 0))
combined_data

combined_data_1 <- combined_data[-1,] %>%
  rowwise() %>%
  mutate(mean_error = ifelse(mean_error_2018 != 0 & mean_error_2023 != 0, (mean_error_2018 + mean_error_2023) / 2,
                             ifelse(mean_error_2018 != 0, mean_error_2018, mean_error_2023))) %>%
  ungroup()
kable(combined_data_1)

data_2023_2 <- dataset[[3]]
data_2023_2_polls <- data_2023_2 %>%
  select(Pollster)
kable(data_2023_2)

matched_polls <- combined_data_1 %>%
  filter(Pollster %in% unlist(data_2023_2_polls))

weights <- 1 / matched_polls$mean_error
weights <- weights / sum(weights)
weights

matched_polls <- matched_polls %>%
  mutate( weights = weights)
kable(matched_polls)

data_2023_2 <- data_2023_2 %>%
  filter(Pollster %in% unlist(matched_polls$Pollster))%>%
  full_join(matched_polls, by = "Pollster")

kable(data_2023_2 )

#Accuracy test on 2023 election 1st term

library(ggplot2)
data_2023_pred <- data_2023 %>%
  filter(Pollster %in% unlist(matched_polls$Pollster))%>%
  full_join(matched_polls, by = "Pollster")

ggplot(data_2023_pred, aes(Pollster, weights)) +
  geom_col()+
  labs(title = "Histogram of Weights by Pollster",
       x = "Pollster",
       y = "Weight")

RTE_pred_1 <- data_2023_pred %>%
  summarise(vote_share=sum(Erdoðan * weights))%>%
  mutate(candidate = "Erdoðan")%>%
  select(candidate, vote_share)

Kýlýçdaroðlu_pred_1 <- data_2023_pred %>%
  summarise(vote_share=sum(Kýlýçdaroðlu * weights))%>%
  mutate(candidate = "Kýlýçdaroðlu")%>%
  select(candidate, vote_share)

Oðan_pred_1 <- data_2023_pred %>%
  summarise(vote_share=sum(Oðan * weights))%>%
  mutate(candidate = "Oðan")%>%
  select(candidate, vote_share)

Ýnce_pred_1 <- data_2023_pred %>%
  summarise(vote_share=sum(Ýnce * weights))%>%
  mutate(candidate = "Ýnce")%>%
  select(candidate, vote_share)

forecast_2023_1 <- rbind(RTE_pred_1,Ýnce_pred_1,Kýlýçdaroðlu_pred_1,Oðan_pred_1)
election_result_23 <- election_result[, c( "Erdoðan", "Ýnce", "Kýlýçdaroðlu", "Oðan")]
election_result_23 <- as.data.frame(t(election_result_23))

kable(forecast_2023_1)
kable(election_result_23)

ggplot(forecast_2023_1, aes(candidate, vote_share)) +
  geom_col()+
  labs(title = "Histogram of Vote Share Forecast by Candidates",
       x = "Candidates",
       y = "Vote Share")

Error <- forecast_2023_1 %>%
  summarise(error = mean(abs(vote_share-election_result_23[,1] )))
kable(Error)

#Prediction for 2023 election 2nd term
RTE_pred <- data_2023_2 %>%
  summarise(vote_share=sum(Erdoðan * weights))%>%
  mutate(candidate = "Erdoðan")%>%
  select(candidate, vote_share)

Kýlýçdaroðlu_pred <- data_2023_2 %>%
  summarise(vote_share=sum(Kýlýçdaroðlu * weights))%>%
  mutate(candidate = "Kýlýçdaroðlu")%>%
  select(candidate, vote_share)

forecast_2023_2 <- rbind(RTE_pred,Kýlýçdaroðlu_pred)
kable(forecast_2023_2)




