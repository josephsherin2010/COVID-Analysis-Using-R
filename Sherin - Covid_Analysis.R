setwd("C:\\D Drive\\Data Science\\4. R\\Project")
getwd()

rm(list = ls())
original_Covid_Df <- read.csv("covid_dataset.csv")

Covid_Df <- original_Covid_Df #taking a backup

#######################################
#Getting familiar with data
#######################################

dim(Covid_Df) #shape of the dataframe

summary(Covid_Df) #summary of dataframe

sum(duplicated(Covid_Df)) #number of duplicated observations

Covid_Df[Covid_Df=='']<-NA #converting Null to NA

dim(Covid_Df) #shape of the dataframe

summary(Covid_Df) #summary of dataframe

colnames(Covid_Df) #column Names

str(Covid_Df) #structure of data

Covid_Df$Date <- as.Date(Covid_Df$Date) #Converting the date column to Date format

str(Covid_Df) #structure of data

sapply(Covid_Df, class)  # show classes of all columns

head(Covid_Df) # Checking the head of the dataset

tail(Covid_Df) # Checking the tail of the dataset

Covid_Df<-head(Covid_Df,-1) #removed the last column because of outlier
tail(Covid_Df) 

colSums(is.na(Covid_Df)) # number of missing values:

round(colMeans(is.na(Covid_Df))*100,2)#2:digit=2

Covid_Df_WithoutNA <- na.omit(Covid_Df) #Eliminating rows with NA


Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA

#Here the "Vulnerability" column is the target because, based on the covid cases, deaths and 
#recovered we should be able to predict the state to be in particular vulnerability category

#Q1.what is the distribution of target(Vulnerability)? **************************************

#how many missing values we have for Vulnerability
sum(is.na(Covid_Df$Vulnerability))

#since target is categorical variable, in univariate Analysis for summarization
tbl<-table(Covid_Df$Vulnerability)
addmargins(table(Covid_Df$Vulnerability)) #Gives row and column wise sum
prop.table(table(Covid_Df$Vulnerability)) #Gives probability of each item

# Pie Chart with Percentages
tbl<-table(Covid_Df$Vulnerability)
tbl
freq1 <- c(tbl[1], tbl[2],tbl[3])
lbls <- c("Critical", "Low", "Moderate")
pct <- round(freq1/sum(freq1)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Vulnerability")

#Q2.what is the distribution of States? ***********************************************

Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA

sum(is.na(Covid_Df$State))
r1<-which(is.na(Covid_Df$State))
Covid_Df<-Covid_Df[-r1,]

#since target is categorical variable, in univariaite Analysis for summarization

tbl<-table(Covid_Df$State)
prop.table(table(Covid_Df$State)) #Gives probability of each item
tbl

# Simple Bar Plot

barplot(c(tbl[1], tbl[2],tbl[3],tbl[4],tbl[5],tbl[6]), main="State Distribution",
        ylab="Number",col = rainbow(length(tbl)),horiz = FALSE,ylim=c(0,120))

#Q3.what is the distribution of cases reported in all states ***********************
Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA

sum(is.na(Covid_Df$Cases))

r1<-which(is.na(Covid_Df$Cases))
Covid_Df<-Covid_Df[-r1,]

mean(Covid_Df$Cases,na.rm=TRUE,trim=0.1) # trim the 10% percent from each end

median(Covid_Df$Cases,na.rm=TRUE)

quantile(Covid_Df$Cases,na.rm=TRUE)

install.packages('ggplot2') 	# Installation 
library(ggplot2) 	

ggplot(Covid_Df, aes(x=Cases)) + 
  geom_boxplot(fill="gray")+
  labs(title="Distribution of Cases Reported",x="Cases", y = "")+
  theme_classic()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=1)

eliminate_outliers <- Covid_Df[order(Covid_Df$Cases),] #ordered by Cases

eliminate_outliers<-head(eliminate_outliers,-100)

ggplot(eliminate_outliers, aes(x=Cases)) + 
  geom_boxplot(fill="gray")+
  labs(title="Distribution of Cases Reported",x="Cases", y = "")+
  theme_classic()+geom_boxplot(outlier.colour="red", outlier.shape=8,
                               outlier.size=1)

#Q4.what is the pattern of recovery reported in all states ***********************

Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA

sum(is.na(Covid_Df$Recovered))

r1<-which(is.na(Covid_Df$Recovered))
Covid_Df<-Covid_Df[-r1,]

mean(Covid_Df$Recovered,na.rm=TRUE)

quantile(Covid_Df$Recovered, c(0.2,0.7,0.9),na.rm = T) #taking desired percentage

min(Covid_Df$Recovered) #-123 which is wrong

minValue <- min(Covid_Df$Recovered)
r1<-which(Covid_Df$Recovered==min(Covid_Df$Recovered))#which returns row numbers

Covid_Df[r1,]$Recovered <- min(Covid_Df$Recovered) * -1 #corrected the value in recovery for that row

summary(Covid_Df$Recovered)

qplot(Recovered, data = Covid_Df, geom = "histogram",ylab="Count", fill=I("green"), col=I("black"))

tail(Covid_Df[order(Covid_Df$Recovered),]) #shows the highest values of Recovered count


#Q5.what is the total death in all states ***********************
Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA

summary(Covid_Df$Deaths)

sum(is.na(Covid_Df$Deaths))

max(Covid_Df$Deaths,na.rm = T) #potential outlier

which(Covid_Df$Deaths == max(Covid_Df$Deaths,na.rm = T))

Covid_Df_WithOutOutlier <- Covid_Df[-which(Covid_Df$Deaths == max(Covid_Df$Deaths,na.rm = T)),] #removed outlier row

summary(Covid_Df_WithOutOutlier$Deaths)

sum(Covid_Df_WithOutOutlier$Deaths,na.rm = T) #total deaths in all states

ggplot(Covid_Df_WithOutOutlier, aes(x = Deaths)) +
  geom_density(fill="red", color="black", alpha=2)

#Q6.what is the relation between states and Vulnerability Level***********************
Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA
sum(is.na(Covid_Df$State)) #13 rows
which(is.na(Covid_Df$State))
temp_vec <- which(is.na(Covid_Df$State)) #collecting index of the rows with states as NA
for(i in temp_vec){
  x<- i-1
  Covid_Df[i,"State"] <- Covid_Df[x,"State"]
}
sum(is.na(Covid_Df$State)) #0 rows

#summerization of both continuous variables ---> contingency table (two-way table)
cont_tble <- table(Covid_Df$State,Covid_Df$Vulnerability)
addmargins(table(Covid_Df$State,Covid_Df$Vulnerability)) #Gives row and column wise sum
prop.table(xtabs(~Covid_Df$State+Covid_Df$Vulnerability)) #Gives probability of each combination

#Visualization ---> grouped bar plot
par(mar = c(4, 2, 2, 3))
barplot(t(cont_tble), main="States vs Vulnerablity Level",
        col=c("red","green","darkblue"),
        beside=TRUE,legend = rownames(t(cont_tble)))

#Test of independence --> chi-squared test
chisq.test(cont_tble)

#As the p-value 3.508e-15 is less than the .05 significance level, 
#we reject the null hypothesis and so there is association between the states 
# and the level of vulnerability at 5% significant level

#Q7.Is there relation between the month and Vulnerability Level***********************
Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA
summary(Covid_Df$Date) #character
sum(is.na(Covid_Df$Date)) #13 rows
which(is.na(Covid_Df$Date))
Covid_Df <- Covid_Df[-which(is.na(Covid_Df$Date)),] #removing DF rows with dates with NA
Covid_Df$Date <- as.Date(Covid_Df$Date)
Extract_Months <- function(x){
  format(x,"%b")
}
Covid_Df["Month"] <- sapply(Covid_Df$Date,Extract_Months)
levels(as.factor(Covid_Df$Month)) #gives 3 levels {"Oct","Nov" and "Dec"}

#summerization of both categorical variables ---> contingency table (two-way table)
cont_tble <- table(Covid_Df$Vulnerability,Covid_Df$Month)
addmargins(table(Covid_Df$Vulnerability,Covid_Df$Month)) #Gives row and column wise sum
prop.table(xtabs(~Covid_Df$Vulnerability+Covid_Df$Month)) #Gives probability of each combination

#Visualization ---> grouped bar plot
colours()
par(mar = c(3, 5, 2, 3))
barplot(cont_tble, main="Month vs Vulnerablity Level",
        col=c("violetred","springgreen3","skyblue2"),
        beside=TRUE,legend = rownames(cont_tble),ylab = "Count")

#Test of independency --> chi-squared test
chisq.test(cont_tble)
#As the p-value 0.3928 is greater than the .05 significance level, we accept the null hypothesis 
#and so there is no association between the months and the level of vulnerability.


#Q8.Is there relation between the Cases and Recovery***********************
Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA
summary(Covid_Df$Cases)
sum(is.na(Covid_Df$Cases)) #12 rows
which(is.na(Covid_Df$Cases))
temp_case_vec <- which(is.na(Covid_Df$Cases))
col_names <- colnames(Covid_Df) #gives column names

get.adj.count <- function (y,colName){ #function to return the count of adjacent row of same column
  count <- 0
  if(!is.na(y) & y <= nrow(Covid_Df)){
    count <- count + Covid_Df[y,colName]
  }else{
    count
  }
}
for(x in temp_case_vec){
  adjuscent_values <- c(x-1,x-2,x-3,x+1,x+2,x+3) #6 adjuscent values
  count <- 0
  Counter <- 0
  for(y in adjuscent_values){
      prevCount <- get.adj.count(y,col_names[3])
      if(prevCount == 0){
        Counter <- Counter -1
      }
      count <- count + prevCount
      Counter <- Counter +1
      Covid_Df[x,"Cases"] <- round(count/Counter,2) #updating the NA row with average value
  }
}

sum(is.na(Covid_Df$Cases)) # 0 rows

sum(is.na(Covid_Df$Recovered)) # 20 rows

temp_Rec_vec <- which(is.na(Covid_Df$Recovered)) #returns index of the recovered with NA

for(z in temp_Rec_vec){
  adjuscent_rec_values <- c(z-1,z-2,z-3,z+1,z+2,z+3) #6 adjuscent values
  count <- 0
  Counter <- 0
  for(y in adjuscent_rec_values){
      prevCount <- get.adj.count(y,col_names[5])
      if(prevCount == 0){
        Counter <- Counter -1
      }
      count <- count + prevCount
      Counter <- Counter +1
      Covid_Df[z,"Recovered"] <- round(count/Counter,2) #updating the NA row with average value
  }
}
sum(is.na(Covid_Df$Recovered)) # 0 rows

#Visualization ---> scatter plot
par(mar = c(4, 4, 2, 3))
plot(Covid_Df$Cases, Covid_Df$Recovered, type = "p", #"p" for points
     main = "Cases vs. Recovered",#an overall title for the plot
     xlab = "Cases", ylab = "Recovered",col=14,pch=20)

#Test of independency ---> Correlation between them
cor(Covid_Df$Cases,Covid_Df$Recovered) #default method = "pearson"
cor(Covid_Df$Cases,Covid_Df$Recovered,method = "spearman")
#Both the correlations gives high correlation values 0.9977609 and 0.9804133 anf hence
#both cases and recovery have high positive correlation

#Q9.Is there relation between the Deaths and Recovery***********************

Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA
summary(Covid_Df$Deaths) #gives last value as unrealistic
Covid_Df[which.max(Covid_Df$Deaths),"Deaths"] <- NA #Converter outlier to NA
sum(is.na(Covid_Df$Deaths)) #10 rows

temp_de_vec <- which(is.na(Covid_Df$Deaths)) #assign rows index of NA of that column to a variable
col_names <- colnames(Covid_Df) #gives column names

for(x in temp_de_vec){
  adjuscent_values <- c(x-1,x-2,x-3,x+1,x+2,x+3) #6 adjuscent values
  count <- 0
  Counter <- 0
  for(y in adjuscent_values){
    prevCount <- get.adj.count(y,col_names[4])
    if(prevCount == 0){
      Counter <- Counter -1
    }
    count <- count + prevCount
    Counter <- Counter +1
    Covid_Df[x,"Deaths"] <- round(count/Counter,2) #updating the NA row with average value
  }
}
sum(is.na(Covid_Df$Deaths)) # 0 rows

sum(is.na(Covid_Df$Recovered)) # 20 rows
temp_Rec_vec <- which(is.na(Covid_Df$Recovered)) #returns index of the recovered with NA
for(z in temp_Rec_vec){
  adjuscent_rec_values <- c(z-1,z-2,z-3,z+1,z+2,z+3) #6 adjuscent values
  count <- 0
  Counter <- 0
  for(y in adjuscent_rec_values){
    prevCount <- get.adj.count(y,col_names[5])
    if(prevCount == 0){
      Counter <- Counter -1
    }
    count <- count + prevCount
    Counter <- Counter +1
    Covid_Df[z,"Recovered"] <- round(count/Counter,2) #updating the NA row with average value
  }
}
sum(is.na(Covid_Df$Recovered)) # 0 rows

#Visualization ---> scatter plot
install.packages("lattice")# Install
library("lattice")# Load
xyplot(Deaths ~ Recovered, data = Covid_Df)

#Test of independency ---> Correlation between them
cor(Covid_Df$Deaths,Covid_Df$Recovered) #default method = "pearson"
cor(Covid_Df$Deaths,Covid_Df$Recovered,method = "spearman")
#Both the correlations gives high correlation values 0.9454808 and 0.8069523 and hence
#both death and recovery have high positive correlation

#Q10.Is there relation between the Cases and Vulnerability***********************

Covid_Df <- original_Covid_Df #taking a backup
Covid_Df[Covid_Df=='']<-NA #converting Null to NA
levels(as.factor(Covid_Df$Vulnerability)) #gives 3 levels {"Critical","Low" and "Moderate"}
sum(is.na(Covid_Df$Cases)) #12 rows
temp_case_vec <- which(is.na(Covid_Df$Cases))

for(x in temp_case_vec){
  adjuscent_values <- c(x-1,x-2,x-3,x+1,x+2,x+3) #6 adjuscent values
  count <- 0
  Counter <- 0
  for(y in adjuscent_values){
    prevCount <- get.adj.count(y,col_names[3])
    if(prevCount == 0){
      Counter <- Counter -1
    }
    count <- count + prevCount
    Counter <- Counter +1
    Covid_Df[x,"Cases"] <- round(count/Counter,2) #updating the NA row with average value
  }
}
sum(is.na(Covid_Df$Cases)) #0 rows
sum(is.na(Covid_Df$Vulnerability)) #0 rows

r1<- which(Covid_Df$Vulnerability == "Critical")
Covid_Df[r1,"LockDown"]<- "Yes"

r2<- which(is.na(Covid_Df$LockDown))
Covid_Df[r2,"LockDown"]<- "No"

#summerization ---> summary by (aggregate fn of Cases and group by LockDown)
fx <- function(x){
  c(average=mean(x,na.rm=T), minimum=min(x,na.rm=T), max=max(x,na.rm=T))
}

#tapply(Covid_Df$Cases,Covid_Df$Vulnerability,FUN= fx)
aggregate(Cases ~ LockDown,data=Covid_Df,FUN= fx)

#Visualization ---> group histogram
p<-ggplot(Covid_Df, aes(x=Cases, fill=LockDown, color=LockDown)) +
  geom_histogram(position="identity", alpha=0.5)

# Add mean lines
library(plyr)
mu <- ddply(Covid_Df, "LockDown", summarise, group.mean=mean(Cases,na.rm=T))
mu #It will have the mean values in each group
p<-p+geom_vline(data=mu, aes(xintercept=group.mean, color=LockDown),linetype="dashed")
p<-p+scale_color_brewer(palette="Dark2")+scale_fill_brewer(palette="Dark2")

#Test of independence ---> t-test
t.test(Cases ~ LockDown, data=Covid_Df) #two levels "Yes" or "No"
# So because p-value 1.414e-07<0.05 , we reject null hypotheses and 
# get this conclusion that there is a difference between mean of both categories of lock down
# at 5% significant level

