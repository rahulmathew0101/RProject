df <- read.csv('~/Documents/Udemy/Data Science with R/AutoInsurance.csv')

head(df)
str(df)

unique(df$State)
unique(df$Education)

df$State <- factor(df$State)
df$Response <- factor(df$Response)
df$Coverage <- factor(df$Coverage)
df$Education <- factor(df$Education)
df$EmploymentStatus <- factor(df$EmploymentStatus)
df$Gender <- factor(df$Gender)
df$Location.Code <- factor(df$Location.Code)
df$Marital.Status <- factor(df$Marital.Status)
df$Number.of.Open.Complaints <- factor(df$Number.of.Open.Complaints)
df$Number.of.Policies <- factor(df$Number.of.Policies)
df$Policy.Type <- factor(df$Policy.Type)
df$Policy <- factor(df$Policy)
df$Renew.Offer.Type <- factor(df$Renew.Offer.Type)
df$Sales.Channel <- factor(df$Sales.Channel)
df$Vehicle.Class <- factor(df$Vehicle.Class)
df$Vehicle.Size <- factor(df$Vehicle.Size)


library(lubridate)

df$Effective.To.Date <- mdy(df$Effective.To.Date)
df$Effective.To.Month <- month(df$Effective.To.Date)
df <- subset(df, select = -c(Customer, Effective.To.Date))
df$Effective.To.Month <- as.factor(df$Effective.To.Month)

summary(df) 

colSums(is.na(df))

#### Checking For Multi-Collinearity ####

library(corrplot)

num.cols <- sapply(df, is.numeric)
corr.data <- cor(df[, num.cols])
corrplot(corr.data, method = 'color', type = 'lower', addCoefasPercent = T)

#### EDA ####

### 1-D Plots ###

## Density Plots ##

num.col.names <- names(num.cols[num.cols == T])
df.num <- df[, num.col.names]
head(df.num)

library(gridExtra)
library(ggplot2)

den.list <- list()

for (n in num.col.names){
  den <- ggplot(df.num, aes_string(n)) + geom_density(color = 'black',fill = 'lightblue')
  den.list[[n]] <- den
}

grid.arrange(grobs = den.list, ncol = 3)

## Bar Plots ##

cat.col.names <- names(num.cols[num.cols == F])
df.cat <- df[, cat.col.names]
head(df.cat)

bar.list <- list()

for (b in cat.col.names){
  bar <- ggplot(df.cat, aes_string(b)) + geom_bar(color = 'black', fill = 'lightblue')
  bar.list[[b]] <- bar
}

grid.arrange(grobs = bar.list, ncol = 3)

### 2-D Plots ###

# Customer Lifetime Value

den.1 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ State, ncol = 3)  + labs(title = 'Customer.Lifetime.Value v/s State')
den.1

den.2 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ Coverage, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Coverage')
den.2

den.3 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ Education, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Education')
den.3

den.4 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ EmploymentStatus, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Employment Status')
den.4

den.5 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ Location.Code, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Location Code')
den.5

den.6 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ Marital.Status, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Marital Status')
den.6

den.7 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ Number.of.Open.Complaints, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Number of open complaints')
den.7

den.8 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ Number.of.Policies, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Number of policies')
den.8

den.9 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
         facet_wrap(~ Policy.Type, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Policy Type')
den.9

den.10 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Renew.Offer.Type, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Renew Offer Type')
den.10

den.11 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Sales.Channel, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Sales Channel')
den.11

den.12 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Class, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Vehicle Class')
den.12

den.13 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Size, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Vehicle Size')
den.13

den.14 <- ggplot(df, aes(Customer.Lifetime.Value)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Effective.To.Month, ncol = 3) + labs(title = 'Customer.Lifetime.Value v/s Effective to Month')
den.14

# Income

den.15 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ State, ncol = 3)  + labs(title = 'Income v/s State')
den.15

den.16 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Coverage, ncol = 3) + labs(title = 'Income v/s Coverage')
den.16

den.17 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Education, ncol = 3) + labs(title = 'Income v/s Education')
den.17

den.18 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ EmploymentStatus, ncol = 3) + labs(title = 'Income v/s Employment Status')
den.18

den.19 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Location.Code, ncol = 3) + labs(title = 'Income v/s Location Code')
den.19

den.20 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Marital.Status, ncol = 3) + labs(title = 'Income v/s Marital Status')
den.20

den.21 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Open.Complaints, ncol = 3) + labs(title = 'Income v/s Number of open complaints')
den.21

den.22 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Policies, ncol = 3) + labs(title = 'Income v/s Number of policies')
den.22

den.23 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Policy.Type, ncol = 3) + labs(title = 'Income v/s Policy Type')
den.23

den.24 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Renew.Offer.Type, ncol = 3) + labs(title = 'Income v/s Renew Offer Type')
den.24

den.25 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Sales.Channel, ncol = 3) + labs(title = 'Income v/s Sales Channel')
den.25

den.26 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Class, ncol = 3) + labs(title = 'Income v/s Vehicle Class')
den.26

den.27 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Size, ncol = 3) + labs(title = 'Income v/s Vehicle Size')
den.27

den.28 <- ggplot(df, aes(Income)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Effective.To.Month, ncol = 3) + labs(title = 'Income v/s Effective to Month')
den.28

# Monthly Premium Auto #

den.29 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ State, ncol = 3)  + labs(title = 'Monthly.Premium.Auto v/s State')
den.29

den.30 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Coverage, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Coverage')
den.30

den.31 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Education, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Education')
den.31

den.32 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ EmploymentStatus, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Employment Status')
den.32

den.33 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Location.Code, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Location Code')
den.33

den.34 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Marital.Status, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Marital Status')
den.34

den.35 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Open.Complaints, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Number of open complaints')
den.35

den.36 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Policies, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Number of policies')
den.36

den.37 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Policy.Type, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Policy Type')
den.37

den.38 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Renew.Offer.Type, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Renew Offer Type')
den.38

den.39 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Sales.Channel, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Sales Channel')
den.39

den.40 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Class, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Vehicle Class')
den.40

den.41 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Size, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Vehicle Size')
den.41

den.42 <- ggplot(df, aes(Monthly.Premium.Auto)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Effective.To.Month, ncol = 3) + labs(title = 'Monthly.Premium.Auto v/s Effective to Month')
den.42

# Months Since Last Claim #

den.43 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ State, ncol = 3)  + labs(title = 'Months.Since.Last.Claim v/s State')
den.43

den.44 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Coverage, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Coverage')
den.44

den.45 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Education, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Education')
den.45

den.46 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ EmploymentStatus, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Employment Status')
den.46

den.47 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Location.Code, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Location Code')
den.47

den.48 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Marital.Status, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Marital Status')
den.48

den.49 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Open.Complaints, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Number of open complaints')
den.49

den.50 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Policies, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Number of policies')
den.50

den.51 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Policy.Type, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Policy Type')
den.51

den.52 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Renew.Offer.Type, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Renew Offer Type')
den.52

den.53 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Sales.Channel, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Sales Channel')
den.53

den.54 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Class, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Vehicle Class')
den.54

den.55 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Size, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Vehicle Size')
den.55

den.56 <- ggplot(df, aes(Months.Since.Last.Claim)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Effective.To.Month, ncol = 3) + labs(title = 'Months.Since.Last.Claim v/s Effective to Month')
den.56

# Months Since Policy Inception #

den.57 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ State, ncol = 3)  + labs(title = 'Months.Since.Policy.Inception v/s State')
den.57

den.58 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Coverage, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Coverage')
den.58

den.59 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Education, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Education')
den.59

den.60 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ EmploymentStatus, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Employment Status')
den.60

den.61 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Location.Code, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Location Code')
den.61

den.62 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Marital.Status, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Marital Status')
den.62

den.63 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Open.Complaints, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Number of open complaints')
den.63

den.64 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Policies, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Number of policies')
den.64

den.65 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Policy.Type, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Policy Type')
den.65

den.66 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Renew.Offer.Type, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Renew Offer Type')
den.66

den.67 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Sales.Channel, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Sales Channel')
den.67

den.68 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Class, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Vehicle Class')
den.68

den.69 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Size, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Vehicle Size')
den.69

den.70 <- ggplot(df, aes(Months.Since.Policy.Inception)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Effective.To.Month, ncol = 3) + labs(title = 'Months.Since.Policy.Inception v/s Effective to Month')
den.70

# Total Claim Amount #

den.71 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ State, ncol = 3)  + labs(title = 'Total.Claim.Amount v/s State')
den.71

den.72 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Coverage, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Coverage')
den.72

den.73 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Education, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Education')
den.73

den.74 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ EmploymentStatus, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Employment Status')
den.74

den.75 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Location.Code, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Location Code')
den.75

den.76 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Marital.Status, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Marital Status')
den.76

den.77 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Open.Complaints, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Number of open complaints')
den.77

den.78 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Number.of.Policies, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Number of policies')
den.78

den.79 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Policy.Type, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Policy Type')
den.79

den.80 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Renew.Offer.Type, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Renew Offer Type')
den.80

den.81 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Sales.Channel, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Sales Channel')
den.81

den.82 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Class, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Vehicle Class')
den.82

den.83 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Vehicle.Size, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Vehicle Size')
den.83

den.84 <- ggplot(df, aes(Total.Claim.Amount)) + geom_density(color = 'black', fill = 'lightblue') + 
          facet_wrap(~ Effective.To.Month, ncol = 3) + labs(title = 'Total.Claim.Amount v/s Effective to Month')
den.84


#### Data Pre-Processing ####

## Categorical Data Encoding ##

df.cat$Response <- ifelse(df.cat$Response == 'No', 0, 1)
df.cat$Gender <- ifelse(df.cat$Gender == 'M', 0, 1)

df.cat <- fastDummies::dummy_cols(df, select_columns = c('State', 'Coverage', 'Education', 
                                                         'EmploymentStatus', 'Location.Code', 
                                                         'Marital.Status', 'Number.of.Open.Complaints',
                                                         'Number.of.Policies', 'Policy.Type', 'Policy',
                                                         'Renew.Offer.Type', 'Sales.Channel', 'Vehicle.Class',
                                                         'Vehicle.Size', 'Effective.To.Month'))
df.cat <- subset(df.cat, select = -c(State, Response, Coverage, Education, 
                                     EmploymentStatus, Gender, Location.Code, 
                                     Marital.Status, Number.of.Open.Complaints,
                                     Number.of.Policies, Policy.Type, Policy,
                                     Renew.Offer.Type, Sales.Channel, Vehicle.Class,
                                     Vehicle.Size, Effective.To.Month))


## Scaling Numerical Columns ##

head(df.num)

df.num <- subset(df.num, select = -Total.Claim.Amount) # Removing Target Column

df.num$Customer.Lifetime.Value <- scale(df.num$Customer.Lifetime.Value, center = T, scale = T)
df.num$Income <- scale(df.num$Income, center = T, scale = T)
df.num$Monthly.Premium.Auto <- scale(df.num$Monthly.Premium.Auto, center = T, scale = T)
df.num$Months.Since.Last.Claim <- scale(df.num$Months.Since.Last.Claim, center = T, scale = T)
df.num$Months.Since.Policy.Inception <- scale(df.num$Months.Since.Policy.Inception, center = T, scale = T)

library(dplyr)

df.new <- bind_cols(df.num, df.cat)
df.new$Total.Claim.Amount <- df$Total.Claim.Amount

df.new$Total.Claim.Amount <- scale(df.new$Total.Claim.Amount, center = T, scale = T)

head(df.new)


############################## Machine Learning #########################################

## Linear Regression ##

library(caTools)

set.seed(101)

sample <- sample.split(df.new, SplitRatio = 0.7)
train <- subset(df.new, sample == T)
test <- subset(df.new, sample == F)

lr.model <- lm(Total.Claim.Amount ~ ., data = train)
summary(lr.model)

res <- residuals(lr.model)
res <- as.data.frame(res)

pl <- ggplot(res, aes(res)) + geom_histogram(color = 'black', fill = 'lightblue', alpha = 0.5)
pl

plot(lr.model)

lr.pred <- predict(lr.model, test[1:85])

results <- cbind(lr.pred, test$Total.Claim.Amount)
colnames(results) <- c('Predicted', 'Actual')
results <- as.data.frame(results)

results$Predicted <- (results$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results$Actual <- (results$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.lr <- mean((results$Actual - results$Predicted)^2)
mse.lr

rmse.lr <- sqrt(mse.lr)
rmse.lr

mae.lr <- mean(abs(results$Actual - results$Predicted))
mae.lr

r2.lr <- summary(lr.model)$r.squared
r2.lr 

adj_r2.lr <- summary(lr.model)$adj.r.squared
adj_r2.lr

rse.lr <- summary(lr.model)$sigma
rse.lr

mape.lr <- mean(abs((results$Actual - results$Predicted)/ results$Actual)) * 100
mape.lr

## Ridge Regression (L2 Regression) ##

library(glmnet)

x.train <- as.matrix(train[,-86])
y.train <- as.matrix(train[,86])

x.test <- as.matrix(test[,-86])
y.test <- as.matrix(test[,86])

ridge.model <- glmnet(x.train, y.train, alpha = 0, family = 'gaussian')

cv.ridge <- cv.glmnet(x.train, y.train, alpha = 0, family = 'gaussian')
plot(cv.ridge)
lambda.ridge <- cv.ridge$lambda.min
lambda.ridge

ridge.pred <- predict(ridge.model, s = lambda.ridge, newx = x.test)

results.ridge <- cbind(ridge.pred, test$Total.Claim.Amount)
colnames(results.ridge) <- c('Predicted', 'Actual')
results.ridge <- as.data.frame(results.ridge)

results.ridge$Predicted <- (results.ridge$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.ridge$Actual <- (results.ridge$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.ridge <- mean((results.ridge$Actual - results.ridge$Predicted)^2)
mse.ridge

rmse.ridge <- sqrt(mse.ridge)
rmse.ridge

mae.ridge<- mean(abs(results.ridge$Actual - results.ridge$Predicted))
mae.ridge

rss.ridge <- sum((results.ridge$Predicted - results.ridge$Actual) ^ 2)
tss.ridge <- sum((results.ridge$Actual - mean(results.ridge$Actual)) ^ 2)
r2.ridge <- 1 - rss.ridge/tss.ridge
r2.ridge

n <- nrow(test)
p <- ncol(test) - 1

adj_r2.ridge <- 1 - (((1-r2.ridge)*(n-1))/(n-p-1))
adj_r2.ridge

mape.ridge <- mean(abs((results.ridge$Actual - results.ridge$Predicted)/ results.ridge$Actual)) * 100
mape.ridge

## Lasso Regression ##

lasso.model <- glmnet(x.train, y.train, alpha = 1, family = 'gaussian')

cv.lasso <- cv.glmnet(x.train, y.train, alpha = 1, family = 'gaussian')
plot(cv.lasso)
lambda.lasso <- cv.lasso$lambda.min
lambda.lasso

lasso.pred <- predict(lasso.model, s = lambda.lasso, newx = x.test)

results.lasso <- cbind(lasso.pred, test$Total.Claim.Amount)
colnames(results.lasso) <- c('Predicted', 'Actual')
results.lasso <- as.data.frame(results.lasso)

results.lasso$Predicted <- (results.lasso$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.lasso$Actual <- (results.lasso$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.lasso <- mean((results.lasso$Actual - results.lasso$Predicted)^2)
mse.lasso

rmse.lasso <- sqrt(mse.lasso)
rmse.lasso

mae.lasso <- mean(abs(results.lasso$Actual - results.lasso$Predicted))
mae.lasso

rss.lasso <- sum((results.lasso$Predicted - results.lasso$Actual) ^ 2)
tss.lasso <- sum((results.lasso$Actual - mean(results.lasso$Actual)) ^ 2)
r2.lasso <- 1 - rss.lasso/tss.lasso
r2.lasso

n <- nrow(test)
p <- ncol(test) - 1

adj_r2.lasso <- 1 - (((1-r2.lasso)*(n-1))/(n-p-1))
adj_r2.lasso

mape.lasso <- mean(abs((results.lasso$Actual - results.lasso$Predicted)/ results.lasso$Actual)) * 100
mape.lasso

## Elastic-Net Regression ##

elnet.model <- glmnet(x.train, y.train, alpha = 0.5, family = 'gaussian')

cv.elnet <- cv.glmnet(x.train, y.train, alpha = 0.5, family = 'gaussian')
plot(cv.elnet)
lambda.elnet <- cv.elnet$lambda.min
lambda.elnet

elnet.pred <- predict(elnet.model, s = lambda.elnet, newx = x.test)

results.elnet <- cbind(elnet.pred, test$Total.Claim.Amount)
colnames(results.elnet) <- c('Predicted', 'Actual')
results.elnet <- as.data.frame(results.elnet)

results.elnet$Predicted <- (results.elnet$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.elnet$Actual <- (results.elnet$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.elnet <- mean((results.elnet$Actual - results.elnet$Predicted)^2)
mse.elnet

rmse.elnet <- sqrt(mse.elnet)
rmse.elnet

mae.elnet <- mean(abs(results.ridge$Actual - results.ridge$Predicted))
mae.elnet

rss.elnet <- sum((results.elnet$Predicted - results.elnet$Actual) ^ 2)
tss.elnet <- sum((results.elnet$Actual - mean(results.elnet$Actual)) ^ 2)
r2.elnet <- 1 - rss.elnet/tss.elnet
r2.elnet

n <- nrow(test)
p <- ncol(test) - 1

adj_r2.elnet <- 1 - (((1-r2.elnet)*(n-1))/(n-p-1))
adj_r2.elnet

mape.elnet <- mean(abs((results.elnet$Actual - results.elnet$Predicted)/ results.elnet$Actual)) * 100
mape.elnet

## Decision Tree Regressor ##

library(rpart)

tree.model <- rpart(Total.Claim.Amount ~ ., data = train, method = 'anova')

plotcp(tree.model)

tree.pred <- predict(tree.model, test[1:85])

results.tree <- cbind(tree.pred, test$Total.Claim.Amount)
colnames(results.tree) <- c('Predicted', 'Actual')
results.tree <- as.data.frame(results.tree)

results.tree$Predicted <- (results.tree$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.tree$Actual <- (results.tree$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.tree <- mean((results.tree$Actual - results.tree$Predicted)^2)
mse.tree

rmse.tree <- sqrt(mse.tree)
rmse.tree

mae.tree <- mean(abs(results.tree$Actual - results.tree$Predicted))
mae.tree

rss.tree <- sum((results.tree$Predicted - results.tree$Actual) ^ 2)
tss.tree <- sum((results.tree$Actual - mean(results.tree$Actual)) ^ 2)
r2.tree <- 1 - rss.tree/tss.tree
r2.tree

adj_r2.tree <- 1 - (((1-r2.tree)*(n-1))/(n-p-1))
adj_r2.tree

mape.tree <- mean(abs((results.tree$Actual - results.tree$Predicted)/ results.tree$Actual)) * 100
mape.tree

## Random Forest Regressor ##

library(randomForest)

names(train) <- make.names(names(train))
names(test) <- make.names(names(test))

rf.model <- randomForest(Total.Claim.Amount ~ ., data = train, ntree = 500, importance = T, mtry = 28)

rf.pred <- predict(rf.model, test[1:85])

results.rf <- cbind(rf.pred, test$Total.Claim.Amount)
colnames(results.rf) <- c('Predicted', 'Actual')
results.rf <- as.data.frame(results.rf)

results.rf$Predicted <- (results.rf$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.rf$Actual <- (results.rf$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.rf <- mean((results.rf$Actual - results.rf$Predicted)^2)
mse.rf

rmse.rf <- sqrt(mse.rf)
rmse.rf

mae.rf <- mean(abs(results.rf$Actual - results.rf$Predicted))
mae.rf

rss.rf <- sum((results.rf$Predicted - results.rf$Actual) ^ 2)
tss.rf <- sum((results.rf$Actual - mean(results.rf$Actual)) ^ 2)
r2.rf <- 1 - rss.rf/tss.rf
r2.rf

n <- nrow(test)
p <- ncol(test) - 1

adj_r2.rf <- 1 - (((1-r2.rf)*(n-1))/(n-p-1))
adj_r2.rf

mape.rf <- mean(abs((results.rf$Actual - results.rf$Predicted)/ results.rf$Actual)) * 100
mape.rf

## XGBoost Regressor ##

library(xgboost)

dtrain <- xgb.DMatrix(data = x.train, label = y.train)
dtest <- xgb.DMatrix(data = x.test, label = y.test)

params <- list(objective = 'reg:squarederror', eta = 0.1, 
               max_depth = 7, subsample = 0.8, colsample_bytree = 0.8)

xgb.model <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = F)

xgb.pred <- predict(xgb.model, dtest)

results.xgb <- cbind(xgb.pred, test$Total.Claim.Amount)
colnames(results.xgb) <- c('Predicted', 'Actual')
results.xgb <- as.data.frame(results.xgb)

results.xgb$Predicted <- (results.xgb$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.xgb$Actual <- (results.xgb$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.xgb <- mean((results.xgb$Actual - results.xgb$Predicted)^2)
mse.xgb

rmse.xgb <- sqrt(mse.xgb)
rmse.xgb

mae.xgb <- mean(abs(results.xgb$Actual - results.xgb$Predicted))
mae.xgb

rss.xgb <- sum((results.xgb$Predicted - results.xgb$Actual) ^ 2)
tss.xgb <- sum((results.xgb$Actual - mean(results.xgb$Actual)) ^ 2)
r2.xgb <- 1 - rss.xgb/tss.xgb
r2.xgb

n <- nrow(test)
p <- ncol(test) - 1

adj_r2.xgb <- 1 - (((1-r2.xgb)*(n-1))/(n-p-1))
adj_r2.xgb

mape.xgb <- mean(abs((results.xgb$Actual - results.xgb$Predicted)/ results.xgb$Actual)) * 100
mape.xgb

## Support Vector Regressor ##

library(e1071)

svm.model <- svm(Total.Claim.Amount ~ ., data = train, type = 'eps-regression', kernel = 'radial', cost = 10, epsilon = 0.1)

svm.pred <- predict(svm.model, test[1:85])

results.svm <- cbind(svm.pred, test$Total.Claim.Amount)
colnames(results.svm) <- c('Predicted', 'Actual')
results.svm <- as.data.frame(results.svm)

results.svm$Predicted <- (results.svm$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.svm$Actual <- (results.svm$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.svm <- mean((results.svm$Actual - results.svm$Predicted)^2)
mse.svm

rmse.svm <- sqrt(mse.svm)
rmse.svm

svm.tune <- tune(svm, train.x = train[1:85], train.y = train[, 86], kernel = 'radial', 
                 ranges = list(cost = c(0.1,1,10), epsilon = c(0.1,0.2,0.3,0.4,0.5)))
summary(svm.tune)

tuned_svm.model <- svm(Total.Claim.Amount ~ ., data = train, type = 'eps-regression', kernel = 'radial', cost = 1, epsilon = 0.1)

tuned_svm.pred <- predict(tuned_svm.model, test[1:85])

results.tuned_svm <- cbind(tuned_svm.pred, test$Total.Claim.Amount)
colnames(results.tuned_svm) <- c('Predicted', 'Actual')
results.tuned_svm <- as.data.frame(results.tuned_svm)

results.tuned_svm$Predicted <- (results.tuned_svm$Predicted * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)
results.tuned_svm$Actual <- (results.tuned_svm$Actual * sd(df$Total.Claim.Amount)) + mean(df$Total.Claim.Amount)

mse.tuned_svm <- mean((results.tuned_svm$Actual - results.tuned_svm$Predicted)^2)
mse.tuned_svm

rmse.tuned_svm <- sqrt(mse.tuned_svm)
rmse.tuned_svm

mae.tuned_svm <- mean(abs(results.tuned_svm$Actual - results.tuned_svm$Predicted))
mae.tuned_svm

rss.tuned_svm <- sum((results.tuned_svm$Predicted - results.tuned_svm$Actual) ^ 2)
tss.tuned_svm <- sum((results.tuned_svm$Actual - mean(results.tuned_svm$Actual)) ^ 2)
r2.tuned_svm <- 1 - rss.tuned_svm/tss.tuned_svm
r2.tuned_svm

n <- nrow(test)
p <- ncol(test) - 1

adj_r2.tuned_svm <- 1 - (((1-r2.tuned_svm)*(n-1))/(n-p-1))
adj_r2.tuned_svm

mape.tuned_svm <- mean(abs((results.tuned_svm$Actual - results.tuned_svm$Predicted)/ results.tuned_svm$Actual)) * 100
mape.tuned_svm


######## Final Scores #######

models <- c('Linear Regressor', 'Ridge', 'Lasso', 'Elastic Net', 'Decision Tree', 'Random Forest',
            'XGBoost', 'Tuned_SVM')

rmse_scores <- c(rmse.lr, rmse.ridge, rmse.lasso, rmse.elnet, rmse.tree, rmse.rf, rmse.xgb, 
                 rmse.tuned_svm)

r2_scores <- c(r2.lr, r2.ridge, r2.lasso, r2.elnet, r2.tree, r2.rf, r2.xgb, r2.tuned_svm)

adjusted_r2 <- c(adj_r2.lr, adj_r2.ridge, adj_r2.lasso, adj_r2.elnet, adj_r2.tree, adj_r2.rf,
                 adj_r2.xgb, adj_r2.tuned_svm)

mape.scores <- c(mape.lr, mape.ridge, mape.lasso, mape.elnet, mape.tree, mape.rf, mape.xgb, 
                 mape.tuned_svm)

score.df <- data.frame(Model = models, RMSE = rmse_scores, R_squared = r2_scores, 
                          Adjusted.R_Squared = adjusted_r2, MAPE = mape.scores)
score.df

# So, for our final model, we can choose Random forest, or XGBoost algorithm to find the target.
# We can improve on these performances by tuning the final models.
























































































