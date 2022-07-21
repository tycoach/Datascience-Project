
##Loading Packages 
library(flexdashboard)
library(knitr)
library(shiny)
library(plotly)
library(tidyverse)
library(DT)
library(GGally)
library(caret)

library(ggplot2)
library(viridis)
library(hrbrthemes)
library(plotly)

#Importing Data 

str(loan_data)
loan_data<- read.csv("Loan (5).csv")
Independent_variables<- c("Age","Experience","CCAvg","Mortage" )
Binary_Cat_Var <- c("PersonalLoan", "SecuritiesAccount", "CDAccount", "Online", "CreditCard")
Categorical_Var <- c("Family", "Education")


##Exploratory  Data Analysis 
head(loan_data)
colSums(is.na(loan_data))
summary(loan_data$Age)
#Average Age = 45
#mean Age= 45.34
#Max Age = 67.00
which(loan_data$Experience < 0)

#Handling Negative  Value in The Experience Variable
loan_data <- loan_data%>%
mutate(Experience = ifelse(Experience<0,0,Experience))

#Converting some Variables To Factors 
loan_data <- loan_data %>% 
  mutate(PersonalLoan = as.factor(PersonalLoan), SecuritiesAccount = as.factor(SecuritiesAccount),
         CDAccount = as.factor(CDAccount), Online = as.factor(Online), Family = as.factor(Family),
         CreditCard = as.factor(CreditCard), Education = as.factor(Education)) 

summary(loan_data$Experience)
str(loan_data)
summary(loan_data)

prop.table(table(loan_data$PersonalLoan))

#DATA VISUALIZATION



#Checking Multicollinearity
ggcorr(loan_data, label = T)
#Experience and Age have strong correlation 

#Plotting Age VS Experience 
plot( loan_data$Age , loan_data$Experience ,xlab = "Age" , ylab =  "Experience" )
#There is a Strong Correlation with Age and Experience 


str(loan1)
loan1 <- loan_data%>%
  filter(PersonalLoan==1)

summary(loan1)
hist(loan1$Income, breaks = 100, xlab = "income")

##Experience
loan_data%>%
  ggplot( aes(x=Experience)) +
  geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

#Income 
ggplot(loan_data, aes(x=Income, y= , fill=Age)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")

summary(loan_data$Income)
table(loan_data$Income>150)
 plot_ly(loan_data, x = ~Income, color = ~Family, type = "box")
 
#442 person earned above 150k per annum 
#an average person earn about 63k


ggplot(loan_data, aes(x= PersonalLoan, fill=as.factor(PersonalLoan) )) +  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")
#Lesser People Took Personal Loan In the Last Campaign 
#480 People took the loan out of 4520
# so therefore 9.417% of the population took the loan 
summary(loan_data$Online)
barp<-ggplot(loan_data, 
       aes(x = Online, 
           fill = PersonalLoan )) + 
  geom_bar(start = "Identity")
ggplotly(barp)
#2016 persons does not use  the internet

#Experience VS Income 
ggplot(loan_data, 
       aes(x = Experience, 
           y = Income)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 300)) +
  scale_x_continuous(breaks = seq(0, 60, 10), 
                     limits=c(0, 50)) + 
  labs(x = "Experience",
       y = "Income",
       title = "Experience vs. Income")
#Income Vs Education
plot_ly(loan_data, x = ~Education, y= ~ Income ,color = ~Education, type = "box")
#People who are Undergraduates Tends to earn higher than the rest

ggplot(loan_data, aes( x = Online, y= Mortgage, fill = )) +
  geom_boxplot()


table(loan_data$Mortgage>600)

ggplot(loan_data, aes( x = Family, y= Income, fill = PersonalLoan)) +
  geom_boxplot()
#People who took the personal loan have more access to the internet than who didnt : By the difference of  3 people  


ggplot(loan_data, aes( x = Education, y= Age)) +
  geom_boxplot() + coord_flip()
#Age Vs Education
loan_data%>%
plot_ly(x = ~Education, y = ~Age, type = "box" , color = ~Education)

#They are evenly Distributed 



