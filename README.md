# To-Evaluate-the-Global-Suicide-Rates
Suicide is the act of causing your own death intentionally, there are so many reasons for it. We are not discussing the reasons here but we will discuss how is this trend growing in numbers.  From 1985 to 2016 we will see suicide rates with respect to every country. And further we will see the relation of rich countries with the suicide cases. Now in this analysis we will see the global trend of suicide cases from 1985 to 2016.  We are using global suicide data per country from World Health Organization. And also, we are using country per capita data from World Bank to analyze which countries per capita shows what relations to suicide cases?
---
title: "R Notebook"
output: html_notebook
---

Project: "CIS 548 Final Project"
Title:   "To evaluate Global Suicide Rates." 
author: "Jahnavi Patchipalu"


#Install Packages

```{r}
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("caTools")
#install.packages("caret")
#install.packages("Metrics")
#install.packages("cli")
#install.packages("olsrr")
#install.packages("splines")
#install.packages("earth")
#install.packages("corrplot")

```


#Import libraries
```{r}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caTools)
library(caret)
library(cli)
library(splines)
library(plotly)
```

#Import Data
```{r}
Suicide_rate <-  read.csv("C:/Users/jaanu/Downloads/Suicide_dataset.csv",stringsAsFactors = F)
#view(Suicide_rate)

```

# Tidying the Data 
#changing data types
```{r}
#Removing variable “HDI per Year” from data frame as variable is showing 75% missing values. No need of this variable
Suicide_rate_clean <- Suicide_rate[,-9]
#change data types
Suicide_rate <- drop_na(Suicide_rate_clean)
Suicide_rate$country <- as.factor(Suicide_rate$country)
Suicide_rate$year <- as.factor(Suicide_rate$year)
Suicide_rate$sex <- as.factor(Suicide_rate$sex)
Suicide_rate$age <- as.factor(Suicide_rate$age)
Suicide_rate$country.year <- as.factor(Suicide_rate$country.year)
Suicide_rate$generation <- as.factor(Suicide_rate$generation)
Suicide_rate$gdp_for_year.... <- as.factor(Suicide_rate$gdp_for_year....)
Suicide_rate$gdp_for_year.... <- as.integer(Suicide_rate$gdp_for_year....)
Suicide_rate$gdp_per_capita.... <- as.integer(Suicide_rate$gdp_per_capita....)
```


#renaming colnames

```{r}
Suicide_rate1 <- rename(Suicide_rate,c(gdp_per_capita=gdp_per_capita....,gdp_for_year=gdp_for_year....,suicides_per_100kpop =suicides.100k.pop))
Suicide_rate1
```


# Exploratory Data Analysis

# Number of suicides during 1985-2016

```{r}

ggplot(data=Suicide_rate1,mapping = aes(x=year,y=suicides_no)) + geom_point(color='red') + labs(title="Number of suicides during 1985-2016",x="Year",y="Number of Suicides") + theme(axis.text.x = element_text(angle = 90))

```


# Identifing the suicide committed by different age groups over the years
```{r}
p=ggplot(data=Suicide_rate,mapping = aes(x=year,y=suicides_no/100))+ geom_bar(aes(fill=age),stat = 'Identity') + labs(title = "Suicide committed by different age groups over the years",y="Number of suicides") + theme(axis.text.x = element_text(angle = 90))
ggplotly(p)

```


```{r}
top_20sr <- Suicide_rate1 %>%
  group_by(country)%>% 
  summarise(suicides_rate = max(suicides_per_100kpop)) %>% top_n(20)
top_20sr= top_20sr %>% arrange(desc(suicides_rate))
top_20sr$country <- factor(top_20sr$country, levels = top_20sr$country)

ggplot(top_20sr,aes(y=country,x=suicides_rate))+geom_bar(stat="identity",fill="seagreen")+ labs(title="Top 20countries with suicide rates")
```

# Top 10 Countries with highest number of Suicides 1985-2016

```{r}
top_10sr <- Suicide_rate1 %>%
  group_by(country)%>% 
  summarise(max = max(suicides_per_100kpop)) %>% 
top_n(10)
head(top_10sr)


```


# Top 10 Countries with highest number of Suicides 1985-2016

```{r}
ggplot(data=top_10sr,mapping = aes(x=max,y=country))+geom_bar(stat='Identity',fill='Seagreen') + labs(title="Top 10 countries with highest number of suicides Rates",x="Suiciderate",y="Country")

``` 


# GDP per capita VS Number of suicides

```{r}
ggplot(data=Suicide_rate1,mapping = aes(x=gdp_per_capita/10,y=suicides_per_100kpop))+geom_point(color='blue') + labs(title="GDP per Captia Vs Suicide",x="GDP per captia",y="Suicidesrate")


```
#Number of suicides during 1985-2016 per generation 

```{r}
ggplot(data=Suicide_rate1,mapping = aes(x=generation,y=suicides_no/100)) + geom_bar(fill='Orange',stat = 'Identity')+ labs(title='Number of suicides during 1985-2016 per generation',x='Generation',y='Number of Suicides')


```
# Finding 5 countries with high gdp per captia income


```{r}
#install.packages("sqldf")
library(sqldf)
gdp_df <- sqldf("select country,sum(gdp_per_capita) as gdp from Suicide_rate1 group by country")
View(gdp_df)
gdp_df1 <- sqldf("select country,gdp from gdp_df where gdp>14800000")
gdp_df1
ggplot(gdp_df1,aes(x=country,y=gdp/1000))+geom_bar(stat="identity",fill="maroon")

```

# count of the countries
```{r}
no_of_dist_count= Suicide_rate1 %>% select(country) %>% group_by(country) %>% summarize(count =n())
nrow(no_of_dist_count)
```

# Year Vs Suicide per 100K population

```{r}
ggplot(Suicide_rate1,aes(x=year,y=suicides_per_100kpop)) +geom_bar(stat = "Identity",fill='blue')+ theme(axis.text.x = element_text(angle = 90))

#World suicide cases are going down, which is very good
#Suicide cases has peak on 1995
```

# Gender Vs Number of suicides

```{r}
ggplot(Suicide_rate1,aes(x=sex,y=suicides_per_100kpop/10000)) + geom_bar(fill='Seagreen',stat="Identity") + 
  labs(title = "Gender Vs suicidal rate",x="Gender",y="Suicide_rate")

```
# Age Vs Number of suicides Over Gender


```{r}
ggplot(Suicide_rate1,aes(x=age,y=suicides_no)) + geom_bar(aes(fill=sex),stat="Identity",position = 'dodge') + 
  labs(title="Age Vs Number of suicides Over Gender",x="Age",y="Number of suicides")


```

# Number of suicides per year 

```{r}

ggplot(Suicide_rate1,aes(x=year,y=suicides_per_100kpop)) + geom_bar(fill="darkgreen",stat="Identity") + labs(title="Number of suicides per year",y="suicide_rate") +  theme(axis.text.x = element_text(angle = 90))


```

# GDP Vs Suicide rate over Gender

```{r}
ggplot(Suicide_rate1,aes(x=gdp_per_capita/100,y=suicides_per_100kpop)) + geom_point(aes(color=sex)) + labs(title="Number of suicides per year over Gender",y="Suicide rate")
```

#Correlation plot

```{r}
library(corrplot)
cor_data <- cor(Suicide_rate1[,c(5:7,9:10)])
corrplot(cor_data,method='number')
```


# Split the data into train and test

```{r}
library(caTools)
set.seed(123)
sample <- sample.split(Suicide_rate1,SplitRatio=0.75)
train <- subset(Suicide_rate1,sample==TRUE)
test <- subset(Suicide_rate1,sample==FALSE)

```

# Multiple Linear regression
# Used only required variables

```{r}
lm_full_model <- lm(suicides_per_100kpop ~ country+ year+ sex+
                      age+suicides_no+population+gdp_for_year+gdp_per_capita+generation, 
                    data=train)
summary(lm_full_model) # summary of the model
```

#AIC(Akaike Information Criterion) is a mathematical method for evaluating how well a model fits the data. It is used to compare different possible models and determine which one is the best fit for the data.
```{r}
stepwise_model <- step(lm_full_model,direction="both")

```

#Lm model with best variables

```{r}
lm_model <- lm(suicides_per_100kpop ~ country + sex + age + suicides_no + population + 
    gdp_for_year + gdp_per_capita + generation
                 , data=train)
summary(lm_model)

# The Adjusted R2 is 0.54 which is good fit 
```

# Statistic Test: Anova test for two models
#H0: variables has no correlation with the dependent variable.
#HA: variable has correlation with the dependent variable.
#p-value less than significance level then we reject null hypothesis

```{r}
anova(lm_full_model,lm_model,test="Chisq") # created anova model
```
#The model 2 is better than the model 1, p-value is less than significance level(0.05) then we reject null hypothesis.
#we conclude that variable has no correlation with the dependent variable.
#Change in the independent variable will have the change in the dependent varaible


#predict the final model
```{r}
SRprediction <- predict(lm_model,test)

SRprediction <- as.data.frame(SRprediction)
```

# Error rate 
```{r}
error_rmse <- RMSE(test$suicides_per_100kpop,SRprediction$SRprediction)
error_rmse
# The error rate is 12.6
```

# Decision Tree (CTREE)

```{r}

library(party)
library(partykit)
ctreemodel <- ctree(suicides_per_100kpop ~  sex + age + suicides_no + population + 
    gdp_for_year + gdp_per_capita + generation, data = train)
#plot
plot(ctreemodel)
#predict
predictCTREE <- predict(ctreemodel, test, type = "response")
str(train)
predictCTREE <- as.data.frame(predictCTREE)
error_rmse <- RMSE(test$suicides_per_100kpop,predictCTREE$predictCTREE)
error_rmse 
```
# Decision Tree (RPART)

```{r}

library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
CARTmodel <- rpart(suicides_per_100kpop~ sex + age + suicides_no + population + 
    gdp_for_year + gdp_per_capita + generation, data = train)
summary(CARTmodel)
#plot
prp(CARTmodel)
fancyRpartPlot(CARTmodel)
#predict
predictCART <- predict(CARTmodel, newdata = test)
predictCART
predictCART <- as.data.frame(predictCART)
error_rmse1<- RMSE(test$suicides_per_100kpop,predictCART$predictCART)
error_rmse1
cart_r2 <- cor(test$suicides_per_100kpop, predictCART$predictCART) ^ 2
cart_r2
```








