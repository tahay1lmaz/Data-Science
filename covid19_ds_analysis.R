library(tidyverse)

# Reading the covid 2020 dataset and taking 1000 random data
mydata <- read_csv2("C:/Users/mtaha/desktop/covid-data-2020.csv")
my_data<-mydata[sample(nrow(mydata), 1000),]
my_data

# Q1 : Calculate the five-number summary statistics (minimum-Q1-median-Q3-maximum) 
# of covid-19 daily new cases for each country within each month. (you can use quantile() function to get the quartiles)
# Answer 1
answer1 <- my_data %>% group_by(location, month) %>% 
  summarize(
    min = min(new_cases,na.rm = TRUE),
    Q1 = quantile(new_cases, 0.25,na.rm = TRUE),
    Q2 = quantile(new_cases, 0.50,na.rm = TRUE),
    Q3 = quantile(new_cases, 0.75,na.rm = TRUE),
    max = max(new_cases,na.rm = TRUE)
  )
answer1

# Q2 : Find the highest daily cases and deaths separately for each country. 
# Answer 2
answer2 <- my_data %>% group_by(location) %>%
  summarize(
    max_case = max(new_cases), 
    max_death = max(new_deaths, na.rm = TRUE)
  )
answer2 <- arrange(answer2,desc(max_case))
answer2

# Q3 : Identify the month in which the mean daily cases is the highest for each country.
# Answer 3
answer3 <- my_data %>% group_by(location,month) %>%
  summarize(
    Mean_of_dailycases = mean(new_cases, na.rm=TRUE)
  )
answer3 <-filter(answer3, Mean_of_dailycases == max(Mean_of_dailycases))
answer3

# Q4 : Select 3 country and plot the distribution of daily cases by month. Use location as clusters 
# (i.e., color=location) to show the difference between countries.
#Answer 4
ggplot(data=filter(my_data, location %in% c("United Kingdom","Italy","Switzerland"))) +
  geom_boxplot(mapping=aes(x=as.factor(month), y=new_cases, color=location))

