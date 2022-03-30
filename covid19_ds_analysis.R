install.packages("tidyverse")

library("tidyverse")

starwars

# Q1 : How many characters do have at least one starship? List the names of the characters having at least one starship.
# Answer 1
select(starwars, name, starships)
characterNames<-filter(starwars,starships !="character(0)")
select(characterNames, name)


# Q2 : Get the frequencies of the eye colors of the characters. Rank them from most to least.
# Answer 2
rankColor<-starwars %>%
  group_by(eye_color) %>%
  summarize(count=n())
arrange(rankColor,desc(count))


# Q3 : According to the data available, what are the average age values across each species? Find the 3 oldest species.
# Answer 3
notNA_age<-starwars %>%
  filter(!is.na(birth_year))
average_age<-notNA_age %>%
  group_by(species) %>%
  summarize(
    mean_birth=mean(birth_year))
age<-arrange(average_age,desc(mean_birth))
head(age,3)
age


# Q4 : Create a new data set by adding a new observation to this data.
# Answer 4
myCharacter<-rbind(starwars,c(name="Taha Yilmaz",height=178,mass=95.0,hair_color="black",
                              skin_color="white",eye_color="blue",birth_year=22.0,sex="male",
                              gender="masculine",homeworld="Tatooine",
                              species="Human", films="Return of the Jedi",
                              vehicles="character(0)", starships="character(1)"))
myCharacter$height<-as.integer(myCharacter$height)
myCharacter$mass<-as.double(myCharacter$mass)
myCharacter$birth_year<-as.double(myCharacter$birth_year)
tail(myCharacter, 1)


# Q5 : Calculate the body mass index (BMI) values (dividing the mass value in kg to the square of the height value in meter) 
# for all observations and categorize the observations as underweight (BMI below 18.5), healthy (BMI between 18.5-24.99), 
# overweight (BMI between 25.0-29.99) and obese (BMI above 30.0). Add these two variables to your new data created at the 4th question.
# Answer 5
myCharacter<-myCharacter %>%
  mutate(name,BMI=mass/((height/100)^2))

myCharacter<-myCharacter %>%
  mutate(name,BMI_categories = if_else(BMI>30, "obese",if_else(BMI>25,"overweight",if_else(BMI>18.5,"healthy","underweight"))))
select(myCharacter, name, BMI, BMI_categories)


# Q6 : Plot the distribution of ages less than 100 by BMI groups. (i.e. use filter function to select the ages less then 100)
# Answer 6
myCharacter<-na.omit(myCharacter)
ggplot(data=myCharacter, mapping = aes(y=birth_year, x=BMI_categories)) +
  geom_boxplot(data=filter(myCharacter, birth_year<100.0))


# Q7 : By plotting a graph, show the relationship between age and BMI values (use point and line at the same time).
# Answer 7
ggplot(data=myCharacter, mapping=aes(x=birth_year,y=BMI))+
  geom_point()+
  geom_smooth()

# Re-plot the same graph after filtering the data as both age and BMI less than 100.
ggplot(data=filter(myCharacter,birth_year<100 & BMI<100),
       mapping=aes(x=birth_year,y=BMI))+
  geom_point()+
  geom_smooth()

