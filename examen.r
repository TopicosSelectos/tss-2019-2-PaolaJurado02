#1.- Create a numeric vector that contains the eight planets of our solar system. Then, create another vector containing the name for each planet (Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, and Neptune). After that, you should assign the number of the planet and its respective name. Finally, you should select and print the planets related to the names Earth, Mars, and Jupiter.
cont<- c(1,2,3,4,5,6,7,8)
nom_coun<- c("Mercury", "Venus", "Earth", "Mars", "jUpiter", "Saturn", "Uranus", "Neptune")
juntos<- matrix(nom_coun, cont, byrow = TRUE)
juntos
planets<- juntos[3:5]
planets
#2.- Construct a matrix with 10 rows containing the numbers 1 up to 50, filled row-wise
num<- c(1:50)
num_mat<- matrix(num, ncol = 10 , byrow = TRUE)
num_mat

#3.-Create a factor with these observations "Breakfast", "Breakfast", "Breakfast", "Luch", "Luch", "Dinner", "Dinner", "Dinner", "Collation", "Collation", "Collation" and print a summary.
obs<-c("Breakfast", "Breakfast", "Breakfast", "Luch", "Luch", "Dinner", "Dinner", "Dinner", "Collation", "Collation", "Collation", "Collation")
obs_obs<-factor(obs)
obs_obs
#4.- Create and print a data frame that should contain:
#5 of your courses (e.g. Fundamentals of Programming, Object-Oriented Programming, Databases, etc ).
#5 of your marks
#5 of your professors' names
course<- c("Fundamentos", "Arquitectura de computadoras", "Innovacion tecnologica", "Base datos","Sistema web")
mark<- c(10,5,9,8,7)
names<- c("Alan ponce"," Vicente Garcia", "Oscar Martinez", "Roberto Piña", "Lucero saenz")
juntos<- data.frame(course, mark, names)
juntos
#5.- Load the dataset labeled as odb.csv that is located in the data folder and storage this dataset in a variable called od. Then, you should describe the composition of the dataset. This description must contain the number of observations, variables and the type of each variable.
od<- read.csv("C:/Users/Alumnos/Desktop/tss-2019-2-PaolaJurado02-master/data/odb.csv")
ncol(od)
nrow(od)
total<- ncol(od)*nrow(od)
total
nums<-names(total)
nums
lapply(od, class)
glimpse(od)

#6.- Filter the od dataset to retrieve only the observation from Mexico, grouped by year and in descending order selecting the "ODB_Rank column
od %>% filter(Economy == "Mexico") %>% arrange(desc(ODB_Rank)) %>% group_by(year)

#7.- Create a new variable called od_year grouping per "year" and "Income group" variables. Then, remove NAs observations. Later, using the function summarize() you should estimate the median of the GDP_PPP variable and store it in a variable called "medianValue"
od %>% filter(Economy == "Mexico") %>% arrange(desc(ODB_Rank)) %>% group_by(year)
od_year <- group_by(year, income.group)
na.omit(od_year)
summarize <- function(){
  medianValue <- median(income.group)
  medianValue
}
#8.-Create a line plot graph to visualize trends over time. You should use the variables created in the od_year data frame using Income group as color
ggplot(od_year, aes(x = year, y= income.group)) + geom_line()
#9.- Create a new variable called od_latin filtering per Region, selecting Latin America & Caribbean and grouping per Income group.
od_latin <- od %>% filter(Region == "Latin America", Region =="Caribbean") %>% group_by(income.group)
#10.- Create a new variable called od_2016 filtering the year 2016 and removing observations that contain NAs.
od_2016 <- od%<% na.omit(filter(year == 2016))