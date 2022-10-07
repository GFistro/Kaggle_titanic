library(tidyverse)

filepath_train <- "D:/Users/Gregor/Desktop/Kaggle/Titanic/train.csv"
filepath_test <- "D:/Users/Gregor/Desktop/Kaggle/Titanic/test.csv"

titanic_data <- read_csv(filepath_train) #Import training data
test_data <- read_csv(filepath_test) #Import testing data


## Observations
glimpse(titanic_data) #Taking a glimpse, we need to recode Pclass, Sex, Ticket, Cabin and Embarked to factors. Ticket and Cabin seem interesting variables.

table(titanic_data$Ticket) #Taking a look at all the different combinations of tickets.
#We see that most tickets are just numbers, while some have additional info (PC, SOTON, SOC,...). Some are also repeated several times

duplicated_tickets <- titanic_data %>% #We filter out the duplicated tickets
  group_by(Ticket) %>%
  filter(n() > 1) %>%
  ungroup()

summary(titanic_data)
summary(duplicated_tickets) #Compared to the general data, the duplicated tickets have in general higher Fares and more siblings, suggesting they might be group tickets.

duplicated_tickets_table <- duplicated_tickets %>%  #We add a total column, to see how many times each ticket is repeated
  group_by(Ticket) %>%
  mutate(total = n()) %>%
  arrange(desc(total)) %>%
  ungroup()

duplicated_tickets_table %>%
  filter(grepl("CA", Ticket))

duplicated_tickets_table %>%
  filter(grepl("S.O.C", Ticket)) #Looking at a couple of examples, it appears that our theory of group tickets might be right. We can see some families forming groups for instance.
#In order for the ticket variable to more closely resemble reality, we will divide the price of the tickets by the duplicates and save the new variable as fare_real

titanic_data_clean <- titanic_data %>%
  group_by(Ticket) %>%
  mutate(total = n()) %>%
  mutate(fare_real = Fare / total) %>%
  ungroup()


table(titanic_data_clean$Cabin) #We see that cabins range from A to G. 
#Seems worth it to try to use it, especially given the diagram of cabins where A is on top and G on bottom - https://upload.wikimedia.org/wikipedia/commons/0/0d/Olympic_%26_Titanic_cutaway_diagram.png
#It seems reasonable to assume people on the upper decks had a higher survival rate than on the bottom



