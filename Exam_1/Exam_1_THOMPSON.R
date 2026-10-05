library(tidyverse)

Data <- read.csv("data/cleaned_covid_data.csv") #This assigns "Data" as the object for the data in the cleaned_covid_data.csv data file in the Exam_1 folder
Data

A_states <- Data |>
  filter(grepl("^A", Province_State)) #This assigns "A_states" as the object for finding only states that begin with the letter "A"
A_states

ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") #This creates a scatter plot of the loess curve for each state in the A_states data subset

state_max_fatality_rate <- Data |>
  group_by(Province_State) |>
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) |>
  arrange(desc(Maximum_Fatality_Ratio)) #This compiles each state as a group and sorts through each group (ie state) to find the maximum fatality excluding points that are NA and arranging in descending order
state_max_fatality_rate

state_max_fatality_rate$Province_State <- factor(state_max_fatality_rate$Province_State,
                                                 levels = state_max_fatality_rate$Province_State) #This forces the Province_State data to be in descending order to use in the graph plotting command that comes next

ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) #This plots the maximum fatality ratio in a bar graph with the x-axis text rotated 90 degrees


