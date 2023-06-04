####################################################################
## Final Project: Anti-LGBTQ Bills of 2023

## For this project I am using a dataset of the ACLU's Anti-LGBTQ Legislation tracker that has been updated as of 5/26/2023. This data will continue to change throughout the rest of 2023 so any conclusions reached from this project will only be subject to change past June 2023. 

## link to dataset: https://github.com/mdhill13/LIS-572-Final-Project/blob/main/Anti%20LGBTQ%20Legislation%20Tracker%202023%20-%20Sheet1.csv 

## Load dataset

ACLU_df <- read.csv("https://raw.githubusercontent.com/mdhill13/LIS-572-Final-Project/main/Anti%20LGBTQ%20Legislation%20Tracker%202023%20-%20Sheet1%20(1).csv", stringsAsFactors = FALSE)

## Load libraries
install.packages("tidyverse")
library(dplyr)
install.packages("ggplot2")
library("ggplot2")
install.packages("plotly")
library("plotly")

View(ACLU_df)

## Count the number of bills by type

bills_by_type <- ACLU_df %>% group_by(ISSUE) %>% summarize(bill_count = n())
View(bills_by_type)

## Count the number of bills per state

bills_per_state <- ACLU_df %>% group_by(State) %>% summarize(bill_count = n())
View(bills_per_state)

## Count the number of bills in each state by the bill type

bill_type_by_state <- ACLU_df %>% group_by(State, ISSUE)  %>% summarize(total_passed = n())

View(bill_type_by_state)

## Count the number of bills that have been passed in each state

passed_bills_by_state <- ACLU_df %>% group_by(State) %>% filter(Status %in% c("Passed into Law")) %>% summarize(total_passed = n())

View(passed_bills_by_state)

## Count the total number of bills that have been passed into law

total_passed_bills <- ACLU_df %>% filter(Status %in% c("Passed into Law")) %>% summarize(total_passed = n())

View(total_passed_bills)

## Count the number of bills that have been defeated in each state

defeated_bills_by_state <- ACLU_df %>% group_by(State) %>% filter(Status %in% c("Defeated")) %>% summarize(total_defeated = n())

View(defeated_bills_by_state)

## Count the number of bills that have been defeated in total

total_defeated_bills <- ACLU_df %>% filter(Status %in% c("Defeated")) %>% summarize(total_defeated = n())

View(total_defeated_bills)

## Create a bar graph that shows a count of every bill for each state, grouped by their status. The goal being to see how these bills are tracking in each state and how states compare to one another. 

 ## First create the dataframe that will group the counts by both states and by status

Status_Count_By_State <- ACLU_df %>% group_by(State, Status) %>% summarize(total_count = n())

View(Status_Count_By_State)

## Then create a bar graph using this dataframe

bar_plot <- ggplot(data = Status_Count_By_State)+
  geom_col(mapping = aes(
    x = State,
    y = total_count,
    fill = Status,
    text = paste("Count:", total_count)
    ))+
  labs(
    title = "Count of Anti-LGBTQ+ Bills Introduced in 2023: Status Updated May 2023",
    x = "State",
    y = "Count",
    fill = "Bill Status"
  )
ggplotly(bar_plot, tooltip = c("text"))
  
## Now that we have the full graph, it would nice to zero in on which states have had the most bills introduced and how those bills are fairing in the process of becoming a law. Filter the data for the top 10 states with the highest number of bills introduced and then create a new bar graph with only those states. 

## First create a new dataframe with the 10 states that have introduced the highest number of bills

top_states <- ACLU_df %>% group_by(State) %>% summarize(total_count = n()) %>% slice_max(n=10,order_by = total_count)

View(top_states)

## Then use this dataframe to filter the top 10 states out of the original dataset. This dataframe should contain columns for the state, status, and count. 

top_states_with_status <- ACLU_df %>% group_by(State, Status) %>% filter(State %in% top_states$State) %>% summarize(total_count = n())

View(top_states_with_status)

## Now create a bar graph with this final dataframe.

bar_plot2 <- ggplot(data = top_states_with_status)+
  geom_col(mapping = aes(
    x = reorder(State, -total_count),
    y = total_count,
    fill = Status,
    text = paste("Count:", total_count)
  ))+
  labs(
    title = "States with the Highest Number of Anti-LGBTQ+ Bills Introduced in 2023: Status Updated May 2023",
    x = "State",
    y = "Count",
    fill = "Bill Status"
  )
ggplotly(bar_plot2, tooltip = c("text"))