library(tidyverse)
library(sqldf)
#read in the data set and assign to df variable name
df <- read.csv("C:/Users/PC/Desktop/HAMOYE/izev-may-2023-webstats-en-csv.csv")

#stating the shape of the data
rows <- dim(df)[1]
columns <- dim(df)[2]
print(paste("The dataframe has",rows,"rows and", columns,"columns."))
#looking at first 5 rows of the data frame
df %>% head(5)
#removing columns
clean_df <- df[,-c(1,3,5,6,13,14,16,18)]
clean_df
#renaming and shortening particular column names
clean_df <- clean_df %>% rename(EV.Type= 8, Province.Recipient = 10)
clean_df
#adding underscores between column names
colnames(clean_df) <- gsub("[^A-Za-z0-9_]", "_", colnames(clean_df))

colnames(clean_df)
#remove nulls
clean_df <- na.omit(clean_df)
clean_df
rows_clean <- dim(clean_df)[1]
columns_clean <- dim(clean_df)[2]
print(paste("The clean dataframe has",rows_clean,"rows and", columns_clean,"columns."))

#grabbing years and associated license totals
df_total_incentives <- clean_df %>%
  group_by(Calendar_Year) %>%
  summarise(total=n())

df_total_incentives %>% head(5)

#setting theme for all charts
theme_set(theme_light())

# Converting to a regular data frame
df_total_incentives <- as.data.frame(df_total_incentives)

df_total_incentives %>%
  ggplot(aes(x = Calendar_Year, y = total)) +
  geom_bar(stat = "identity", fill = "#5886a5") +   #creating bar chart
  geom_text(aes(label = total), size = 3.5, vjust = 1.5, colour = "white") +  #total count labels
  labs(x = "Year", y = "EV vehicles Registered") +    #x and y axis labels
  ggtitle("Yearly EV Vehicles registered under iZEV Canada initiative until March 2023)") #title

#writing the SQL query
query_total_incentives_province <- 'WITH total_incentives AS (
                                      SELECT   Calendar_Year,
                                               Province_Recipient,
                                               COUNT(*) AS total
                                      FROM     clean_df
                                      GROUP BY Calendar_Year, Province_Recipient)
                                    SELECT     *
                                    FROM       total_incentives'


#calling the query with sqldf
df_total_incentives_province <- sqldf(query_total_incentives_province)

df_total_incentives_province %>%
  #case when statement to separate out top 3 provinces from the rest
  mutate(Province_Summary = case_when(
    Province_Recipient == "Quebec" ~ "Quebec",
    Province_Recipient == "Ontario" ~ "Ontario",
    Province_Recipient == "British Columbia" ~ "British Columbia",
    TRUE ~ "Other" ))  %>% group_by(Calendar_Year, Province_Summary) %>% summarise(total = sum(total),.groups = "drop") %>%
  #start of ggplot build
  ggplot(aes(x = Calendar_Year, y = total, fill= Province_Summary)) +
  geom_bar(stat = "identity",position="stack") +
  geom_text(aes(label = total), size = 2.5, position = position_stack(vjust = 0.5), colour = "white") +
  labs(x = "Year", y = "EV vehicles registered") +
  ggtitle("Yearly EV Vehicles registered under IZEV initiative in Canada until March 2023)") +
  scale_fill_brewer(palette = "Spectral")
df_total_incentives_province

# create a query for totals based on the year
query_vehicle_counts <- 'SELECT   Vehicle_Make,
                                  Calendar_Year AS year,
                                  COUNT(*) AS total
                         FROM     clean_df\
                         GROUP BY Vehicle_Make,Calendar_Year'

query_vehicle_counts = sqldf(query_vehicle_counts)  #calling the query
query_vehicle_counts <- query_vehicle_counts %>% arrange(year)  #arranging the rows by year
query_vehicle_counts %>% head(5)  #looking at first 5 rows

#pivoting dataframe to contain totals by make by year
df_brand <- query_vehicle_counts %>%
  pivot_wider(names_from = year, values_from = total, names_prefix = "Totals_")

years <- 2019:2023 #declaring years

#looking at proportion every 1000 cars - for loop
for (year in years) {
  column_name <- paste0("per_1K_",year)
  total_column <- paste0("Totals_", year)
  df_brand[[column_name]] <- round(df_brand[[total_column]]/sum(df_brand[[total_column]], na.rm = TRUE),4) *1000
}
df_brand %>% head(5)

df_brand[is.na(df_brand)] <- 0 #setting null values to 0
df_brand <- df_brand %>% mutate(prop_num_change = per_1K_2022 - per_1K_2019)
df_brand <- df_brand %>% mutate(abs_num_change = Totals_2022 - Totals_2019)

df_brand %>% head(5)

# Pivoting for totals
cars_per1K <- df_brand %>%
  pivot_longer(
    cols = c(per_1K_2019:per_1K_2023),
    names_to = "year",
    values_to = "per_1K"
  ) %>%
  select(Vehicle_Make, year, per_1K, prop_num_change)

cars_per1K$year <- as.numeric(str_replace_all(cars_per1K$year, "^per_1K_", ""))
ev_totals <- query_vehicle_counts %>%
  full_join(cars_per1K, by = c("Vehicle_Make" = "Vehicle_Make", "year" = "year"))

ev_totals <- sqldf("SELECT *,
                    RANK() OVER (PARTITION BY year ORDER BY TOTAL DESC) AS rank_num
                    FROM ev_totals")
ev_totals %>% head(5)

# Filter the data to include only the top 16 brands
top_16_2022 <- ev_totals %>%
  filter(year==2022, rank_num <= 16)

top_16_2022 %>%
  mutate(Vehicle_Make = fct_reorder(Vehicle_Make, total)) %>%
  ggplot(aes(x = total, y = Vehicle_Make)) +
  geom_bar(stat = "identity", fill = "#62929E") +
  geom_text(aes(label = total), size = 3, hjust = 1.1, colour = "white") +
  geom_text(aes(x=-250, label = rank_num), size = 3.25, color = "darkslategrey", hjust = 'center') +
  scale_x_continuous(expand = c(.02, .01)) +
  labs(x = NULL, y = NULL) +
  ggtitle("Top 15 EV Carmakers in 2022")
top_16_2022
top_16_2022 %>%
  #dropping columns with null values of prop_num_change
  drop_na(prop_num_change) %>%
  #case statement for adding labels
  mutate(num_change_label = case_when( prop_num_change > 0 ~ paste0("+", round(prop_num_change, 0)),
                                       prop_num_change < 0 ~ paste0("-",abs(round(prop_num_change,0))),
                                       TRUE ~ as.character(prop_num_change) )) %>%
  #organizing the vehicle make in the correct order
  mutate(Vehicle_Make = as.factor(Vehicle_Make)) %>%
  mutate(Vehicle_Make = fct_reorder(Vehicle_Make, prop_num_change)) %>%
  ggplot(aes(x = prop_num_change, y = Vehicle_Make)) +
  geom_bar(stat = "identity", fill = "#C6C5B9") +
  geom_text(aes(x = -150, label = num_change_label), size = 3.25, color = "darkslategrey", hjust = 'left') +
  scale_x_continuous(expand = c(.01, .01), limits = c(-150, 100)) +
  labs(x = NULL, y = NULL) +
  ggtitle("Top 15 EV makers difference per 1,000 EVs sold from 2019-2022")

#plotting out graph
ev_totals %>%
  filter(Vehicle_Make %in% top_16_2022$Vehicle_Make) %>%
  ggplot(aes(year, per_1K)) +
  geom_area(aes(fill = Vehicle_Make, group = Vehicle_Make), na.rm = TRUE) +
  theme(legend.position = "none") +
  facet_wrap(~fct_reorder(Vehicle_Make, rank_num)) +
  labs(x = NULL, y = "Electric Vehicles per 1,000 licenses") +
  theme(panel.spacing.x = unit(4, "mm")) +
  ggtitle("Changes in Popularity for Top EV Automakers (2019 - 2023)")

# Create a column for totals based on the year
query_model_counts <- 'SELECT     Vehicle_Make,
Calendar_Year AS year,
COUNT(*) AS total
FROM       clean_df
GROUP BY   Vehicle_Make,Calendar_Year'
query_model_counts = sqldf(query_model_counts)

query_model_counts <- query_model_counts %>% arrange(year)

#pivoting dataframe to contain totals by make by year
df_model <- query_model_counts %>%
  pivot_wider(names_from = year, values_from = total, names_prefix = "Totals_")

years <- 2019:2023 #declaring years

#looking at per every 1000 cars - for loop
for (year in years) {
  column_name <- paste0("per_1K_",year)
  total_column <- paste0("Totals_", year)
  df_model[[column_name]] <- round(df_model[[total_column]]/sum(df_model[[total_column]], na.rm = TRUE),4) * 1000
}
df_model[is.na(df_model)] <- 0 #setting null values to 0
df_model <- df_model %>% mutate(prop_num_change = per_1K_2022 - per_1K_2019) #calculating change

# Finding the biggest changes in increases and decreases
top_increases <- df_model %>%
  top_n(15, prop_num_change)
top_increases

top_decreases <- df_model %>%
  top_n(15, desc(prop_num_change))
top_n

# Plotting the top 15 model increases by proportions
top_increases %>%
  mutate(Vehicle_Make = fct_reorder(Vehicle_Make, prop_num_change)) %>%
  ggplot(aes(x = prop_num_change, y = Vehicle_Make)) +
  geom_bar(stat = "identity", fill = "#393D3F") +
  geom_text(aes(label = round(prop_num_change, 0)), size = 3, hjust = 1.3, colour = "white") +
  scale_x_continuous(expand = c(.02, .01)) +
  labs(x = NULL, y = NULL) +
  ggtitle("Top Model increases per 1,000 EVs sold (2019-2022)")
ggsave("top_increases.png", width = 8, height = 6)

# Plotting the top 15 model decreases by proportions
top_decreases %>%
  mutate(Vehicle_Make = fct_reorder(Vehicle_Make, desc(prop_num_change))) %>%
  ggplot(aes(x = prop_num_change, y = Vehicle_Make)) +
  geom_bar(stat = "identity", fill = "#DD7373") +
  geom_text(aes(label = round(prop_num_change, 0)), size = 3, hjust = -0.4, colour = "white") +
  scale_x_continuous(expand = c(.02, .01)) +
  labs(x = NULL, y = NULL) +
  ggtitle("Top Model decreases per 1,000 EVs sold (2019-2022)")
ggsave("top_decreases.png", width = 8, height = 6)



