---
#Analysis of Bike Share Cyclistic Data"
#By Edmond Nathan
#2023-05-02"


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Background of the Cyclistic data
Cyclistic is a fictitious bike share company that was established in 2016, providing bike-sharing services to Chicago with 5,824 bikes available in 692 docking stations. Their bike fleet includes various options such as reclining bikes, hand bicycles, and cargo bikes, catering to people with disabilities and those who prefer non-standard two-wheeled bikes. While most of their customers use the bikes for leisurely rides, approximately 30% use them daily for commuting to work. The subscription options available are casual riders with single-ride and full-day passes and Cyclistic members with annual membership.
This analysis will be carried out with the help of the data analysis process : Ask, Prepare, Process, Analyze, Share, and Act.

## Ask
How do annual members and casual riders use Cyclistic bikes differently?

## Stakeholders
The stakeholders for the fictitious cyclistic bike share company are:

*1.   Director of marketing 
*2.   Cyclistic marketing analytics team
*3.   Cyclistic executive team
*4.   Bike users

## Business Task
The business task is to better understand how annual members and casual riders use Cyclistic bikes differently. This insight can help the marketing team design design a strategy that help convert more casual members into annual members.


## Prepare
The Cyclistic data for the bike share was downloaded from <https://divvy-tripdata.s3.amazonaws.com/index.html> provided by **Motivate International Inc** under a strict license agreement. Owing to the privacy agreement, the data cannot be connected with personal information. However, since this data is owned by the city of Chicago and made available to the public through Divvy data system by licence agreement, we can say that this data has integrity since it ROCCCs (Reliable, Original, Comprehensive, Current and can be Cited).

### Loading Packages
Prior to the importation of the dataset, all relevant packages for cleaning, analysis and visualization and installed and loaded into the RMarkdown.
```{r Loading Packages}
#Loading required packages
library(tidyverse)
library(dplyr)
library(scales)
library(ggthemes)
library(ggplot2)
library(skimr)
library(stats)
library(lubridate)
```
### Importation of the Dataset
The read.csv() from the tidyverse package was used to import the Cyclistic bikeshare data: The 1st, 2nd, 3rd, and 4th quarter of the 2019 dataset into the RMarkdow and saved in dataframes called bs_trips_2019_q1, bs_trips_2019_q2, bs_trips_2019_q3, and bs_trips_2019_q4 respectively.
```{r Loading dataset}
#Loading the Cyclistic bikeshare data
bs_trips_2019_q1 <- read.csv("Divvy_Trips_2019_Q1.csv")
bs_trips_2019_q2 <- read.csv("Divvy_Trips_2019_Q2.csv")
bs_trips_2019_q3 <- read.csv("Divvy_Trips_2019_Q3.csv")
bs_trips_2019_q4 <- read.csv("Divvy_Trips_2019_Q4.csv")
```

### Data Exploration
The data was explored to gain a deep understanding of the internal structures of the dataset.
```{r exploring data}
#exploring data
View(bs_trips_2019_q1)
head(bs_trips_2019_q1)
tail(bs_trips_2019_q1)
head(bs_trips_2019_q2)
View(bs_trips_2019_q2)
head(bs_trips_2019_q3)
View(bs_trips_2019_q3)
head(bs_trips_2019_q4)
```
On exploring the data by using the head and the tail function take a look at the first/last few rows, it was discover that although the data contain the same number and types of observations, the column names for the 2nd quarter is different. This implies that the data will be manipulated before merging all the four quarters into a single year.

### Data Clean and Wrangling
The variable name for quarter 2 was changed to match the column names of the 1st, 3rd, and 4th quarter for easy merging of the data using the join function from the dplyr package. The output was saved using the same dataframe name, then the quarter 2 dataframe is passed to colnames to confirm the change.

```{r Changing column names}
# Renaming the q2 data for uniformity of column names with q1, q3 and q4
bs_trips_2019_q2 <- bs_trips_2019_q2 %>% 
  rename(trip_id = X01...Rental.Details.Rental.ID, start_time = X01...Rental.Details.Local.Start.Time, end_time = X01...Rental.Details.Local.End.Time, bikeid = X01...Rental.Details.Bike.ID, tripduration = X01...Rental.Details.Duration.In.Seconds.Uncapped, from_station_id = X03...Rental.Start.Station.ID, from_station_name = X03...Rental.Start.Station.Name,  to_station_id = X02...Rental.End.Station.ID, to_station_name = X02...Rental.End.Station.Name,  
             usertype = User.Type, gender = Member.Gender, birthyear = X05...Member.Details.Member.Birthday.Year)

colnames(bs_trips_2019_q2)
```

### Merging the datasets
In order to obtain the Cyclistic bike share data for the year 2019, the data from the four quarters of that year were merged. The merge operation was performed using the full_join() function from the dplyr package since all the data points were considered important and had the same number and types of variables. The merged dataset was then saved as a dataframe named bike_share_trips_2019_merged. The rbind() could also have been used to merge the four dataframe into one.

```{r merging data set}
#merging the four quarters into a single year
q1_q2 <- full_join(bs_trips_2019_q1, bs_trips_2019_q2)
q1_q2_q3 <- full_join(q1_q2, bs_trips_2019_q3)
bike_share_trips_2019_merged <- full_join(q1_q2_q3, bs_trips_2019_q4)
View(bike_share_trips_2019_merged)
```

Taking a peak at the first and last few rows of the data using the head() and the tail() gives an understanding of general outlook of the data.
```{r A peak at the data}
#Having a look at the data
head(bike_share_trips_2019_merged)
tail(bike_share_trips_2019_merged)
```


The str() is used to gain understanding of the internal structure of the variables.
```{r structure of data}
#Understanding the internal structure of the 12 variables 
str(bike_share_trips_2019_merged)
glimpse(bike_share_trips_2019_merged)
```
The start and end time needs to be reformatted to date format and the trip duration to numeric for this analysis as they are currently recorded as characters

## Process
The data integrity has been verified, and due to the large size of the dataset, I will be utilizing RStudio for data cleaning, manipulation, analysis, and visualization. With over 3 million observations and 12 variables, Excel will not be suitable for handling the data. However, R programming will enable me to effectively manage the data from cleaning and manipulation to analysis and visualization, all within RStudio, without the need for additional data analysis tools.

### Data cleaning and wrangling
In order to have a better understanding of how casual and annual members use the bikes differently, certain transformations need to be performed on the data. Firstly, the start and end time variables need to be converted from character to date format and the trip duration variable needs to be converted from character to numeric. Additionally, the time stamp needs to be split into date, month and day components for better analysis.

### Variable of Interest
Variables of interest were selected before cleaning and reformatting the data for analysis.

```{r selecting variable of interest}
#selecting variables of interest
bike_share_trips_selected <- bike_share_trips_2019_merged %>% 
  select(start_time, end_time, tripduration, usertype)
View(bike_share_trips_selected)
```
### Converting timestamp to date format
The start and end time are converted to date format before extracting date, month, and day of the week from it. This is necessary because the timestamp is not is formatted adequate for this analysis.

```{r start and end time conversion to date format}
# convert start and end times to date format
bike_share_trips_selected <- bike_share_trips_selected %>% 
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))
glimpse(bike_share_trips_selected)
```
Now that the timestamp has been converted to date-time format, the date, month and day of the week can easily be extracted from the start time and the trip duration be converted to numerical values and the the glimpse() shows that it is was recorded as character.

```{r new columns creation}
# Create new columns for trip duration in minutes, date, month, and day of week and hours using the functions from the lubridate package
bike_share_trips_selected <- bike_share_trips_selected %>% 
  mutate(ride_length = as.numeric(difftime(end_time, start_time, units = "secs")), #This shows how the trip duration was calculated, this will basically be a duplicate of tripduration, hence, its done for learning purpose.
         date = as.Date(start_time),
         month = month(start_time, label = TRUE),#Creates the month column
         day_of_week = wday(start_time, label = TRUE),#Creates the ay of the week column
         hours = hour(start_time)) ##Creates the hours column
View(bike_share_trips_selected)
```

Converting the tripduration to numerical values is giving some errors, which could imply that there might be some whites-paces or special characters present in the column.
Therefore, white spaces and non numeric characters such as special characters are removed using regular expressions. Then the tripduration in seconds is seamlessly converted to numeric data type. a new variable was also created converting the trip duration to hours and saved as trip_duration. After execution of the code, the glimpse() is used to check the data type of the column again.

```{r removing white-spaces}
#removing whites-paces and non numeric characters such as special characters and converting tripduration to numeric
bike_share_trips_selected <- bike_share_trips_selected %>% 
  mutate(tripduration = as.numeric(gsub("[^0-9.]+", "", tripduration)),
         #trip_duration_hr = round(tripduration/360,2),
         #ride_length = round(ride_length/360,2)
         ) 
glimpse(bike_share_trips_selected)
```
### Checking for duplicates

```{r Checking for duplicates}
#Checking for duplicates using the pipe operator( %>% ) in the dplyr package
bike_share_trips_2019_merged %>% 
  group_by_all() %>% 
  filter(n()>1) %>%
  sum() #display all number duplicate rows
```
The above code shows that there zero (0) columns with duplicates in the dataframe.

### Evaluating missing values
Handling missing values or NA values is an important part of data cleaning process. This is because the presence of missing values in a dataset can greatly impact the outcome of an analysis in a negative manner and subsequently the insights derived from the analysis.

```{r Evaluating Missing Value}
# Evaluating missing values
bike_share_trips_selected %>% 
  summarise_all(~sum(is.na(.))) %>% 
  select(1:7) %>% 
  sum()
```
There are no missing values present the variable of interest.


The *"usertype"* variable is renamed to membership type and its subset also renamed to member and casual from subscriber and customer respectively.

### Renaming Variables and its subset
```{r renaming variables}
#Renaming variables for readability
bike_share_trips_selected <- bike_share_trips_selected %>%
  rename(membership_type = "usertype",
         trip_duration = "tripduration")

#Renaming the subset of membership type
bike_share_trips_selected <- bike_share_trips_selected %>%
  mutate(membership_type = case_when(
    membership_type == "Subscriber" ~ "Member",
    membership_type == "Customer" ~ "Casual",
    TRUE ~ membership_type
  ))
View(bike_share_trips_selected)
```

## Distribution of the ride length by month
```{r distribution of ride length}
#A boxplot showing the distribution of ride duration by membership type 
bike_share_trips_selected %>% 
  group_by(month) %>% 
  ggplot(aes(month, ride_length, color = membership_type)) +
  geom_boxplot() +
  facet_wrap(~membership_type) +
  scale_y_continuous(breaks = seq(0, 12000000, 700000)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),  
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Boxplot Showing Distribution of Ride Length",
       x = "Month",
       y = "Ride length",
       color = "Membership Type",
       caption = "Visualization by: Edmond Nathan" ) 
  
```
The box plot displays the distribution of ride lengths across the 12 months. It indicates that casual members, on average, have longer ride duration compared to members. Additionally, there are outliers present in the data, particularly in February for casual riders and July for subscribers (members). These outliers need to be appropriately addressed since they can have a negative impact on the analysis results, potentially skewing them.

## Handling Outliers in the ride length
The ride duration beyond 24 hours (8640 secs) will be considered outliers and removed from the dataset. This is because, the bikes are supposed to be returned to docking stations.

```{r fitered data}
#Removing all ride length below zero and above 24 hours
bike_share_trips_filtered <- bike_share_trips_selected %>% 
  filter(ride_length >= 0 & ride_length <= 8640)
```

### Summary statistics
```{r summary statistics}
summary(bike_share_trips_filtered)
```
The summary statistics provide information about the duration of the trips. The range of trip duration or ride length is from a minimum of 1.02 hours to a maximum of 24 hours (or 8640 seconds). The mean value of the trip duration is calculated as 2.88 hours (or 1036 seconds).
 
## Determine the mean, mode and median of the ride length
```{r}
mean_max_ride_length <- bike_share_trips_filtered %>% 
  summarise(ride_length_mean = mean(ride_length),
            ride_length_min = min(ride_length),
            ride_length_max =max(ride_length),
            ride_length_median = median(ride_length))
print(mean_max_ride_length)
  
```
The central tendency analysis shows that the outliers have been removed and that the data is ready for analysis.

## Analyze and Visualization (Share)
 After ensuring that the data is free from duplicates, missing values, and properly formatted, and after verifying the integrity of the data, I proceeded to the analysis phase. 

 
```{r Total Rides}
#summarizing the total number of rides and percentages, ymax and ymin
total_rides_by_membership <- bike_share_trips_filtered %>% 
  group_by(membership_type) %>% 
  summarise(total_rides = n()) %>% 
  mutate(percent = round(total_rides/sum(total_rides)*100,2),
         ymax = cumsum(percent), 
         ymin = c(0, head(ymax, n=-1)))
View(total_rides_by_membership) #This results displays data frame for the total number of casual and member of the membership type
```
  First, the total rides by membership was calculated. The proportion for each membership type is also calculated and visualized.
 
The results of the total rides by membership type are piped, using the pipe operator from the dplyr package, into ggplot to visualize the proportion of total rides for each membership type. This visualization is achieved with the aid of a doughnut chart.

```{r Doughnut chart, warning=FALSE}
#Plotting a doughnut chart to show the proportion of both casual and member through 2019
total_rides_by_membership %>% 
  ggplot(aes(ymax =ymax, ymin=ymin, xmax = 4, xmin = 3, fill=as.factor(membership_type)))+
  geom_rect(color = "white")+
  coord_polar(theta="y")+
  xlim(c(2,4))+
  scale_fill_manual(values=c("#1f77b4", "#ff7f0e")) +
  geom_text(aes(y=((ymax+ymin)/2), label = paste0(total_rides, "\n", percent,"%"), x=3.5)) +
  theme_void() +
  annotate("text", x = 0, y = 0, label = "sum(total_riders)\n1000") +
  theme(legend.position="right") +
  labs(title = "Percentage of Total Rides by Membership Type in 2019",
       caption = "Visualization by: Edmond Nathan",
       fill = "Membership Type") 

```
 
The doughnut chart shows the proportion of the two membership type. It can can be seen that the Members have a whooping 76.93% of the total number of member an the Casual riders are 23.07% of the total number of the membership. However, the aim is to convert more of the 26% to Members.

Afterwards, the total monthly rides was calculated by grouping by membership type.
```{r Total Monthly Rides}
#calculating total monthly rides
total_monthly_rides <- bike_share_trips_filtered %>% 
  group_by(membership_type, month) %>% 
  summarise(monthly_rides = n())
View(total_monthly_rides)
```

The results of the total monthly rides is also piped into *ggplot* and visualized
```{r monthly rides viz}
#Plot to show monthly rides
total_monthly_rides %>% 
  ggplot(aes(x = month, y = monthly_rides, fill = factor(membership_type))) +
  geom_col(position = "dodge") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 500000, 50000)) +
  scale_fill_manual(values=c("#ff0478","#514689")) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Monthly Total Rides by Membership Type",
       subtitle = "Seasonality of Usage of Bikes",
       x = "Month",
       y = "Total Duration in Hours", 
       fill = "Membership Type",
       caption = "Visualization by: Edmond Nathan")

```
The graph shows that the rides peaks during the summer months (July, August and September), this followed by the spring season which isn't doing badly either. The fall seasons has the lowest number of total rides for both Casual and Member riders, most likely because of the how cold it is during that period, this closely followed by the winter season.


```{r}
#calculating total daily rides
total_daily_rides <- bike_share_trips_filtered %>% 
  group_by(membership_type, day_of_week) %>% 
  summarise(daily_rides = n())
View(total_daily_rides)


#Plot to show daily trips
total_daily_rides %>% 
  ggplot(aes(x = day_of_week, y = daily_rides, fill = membership_type)) +
  geom_col(position = "dodge") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 500000, 50000)) +
  scale_fill_manual(values=c("#ad1035","#717286")) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Daily Total Rides",
       x = "Day of the Week",
       y = "Total Rides", 
       fill = "Membership Type",
       caption = "Visualization by: Edmond Nathan")
```
The daily rides analysis reveals the contrasting usage patterns of the two membership types throughout the days of the week. Annual members tend to utilize the bikes more on weekdays and less on weekends, suggesting that they primarily use the bikes for commuting to work during the workweek (Monday to Friday). On the other hand, casual riders demonstrate higher bike usage over the weekends and lower usage during weekdays, implying that they use the bikes more for leisure activities and running errands, which typically peak on weekends.

```{r}
#Calculating average duration by hour for each membership type
avg_monthly_ride_length <- bike_share_trips_filtered %>% 
  group_by(membership_type, month) %>% 
  summarise(avg_ride_length_hrs = round(mean(ride_length/360),2))
View(avg_monthly_ride_length)

#Plotting average duration by month
avg_monthly_ride_length %>% 
  ggplot(aes(month, avg_ride_length_hrs, fill = membership_type)) +
  geom_col(position = "dodge") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 24, 1)) +
  #scale_fill_manual(values=c("#ad1035","#717286")) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Average Monthly Ride Length by Membership Type",
       x = "Month",
       y = "Average Monthly Ride Length Per Hour", 
       fill = "Membership Type",
       caption = "Visualization by: Edmond Nathan")

```

The plot for the monthly average ride length indicates that casual riders have longer ride duration compared to subscribers throughout the months (January to December)


```{r}
#Calculating average duration by hour for each membership type
avg_daily_ride_length <- bike_share_trips_filtered %>% 
  group_by(membership_type, day_of_week) %>% 
  summarise(avg_ride_length_hrs = round(mean(ride_length/360),2))

#Plotting average duration by month
avg_daily_ride_length %>% 
  ggplot(aes(day_of_week, avg_ride_length_hrs, fill = membership_type)) +
  geom_col(position = "dodge") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 24, 1)) +
  #scale_fill_manual(values=c("#ad1035","#717286")) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title = "Average Daily Ride Length by Membership Type",
       x = "Month",
       y = "Average Daily Ride Length Per Hour", 
       fill = "Membership Type",
       caption = "Visualization by: Edmond Nathan")

```
The average daily data further reveals that casual riders tend to have longer rides throughout the week. On average, the duration of their rides is more than triple the duration of rides taken by subscribers.


## Key Take Always

1. The doughnut chart reveals that annual members account for a significant proportion (76.93%) of the total membership, while casual riders make up the remaining 23.07%. The goal should be to convert more of the casual riders to become members.

2. The plot showing the total rides throughout the year demonstrates that ride numbers peak during the summer months (July, August, and September), followed by a strong performance during the spring season. The fall season has the lowest number of rides for both casual riders and members, likely due to colder weather. This is closely followed by the winter season.

3. The daily rides analysis distinctly shows the different usage patterns between annual members and casual riders. Annual members predominantly use the bikes on weekdays, potentially for commuting purposes, and exhibit lower usage on weekends. In contrast, casual riders show higher bike usage over weekends, indicating a preference for leisure activities and errands during that time.

4. The monthly average ride length plot shows that casual riders consistently have longer ride duration compared to subscribers across all months, from January to December.

5. The average daily data further confirms that casual riders tend to have significantly longer rides throughout the week, with their average ride length being more than triple that of subscribers.

These conclusions provide valuable insights into the membership distribution, seasonal ride trends, usage patterns by day of the week, and ride length for different membership types. They can inform strategies to target casual riders for conversion to members and help optimize services and marketing efforts based on seasonal variations and usage patterns.


## Recommendations
Based on my analysis of the cyclistic bike share data for 2019. These are my top three recommendations:

1.  Promotional Offers can be designed specifically tailored to attract casual riders to become annual members. These offers should highlight the benefits of an annual membership, such as cost savings, exclusive perks, priority bike access during peak times, and additional features like free rentals or discounts on accessories. Create limited-time offers or discounts that creates a fear of loss in the casual riders to make the switch and emphasize the long-term value of an annual membership.

2.  Seasonal Conversion Campaigns should be introduced to leverage on the seasonal patterns identified in the analysis. The conversion campaigns should be launched especially in the summer and spring months where the bike usage is high. The campaigns should provide special offers for casual riders to upgrade to annual membership. These incentives or offers should include but not limited to extended trial periods, discounted rates for the remainder of the year, bonus rewards for signing up during these seasons. By capitalizing on the increased interest in biking during these periods, you can encourage casual riders to commit to annual membership.

3.  Introducing flexible payment options for casual riders is recommended to address their concerns about committing to an annual membership due to inflexible payment terms. By offering monthly or quarterly payment plans for annual memberships, casual riders can choose a payment schedule that suits their financial situation. This flexibility enables them to spread out the cost over a longer period, making the transition to an annual membership more affordable and manageable. Implementing such flexible payment options will effectively eliminate barriers or biases that casual riders may have, making the switch to an annual membership more appealing and enticing to them.
