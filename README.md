# IMPACT OF COVID-19 ON AIR TRAVEL
# By Edmond Nathan
# On 2023-04-20"

```{r global-options, include=FALSE}
knitr::opts_chunk$set(fig.width=14, fig.height=10, fig.path='Figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)
```

## INTRODUCTION
The European flights dataset is collected monthly by [Eurocontrol](https://ansperformance.eu/data/): A pan-European, civil-military organisation dedicated to supporting European aviation. The data on commercial flights is inclusive of scheduled and non-scheduled commercial flights such as passengers, freight and mail performed under Instrument Flight Rules (IFR).
According to an article on <https://ec.europa.eu>, countries around the world took different restrictive measures since the beginning of 2020: The inception of COVID-19, to prevent the spread of the virus. The aviation industry like most, suffered daring effects of the pandemic. However, recent studies on commercial flights show plausible signs of recovery. The datasets provided herein is useful for exploring and visualizing the patterns in air transport over time. Although, the European flights is from [Eurocontrol](https://ansperformance.eu/data/), further information provided was found on [github tidytuesday](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-12).
### Aim
The aim of this coursework is to proficiently prepare an excellent visualization and analysis of the European Flights data from [Eurocontrol](https://ansperformance.eu/data/)
### Objectives

*   a.	The analysis of European Flight Dataset between 2016 and 2022 was executed to gain insights into the trends and patterns of the aviation industry during this period. 
*   b.	Exploration of the dataset was presented using various statistical and visualization techniques to discover trends and pattern for the span of the datatset and changes during the pandemic.

## Research Questions
The following information and questions will be investigated and treated in this report:

  * What is the trend of air traffic demand over time, and what is the impact of the COVID-19 pandemic on the number of flights?
  * Which countries have the highest air traffic by year and month? 
  * How does seasonality affect flight demand and capacity?


1.	**The Impact of COVID-19 on air travel**
The COVID-19 pandemic had a significant impact on air travel patterns worldwide, and European countries were not exempted. To curb the spread of the virus, many countries implemented travel restrictions and lock-down procedures. This led to a decline in the demand for air travel, resulting in a drastic decline in the number of flights and passengers. Analyzing the impact of the COVID-19 pandemic on air travel patterns is important to inform policymakers and industry stakeholders on the strategic need for policies that can mitigate the impact of future pandemics, and to provide valuable information for travelers and companies in the aviation sector on trends that may emerge post-pandemic.
2.	**Identifying Countries with highest air traffic by year and month**
It is important to analyze dataset for countries with most traffic volume by year and month, as this will assist in:
*   a.	Resource allocation: The efficient allocation of resources by airport authorities to handle the high traffic volume experienced i.e., understanding the highest air traffic will inform stakeholders to invest in areas with high demands.
*   b.	Market analysis: analysis on airports and airlines with high traffic can provide insights into most popular destinations and routes which can inform on marketing and pricing strategies.
*   c.	Capacity planning: an understanding of the busiest airlines and airports can help in planning for future capacity needs, such as need for more flights, larger aircraft, or expanding airport infrastructure to accommodate demand.
3.	**Seasonality of air transport in Europe**
Understanding how seasonality affects flights demand and capacity can help airlines and airports make informed decisions on improving their operations and profitability through;
*   a.	Planning and forecasting: Understanding seasonal patterns can help plan and forecast on resource and capacity allocation. Flight schedules and staffing can be adjusted to meet demand during peak travel seasons and avoid over or under staffing.
*   b.	Pricing: Airlines can adjust their pricing strategies based on seasonal demand. For example, they may increase ticket prices during peak travel seasons when demand is high and reduce them at off-peak seasons when demand is low.
*   c.	Marketing and promotions: Airlines can target their marketing and promotions efforts to specific regions and destinations based on seasonal demand. For example, promoting tropical destinations during winter when travelers are looking to avoid low temperate regions due to cold.

## Installation and Loading of Packages
To pre-process and explore the flights data using visualization, necessary packages such as the core *tidyverse*: According to [CRAN](https://cran.r-project.org/web/packages/tidyverse/) It is set of packages that work in harmony because they share common data representations and 'API' design. This package is designed to make it easy to install and load multiple 'tidyverse' packages in a single step  and other required packages were installed and loaded into both R and RMarkdown respectively as shown below.

```{r Loading packages, warning=FALSE}
#Loading all required packages, they have been earlier installed 
library(tidyverse)
library(stats)
library(skimr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(scales)
```

## European Flights Data Importation
The **Flights Dataset** was downloaded from the [Github link](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-12) and the *read.csv()* function was used to import it from a .csv file domiciled on my computer hard drive into an already created project folder and saved as a data frame named flight.

```{r flights, warning=FALSE}
#Loading the dataset 
flight <- read.csv("flights.csv")
```
# Data Exploration
### Using summary tools to understand the dataset
The dimension of the flight dataset can be obtained by using the dim() function from the 'dplyr' library
```{r Dimension, warning=FALSE}
#Checking for the dimension of the flight dataset
dim(flight)
```
The dimension of the data is a length of *688,099* observations (rows) and *15* variables (columns).

## A Peak of the Data
The head() and tail()function are important tools for data exploration and thus the reason it was used to display the first few rows and the last few rows of the flight data frame. Also, this quick data check helped to gain insight into the data structure, and to explore the characteristics of the of each variables..

```{r head sample data, warning=FALSE}
# Using the 'head()' function to look at the first three rows to gain understanding of the data
head(flight, 3)
```
```{r Tail, warning=FALSE}
# The 'tail()' provides the last three rows as speecified
tail(flight, 3)
```
The flight data between January 1, 2016 and Mya 5,2022 provides a comprehensive data on the YEAR (year of data), MONTH_NUM (month of data), FLT_DATE (the day of flight), APT_ICAO (airport code), APT_NAME(unique airport name), STATE_NAME(unique country name of flight movement), FLT_DEP_1 (flight departure), FLT_ARR_1 (flight arrival), FLT_TOT_1 (total flight arrival and departure), FLT_DEP_IFR_2, FLT_ARR_IFR_2, FLT_TOT_IFR_2, Pivot.Label (unique airport name and code combined). The sample of the data frame also shows that date which is a continuous variable is recorded as *character* which would be converted to date format while preparing the data for analyses. For aesthetics and readability, the column names which are all capitalized would be converted to lowercase during the data manipulation process, before the analyses of the flight data. Some duplicated data and others considered irrelevant for this analyses will be removed.

The 'str()' function can also be used to gain more understanding of the dataset  simply by providing the 'str()' with the flight dataset as argument.
```{r structure, warning=FALSE}
str(flight)
colnames(flight)
```
Further to the dimension of the flight data, the str() function also provides understanding of the structure of the internal variables present in the dataset. In the flight dataset, we have *9* integers (numeric variables) and *6* characters (categorical variables). It also shows that there are NA values also known as missing values present..

## Description of Variables and their Characteristics
An understanding of the characteristics of the variables in the dataset is important for data analysis and interpretation. It allows us to determine the appropriate statistical methods to use and helps us to understand the relationships between the variables. Having looked at the structures and the type of variable in the flight dataset, we can conveniently describe the variables of interest and their characteristics as below:

*   YEAR: This is a categorical variable that represents the year in which the flight occurred with each year being a separate category.

*   MONTH_NUM: This is also a categorical variable that represents the month in which the flight occurred with each month being a separate category.

*   FLT_DATE: This variable represents the date of flight. It is a continuous variable, with each date being a unique value. It's recorded as *character* and would be converted to date format during the pre-processing stage.

*   APT_ICAO: It is a categorical variable that represents the unique code assigned to each airport and it is unique for each airport. 

*   APT_NAME: This variable represents the name of the airport. It is a categorical variable, with each airport having a unique name.

*   STATE_NAME: This is also a categorical variable that represents the country in which the airports are located with each country having a unique value.

*   FLT_DEP_1: This variable represents the total number of departures from an airport. It is a continuous variable, with values ranging from 0 to some the maximum value *(847.00)*.

*   FLT_ARR_1: This variable represents the total number of arrivals at an airport. It is also a continuous variable, with values ranging from 0 to some maximum value *(813.00)*.

*   FLT_TOT_1: This variable represents the total number of arrivals and departures at an airport. It is a continuous variable, with values ranging from 0 to some maximum value *(1628)*.


## Statiscal Description
Descriptive statistics comprises mathematical techniques that are utilized to summarize data. The components of descriptive statistics include the distribution, central tendency, and dispersion of data. The distribution can be of different types, such as normal distribution or binomial distribution. The central tendency can be measured using mean, median, or mode, while the dispersion or spreadness can be assessed through the range, interquartile range, variance, or standard deviation (Eric, 2019). To generate the statistical summary of flight data, the summary() function is utilized, whereby the flight data is provided as an argument to the function.

```{r summary, warning=FALSE}
summary(flight)

```
The summary() has clearly given us a statistical description of all the numerical variables by summarizing each column and the results displayed.  
From the output above, the statistical summary table shows that the flight data is for the period between *2016* and *2022*: which is a span of *6 years*. Also, it shows the minimum and maximum mean and median, and the interquartile range of each numeric variables.
For instance, the central tendencies mean and median for the flight departure (FLT_DEP_1) are *62.23* and *17*, while the range of values are *0* to *847*, and the 1st, 2nd and 3rd interquartile ranges are *5*, *17*, and *71* respectively.
From the above statistical summary table, it can also be seen that three columns (FLT_DEP_IFR_2, FLT_ARR_IFR_2, FLT_TOT_IFR_2) contain *479785* missing values (NA) each. WHile the categorical variable STATE_name has a length of 688099 observations and of class character.

#### Using boxplot to explore the flight data
A boxplot was necessary to have a graphical representation of the data, which displays the distibution of the dataset. A boxplot was plotted for number departures and another for the number of arrivals to see the distribution across years.
```{r warning=FALSE}
# Boxplot to view the distribution of departures across years
flight %>% ggplot(aes(x = as.factor(YEAR), y = FLT_DEP_1, fill = as.factor(YEAR))) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(title = "Number of Departures by Year",
       x = "Year",
       y = "Number of Departures",
       fill = "Years")+
  theme_bw()
```
Figure 1.1: Boxplot: Number of departures across years

```{r warning=FALSE}
# Boxplot to view the distribution of arrivals across years
flight %>% 
  ggplot(aes(x = as.factor(YEAR), y = FLT_ARR_1, fill = as.factor(YEAR))) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(title = "Number of Arrivals by Year",
       x = "Year",
       y = "Number of Arrivals")
```
Figure 1.2: Boxplot: Number of arrivals across years

Figure 1.1 and 1.2 above shows the distribution of the mean, 1st, 2nd and 3rd interquartile range. It can also be seen that years such as 2021 contain outliers: the values beyond the whiskers, they can sometimes negatively skew result of analysis. Each box represent the 1st, 2nd and 3rd interquartile range. For instance,year 2017 in figure 1.2 seem to have a larger interquartile range (the box), this means the middle 50% has more spread than that of 2020 which as a smaller interquartile range.

## Data Cleaning/Wrangling Process

## Evaluating Missing data
R provides a number of functions for identifying observations that contain missing values. The function is.na() allows one to test for the presence of missing values (Robert, 2015). The pipe operator and the *summarise_all()* from the **dplyr** package was used to evaluate the total number of missing values in the flight dataset as shown below
```{r totalnumber of missing values, warning=FALSE}
# Checking for the number of missing data in the dataset
flight %>% 
  summarise_all(~sum(is.na(.))) %>% 
  select(1:15) %>% 
  sum()
```

The total number of missing values were evaluated to be *1,439,355* in the European flights data.

Using the pipe operator from the 'dplyr' package, the summarise_all() and select functions were utilized to identify the columns containing missing values. This step was taken to determine which columns require further action.

```{r Columns with missing values, warning=FALSE}
# Evaluating missing data in individual columns
flight %>% 
  summarise_all(~sum(is.na(.))) %>% 
  select(1:15)
```
The result was displayed in a tabular form, it can be seen that the columns FLT_DEP_IFR_2, FLT_ARR_IFR_2, FLT_TOT_IFR_2, were seen to contain a total missing or NA values of *1439355*. Each having a total of *479785*.

## Handling Missing Values
According to the information provided on [Github link](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-12), the FLT_DEP_1, FLT_ARR_1, FLT_TOT_1 and FLT_DEP_IFR_2, FLT_ARR_IFR_2, FLT_TOT_IFR_2 are the same data. only that the data sources differs. The former is recorded by the *network manager*, while the later is recorded by the *airport operators*. As a result, the columns FLT_DEP_IFR_2, FLT_ARR_IFR_2, FLT_TOT_IFR_2 which contain all the missing values will be completely removed, as well as column X which is just serial numbers for the observation, MONTH_MON (month in words), and Pivot.Label (which is combination of the airport name and the airport code).
In this analysis, variables of interest are YEAR, MONTH_NUM, FLT_DATE, APT_ICAO, APT_NAME, STATE_NAM, FLT_DEP_1, FLT_ARR_1, FLT_TOT_1.

## Data Manipulation
The column names was renamed for intuitive reading of the columns. The *pipes operator* ( %>% ) from the **dplyr** package was ustilized to pass the *flight data* into the rename() function, then piped into a deselect function to remove irrelevant variables and missing values. Then the column names are also converted to lower case for aesthetics and readability and saved as a data frame *flights*. *Flights* contains only variables of interest.

```{r warning=FALSE}
# converting columns fromn from uppercae to lowercase, also removing irrelevant columns
flights <- flight %>% 
  rename(date_of_flight = FLT_DATE, airport_code = APT_ICAO, country = STATE_NAME, airport_name = APT_NAME, month_mon = MONTH_MON, month =MONTH_NUM, flight_departure = FLT_DEP_1, flight_arrival = FLT_ARR_1, flight_arrival_departure = FLT_TOT_1) %>% 
  select(-c(X,FLT_DEP_IFR_2, FLT_ARR_IFR_2, FLT_TOT_IFR_2, Pivot.Label)) %>%
  rename_all(tolower) 
View(flights)
```

## Cleaning the date column and filtering out year 2022
The date column was captured as a character; however, the date format is required for this analysis. Hence, the conversion to date column. It was also observed that the year 2022 contains data for only five months. If used for the analysis, it might skew the result. Therefore, the year 2022 is filtered out from the flight dataset. All analyses are carried out for a six-year period from 2016 to 2021.
```{r warning=FALSE}
# Filtering out year 2022 and converting the date column to date format.
flights <- flights %>% 
  filter (year < 2022) %>% 
  mutate(date_of_flight = ymd(date_of_flight))
head(flights)
tail(flights)
```
Using the mutate() function from the *'dplyr'* library and the mdy()function from the *'lubridate'* library, the date of flight (chr) was converted to date format for seamless analysis. Applying the head()function shows that the date column has been converted to date format.

# Exploratory Data Analysis

## Air travel pattern from 2016 to 2021

### 1.    What is the trend of air traffic demand over time, and what is the impact of the COVID-19 pandemic on the number of flights? 

The total flight count was calculated and grouped by year and month as below
```{r warning=FALSE}
#calculating total flights counts
total_flight_counts <- flights %>%
  group_by(year, month) %>%
  summarise(total_flights = sum(flight_arrival_departure))
View(total_flight_counts)
```

Since the essence of the visualization is to display the pattern and trend of the total flight in the data over time, a line plot was used to adequately show the pattern of air traffic by month and year from 2016 to 2021

```{r warning=FALSE}
#Visualizing trend of air traffic over time
total_flight_counts %>% 
  ggplot(aes(x = month, y = total_flights, color = factor(year))) +
  geom_line(size=1) +
  geom_point(size=2) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  theme(axis.text.x = element_text(angle = 90),  
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(x = "Month", 
       y = "Total Flights", 
       title = "European Flight Data: Trend of Total Number of Flights by Month", 
       caption = "Visualization by: Edmond Nathan") +
  scale_color_discrete(name = "Year") 
head(flights)
ggsave("d.png", height = 8, width = 6)
```
Figure 2.1: Trend of Total Number of Flights by Month

The line graph was utilized as a great visualization tool for showing trends or patterns over time. The results indicate that from 2016 to 2019, January and February always started slow before trending upwardly until they peak around July and August each year. Then, they gradually decline until they reach the same level as the beginning of the year. An anomaly to this trend occurred in 2020 with a sharp decline around March, which coincided with the start of the COVID-19 pandemic. There were fluctuations in the total number of flights, likely due to restrictions imposed by airline authorities worldwide. In 2021, there were some signs of improvement in the pattern, although the total number of flights remained significantly lower relative to the pre-pandemic years.


```{r warning=FALSE}
# filtering for 2019, 2020, 2021, to see the trend of total flight pre, during, and post pandemic
pandemic_df <- flights %>% 
  filter(year >= 2019 & year < 2022) %>% 
   mutate(year_month = floor_date((date_of_flight), "month"))
head(pandemic_df)
tail(pandemic_df)

# Group by month-year and calculate total flights
total_flights_by_year_month <- pandemic_df %>% 
  group_by(year_month) %>% 
  summarize(total_flights = sum(flight_arrival_departure))
View(total_flights_by_year_month)

# Create a line plot of total flights by month-year
total_flights_by_year_month %>% 
  ggplot(aes(x = year_month, y = total_flights)) +
  scale_y_continuous(breaks = seq(0, 2000000, 100000)) +
  geom_line(size = 1, color = "#e51b21") +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 90),  
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(title = "European Flight Data: Impact of COVID-19 on Air Travel",
       subtitle = "Total Flights by Month-Year Between 2019 and 2021",
       x = "Year_Month", 
       y = "Total Number of Fights",
       caption = "Visualization by: Edmond Nathan")

```
Figure 2.2: Impact of COVID-19 on air travel (2019-2021)

The graph above represent the trend of data showing flight patterns from 2019, the pre-pandemic period, 2020, the COVID-10 pandemic era and 2021 post pandemic era. It can be seen that 2019 started progressively and peaked in July 2019. Then there was slight decline. However, there was sharp decline in March of 2020, the lowest number of flights were recorded in April 2020 which coincides with the inception of the COVID-19 pandemic and the restrictions of flight movement that followed. However, slight improvement is seen from March 2021 and beyond. From the plot it can be inferred that the total number of flight was drastically affected during the COVID-19 pandemic. The ripple effect of this will impact overall flight capacity monthly or yearly. This plot has helped identified the trend in air traffic movement pre-pandemic, during pandemic, and post pandemic. 


The use of a doughnut chart allows for a clear representation of the proportion of total flights for the three years visualized above, including the display of calculated percentages or proportion. Therefore,  

```{r warning=FALSE}
#filtering for the years 2019,2020, and 2021 and grouping year
flight_COVID_19 <- flights %>% 
  filter( year %in% c(2019, 2020,2021)) %>% 
  group_by(year) %>% 
  summarise(total_flights = sum(flight_arrival_departure)) %>% 
  mutate(percent = round(total_flights / sum(total_flights), 2), 
         ymax = cumsum(percent), 
         ymin = c(0, head(ymax, n=-1)))
View(flight_COVID_19)

flight_COVID_19 %>% 
  ggplot(aes(ymax =ymax, ymin=ymin, xmax = 4, xmin =3, fill=as.factor(year)))+
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2,4))+
  scale_fill_manual(values=c("#009E73","#F0E442","#f73d27")) +
  geom_text(aes(y=((ymax+ymin)/2), label = paste0(year, "\n", percent *100,"%"), size = 20, fontface = "bold", x=3.5)) +
  theme_void() +
  theme(legend.position="none") +
  labs(title = "Percentage of Total Flights by Year (2019-2021)",
       caption = "Visualization by: Tunde Adeyi",
       fill = "Year")
ggsave("DP.png", height = 8, width = 10)  

# Visualizing the various proportions using doughnut chart
DP1 <- flight_COVID_19 %>% 
  ggplot(aes(ymax =ymax, ymin=ymin, xmax = 4, xmin =3, fill=as.factor(year)))+
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2,4))+
  scale_fill_manual(values=c("#009E73","#F0E442","#f73d27")) +
  #geom_text(aes(y=((ymax+ymin)/2), label = paste0(year, "\n", percent *100,"%"), size = 20, fontface = "bold", x=3.5)) +
  theme_void() +
  theme(legend.position="none") 
  #labs(title = "Percentage of Total Flights by Year (2019-2021)",
       #caption = "Visualization by: Edmond Nathan",
      # fill = "Year")
ggsave("DP1.png", height = 8, width = 10)  

```
Figure 2.3: Percentage of Total Flight by Year (2019-2021)

This type of plot was chosen for its ability to facilitate easy comparison of the relative proportion of total flights for each year. Analysis of the chart reveals a significant decline in the total number of flights for 2020 compared to 2019 and 2021. This decline is likely attributed to the COVID-19 pandemic and subsequent flight restrictions. Additionally, the proportion of the total number of flights for 2019, which accounts for 51% of the combined three years total, is twice the proportion of the total number of flights for 2020 and 2021, which accounts for only 22% and 27% respectively. This demonstrates a gradual increase in the total number of flights in 2021. Calculated percentages and total values for each part also aid in providing clarity of the chart. Calculated percentages and total values for each part also assist in providing clarity of the chart. The observation of subtle increase in the total number of flights in 2021 is encouraging as it suggest sign of recovery for the aviation industry. This insights can be used for scheduling and airport planning.



## 2     Which countries have the highest air traffic by year and month?
To evaluate which countries have the highest air traffic by year and by month. We can calculate the total number of arrivals and departures for each country, and then rank them by year and by month.
The total number of arrivals and departures by country and year is calculated thus:

```{r warning=FALSE }
# calculating the top countries with the highest air traffic
total_flight_by_country_year <- flights %>% 
  group_by(country, year) %>% 
  summarise(total_flights = sum(flight_arrival_departure)) %>% 
  arrange(desc(total_flights))
View(total_flight_by_country_year)


#Visualizing top 10 busiest countries by year using column plot

total_flight_by_country_year %>% 
  group_by(year) %>% 
  top_n(10, total_flights) %>% 
  arrange(year, desc(total_flights)) %>% 
  ggplot(aes(x = year, y = total_flights, fill = country)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = seq(2016, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 2500000, 200000)) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title = "European Flights data: Countries with highest traffic by Year",
      subtitle = "Top 10 countries",
      x = "Year",
      y = "Total Number of Flights", 
      caption = "Visualization by: Edmond Nathan")
  
```
 Figure 3.1: Top 10 countries with the highest traffic by year
 
  The above column bar shows the top 10 countries with most air traffic, and from the plot, the color-hex clearly represent each country as presented by the legend. It can be seen that Spain, Germany, and the United Kingdom in no particular order are the top 3 countries with heavier air traffic. This insight can help the airlines and airports to plan and allocate resources accordingly. By analyzing the busiest airports, airlines can also gain insights into the most popular destinations and routes, which can inform their marketing and pricing strategies. this finding could also impart the capacity development of the airports to adequately manage the traffic.
  
  
  To further understand the air traffic, the top busiest airports by month is also calculated and visualized
First the flights data is further analyzed to obtain a table containing the total flight by month, grouped by country , year, and month.

```{r flight by month, warning=FALSE}
#Calculating total flight by country and month
total_flight_by_month <- flights %>% 
  group_by(country, year, month) %>% 
  summarise(total_flights = sum(flight_arrival_departure)) %>% 
  arrange(desc(total_flights))
View(total_flight_by_month)
```

The total flight by month is further manipulated and fed into ggplot: one of the core tidyverse packages used for visualization by utilizing the pipe operator from the *dplyr* library. Then the month is plotted against the total flights filled by country to attain distinct color for each country. The "dodge" was chosen for the position column geometry for clarity and neatness. 

```{r Busiest countries by month, warning=FALSE}
#Visualizing busiest countries by month
total_flight_by_month %>% 
    group_by(month) %>% 
    top_n(10, total_flights) %>% 
    arrange(month, desc(total_flights)) %>% 
    ggplot(aes(x = month, y = total_flights, fill = country)) +
    geom_col(position = "dodge") +
    scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
    theme_bw() +
    labs(title="European Flight Data: Top Countries with Highest Air Traffic by Month", 
         x = "Month", 
         y = "Total Number of Flights", 
         caption = "Visulization by: Edmond Nathan")
```
 Figure 3.2: Top Countries with Highest Air Traffic by Month
 
 It can be seen that Germany, Spain, and United Kingdom remains the three countries with the highest air traffic in Europe even when the data is grouped by country and month.
    
    
### Percentage of countries most affected by the COVID-19 Pandemic
```{r}
# Group the data by country and year, and calculate the total number of flights
flight_totals <- flights %>%
  group_by(country, year) %>%
  summarize(total_flights = sum(total_arrival_departure))

# Calculate the percentage change in total flights for each country between 2019 and 2020
flight_pct_change <- flight_totals %>%
  filter(year %in% c("2019", "2020")) %>%
  group_by(country) %>%
  summarize(percent_change = round((total_flights[year == "2020"] - total_flights[year == "2019"]) / total_flights[year == "2019"] * 100,0))

# Sort the data by percentage change
flight_pct_change <- flight_pct_change %>%
  arrange(desc(percent_change))
View(flight_pct_change)

library(stringr)
# Visualize the results using a bar plot
 ggplot(flight_pct_change, aes(x = reorder(country, percent_change), y = percent_change)) +
  geom_bar(stat = "identity", fill = "brown", width = 0.95) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label = paste0(percent_change,"%")), position = position_stack(vjust = 1.1), face = "bold", angle = 90, size = 3) +
  #coord_flip() +
  #scale_x_discrete(labels = label_wrap(10)) +
  #str_wrap() +
  scale_y_reverse() +
  labs(x = "Country", y = "Percent Change in Total Flights (2019-2020)", title = "Countries Most Affected by COVID-19")

ggsave("most_affected.png", height = 8, width = 14)
```



### How does seasonality affect flight demand and capacity??

```{r warning=FALSE}
# Group data by year and country to calculate the total number of flights.
flight_total_2016_2021 <- flights %>%
  group_by(country, airport_name, year, month) %>%
  summarize(total_flights = sum(flight_arrival_departure))
View(flight_total_2016_2021)
```

To better understand how seasonality air traffic and pattern and trends, the flight data was further grouped by month and year, then a bar chart was plotted: month against monthly total of the number of flights, facet wrapped by year, so the monthly trend of each year can be visualized and analyzed.

```{r warning=FALSE}
#Total flight per month and year
monthly_total_flight<- flight_total_2016_2021 %>% 
  group_by(month, year) %>% 
  summarise(monthly_total=sum(total_flights),
            avg_monthly_total = mean(total_flights)) 
View(monthly_total_flight)




monthly_total_flights<- flight_total_2016_2021 %>% 
  group_by(month) %>% 
  summarise(monthly_total=sum(total_flights),
            avg_monthly_total = round(mean(total_flights,0)))
View(monthly_total_flights)
```
### Visualization of monthly flight summary trend
```{r warning=FALSE}
#Visualize the trend
monthly_total_flight %>% 
  ggplot(aes(x=month, y=monthly_total, fill= month)) +
  geom_bar(stat = 'identity', alpha=0.8) +
  facet_wrap(~year)+
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  theme(axis.text.x = element_text(angle = 90),  
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic"),
        legend.position = "none") +
  labs(title="European Fllight Data: The Total Monthly Flight Pattern",
       x= "Month",
       y= "Flight Monthly Total Arrivals and Departures",
       caption = "Visualization by: Edmond Nathan")



ab<-monthly_total_flights %>% 
  ggplot(aes(x=month, y=(avg_monthly_total), fill= month)) +
  geom_bar(stat = 'identity', alpha=0.8) +
  #facet_wrap(~year)+
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  theme(axis.text.x = element_text(angle = 90),  
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic"),
        legend.position = "none") +
  geom_text(aes(label = paste0(avg_monthly_total)), position = position_stack(vjust = 0.5), color = "white", fontface = "bold", angle = 90, size = 8) +
  labs(title="European Fllight Data: The Average Total Monthly Flight Pattern",
       x= "Month",
       y= "Flight Average Monthly Total Arrivals and Departures",
       caption = "Visualization by: Edmond Nathan")


ggsave("ab.png", height = 8, width = 12)
```
Figure 4.1: The Total Monthly Flight Pattern

The plot shows that the peak period for total flights is always around July and August, which coincides with the summer season. This pattern is consistent across all the years except for a slight anomaly in 2020, most likely due to the pandemic. Typically, each year starts with a slow period before progressively increasing until it peaks during the summer. Prior to the COVID-19 pandemic, the winter period (January, February, and March) consistently recorded the lowest number of total flights. However, there was a sudden dip in 2020 due to the pandemic. The good news is that the plot for 2021 shows signs of recovery, which is encouraging


**The flights monthly average for departures and arrivals**
To further investigate the effect of seasonality of the European flight data, the dataset is grouped by month and the monthly average number of departures and arrivals was calculated as follows:

```{r warning=FALSE}
monthly_flights_avg <- flights %>% 
  group_by(month) %>% 
  summarise(avg_departures = mean(flight_departure),
            avg_arrivals = mean(flight_arrival),
            avg_departure_arrival = mean(flight_arrival_departure))

```


Visualization of the flight monthly average to investigate seasonality effect on the flight dataset.

```{r warning=FALSE}
monthly_flights_avg %>% 
  ggplot(aes(x = month)) +
  geom_line(size = 1, aes(y = avg_departures, color = "flight_departure")) +
  geom_line(size = 1, aes(y = avg_arrivals, color = "flight_arrival")) +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  theme(axis.text.x = element_text(angle = 90),  
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  scale_color_manual(values = c("flight_departure" = "red", "flight_arrival" = "blue")) +
  labs(title ="European Flights Data: Flight demand and capacity by month", 
       x= "Month", 
       y = "Average number of flights",
       caption = "Visualization by: Edmond Nathan") 
```
Figure 4.2: Flight demand and capacity by month
 
Th plot still shows that flight demand and capacity tend to be higher during the summer months (June-August) while they tend to be lower during  winter months (December to February). This information would be useful for airlines to plan their schedules and allocate resources accordingly.


**Visualization of total number of flights per month to uncover patterns**

```{r warning=FALSE}
#Visualization of monthly flight to observe seasonality
monthly_total_flights <- flights %>% 
group_by(month) %>% 
  summarise(total_flights_per_month = sum(flight_arrival_departure))


#Plot the monthly flight trend.
ggplot(monthly_total_flights, aes(x = month, y = total_flights_per_month)) + 
  geom_line(size = 1.5, color = "#dc143c") + 
  geom_point(size = 3, color ="#704241") +
  scale_x_continuous(breaks = seq_along(month.abb), labels = month.abb) +
  theme(axis.text.x = element_text(angle = 90),  
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "italic")) +
  labs(title="European Flight Data: Total Monthly Flights", 
       x = "Month", 
       y = "Total Flights",
       caption = "Visualization by: Edmond Nathan")
```
  Figure 4.3: Total Monthly Flights
  
The chart presented above displays the total count of arrivals and departures per month in the European flight dataset. It is evident from the plot that there exists a seasonal trend in the data, wherein the demand and capacity are relatively high during the summer months (June to August) and low during the winter months (December to January). Moreover, the chart indicates a consistent rise in the overall number of flights from 2016 to 2019, with a substantial decline observed in 2020, which correlates with the emergence of the COVID-19 pandemic. The pattern discerned from the chart can aid airlines in managing their flight schedules and capacity planning to enhance profitability.

