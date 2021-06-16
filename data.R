#-------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------

library(DT)
library(gameofthrones)
library(gt)
library(highcharter)
library(janitor)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(readxl)
library(scales)
library(tidyverse)


#-------------------------------------------------------------------
# Load csv files (title master reports)
#-------------------------------------------------------------------

Elsevier_TM <- read.csv("T:/Statistik/ALMA_ART/Rohdaten_Counter_Master/Elsevier/Elsevier_Title_Master_Report.csv", header = T, skip = 12, sep = ",")


#-------------------------------------------------------------------
# Explore data
#-------------------------------------------------------------------

unique(Elsevier_TM$Access_Method)
unique(Elsevier_TM$Publisher)

sapply(Elsevier_TM, function(x) length(unique(x)))

sort(table(Elsevier_TM$Metric_Type), decreasing = TRUE)


#-------------------------------------------------------------------
# Transform data
#-------------------------------------------------------------------


elsevier_tm_test <- Elsevier_TM %>%
  filter(Metric_Type == "Unique_Item_Requests" | Metric_Type == "Unique_Title_Requests" | Metric_Type == "No_License") %>%
  mutate(Total_2019 = rowSums(.[18:29])) %>%
  mutate(Total_2020 = rowSums(.[30:41])) %>%
  select(-c(3, 5, 15, 17:41))


elsevier_tm_test_2 <- Elsevier_TM %>%
  filter(Metric_Type == "Unique_Item_Requests" | Metric_Type == "Unique_Title_Requests" | Metric_Type == "No_License") %>%
  mutate(Total_2019 = rowSums(.[18:29])) %>%
  mutate(Total_2020 = rowSums(.[30:41])) %>%
  select(-c(3, 5, 15, 17:41)) %>%
  group_by(.[1:8], Data_Type, Section_Type, Access_Type, Metric_Type) %>%
  summarize_at(vars(Total_2019:Total_2020), sum)

elsevier_tm_test_3 <- elsevier_tm_test_2 %>%
  filter(Section_Type != "Chapter") %>%
  group_by(Data_Type, Access_Type, Metric_Type) %>%
  summarize_at(vars(Total_2019:Total_2020), sum)

elsevier_tm_test_3_long <- elsevier_tm_test_3 %>%
  pivot_longer(
    cols = starts_with("Total"),
    names_to = "Total",
    values_drop_na = TRUE)

elsevier_tm_test_3_long <- elsevier_tm_test_3_long %>%
  filter(Data_Type == "Journal")

elsevier_tm_test_3_long$Access_Type[elsevier_tm_test_3_long$Metric_Type == "No_License" ] <- "No_License"
elsevier_tm_test_3$Access_Type[elsevier_tm_test_3$Metric_Type == "No_License" ] <- "No_License"



elsevier_tm_test_3 <- elsevier_tm_test_3 %>%
  mutate(perc_change = (Total_2020/Total_2019-1)*100) %>%
  mutate(publisher = "Elsevier", .before = Data_Type)



elsevier_tm_test_3 %>%
  ungroup() %>%
  slice_tail(n = 3) %>%
  hchart("dumbbell",
         hcaes(x = Access_Type, high = Total_2019, low = Total_2020),
         name = "Access Type") %>%
  hc_chart(inverted = TRUE)




categories_grouped <- elsevier_tm_test_3 %>%
  ungroup() %>%
  slice(-2) %>%
  select(Data_Type, Access_Type) %>%
  group_by(name = Data_Type) %>%
  summarise(categories = list(Access_Type)) %>%
  list_parse()

elsevier_tm_test_3 %>%
  ungroup() %>%
  slice(-2) %>%
  arrange(Data_Type, desc(perc_change)) %>%
  hchart( "bar", hcaes(y = perc_change, x = Access_Type)
          ) %>%
  hc_xAxis(categories = categories_grouped, labels = list(style = list(fontSize = "12px")
  )) %>%
  hc_add_dependency("plugins/grouped-categories.js")





elsevier_tm_test_3 %>%
  ungroup() %>%
  slice_tail(n = 2) %>%
hchart(
  "xrange",
  hcaes(x = Total_2019, x2 = Total_2020, y = Access_Type)
)%>%
  hc_chart(inverted = TRUE)

hchart(elsevier_tm_test_3,
       "line",
       hcaes(x = Total_2019, y = Total_2020, group = Data_Type, color = Access_Type))



elsevier_tm_test_3_long %>%
 # filter(Total == "Total_2020") %>%
  hchart(
    "bar",
    hcaes(y = value, x = Access_Type, group = Total))

elsevier_tm_test_3 %>%
  ungroup() %>%
  slice_tail(n = 2) %>%
hchart("bullet", hcaes(x = Access_Type, y = Total_2020, target = Total_2019), color = "black") %>%
  hc_chart(inverted = TRUE)




data(mpg, package = "ggplot2")

mpgg <- mpg %>%
  filter(!manufacturer %in% c("volkswagen", "chevrolet")) %>%
  filter(class %in% c("compact", "midsize", "subcompact")) %>%
  group_by(class, manufacturer) %>%
  summarize(count = n()) %>%
  ungroup()

categories_grouped <- mpgg %>%
  select(class, manufacturer) %>%
  group_by(name = class) %>%
  summarise(categories = list(manufacturer)) %>%
  list_parse()


#-------------------------------------------------------------------------
# Correlation Tests
#-------------------------------------------------------------------------


x <- c(400,	450,	516,	551,	594,	503,	621,	626,	690,	737,	780,	718)
y <- c(9892,	10195,	11071,	10947,	11209,	11191,	11805,	11767,	12142,	12454,	13071,	13282)

cor(x,y,method='pearson')
cor(x,y,method='kendall')
cor(x,y,method='spearman')

######################################################################################
# Stackoverflow question : four variables in treemap
######################################################################################

library(highcharter)

set.seed(42)
n <- 12
dat <- data.frame(sector = factor(rep(LETTERS[1:4], n/4)),
                  ticker = rep(letters[1:12], 1),
                  weight = sample(1:10, n, replace = TRUE),
                  performance = sample(1:400, n, replace = TRUE))
dat

treemap_data <- data_to_hierarchical(dat, c(sector, ticker), weight)

hchart(treemap_data, type = "treemap")

hchart(
  data_to_hierarchical(
    dat, c(sector, ticker), weight),
  type = "treemap"
)


hctreemap2(
  dat,
  group_vars = c("sector", "ticker"),
  size_var = "weight",
  layoutAlgorithm = "squarified",
  levelIsConstant = FALSE,
  levels = list(
    list(level = 1, dataLabels = list(enabled = TRUE), borderWidth = 3),
    list(level = 2, dataLabels = list(enabled = TRUE), borderWidth = 1)
  )
)

######################################################################################
# Order bar charts and add labels
######################################################################################

set.seed(42)
n <- 12
dat <- data.frame(sector = factor(rep(LETTERS[1:4], n/4)),
                  ticker = rep(letters[1:12], 1),
                  weight = sample(1:10, n, replace = TRUE),
                  performance = sample(1:400, n, replace = TRUE))

# Create sum column and arrange
dat <- dat %>%
  group_by(sector) %>%
  mutate(sum = sum(weight)) %>%
  arrange(desc(sum))

# Dont use group variable if you want to order

dat %>%
  hchart(
  type = "bar",
  hcaes(x = sector, y = weight, color = performance),
  stacking = "normal",
  name = "Weight",
  dataLabels = list(enabled = TRUE,
                    format = "{point.ticker}",
                    filter = list(property = "weight",
                                  operator = ">",
                                  value = 5
                                  ))
)


######################################################################################
# Packedbubble with four variables
######################################################################################


dat %>%
  hchart(
    hcaes(name = ticker, value = weight, color = performance, group = sector),
    type = "packedbubble",
    layoutAlgorithm = list(splitSeries = TRUE,
                           parentNodeLimit = TRUE),
    dataLabels = list(enabled = TRUE,
                      format = "{point.ticker}",
                      filter = list(property = "weight",
                                    operator = ">",
                                    value = 5
                      ))
  )

# https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/demo/packed-bubble-split/


######################################################################################
# drilldown (noch nicht ausgefeilt)
######################################################################################


library(highcharter)
library(dplyr)
library(purrr) # for the map() function

the_dates <- c(rep("2021-01-01",3),
               rep("2021-02-01",3))
the_values <- c(2,3,4,5,6,7)
the_group <- c("Group_A","Group_B","Group_B",
               "Group_A","Group_B","Group_B")
the_class <- c("X","Y","Z",
               "X","Y","Z")

the_data <- data.frame(the_dates,
                       the_group,
                       the_class,
                       the_values,
                       stringsAsFactors = FALSE)

mean_data <- the_data %>%
  group_by(the_dates) %>%
  summarise(mean_values = mean(the_values))

drill_data <- the_data %>%
  group_nest(the_dates) %>%
  mutate(
    id   = the_dates,
    type = "column",
    data = map(data, ~ .x %>%
                 mutate(
                   name = the_class,
                   y    = the_values
                 ) %>%
                 list_parse())
  )


mean_data %>%
  hchart(
    type = "spline",
    hcaes(x = the_dates, y = mean_values, drilldown = the_dates, name = the_dates),
    name = "Dates",
    colorByPoint = TRUE) %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(drill_data)
  )



index <- c("Dax", "Nasdaq")
perc_1yr <- c(2,30)
perc_3yr <- c(30,250)
size <- c(10, 100)


the_data <- data.frame(index,
                       perc_1yr,
                       perc_3yr,
                       size,
                       stringsAsFactors = FALSE)


drill_data <- the_data %>%
  group_nest(index) %>%
  mutate(
    id   = index,
    type = "column",
    data = map(data, ~ .x %>%
                 mutate(
                   name = size,
                   y    = perc_1yr
                 ) %>%
                 list_parse())
  )


the_data %>%
hchart(
  type = "scatter",
  hcaes(x = perc_3yr, y = perc_1yr, group = index, size = size)
) %>%
  hc_drilldown(
    allowPointDrilldown = FALSE,
    series = list_parse(drill_data)
  )




