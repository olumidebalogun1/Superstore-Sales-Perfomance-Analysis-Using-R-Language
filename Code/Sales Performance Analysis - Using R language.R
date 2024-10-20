               #####  Recalling some of the Library I will use ##### 

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(corrplot)
library(pastecs)
library(ggpubr)
library(ggrepel)
library(ggstatsplot)


               ##### IMPORTING SALES DATASETS  #####

library(readxl)
Orders <- read_excel("C:/Users/DELL/OneDrive - COVENANT UNIVERSITY/Desktop/Sales Dataset/Orders .xlsx", 
                     sheet = "Orders")
View(Orders)

library(readxl)
Products <- read_excel("C:/Users/DELL/OneDrive - COVENANT UNIVERSITY/Desktop/Sales Dataset/Products.xlsx")
View(Products)

library(readxl)
Location_ <- read_excel("C:/Users/DELL/OneDrive - COVENANT UNIVERSITY/Desktop/Sales Dataset/Location .xlsx")
View(Location_)

library(readxl)
Customers_ <- read_excel("C:/Users/DELL/OneDrive - COVENANT UNIVERSITY/Desktop/Sales Dataset/Customers .xlsx")
View(Customers_)

            #####  Clean Up Products Table before joining with others #####     

glimpse(Products)

Products <- Products %>% 
  select(-c(...5 ,...6))
glimpse(Products)


                    ##### JOINING TABLES TOGETHER #####

df<- left_join(Orders,Products, by ="Product ID")
view(df)

df <- inner_join(df, Location_ , by = "Postal Code")
view(df)

df <- inner_join(df, Customers_, by = "Customer ID")
view(df)

str(df)

                   ##### CONVERT DATE TO DATE FORMAT #####

x <- as.Date(df$`Order Date`)
x

head(x)
class(x)
typeof(x)
str(x)

                   ##### CONVERT TO NUMERIC #####

Year <- as.numeric(format(x,"%Y"))
Year

Month <- as.numeric(format(x, "%m"))
Month

Day <- as.numeric(format(x,"%d"))
Day

               ##### ADD CREATED COLUMNS TO EXISTING DATE COLUMNS and checking for missing values #####

df <- cbind(df, Year, Month, Day)
glimpse(df)
view(df)

# Check for missing data
sum(is.na(df))

#########################################################################################################

         ##### 1. SALES, PROFIT, AND VOLUME PERFORMANCE SUMMARY #####

  #####  1a. Yearly Sales, Profit, and Volume Performance with Percentage Growth  #####

# Installing and loading necessary libraries
install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("gtExtras")
library(gtExtras)

library(lubridate)

# Converting year column to a Date format
df$'Order Date' <- as.Date(df$'Order Date')

# Extracting year from the converted order_date column
df$year <- format(df$'Order Date', "%Y")

# Summarizing the data to calculate Total Sales, Total Profit, and Total Quantity by year
yearly_summary <- df %>%
  group_by(year) %>%
  summarise(
    Total_Sales_Amount =round(sum( Sales )/1e3, 2),
    Total_Profit = round(sum(Profit )/1e3, 2),
    Total_Quantity  = round(sum(Quantity)/1e3, 2)
  )
view(yearly_summary)

# Calculating the percentage difference between the current and previous year for sales amount, profit, and quantity
yearly_summary <- yearly_summary %>%
  arrange(year) %>%
  mutate(
    Pct_diff_Sales_Amount = paste0(round((Total_Sales_Amount  - lag(Total_Sales_Amount )) / lag(Total_Sales_Amount ) * 100, 2), "%"),
    Pct_diff_Profit = paste0(round((Total_Profit  - lag(Total_Profit))  / lag(Total_Profit ) * 100, 2), "%"),
    Pct_diff_Quantity = paste0(round((Total_Quantity  - lag(Total_Quantity))  / lag(Total_Quantity)  * 100, 2), "%")
  )
view(yearly_summary)

# Plotting the Yearly Summary 
plot_yearly_summary <- yearly_summary %>% 
  gt() %>% 
  tab_header(title = "Yearly Sales, Profit, and Volume Performance (in $ Thousand) with Percentage Growth") %>% 
  cols_align(align = "left")

plot_yearly_summary 

plot_yearly_summary <- plot_yearly_summary %>% 
  gt_theme_pff() %>% 
  gt_highlight_rows(column = Total_Sales_Amount, fill="lightpink") %>%
  gt_highlight_rows(column = Total_Profit, fill="lightblue") %>%
  gt_highlight_rows(column = Pct_diff_Sales_Amount, fill="lightpink") %>%
  gt_highlight_rows(column = Pct_diff_Profit, fill="lightblue") %>% 
  gt_highlight_rows(rows = Pct_diff_Sales_Amount < 0 , fill="steelblue") 

plot_yearly_summary

        #####  1b. Summary Statistics Of Sales And Profit Performance  #####

# Group the data by Category, Segment, and Region, then summarize it by Total Sales and Total Profit
summarized_sales <- df %>%
  group_by(Category, Segment, Region) %>%
  summarise(
    Total_Sales_in_K = round(sum(Sales)/ 1e3, 2),
    Total_Profit_in_K = round(sum(Profit)/ 1e3, 2),
    .groups = "drop" ) # to avoid keeping grouped structure
  
view(summarized_sales)

        ##### 1b(i).Top Fifteen (15) by Sales and corresponding Profit  ######

# top 15 records based on Total Sales
top_15_sales <- summarized_sales %>%
  slice_max(Total_Sales_in_K, n = 15)       # Top 15 by Sales out of38
view(top_15_sales)

 #####  1b(ii). Creating for Top Fifteen (15) by Sales and corresponding Profit #####

# Plotting Top N Performance by Sales 
plot <- top_15_sales %>% 
  gt() %>% 
  tab_header(title = "Top Fifteen (15) by Sales (In Thousand)") %>% 
  cols_align(align = "left")

plot <- plot %>% 
  gt_theme_pff() %>% 
  gt_highlight_rows(rows = Total_Sales_in_K >=100.00, fill="lightpink") %>% 
  gt_color_rows(columns = "Total_Sales_in_K", palette = "Pastel1")

plot

     ##### 1b(iii). Top fifteen (15) by Profit and corresponding Sales  ######

# top 15 records based on Total Profit
top_15_profit <- summarized_sales %>%
  arrange(desc(Total_Profit_in_K)) %>% 
  slice_max(Total_Profit_in_K, n = 15)       # Top 15 by Profit out of38

view(top_15_profit)

#####  1b(iv). Creating for Top Fifteen (15) by Profit and corresponding Sales #####

# Plotting Top N Performance by Profit 
plot_profit <- top_15_profit %>% 
  gt() %>% 
  tab_header(title = "Top Fifteen (15) by Profit (In Thousand)") %>% 
  cols_align(align = "left")

plot_profit <- plot_profit %>% 
  gt_theme_pff() %>% 
  gt_highlight_rows(rows = Total_Profit_in_K >=15.00, fill="lightblue") %>% 
  gt_color_rows(columns = "Total_Profit_in_K", palette = "Pastel1")

plot_profit

#####################################################################################

      ##### 2. SALES PERFORMANCE ANALYSIS BY SEGMENT AND CATEGORY ACROSS REGIONS  #####

install.packages("ggsci")
library(ggsci)

# Representing Million as M and Thousand as K
df_sales <- df %>% 
  mutate(Sales_K = round(Sales / 1e3, 2)) %>% 
  mutate(Profit_K = round(Profit / 1e3, 2)) %>% 
  mutate(Quantity_K = round(Quantity / 1e3, 3))

view(df_sales)

           ##### 2a. Total Sales by  Category for Each Region  #####
# Grouping by Region
df_sales_C_R <- df_sales %>% 
  group_by( Category, Region) %>% 
  summarise(Regional_Sales = sum(Sales_K))
view(df_sales_C_R)

# Recalling Library
library(forcats)  # for reordering factors

# Reorder the 'fill_group' in descending order based on the sum of 'value'
df_sales_C_R$Region <- fct_reorder(df_sales_C_R$Region, df_sales_C_R$Regional_Sales, .fun = sum, .desc = TRUE)

#Bar plot
ggplot(df_sales_C_R, aes(x = reorder(Category, -Regional_Sales), y = Regional_Sales, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Regional_Sales, 1), "k")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4, size = 3) +  # Adjust size and position of text
  labs(title = "Total Sales by Category for Each Region", 
       x = "Region", 
       y = " Total Sales (in $ thousand)",
       fill= "Region") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "top") +
  theme_pubr() +
  scale_fill_npg()+
  facet_wrap(~ Region )

# Create a new variable combining Region and Category for Kruskal-Wallis test
data_sales_1 <- df %>%
  mutate(Region_Category = interaction(Region, Category))

# Kruskal-Wallis test for Sales by combined Region and Category
kruskal_test <- kruskal.test(Sales ~ Region_Category, data = data_sales_1)
print(kruskal_test)

             ##### 2b. Total Profit by  Category for Each Region  #####
# Grouping by Region
df_profit_C_R <- df_sales %>% 
  group_by( Category, Region) %>% 
  summarise(Regional_Profit = sum(Profit_K))
view(df_profit_C_R)

# Reorder the 'fill_group' in descending order based on the sum of 'value'
df_profit_C_R$Region <- fct_reorder(df_profit_C_R$Region, df_profit_C_R$Regional_Profit, .fun = sum, .desc = TRUE)

#Bar plot
ggplot(df_profit_C_R, aes(x = reorder(Category, -Regional_Profit), y = Regional_Profit, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Regional_Profit, 1), "k")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4, size = 3) +  # Adjust size and position of text
  labs(title = "Total Profit by Category for Each Region", 
       x = "Region", 
       y = " Total Profit (in $ thousand)",
       fill= "Region") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "top") +
  theme_pubr() +
  scale_fill_npg()+
  facet_wrap(~ Region )

# Create a new variable combining Region and Category for Kruskal-Wallis test
data_profit_1 <- df %>%
  mutate(Region_Category = interaction(Region, Category))

# Kruskal-Wallis test for Sales by combined Region and Category
kruskal_test <- kruskal.test(Profit ~ Region_Category, data = data_profit_1)
print(kruskal_test)

                 ##### 2c. Total Sales by Segment for Each Region  #####
# Grouping by Region
df_sales_S_R <- df_sales %>% 
  group_by( Segment, Region) %>% 
  summarise(Regional_Sales = sum(Sales_K))
view(df_sales_S_R)

# Reorder the 'fill_group' in descending order based on the sum of 'value'
df_sales_S_R$Region <- fct_reorder(df_sales_S_R$Region, df_sales_S_R$Regional_Sales, .fun = sum, .desc = TRUE)

#Bar plot
ggplot(df_sales_S_R, aes(x = reorder(Segment, -Regional_Sales), y = Regional_Sales, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Regional_Sales, 1), "k")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4, size = 3) +  # Adjust size and position of text
  labs(title = "Total Sales by Segment for Each Region", 
       x = "Region", 
       y = " Total Sales (in $ thousand)",
       fill= "Region") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "top") +
  theme_pubr() +
  scale_fill_npg()+
  facet_wrap(~ Region )

# Create a new variable combining Region and Category for Kruskal-Wallis test
data_sales_2 <- df %>%
  mutate(Region_Segment = interaction(Region, Segment))

# Kruskal-Wallis test for Sales by combined Region and Category
kruskal_test <- kruskal.test(Sales ~ Region_Segment, data = data_sales_2)
print(kruskal_test)

            ##### 2d. Total Profit by Segment for Each Region  #####
# Grouping by Region
df_profit_S_R <- df_sales %>% 
  group_by( Segment, Region) %>% 
  summarise(Regional_Profit = sum(Profit_K))
view(df_profit_S_R)

# Reorder the 'fill_group' in descending order based on the sum of 'value'
df_profit_S_R$Region <- fct_reorder(df_profit_S_R$Region, df_profit_S_R$Regional_Profit, .fun = sum, .desc = TRUE)

#Bar plot
ggplot(df_profit_S_R, aes(x = reorder(Segment, -Regional_Profit), y = Regional_Profit, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Regional_Profit, 1), "k")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4, size = 3) +  # Adjust size and position of text
  labs(title = "Total Profit by Segment for Each Region", 
       x = "Region", 
       y = " Total Profit (in $ thousand)",
       fill= "Region") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "top") +
  theme_pubr() +
  scale_fill_npg()+
  facet_wrap(~ Region )

# Create a new variable combining Region and Category for Kruskal-Wallis test
data_profit_2 <- df %>%
  mutate(Region_Segment = interaction(Region, Segment))

# Kruskal-Wallis test for Sales by combined Region and Category
kruskal_test <- kruskal.test(Profit ~ Region_Segment, data = data_profit_2)
print(kruskal_test)
                   
################################################################################
            
           #####  3. SUB-CATEGORY THAT ARE THE TOP PERFORMERS  #####

                     ##### 3a. Total Sales by Sub-Category  #####

# Calculating Total Sales and Total Profit
df_SubCat <- df %>% 
  group_by(`Sub-Category`) %>% 
  summarise(Sales_Sum = sum(Sales),
            Profit_Sum = sum(Profit), .groups = "drop") %>% 
  mutate(Sales_K = round(Sales_Sum / 1e3, 2)) %>% 
  mutate(Profit_K = round(Profit_Sum / 1e3, 2)) %>% 
  arrange(desc(Sales_K)) 

view(df_SubCat)

# Bar plot of Total Sales by Sub-Category - To show the top performers 
ggplot(df_SubCat, aes(x= reorder(`Sub-Category`, - Sales_K), y= Sales_K)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(Sales_K, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4, size = 3) +  # Adjust size and position of text
  labs(title = "Total Sales by Sub-Category", 
       x = "Sub-Category", 
       y = "Sales (in thousand)") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform the Kruskal-Wallis test on Sales by Sub-Category  
kruskal_test_SalesSubcategory <- kruskal.test( Sales ~ `Sub-Category`, data=df)
kruskal_test_SalesSubcategory

              ##### 3b. Total Profit by Sub-Category  #####
  # Bar plot of Total Profit by Sub-Category - To show the top performers #####
ggplot(df_SubCat, aes(x= reorder(`Sub-Category`, - Profit_K), y= Profit_K)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(Profit_K, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4, size = 3) +  # Adjust size and position of text
  labs(title = "Total Sales by Sub-Category", 
       x = "Sub-Category", 
       y = "Profit (in thousand)") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform the Kruskal-Wallis test on Profit by Sub-Category
kruskal_test_ProfitSubcategory <- kruskal.test(Profit ~ `Sub-Category`, data=df)
kruskal_test_ProfitSubcategory

##################################################################################

          ##### 4. OVERALL SALES PERFORMANCE AND TRENDS #####

#Converting of date variable to year & month
df_OD <- df %>% arrange(`Order Date`)         # arrange Order Date accordingly
view(df_OD)

# Formatting Dated Variables  
df_OD <- df_OD %>%                           
  mutate(`Order Date`=as.Date(`Order Date`)) %>% 
  mutate(Year_Month = format(`Order Date`, "%Y-%m")) %>% 
  mutate(Year_Month = paste0(Year_Month, "-01"))
view(df_OD)

# Changing it from char to date
df_OD$Year_Month <- as.Date(df_OD$Year_Month)        

class(df_OD$Year_Month)

# Grouping data into year and month
df_OD1 <- df_OD %>% 
  group_by(Year, Month) %>% 
  summarise(Monthly_Sales = sum(Sales),
            Yearly_Sales = sum(Sales), .groups = "drop")
view(df_OD1)

                ##### 4b. Monthly Sales Cyclical Patterns  #####

#Box plot to see the Monthly Distribution
df_bp <- df_OD1 %>% 
  mutate(Month = as.factor(Month)) %>% 
  mutate(Year = as.factor(Year))

class(df_bp$Month)


ggplot(df_bp, aes(Month, Monthly_Sales, fill = Month)) +
  geom_boxplot(outliers=FALSE, alpha=0.7) +
  theme_minimal() +
  labs(title= "Monthly Sales",
       x = "Month",
       y = "Total Sales") 

# Performing the Kruskal-Wallis test on Monthly Sales 
kruskal_MonthlySales <- kruskal.test(Monthly_Sales ~ Month, data=df_bp)
kruskal_MonthlySales 

##### 4c.  Time Series Analysis of Overall Sales Performance and Trends  #####

# Creating the time series object
Sales_ts <- ts(df_OD1$Monthly_Sales, start = c(2020,1), frequency = 12)

# Time series plot without the x-axis (xaxt = "n")
plot(Sales_ts, xaxt = "n", xlab = "Year_Month", ylab = "Sales", type = "l",
     lwd=2, main = "Sales per Month", col ="red")
abline(h = mean(Sales_ts), col = "blue", lty = 2)  
points(Sales_ts, pch = 16, col = "steelblue", cex = 0.7)
grid(col = "gray", lty = "dotted")

# Extracting the time points from the time series and format them as month-year
time_labels <- time(Sales_ts)
month_year_labels <- format(as.Date(paste(floor(time_labels), (time_labels - floor(time_labels)) * 12 + 1, "01", sep="-")), "%b-%Y")

# Adding the x-axis with formatted month-year labels
axis(1, at = time(Sales_ts), labels = month_year_labels, las = 2, cex.axis = 0.8)

################################################################################

      ##### 5. CORRELATION ANALYSIS (Examining  Relationship) #####

# Grouping year and month and calculating Total Sum, Profit, Quantity, Discount , and the Count of Customers and Orders                     
scatter_group <- df %>% 
  group_by(Year, Month) %>%
  summarise(Sum_Sales = sum(Sales),
            Sum_Profit = sum(Profit),
            Sum_Quantity = sum(Quantity),
            sum_Discount = sum(Discount),
            No_Customers = n_distinct(`Customer ID`),
            No_Orders = n_distinct( `Order ID`), .groups = "drop")

view(scatter_group)

            ##### 5a. The Impact of Sales on Profit  #####

# Scatter plot of sales and profit
p_1<-  ggplot(scatter_group, aes(Sum_Sales, Sum_Profit, fill = Year, colour = Year)) +
  geom_point(size=2, alpha=0.5) +
  geom_smooth(method = "lm", se= TRUE, fullrange= FALSE, level=0.95) +
  theme_minimal() +
  labs(title = "Correlation of Sales and Profit over Time (Months by Year)",
       x = "Sales ($)",
       y = "Profit ($)")

p_1 + stat_ellipse()

# Scatter plot and testing for p value and  corr. value and the 
g_1 <- ggscatterstats(scatter_group, x = Sum_Sales,
                      y = Sum_Profit, type = "nonparametric") 

g_1 + stat_ellipse()

# correlation test  
c_1 <- cor.test(scatter_group$Sum_Sales, scatter_group$Sum_Profit, method = "spearman")
c_1

            #####  5b. The Impact of Quantity on Sales  #####

# Scatter plot of sales and quantity
p_2 <-ggplot(scatter_group, aes(Sum_Quantity, Sum_Sales, fill = Year, colour = Year)) +
  geom_point(size=2, alpha=0.5) +
  geom_smooth(method = "lm", se= TRUE, fullrange= FALSE, level=0.95) +
  theme_minimal() +
  labs(title = "Correlation of Sales and Quantity Sold over Time (Months by Year)",
       x = "Quantity Sold",
       y = "Sales ($)")

p_2 + stat_ellipse()

# Scatter plot and testing for p value and  corr. value and the 
g_2 <- ggscatterstats(scatter_group, x = Sum_Quantity,
                      y = Sum_Sales, type = "nonparametric") 

g_2 + stat_ellipse()

# correlation test 
c_2 <- cor.test(scatter_group$Sum_Quantity, scatter_group$Sum_Sales, method = "spearman")
c_2

                ##### 5c. The Impact of Discount on Sales    #####

# Scatter plot of sales and iscount
p_3 <-ggplot(scatter_group, aes( sum_Discount,Sum_Sales , fill = Year, colour = Year)) +
  geom_point(size=2, alpha=0.5) +
  geom_smooth(method = "lm", se= TRUE, fullrange= FALSE, level=0.95) +
  theme_minimal() +
  labs(title = "Correlation of Sales and Discount over Time (Months by Year)",
       x = "Discount ($)",
       y = "Sales ($)")
p_3

p_3 + ggsave("sales vs Discount.jpg")

# Scatter plot and testing for p value and  corr. value and the
g_3 <- ggscatterstats(scatter_group, x = sum_Discount ,
                      y = Sum_Sales, type = "nonparametric") 

g_3 + stat_ellipse()

# correlation test
c_3 <- cor.test(scatter_group$sum_Discount, scatter_group$Sum_Sales, method = "spearman")
c_3

#######################################################################################

##### 6. KEY DRIVERS OF SALES PERFORMANCE AND PROFITABILITY: INSIGHTS FROM FEATURE IMPORTANCE & REGRESSION ANALYSIS  #######
   
install.packages("randomForest")
library(randomForest)

           #####   6a. Key Sales Performance Drivers    #####

# To determine which variables have the greatest influence on sales using Random Forest
sales_rf_model <- randomForest(Sales ~ Segment + Category + Region + Quantity + Discount + Month  , data = df)
importance_rf <- importance(sales_rf_model)
varImpPlot(sales_rf_model, main = "Feature Importance Plot")

# The Key Drivers of Sales using Regression Model  
sales_model <- lm(Sales ~ Quantity + Discount + Profit + Segment + Category + Region + Month , data = df)
summary(sales_model)
#  Coefficients help identify the strength and direction of influence of each variable on sales.

             #####  6b. Key Drivers of Profitability    #####

# To determine which variables have the greatest influence on profit using Random Forest
sales_rf_model <- randomForest( Profit ~ Sales + Segment + Category + Region + Quantity + Discount + Month  , data = df)
importance_rf <- importance(sales_rf_model)
varImpPlot(sales_rf_model, main = "Feature Importance Plot")

# The Key Drivers of Profit
profit_model <- lm(Profit ~ Sales + Quantity + Discount + Segment + Category + Region + Month , data = df)
summary(profit_model)
#  Coefficients help identify the strength and direction of influence of each variable on profit.

####################################################################################

            ##### 7. TRENDS AND SEASONALITY ANALYSIS IN SALES  #####

# Decomposition of Sales Performance Trend into Trends And Seasonality

plot(stl(Sales_ts, "per"), main = "Sales Decomposition with Seasonal and Trend Components")

#######################################################################################

       #####  8. FORECASTING SALES TRENDS WITH TIME SERIES ANALYSIS    #####

           #####  8a. Augmented Dickey Fuller Test for Stationarity  #####
library(tseries)

# Recalling the time series object
Sales_ts<- ts(df_OD1$Monthly_Sales, start = c(2020,1), end = c(2023,12), frequency = 12) 

# Checking stationarity 
adf_test <- adf.test(Sales_ts)
print(adf_test)

# Performing the difference of the series to make it stationary If the p-value > 0.05
# Checking for stationarity again
adf_test_diff <- adf.test(diff(Sales_ts))
print(adf_test_diff)

               ######  8b. Sales Trend Forecast using ARIMA Model  ########
library(forecast)         

# Since the series is stationary, fit ARIMA and specify d = 1 (1st order differencing)
arima_model <- auto.arima(Sales_ts, d = 1)  # 'd=1' means first-order differencing
summary(arima_model)

# Forecasting Sales Trends using ARIMA Model
arima_model <- auto.arima(Sales_ts)
forecast_sales <- forecast(arima_model, h=24)
plot(forecast_sales, main = "Sales Forecast for the Next 2 Years",
     xlab = "Year_Month", ylab = "Sales Amount")

################################################################################               
  
               #####  9. CLUSTERING AND CUSTOMER SEGMENTATION  #####
  
# Group by Customer ID  
df_f <- df %>% 
  group_by(`Customer ID`) %>% 
  mutate(Transaction=frequency('customer ID'))
  
view(df_f)
  
# Group by Customer Name and also calculating Total Sales and Total Transaction 
df_c <- df_f %>%
  group_by(`Customer Name`) %>% 
  summarise(Sales_C = sum(Sales),
            No_Transaction = sum(Transaction), .groups = "drop") %>% 
  arrange(desc(Sales_C)) %>% 
  mutate(Average_Transaction_Value = Sales_C/No_Transaction) %>% 
  mutate(Sales_percent_C = Sales_C*100/sum(Sales_C)) %>% 
  mutate(Cum_percernt_C = cumsum(Sales_percent_C)) %>% 
  select(`Customer Name`, Sales_C, No_Transaction, 
         Average_Transaction_Value , Sales_percent_C, Cum_percernt_C) %>% 
  arrange(desc(Average_Transaction_Value))
view(df_c)
  
          ##### 9b.  Clustering Model - for group of three (3)   #####
  
library(cluster)
  
kmeans_result <- kmeans(df_c[, c("Sales_C", "No_Transaction")], centers = 3)
df_c$cluster <- kmeans_result$cluster
plot(df_c$Sales_C, df_c$No_Transaction, col = df_c$cluster,  main = "Customer Segmentation using Clustering",
     xlab = "Total Sales Amount (in $ Million)", ylab = "Total Sales Quantity (in $ Thousand)")

##################################################################################

##### 10.  CREATING TABLE FOR TOP N CUSTOMERS USING SLICE MAX & GTEXTRAS #####

customer_sales <- df %>%
  group_by(`Customer Name`) %>% 
  summarise(Total_Sales = sum(Sales)) %>% 
  arrange(desc(Total_Sales)) %>% 
  mutate(CumulativeSales = cumsum(Total_Sales)) %>% 
  mutate(Salespercent = Total_Sales*100/sum(Total_Sales)) %>% 
  mutate(Cumulative_Salespercent = cumsum(Salespercent))

view(customer_sales)

        ##### 10b. Customers that made up 10% of the total Sales #####

# Identify the top-performing customers that drive the majority of revenue.
Top_n <- customer_sales %>% 
  slice_max(Total_Sales, n=10)
view(Top_n)     # Top 10 customers out of 793

           #####  10c. Creating Table for Top N Customers   #####

# Plotting Top N Customers 
plot <- Top_n %>% 
  select(`Customer Name`, Total_Sales, Salespercent) %>%
  mutate(Salespercent=round(Salespercent,2)) %>% 
  gt() %>% 
  tab_header(title = "Top Ten(10) Customers") %>% 
  cols_align(align = "left")

plot <- plot %>% 
  gt_theme_pff() %>% 
  gt_highlight_rows(rows = Total_Sales >=15100, fill="lightpink") %>% 
  gt_plt_bar_pct(Salespercent, fill = "steelblue",height = 15, width = 100) %>% 
  gt_color_rows(columns = "Total_Sales", palette = "Pastel1")

plot

#########################################################################################################