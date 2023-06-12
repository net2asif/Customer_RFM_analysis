library(dplyr)
library(Hmisc)
library(tidyverse)
Sample_data |>
  mutate(date_change = as.Date(`Order Date`)) -> first_p
first_p |>
  group_by(`Customer ID`, `Customer Name`) |>
  summarise(
    first_transaction_date = min(date_change),
    last_transaction_date = max(date_change),
    total_transactions = n_distinct(`Order ID`),
    total_revenue = sum(Sales),
    total_profit  = sum(Profit),
    days_since_last_transaction = as.numeric(difftime(
      as.Date("2019-01-31"), last_transaction_date, units = "days"
    )),
    recency = as.numeric(max(as.Date("2019-01-31") - max(date_change)))
  ) -> transaction_rec
# Calculate the median number of days between orders
first_p |>
  group_by(`Customer ID`, `Customer Name`, `Order ID`) |>
  summarise(Invoice_date = max(date_change)) |>
  mutate(days_between_orders = as.integer(Invoice_date - lag(Invoice_date, order_by = Invoice_date))) |>
  
  summarise(median_days_between_orders = median(days_between_orders, na.rm = TRUE)) -> med_days
transaction_rec |>
  mutate(
    revenue_per_transaction = total_revenue / total_transactions,
    customer_lifetime_value = revenue_per_transaction * 12 * 3
  ) |>
  select(`Customer ID`, customer_lifetime_value) -> clv


transaction_rec |>
  ungroup()|>
  mutate(
    interval_recency = cut2(recency, g = 5),
    interval_frequency = cut2(total_transactions, g = 5),
    interval_spending = cut2(total_revenue, g = 5)
  ) -> rfm_rec
rfm_rec |> 
  mutate(
    score_recency = as.integer(fct_rev(interval_recency)),
    score_frequency = as.integer(interval_frequency),
    score_revenue = as.integer(interval_spending),
    rfm_score = (100 * score_recency) + (10 * score_frequency) + (score_revenue)
  ) |> 
  
  mutate(segment = case_when(
    rfm_score %in% c("555", "554", "545", "544", "455") ~ "1. Best Customers - High Value",
    rfm_score %in% c("535", "534", "525", "524", "435", "425") ~ "2. Loyal Customers - Mid Value",
    rfm_score %in% c("515", "514", "505", "504", "415", "405") ~ "3. Potential Loyalist - Mid Value",
    rfm_score %in% c("535", "534", "525", "524", "435", "425") ~ "4. Recent Customers - Mid Value",
    rfm_score %in% c("111", "112", "121", "122", "211", "212", "221", "222") ~ "5. Churned Customers - Low Value",
    rfm_score %in% c("545", "544", "535", "534") ~ "6. Best Customers - Mid Value",
    rfm_score %in% c("515", "514", "505", "504", "415", "405", "425") ~ "7. Loyal Customers - Low Value",
    rfm_score %in% c("455", "445", "435") ~ "8. High Value - Infrequent Customers",
    rfm_score %in% c("425", "415", "405") ~ "9. Mid Value - Infrequent Customers",
    rfm_score %in% c("345", "335", "325") ~ "10. Low Value - Infrequent Customers",
    rfm_score %in% c("515", "514", "505", "504") ~ "11. High Value - New Customers",
    rfm_score %in% c("425", "415", "405") ~ "12. Mid Value - New Customers",
    rfm_score %in% c("345", "335", "325") ~ "13. Low Value - New Customers",
    TRUE ~ "Other"
  ))   -> rfm_segment1

rfm_segment1 |>
select(
  `Customer ID`,`Customer ID`,
  first_transaction_date,last_transaction_date, days_since_last_transaction, total_transactions,
  total_revenue,total_profit,rfm_score, segment,
) -> fdata
left_join(fdata, med_days) -> f2_data

left_join(f2_data, clv) -> rfm_final


first_p |>
  group_by(`Customer ID`, `Customer Name`, Category) |>
  summarise(Category_purchased = n()) |>
  summarise(Category_most_purchased = first(Category, desc(Category_purchased))
            ) -> cateogory_purchase

left_join(rfm_final, cateogory_purchase) -> rfm_last

left_join(select(first_p$`Ship Mode`),rfm_last)
first_p |>
  mutate(Order_Month = floor_date(`Order Date`, unit = "month")) -> rention
rention |>
  group_by(Order_Month) |>
  summarise(
    total_customers = n_distinct(`Customer ID`),
    retained_customers = n_distinct(`Customer ID`[Order_Month < max(Order_Month)])
  ) |>
  mutate(retention_rate = retained_customers / total_customers) |>
  select(retention_rate) -> retention

left_join(first_p2,rfm_last) |> View()
write_csv(rfm_last2, "E:\\final_submission_data.CSV")

first_p |>
  distinct(`Customer ID`, .keep_all = TRUE) |>
 select(`Ship Date`,`Ship Mode`,Segment,Country,State,Region,City) ->first_p2

  left_join(rfm_final,first_p2)-> rfm_last2
  cross_join(rfm_final,first_p2) |> view()
  
#top_products <- Sample_data |>
  #group_by(`Customer ID`, `Category`, `Sub-Category`, `Product Name`) |>
  #summarise(total_quantity = sum(`Quantity`)) |>
  #arrange(`Customer ID`, desc(total_quantity)) |>
  #group_by(`Customer ID`) |>
  #slice(1)
#left_join(rfm_final, top_products) -> f3_data
