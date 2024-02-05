library(tidyverse)

raw <- read_csv('https://www.dropbox.com/scl/fi/ug8tbxsdd2qtsfqwnnox1/dollar_store.csv?rlkey=fu36g6uhfpx8u644d1rpsq11i&dl=1')

ds <-  janitor::clean_names(raw)

ds

# Convert to number formats: price, star_rating, review_count, stock_status, unit_size
#   Goals:   (1) Don't lose information
#            (2) Missing where appropriate, but only where appropriate



# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Convert date columns to date and/or date-time columns, then calculate how many
# days each product sat on the shelf before its first sale.

library(lubridate)

ds %>% 
  select(date_added, first_sold_day, first_sold_time) %>%
  mutate(sold_time_together = str_c(first_sold_day, first_sold_time, sep = ' ')) %>% 
  mutate(sold_dt = mdy_hms(sold_time_together),
         date_added_c = dmy(date_added)) %>% 
  mutate(time_until_first_sale = as_date(sold_dt) - date_added_c)


ds %>% 
  select(product_info) %>% 
  separate_wider_delim(product_info, delim = ' - ', names = c('brand', 'product'), too_many = c("merge")) %>% 
  mutate(brand = str_remove(brand, 'Brand:'),
         product = str_remove(product, 'Product:'))
