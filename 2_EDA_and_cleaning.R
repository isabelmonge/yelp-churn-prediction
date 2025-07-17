library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(scales)

# connect to database
con <- dbConnect(duckdb::duckdb(), "yelp_restaurants.duckdb", read_only = FALSE)

# BASIC CHECKS
# preview of counts
counts <- dbGetQuery(con, "
SELECT 'businesses'        AS table_name, COUNT(*) FROM businesses
UNION ALL
  SELECT 'users',                   COUNT(*) FROM users
UNION ALL
  SELECT 'user_friends',            COUNT(*) FROM user_friends
UNION ALL
  SELECT 'user_elite_years',        COUNT(*) FROM user_elite_years
UNION ALL
  SELECT 'reviews',                 COUNT(*) FROM reviews
UNION ALL
  SELECT 'tips',                    COUNT(*) FROM tips;
")
kable(counts, caption = "Dataset Entities")

# count date range for reviews
# the reviews span from February 2005 to January 2022
dbGetQuery(con, "
  SELECT 
    MIN(date) AS earliest_review_date,
    MAX(date) AS latest_review_date
  FROM reviews;
")

# count date range for tips
# the tips span from April 2009 to January 2022
dbGetQuery(con, "
  SELECT 
    MIN(date) AS earliest_tip_date,
    MAX(date) AS latest_tip_date
  FROM tips;
")

# explore data during COVID-19 years
# in the early months of 2020 there is a large drop off in reviews, and
# a drop off in tips as well. This makes sense since many restaurants
# we closed or had limited service during the COVID-19 pandemic. 
activity_months <- dbGetQuery(con, "
  SELECT date_trunc('month', date) AS month,
         COUNT(*)                  AS n,
         'reviews'                 AS type
  FROM   reviews
  GROUP  BY month
UNION ALL
  SELECT date_trunc('month', date), COUNT(*), 'tips'
  FROM   tips
  GROUP  BY 1
  ORDER BY month;
")

ggplot(activity_months, aes(month, n, colour = type)) +
  geom_line() +
  geom_vline(xintercept = as.Date('2020-03-11'),
             linetype   = "dashed", linewidth = 0.6) +
  labs(title = "Monthly Activity (2010-2019 cut-off)",
       subtitle = "Dashed line marks the excluded COVID era",
       x = NULL, y = "Rows per month", colour = NULL) +
  theme(legend.position = "top")

# I will use data from 2010 through 2019 (inclusive) for my analysis, as
# this will limit my data to only time periods where the tip feature was
# available and exclude the COVID-19 pandemic and recovery time, which
# follows a different trend than the prior data

# I will filter reviews and tips to dates between January 1, 2010 and December 31, 2019 
# since these two tables are the only ones with row-level time stamps. Users 
# and businesses will automatically inherit this cutoff when later joined to those 
#filtered rows, so I don't need to edit any of the other reference tables. 
dbExecute(con, "
  DELETE FROM reviews
  WHERE date NOT BETWEEN DATE '2010-01-01' AND DATE '2019-12-31';
  DELETE FROM tips
  WHERE date NOT BETWEEN DATE '2010-01-01' AND DATE '2019-12-31';
")

# check to make sure filtering worked correctly
dbGetQuery(con, "
  SELECT 
    MIN(date) AS earliest_review_date,
    MAX(date) AS latest_review_date
  FROM reviews;
")

dbGetQuery(con, "
  SELECT 
    MIN(date) AS earliest_tip_date,
    MAX(date) AS latest_tip_date
  FROM tips;
")

# FILTERING FOR POST-ACTIVATION CHURN
# in my analysis, I'm primarily focused on retention, or post-activation
#churn rather than cold-start churn. This means I will filter for users who
#have taken at least 2 actions (reviews or tips) during my predefined 
# time period from 2010-2019. 

# visualize percentage of cold-start vs post-activation churners
user_action_counts <- dbGetQuery(con, "
  WITH user_actions AS (
    SELECT user_id, COUNT(*) AS action_count
    FROM (
      SELECT user_id FROM reviews
      UNION ALL
      SELECT user_id FROM tips
    )
    GROUP BY user_id
  )
  SELECT 
    CASE 
      WHEN action_count < 2 THEN 'Fewer than 2 actions'
      ELSE '2 or more actions'
    END AS action_group,
    COUNT(*) AS n_users
  FROM user_actions
  GROUP BY action_group
")


ggplot(user_action_counts, aes(x = action_group, y = n_users, fill = action_group)) +
  geom_col() +
  geom_text(aes(label = scales::comma(n_users)), vjust = -0.5) +
  labs(title = "User Activity Levels Before Filtering",
       x = "User Action Group",
       y = "Number of Users") +
  theme_minimal() +
  theme(legend.position = "none")

# we will drop around half the users in my dataset, but will still have
# plenty of observations for my predictive analysis

# create qualifying_users temp and drop one-post users
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE qualifying_users AS
  SELECT user_id
  FROM (
        SELECT user_id FROM reviews
        UNION ALL
        SELECT user_id FROM tips
       )
  GROUP BY user_id
  HAVING COUNT(*) >= 2;
  
  DELETE FROM users            WHERE user_id NOT IN (SELECT user_id FROM qualifying_users);
  DELETE FROM user_friends     WHERE user_id NOT IN (SELECT user_id FROM qualifying_users);
  DELETE FROM user_elite_years WHERE user_id NOT IN (SELECT user_id FROM qualifying_users);
  DELETE FROM reviews          WHERE user_id NOT IN (SELECT user_id FROM qualifying_users);
  DELETE FROM tips             WHERE user_id NOT IN (SELECT user_id FROM qualifying_users);
  DROP TABLE qualifying_users;
")

# ENGAGEMENT CADENCE FOR POST ACTIVATION CHURNERS
# calculate time gaps between consecutive actions for users
combined_gaps <- dbGetQuery(con, "
WITH events AS (
  SELECT user_id, date FROM reviews
  UNION ALL
  SELECT user_id, date FROM tips
), ordered AS (
  SELECT user_id,
         date,
         LAG(date) OVER (PARTITION BY user_id ORDER BY date) AS prev_date
  FROM events
)
SELECT user_id,
       date - prev_date AS days_between
FROM   ordered
WHERE  prev_date IS NOT NULL;
")

# calculate key percentiles for inter-event gaps
gap_percentiles <- quantile(
  combined_gaps$days_between,
  probs = c(.50, .75, .90, .95, .99),
  na.rm  = TRUE
)

# create percentiles table
kable(
  data.frame(
    Percentile = names(gap_percentiles),
    Days       = as.integer(gap_percentiles)
  ),
  caption = "Inter-event Gap Percentiles (Days)"
)

# The median gap between actions is only 7 days, suggesting that typical 
# active users post quite frequently. The 75th percentile is 73 days 
# (approximately 3 months). The 90th percentile is 325 days (just under 1 year). 
#The 95th percentile is 608 days (nearly 2 years), which seems excessively 
# long for a platform like Yelp.

# For this next part of my EDA, I'll use two different thresholds to compare 
#how our definition of churn affects the results:

#1.  **90 days (3 months)** - A shorter-term definition of churn that might 
#be more appropriate for this type of website

#2.  **365 days (1 year)** - A longer-term definition that accounts for users 
#who post only post seasonally (ex. only on vacations, around holidays, etc.)

# set short and long thresholds
short_threshold <- 90
long_threshold <- 365

# Right-Censoring Assessment

# last activity date for retained users
last_dates <- dbGetQuery(con, "
WITH retained_users AS (
  SELECT user_id FROM (
    SELECT user_id, COUNT(*) AS n
    FROM (
      SELECT user_id, date FROM reviews
      UNION ALL
      SELECT user_id, date FROM tips
    ) 
    GROUP BY user_id
    HAVING COUNT(*) >= 2
  )
),
events AS (
  SELECT user_id, date FROM reviews
  UNION ALL
  SELECT user_id, date FROM tips
)
SELECT e.user_id,
       MAX(e.date) AS last_date,
       COUNT(*)  AS n_events
FROM events e
JOIN retained_users r ON e.user_id = r.user_id
GROUP BY e.user_id;")

activity_status_short <- last_dates |>
  mutate(status = case_when(
    as.integer(difftime(end_date, last_date, units = "days")) <= short_threshold ~ "Right-censored",
    TRUE ~ "Complete"
  )) |>
  count(status) |>
  mutate(
    percentage = percent(n / sum(n)),
    threshold = "90 days"
  )

activity_status_long <- last_dates |>
  mutate(status = case_when(
    as.integer(difftime(end_date, last_date, units = "days")) <= long_threshold ~ "Right-censored",
    TRUE ~ "Complete"
  )) |>
  count(status) |>
  mutate(
    percentage = percent(n / sum(n)),
    threshold = "365 days"
  )

kable(activity_status_short,
      caption = paste0("User Status with ", short_threshold, "-day Threshold"))
kable(activity_status_long,
      caption = paste0("User Status with ", long_threshold, "-day Threshold"))

combined_status <- bind_rows(activity_status_short, activity_status_long)

ggplot(combined_status, aes(status, n, fill = status)) +
  geom_col() +
  geom_text(aes(label = paste0("\n(", percentage, ")"))) +
  facet_wrap(~threshold, ncol = 1) +
  labs(title = "User Status Distribution for Different Thresholds",
       x = "Status",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") 

# Modeling Implications

# The choice of inactivity threshold strongly affects how we classify user churn:
# - With a 90-day threshold, only 9% of users are right-censored (still active),
#   while 91% are treated as churned (complete cases)
# - With a 365-day threshold, 30% of users are right-censored,
#   while 70% are considered churned

# This has important implications for modeling:
# 1. Short-term engagement (90 days):
#    - The low censoring rate (9%) suggests that simple classification (e.g., logistic regression)
#      or time-to-event regression could work well
#    - A 90-day window is common in consumer apps and subscription products to define churn
# 2. Long-term retention (365 days):
#    - A higher censoring rate (30%) is ideal for survival analysis (e.g., Cox regression),
#      which is designed to handle censored data
#    - This threshold captures more periodic or seasonal users who may return less frequently

# I will plan to use the 365 day threshold and conduct a survival analysis

# the last step in cleaning my data is to remove "reactivated churners," or
# users who were inactive for at least 365 days before 2019-12-31 but became 
# active again afterward. If left in, they would be mislabeled as churned, which 
# could confuse the model. 
dbExecute(con, "
CREATE OR REPLACE TEMP TABLE activity AS
SELECT user_id, date FROM reviews
UNION ALL
SELECT user_id, date FROM tips;
")
dbExecute(con, "
CREATE OR REPLACE TEMP TABLE reactivated AS
WITH split AS (
  SELECT user_id,
         MAX(CASE WHEN date <= DATE '2019-12-31' THEN date END) AS last_pre,
         MIN(CASE WHEN date >  DATE '2019-12-31' THEN date END) AS first_post
  FROM activity
  GROUP BY user_id
)
SELECT user_id
FROM split
WHERE last_pre IS NOT NULL
  AND DATEDIFF('day', last_pre, DATE '2019-12-31') >= 365
  AND first_post IS NOT NULL;
")
dbExecute(con, "
DELETE FROM users            WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM user_friends     WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM user_elite_years WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM reviews          WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM tips             WHERE user_id IN (SELECT user_id FROM reactivated);
DROP TABLE activity;
DROP TABLE reactivated;
")

# now I will save the clean dataset as a new file
snapshot <- "yelp_clean.duckdb"
if (file.exists(snapshot)) file.remove(snapshot)
dbExecute(con, sprintf("ATTACH '%s' AS snap (READ_ONLY FALSE);", snapshot))
for (tbl in dbListTables(con))
  dbExecute(con, sprintf("CREATE TABLE snap.%s AS SELECT * FROM %s;", tbl, tbl))
dbExecute(con, "DETACH snap;")
cat("✅  snapshot written to", normalizePath(snapshot), "\n")

dbDisconnect(con, shutdown = TRUE)
  