# load libraries
library(duckdb) # database connection + SQL
library(dplyr) # data wrangling and manipulation
library(ggplot2) # graphs/plotting
library(knitr) # formatted tables with kable()
library(scales) # comma() and percent() helpers
library(gt) # publication-ready tables
library(webshot2) # publication-ready tables

# plot style
plot_width  <- 6      
plot_height <- 4
plot_dpi    <- 300
plot_theme  <- theme_minimal(base_size = 12, base_family = "times new roman")

# connect to duckdb
con <- dbConnect(duckdb::duckdb(), "yelp_restaurants.duckdb",
                 read_only = FALSE)

# basic table counts
counts <- dbGetQuery(con, "
  SELECT 'users'            AS table_name, COUNT(*) AS n FROM users UNION ALL
  SELECT 'user_friends',            COUNT(*) FROM user_friends UNION ALL
  SELECT 'user_elite_years',        COUNT(*) FROM user_elite_years UNION ALL
  SELECT 'reviews',                 COUNT(*) FROM reviews UNION ALL
  SELECT 'tips',                    COUNT(*) FROM tips;
")
kable(counts, caption = "row counts for key tables")

# count date range for reviews
# the reviews span from February 2005 to January 2022
dbGetQuery(con, "
  SELECT MIN(date) AS first_review, MAX(date) AS last_review FROM reviews;
")

# count date range for tips
# the tips span from April 2009 to January 2022
dbGetQuery(con, "
  SELECT MIN(date) AS first_tip, MAX(date) AS last_tip FROM tips;
")

# EXPLORE DATA DURING COVID-19 YEARS
# in the early months of 2020 there is a large drop off in reviews, and
# a drop off in tips as well. This makes sense since many restaurants
# we closed or had limited service during the COVID-19 pandemic. 
activity_months <- dbGetQuery(con, "
  SELECT date_trunc('month', date) AS month,
         COUNT(*)                  AS n,
         'reviews'                 AS type
  FROM   reviews GROUP BY month
  UNION ALL
  SELECT date_trunc('month', date), COUNT(*), 'tips'
  FROM   tips GROUP BY 1
  ORDER  BY month;
")

fig1 <- ggplot(activity_months,
               aes(month, n, colour = type)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_vline(xintercept = as.Date("2020-03-11"),
             linetype = "dashed", linewidth = 0.5, colour = "grey30") +
  scale_colour_manual(values = c("reviews" = "#E15759",
                                 "tips" = "#00AFC0")) +
  scale_y_continuous(labels = comma) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits      = as.Date(c("2009-01-01", "2022-02-01")),
    expand      = expansion(mult = c(0, .02))   
  ) +
  labs(
    title    = "Monthly activity (highlighting COVID-19 period)",
    subtitle = "Dashed line = WHO pandemic declaration (11 Mar 2020)",
    y        = "Rows per month"
  ) +
  coord_cartesian(clip = "off") +     
  plot_theme
print(fig1)
ggsave("fig1_COVID_EDA.png", fig1,
       width = plot_width, height = plot_height, dpi = plot_dpi)

# I will use data from 2010 through 2019 (inclusive) for my analysis, as
# this will limit my data to only time periods where the tip feature was
# available and exclude the COVID-19 pandemic and recovery time, which
# follows a different trend than the prior data

# I will filter reviews and tips to dates between January 1, 2010 and December 31, 2019 
# since these two tables are the only ones with row-level time stamps. Users 
# and businesses will automatically inherit this cutoff when later joined to those 
#filtered rows, so I don't need to edit any of the other reference tables. 

# filter data to 2010-01-01 to 2019-12-31
dbExecute(con, "
  DELETE FROM reviews
  WHERE date NOT BETWEEN DATE '2010-01-01' AND DATE '2019-12-31';
  DELETE FROM tips
  WHERE date NOT BETWEEN DATE '2010-01-01' AND DATE '2019-12-31';
")

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

# ONE TIME USAGE VS CHURN
# identify each user’s first and second actions (review or tip)
first_second <- dbGetQuery(con, "
WITH events AS (
  SELECT user_id, date FROM reviews
  UNION ALL
  SELECT user_id, date FROM tips
), ranked AS (
  SELECT user_id, date,
         ROW_NUMBER() OVER (PARTITION BY user_id ORDER BY date) AS rn
  FROM events
)
SELECT user_id,
       MIN(date) FILTER (WHERE rn = 1) AS first_date,
       MIN(date) FILTER (WHERE rn = 2) AS second_date,
       COUNT(*)                        AS n_events
FROM ranked
GROUP BY user_id;
")

# dataset end date after trimming
end_date <- dbGetQuery(con, "
  SELECT MAX(last_seen) AS end_date
  FROM (
    SELECT MAX(date) AS last_seen FROM reviews
    UNION ALL
    SELECT MAX(date) FROM tips
  );
")$end_date

# i’ll start with a simple 365-day threshold for dropout
threshold_365 <- 365

early_churn <- first_second |>
  mutate(
    gap_after_first = as.integer(difftime(coalesce(second_date, end_date),
                                          first_date, units = "days")),
    outcome = case_when(
      n_events == 1 & gap_after_first > threshold_365 ~ "1 action only",
      n_events == 1                                   ~ "uncertain (near end)",
      n_events > 1                                    ~ ">1 action"
    )
  ) |>
  count(outcome)

fig2 <- ggplot(early_churn, aes(outcome, n, fill = outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.4, size = 3.2, 
            family = "Times New Roman") +
  scale_fill_manual(values = c(
    "1 action only"        = "#E15759",
    ">1 action"            = "#00AFC0",
    "uncertain (near end)" = "grey50"
  )) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, .05))) +
  labs(
    title    = "Outcomes after first action",
    subtitle = "Comparing post-activation drop-off with long-term churn",
    y        = "User Count",
    x        = "User Outcome"
  ) +
  coord_cartesian(clip = "off") +
  plot_theme +
  theme(
    legend.position = "none",
    plot.title      = element_text(hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5)
  )
fig2

ggsave("figNEW_short_long_churn.png", fig2,
       width = plot_width, height = plot_height, dpi = plot_dpi)

# keep only users with ≥2 actions
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

# EXPLORE TYPICAL ENGAGEMENT CADENCE
# calculate time gaps between consecutive actions for these users
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
SELECT date - prev_date AS days_between
FROM   ordered
WHERE  prev_date IS NOT NULL;
")  

# fast summary of gap distribution
gap_percentiles <- quantile(
  combined_gaps$days_between,
  probs = c(.50, .75, .90, .95, .99),
  na.rm  = TRUE
)

# create table
percentile_table <- data.frame(
  Percentile = c("50th", "75th", "90th", "95th", "99th"),
  Days = round(gap_percentiles, 2)
)

gt_table <- percentile_table %>%
  gt() %>%
  tab_header(title = "Gap Percentiles (Days Between Events)") %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.title.font.size = 14,
    table.border.top.style = "solid",
    table.border.bottom.style = "solid"
  )

# save as PNG
gtsave(gt_table, "fig2_percentiles_table.png", 
       vwidth = 800,  
       vheight = 400, 
       zoom = 5)       

# The median gap between actions is only 8 days, suggesting that typical 
# active users post quite frequently. The 75th percentile is 73 days 
# (approximately 3 months). The 90th percentile is 325 days (just under 1 year). 
# The 95th percentile is 608 days (nearly 2 years), which seems excessively 
# long for a platform like Yelp.

# For this next part of my EDA, I'll use two different thresholds to compare 
# how our definition of churn affects the results:

#90 days (3 months)- A shorter-term definition of churn that might 
# be more appropriate for this type of website

# 365 days (1 year) - A longer-term definition that accounts for 
# users who post only post seasonally (ex. only on vacations, 
# around holidays, etc.)


æ
# Modeling Implications

# The choice of threshold impacts our view of user activity:
# With the 90-day threshold, only 9% of users are considered active (right-censored),
# while 91% are classified as complete

# With the 365-day threshold, 30% of users are considered active (right-censored),
# while 70% are classified as complete

# This has important implications for modeling:
# 1. Short-term engagement (90 days):
# The low censoring rate suggests classification or regression could be appropriate
# This threshold is common for digital products and subscription services
# 2. Long-term retention (365 days):
# The moderate censoring rate (30%) would be appropriate for survival models
# This threshold provides a more balanced view between truly inactive users
# and those who post occasionally

# REMOVE REACTIVATED CHURNERS
# these are users inactive ≥365 days before 2019-12-31 and active later
dbExecute(con, "
CREATE OR REPLACE TEMP TABLE all_activity AS
SELECT user_id, date FROM reviews
UNION ALL
SELECT user_id, date FROM tips;

CREATE OR REPLACE TEMP TABLE reactivated AS
WITH splits AS (
  SELECT user_id,
         MAX(CASE WHEN date <= DATE '2019-12-31' THEN date END) AS last_pre,
         MIN(CASE WHEN date >  DATE '2019-12-31' THEN date END) AS first_post
  FROM all_activity
  GROUP BY user_id
)
SELECT user_id
FROM   splits
WHERE  last_pre IS NOT NULL
  AND  DATEDIFF('day', last_pre, DATE '2019-12-31') >= 365
  AND  first_post IS NOT NULL;

DELETE FROM users            WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM user_friends     WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM user_elite_years WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM reviews          WHERE user_id IN (SELECT user_id FROM reactivated);
DELETE FROM tips             WHERE user_id IN (SELECT user_id FROM reactivated);

DROP TABLE all_activity;
DROP TABLE reactivated;
")

# save a snapshot of the cleaned database
snapshot <- "yelp_final.duckdb"
if (file.exists(snapshot)) file.remove(snapshot)

dbExecute(con, sprintf("ATTACH '%s' AS snap (READ_ONLY FALSE);", snapshot))
for (tbl in dbListTables(con))
  dbExecute(con, sprintf("CREATE TABLE snap.%s AS SELECT * FROM %s;", tbl, tbl))
dbExecute(con, "DETACH snap;")
cat("✅  snapshot written to", normalizePath(snapshot), "\n")

# close connection
dbDisconnect(con, shutdown = TRUE)
