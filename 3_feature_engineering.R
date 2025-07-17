# load libraries
library(duckdb) # db connection + SQL
library(dplyr) # data wrangling and manipulation
library(tidyverse) # data wrangling and manipulation
library(ggplot2) # graphs/plotting
library(GGally) # correlation matrix with ggcorr()

# plot style
plot_width  <- 6      
plot_height <- 4
plot_dpi    <- 300
plot_theme  <- theme_minimal(base_size = 12, base_family = "sans")

# connect to duckdb
con <- dbConnect(duckdb::duckdb(),
                 "yelp_final.duckdb",
                 read_only = TRUE)

# BUILD SURVIVAL DATASET TO USE FOR PREDICTIVE MODELS
# includes features like timeline, social metrics, compliments, votes, 
# and elite status
survival_data <- dbGetQuery(con, "
WITH user_timeline AS (
  SELECT
    u.user_id,
    u.yelping_since                                   AS join_date,
    MAX(a.date)                                       AS last_post,
    COUNT(*)                                          AS total_posts,
    (MAX(a.date) - u.yelping_since) / 365.0           AS tenure_years
  FROM users u
  LEFT JOIN (
    SELECT user_id, date FROM reviews
    UNION ALL
    SELECT user_id, date FROM tips
  ) a
  ON u.user_id = a.user_id
  GROUP BY u.user_id, u.yelping_since
  HAVING MAX(a.date) > u.yelping_since
),
review_votes AS (
  SELECT
    user_id,
    SUM(useful_per_review + funny_per_review + cool_per_review)
      AS total_votes_received,
    COUNT(*)                                            AS review_count,
    AVG(useful_per_review + funny_per_review + cool_per_review)
      AS avg_votes_per_review
  FROM reviews
  WHERE useful_per_review >= 0
    AND funny_per_review  >= 0
    AND cool_per_review   >= 0
  GROUP BY user_id
),
user_social AS (
  SELECT
    u.user_id,

    /* H1 social integration */
    COALESCE(f.friend_count, 0)                                    AS friend_count,
    COALESCE(f.friend_count, 0) / NULLIF(ut.tenure_years, 0)       AS friends_per_year,

    /* H2 recognition and validation */
    COALESCE(
      u.compliment_hot + u.compliment_more + u.compliment_profile +
      u.compliment_cute + u.compliment_list + u.compliment_note   +
      u.compliment_plain + u.compliment_cool + u.compliment_funny +
      u.compliment_writer + u.compliment_photos,
      0
    )                                                             AS total_compliments,
    COALESCE(
      u.compliment_hot + u.compliment_more + u.compliment_profile +
      u.compliment_cute + u.compliment_list + u.compliment_note   +
      u.compliment_plain + u.compliment_cool + u.compliment_funny +
      u.compliment_writer + u.compliment_photos,
      0
    ) / NULLIF(ut.tenure_years, 0)                                 AS compliments_per_year,
    COALESCE(rv.total_votes_received, 0)                            AS total_votes_received,
    COALESCE(rv.avg_votes_per_review, 0)                            AS avg_votes_per_review,
    COALESCE(rv.total_votes_received, 0) / NULLIF(ut.tenure_years, 0)
                                                                   AS votes_per_year,

    /* H3 status and identity */
    CASE WHEN e.user_id IS NOT NULL THEN 1 ELSE 0 END              AS ever_elite,
    COALESCE(e.years_elite, 0)                                     AS years_elite
  FROM users u
  JOIN user_timeline ut ON u.user_id = ut.user_id
  LEFT JOIN (
    SELECT user_id, COUNT(*) AS friend_count
    FROM user_friends GROUP BY user_id
  ) f  ON u.user_id = f.user_id
  LEFT JOIN review_votes rv ON u.user_id = rv.user_id
  LEFT JOIN (
    SELECT user_id, COUNT(*) AS years_elite
    FROM user_elite_years GROUP BY user_id
  ) e  ON u.user_id = e.user_id
)
SELECT
  ut.user_id,
  ut.join_date,
  ut.last_post,
  ut.tenure_years,
  us.friend_count,
  us.friends_per_year,
  us.total_compliments,
  us.compliments_per_year,
  us.total_votes_received,
  us.avg_votes_per_review,
  us.votes_per_year,
  us.ever_elite,
  us.years_elite,

  /* survival time and event flag */
  CASE
    WHEN DATE '2019-12-31' - ut.last_post > 365
         THEN ut.last_post - ut.join_date
    ELSE DATE '2019-12-31' - ut.join_date
  END AS survival_time,

  CASE
    WHEN DATE '2019-12-31' - ut.last_post > 365 THEN 1 ELSE 0
  END AS churned
FROM user_timeline ut
JOIN user_social  us ON ut.user_id = us.user_id
WHERE ut.join_date <= DATE '2018-12-31'      /* one-year minimum window */
  AND ut.join_date <> ut.last_post;          /* drop single-post users */
")

# convert survival_time to numeric 
survival_data$survival_time <- as.numeric(survival_data$survival_time)

cat("dataset dimensions:",
    nrow(survival_data), "users,",
    ncol(survival_data), "variables\n")
cat("churn rate:",
    round(mean(survival_data$churned) * 100, 1), "%\n\n")

# plot target variable distribution 
fig1 <- ggplot(survival_data, aes(x = factor(churned), fill = factor(churned))) +
  geom_bar() +
  labs(title = "distribution of churn status",
       x = "churned (0 = active, 1 = churned)") +
  plot_theme +
  theme(legend.position = "none")
print(fig1)
ggsave("fig_fe_01_churn_dist.png", fig1,
       width = plot_width, height = plot_height, dpi = plot_dpi)

# plot numeric feature distributions 
df_long <- survival_data |>
  select(friend_count, friends_per_year, total_compliments, compliments_per_year,
         total_votes_received, avg_votes_per_review, votes_per_year,
         years_elite, survival_time, tenure_years) |>
  pivot_longer(cols = everything(),
               names_to = "feature",
               values_to = "value")

fig2 <- ggplot(df_long, aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~feature, scales = "free") +
  plot_theme +
  labs(title = "distribution of numeric features (raw)")
print(fig2)
ggsave("fig_fe_02_raw_dists.png", fig2,
       width = plot_width, height = plot_height, dpi = plot_dpi)

# FEATURE ENGINEERING
# cap extreme friend-growth rates at 99-th percentile
p99_fpy <- quantile(survival_data$friends_per_year,
                    0.99, na.rm = TRUE)

survival_data_transformed <- survival_data |>
  mutate(
    # recompute friends_per_year (guard against divide-by-zero)
    friends_per_year = ifelse(tenure_years > 0,
                              friend_count / tenure_years, 0),
    
    # clamp friends_per_year so a handful of super-social users
    # don’t dominate the model
    friends_per_year = pmin(friends_per_year, p99_fpy),
    
    # flag users with zero compliments / votes so the model
    # can treat “no social feedback” as a distinct signal 
    zero_comp_flag = as.integer(total_compliments    == 0),
    zero_vote_flag = as.integer(total_votes_received == 0),
    
    # log1p() shrinks long-tailed counts while keeping zeros defined
    log_friend_count         = log1p(friend_count),
    log_friends_per_year     = log1p(friends_per_year),
    log_total_compliments    = log1p(total_compliments),
    log_compliments_per_year = log1p(compliments_per_year),
    log_total_votes          = log1p(total_votes_received),
    log_avg_votes_per_review = log1p(avg_votes_per_review),
    log_votes_per_year       = log1p(votes_per_year),
    log_years_elite          = log1p(years_elite),
    
    # interaction term <- does building a friend network earlier lower churn?
    tenure_friend_int = tenure_years * log_friend_count,
    
    # interaction term <- do compliments still matter once the user is elite?
    elite_compl_int   = ever_elite  * log_total_compliments
  )

# histograms of log vars
df_log <- survival_data_transformed |>
  select(starts_with("log_")) |>
  pivot_longer(everything(), names_to = "feature", values_to = "value")

fig3 <- ggplot(df_log, aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~feature, scales = "free") +
  plot_theme +
  labs(title = "distributions after log1p()")
print(fig3)
ggsave("fig_fe_03_log_dists.png", fig3,
       width = plot_width, height = plot_height, dpi = plot_dpi)

# correlation plot
fig4 <- survival_data_transformed |>
  select(starts_with("log_"),
         survival_time, tenure_years,
         tenure_friend_int, elite_compl_int) |>
  ggcorr(label = TRUE, label_size = 3, hjust = 0.8) +
  labs(title = "correlation matrix – log predictors") +
  plot_theme
print(fig4)
ggsave("fig_fe_04_corr_matrix.png", fig4,
       width = plot_width, height = plot_height, dpi = plot_dpi)

# I've engineered many highly-correlated features, which introduced
# multicollinearity. I'll handle this in the next script by fitting 
# penalized models that shrink or drop redundant predictors.

# save engineered dataset
saveRDS(survival_data_transformed, "yelp_survival.rds")
cat("\n✅  saved engineered dataset to yelp_survival.rds\n")

dbDisconnect(con, shutdown = TRUE)
