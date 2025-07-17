# load libraries
library(glmnet) # cox survival analysis
library(survival) # concordance + ph tools
library(dplyr) # data wrangling
library(tidyverse) # data wrangling
library(knitr) # kable()

# load engineered dataset 
df <- readRDS("yelp_survival.rds")

# label snapshot-based vars to drop (these are variables where data
# leakage is possible) <- same as snap_vars in 05.time_safe_ENet_cox.R
snap_vars <- c(
  "friend_count","log_friend_count",
  "friends_per_year","log_friends_per_year",
  "total_votes_received","log_total_votes",
  "avg_votes_per_review", "log_avg_votes_per_review",
  "votes_per_year","log_votes_per_year",
  "total_compliments","log_total_compliments",
  "compliments_per_year", "log_compliments_per_year",
  "elite_compl_int","tenure_friend_int"
)

#time-safe features only
core_vars <- df |>                       
  select(-user_id, -survival_time, -churned,
         -join_date, -last_post, -all_of(snap_vars)) |>
  names()

# modelling dataset -- outcomes + all predictors
mod_df <- df |> select(survival_time, churned,
                       all_of(core_vars), all_of(snap_vars))

# train / test split (time based)
train_flag <- df$join_date <= as.Date("2016-12-31")
set.seed(789)
train_sample <- sample(which(train_flag), 50000) # 50k rows for speed
test_idx     <- which(!train_flag)

y_train <- with(df[train_sample, ], Surv(survival_time, churned))
y_test  <- with(df[test_idx     , ], Surv(survival_time, churned))

# helper -- build sparse model matrix quickly
make_mm <- function(vars, rows) {
  model.matrix(
    as.formula(paste("~", paste(vars, collapse = "+"))),
    data = mod_df[rows, ]
  )[ , -1, drop = FALSE] # drop intercept
}

# helper -- fit elastic-net cox and return C-index
get_cindex <- function(extra_vars) {
  preds  <- c(core_vars, extra_vars)
  mm_tr  <- make_mm(preds, train_sample)
  mm_te  <- make_mm(preds, test_idx)
  
  fit <- cv.glmnet(
    mm_tr, y_train,
    family  = "cox",
    alpha   = 0.5, # 0.5 = elastic-net 
    nfolds  = 5, # using 5 fold instead of 10 to reduce run time
    nlambda = 50, # using 50 instead of 100 to reduce run time
    trace.it = TRUE # track progress
  )
  
  lp <- predict(fit, newx = mm_te, s = "lambda.min", type = "link")
  concordance(y_test ~ lp, reverse = TRUE)$concordance
}

# one-at-a-time sensitivity run
# each row shows how the test-set C-index changes when we add just one
# snapshot-based variable on top of the “core” time-safe features
results <- tibble(
  spec_id = snap_vars,
  c_index = sapply(snap_vars, get_cindex)
) |>
  arrange(desc(c_index))

kable(results,
      caption = "impact of adding each snapshot-based variable (c-index on test set)",
      digits  = 3)
