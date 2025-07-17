# load libraries
library(glmnet) # cox survival analysis
library(survival) # concordance + ph tools
library(readr) # readRDS()

# plot theme
plot_theme  <- theme_minimal(base_size = 12, base_family = "sans")

# load dataset
df <- readRDS("yelp_survival.rds")   

# label snapshot-based vars to drop (these are variables where data
# leakage is possible)
snap_vars <- c(
  "friend_count", "log_friend_count",
  "friends_per_year", "log_friends_per_year",
  "total_votes_received", "log_total_votes",
  "avg_votes_per_review", "log_avg_votes_per_review",
  "votes_per_year", "log_votes_per_year",
  "total_compliments", "log_total_compliments",
  "compliments_per_year", "log_compliments_per_year",
  "elite_compl_int", "tenure_friend_int"
)

# build predictor matrix 
xvars <- df |>
  dplyr::select(-user_id, -survival_time, -churned,
                -join_date, -last_post) |>
  dplyr::select(-dplyr::all_of(snap_vars)) |>
  dplyr::mutate(dplyr::across(where(is.logical), as.numeric)) |>
  as.matrix()

y <- with(df, Surv(survival_time, churned))

# train / test split (time-based) 
train_idx <- df$join_date <= as.Date("2016-12-31")
x_train <- xvars[train_idx, ]; y_train <- y[train_idx]
x_test  <- xvars[!train_idx, ]; y_test  <- y[!train_idx]

# fit elastic-net cox 
set.seed(456)
cvmod <- cv.glmnet(
  x_train, y_train,
  family   = "cox",
  alpha    = 0.5, # 0.5 = elastic-net 
  nfolds   = 5, # using 5 fold instead of 10 to reduce run time
  nlambda  = 50, # using 50 instead of 100 to reduce run time
  trace.it = 1 # track progress
)

# test set performance 
lp_test <- predict(cvmod, newx = x_test, s = "lambda.min", type = "link")
c_test  <- concordance(y_test ~ lp_test, reverse = TRUE)$concordance
cat("elastic-net cox – test C-index:", round(c_test, 3), "\n")

# proportional-hazards check 
# 1. variables that survive shrinkage
survivors <- rownames(coef(cvmod, s = "lambda.min"))[
  as.numeric(coef(cvmod, s = "lambda.min")) != 0 ]

# 2. plain cox on train rows with those variables
cox_fml <- as.formula(
  paste("Surv(survival_time, churned) ~",
        paste(survivors, collapse = " + "))
)
cox_std <- coxph(cox_fml, data = df[train_idx, ])

# 3. schoenfeld residual PH test
ph_test <- cox.zph(cox_std)
print(ph_test)   
plot(ph_test)  
