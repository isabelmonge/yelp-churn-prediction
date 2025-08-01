# load libraries
library(glmnet) # cox survival analysis
library(survival) # concordance + ph tools
library(dplyr) # data wrangling and manipulation
library(tidyverse) # data wrangling and manipulation
library(knitr) # kable()
library(doParallel) # parallel cv (speeds up model run time)
library(gt) # publication-ready tables
library(webshot2) # publication-ready tables

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

# build predictor matrix (time-safe version)
xvars <- df |>
  select(-user_id, -survival_time, -churned,
         -join_date, -last_post) |>
  select(-all_of(snap_vars)) |>
  mutate(across(where(is.logical), as.numeric)) |>
  as.matrix()

y <- with(df, Surv(survival_time, churned))

# diagnose non-finite values 
nonfinite_pct <- colMeans(!is.finite(xvars)) * 100
print(round(nonfinite_pct[nonfinite_pct > 0], 3))
# should all be ~0: artefacts from division-by-zero, not real missingness

# train/test split (time-based)
train_ids <- which(df$join_date <= as.Date("2016-12-31"))

# subsample large training set for speed
set.seed(123)
sample_train <- sample(train_ids, 100000) # can adjust up/down as time allows
x_train <- xvars[sample_train, ]
y_train <- y[sample_train]
x_test  <- xvars[-train_ids, ]
y_test  <- y[-train_ids]

# fit elastic-net cox
set.seed(123)
cvmod <- cv.glmnet(
  x_train, y_train,
  family   = "cox",
  alpha    = 0.5, # 0.5 = elastic-net 
  nfolds   = 5, # using 5 fold instead of 10 to reduce run time
  nlambda  = 50, # using 50 instead of 100 to reduce run time
  parallel = TRUE, # faster processing
  trace.it = 1 # track progress
)

# test set performance
lp_test <- predict(cvmod, newx = x_test, s = "lambda.min", type = "link")
c_test  <- concordance(y_test ~ lp_test, reverse = TRUE)$concordance
cat("elastic-net cox – test C-index:", round(c_test, 3), "\n\n")

# variable importance
imp_df <- coef(cvmod, s = "lambda.min") |>
  as.matrix() |>
  as.data.frame() |>
  setNames("beta") |>
  rownames_to_column("variable") |>
  filter(beta != 0) |>
  mutate(
    hazard_ratio = exp(beta),
    effect       = ifelse(beta < 0, "↓ protective", "↑ risk")
  ) |>
  arrange(effect, desc(abs(beta))) |>
  mutate(
    beta         = round(beta, 3),
    hazard_ratio = round(hazard_ratio, 3)
  )

# create GT table 
gt_table <- imp_df |>
  gt() |>
  tab_header(title = "Time-Safe Elastic-Net Cox – Influential Predictors") |>
  cols_label(
    variable = "Variable",
    beta = "Beta",
    hazard_ratio = "Hazard Ratio",
    effect = "Effect"
  ) |>
  cols_align(
    align = "left",
    columns = variable
  ) |>
  cols_align(
    align = "right", 
    columns = c(beta, hazard_ratio)
  ) |>
  cols_align(
    align = "center",
    columns = effect
  ) |>
  tab_source_note(
    source_note = md(paste("**Test Set C-index:**", round(c_test, 3)))
  ) |>
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 14,
    heading.title.font.size = 16,
    table.border.top.style = "solid",
    table.border.bottom.style = "solid"
  )

# save as PNG
gtsave(gt_table, "fig4_timesafe_Enet_variable_imp.png", 
       vwidth = 1000,
       vheight = 600,
       zoom = 5)

# proportional-hazards check 
# 1. variables that survive shrinkage
survivors <- rownames(coef(cvmod, s = "lambda.min"))[
  as.numeric(coef(cvmod, s = "lambda.min")) != 0 ]

# 2. plain cox on train rows with those variables
cox_fml <- as.formula(
  paste("Surv(survival_time, churned) ~",
        paste(survivors, collapse = " + "))
)
cox_std <- coxph(cox_fml, data = df[sample_train, ])

# 3. schoenfeld residual PH test
ph_test <- cox.zph(cox_std)
print(ph_test)   

# save proportional hazards diagnostic plots
png("fig5_proportional_hazards_diagnostics.png", width = 3000, height = 2000, res = 150)
par(mfrow = c(2, 3))
par(mar = c(5, 5, 4, 2))
par(oma = c(3, 3, 4, 2))
par(family = "Times New Roman")
par(cex.lab = 2.5)     
par(cex.axis = 1.1) 
plot(ph_test)
mtext("Schoenfeld Residuals: Proportional Hazards Test", outer = TRUE, cex = 3, line = 1, family = "Times New Roman")
dev.off()