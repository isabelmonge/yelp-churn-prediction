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

# build predictor matrix
xvars <- df |>
  select(-user_id, -survival_time, -churned,
         -join_date, -last_post) |>
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
set.seed(42)
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
  tab_header(title = "Elastic-Net Cox – Influential Predictors") |>
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
gtsave(gt_table, "fig3_full_Enet_variable_imp.png", 
       vwidth = 1000,
       vheight = 600,
       zoom = 5)