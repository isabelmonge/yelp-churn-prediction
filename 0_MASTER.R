# master file to run the yelp-churn analysis
    # - scripts are numbered in the order they should run
    # - each script still loads its own libraries so it can run standalone


# part 1 – build & clean the raw database
source("1_load_and_create_tables.R") # builds duckdb file from raw JSON
source("2_preliminary_EDA_and_cleaning.R") # trims covid era, drops 1-post users, etc.

# part 2 – feature engineering
source("3_feature_engineering.R") # caps, log1p, interactions, saves RDS

# part 3 – modelling
source("4_initial_ENet_cox.R") # initial survival model with all variables
source("5_time_safe_Enet_cox.R") # final survival model with time-safe variables
source("6_sensitivity_snap_vars.R") # one-at-a-time leakage check

cat("\n✅  all scripts finished – analysis complete\n")
 