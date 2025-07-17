# Yelp Churn Prediction

This repository contains my Master's thesis project for the Master's in Computational Social Sciences (UC3M): *Predicting User Churn Through Engagement Pattern Analysis on Yelp*. It includes all developed code and a pre-processed dataset (`yelp_survival.rds`) that allows partial reproduction of the project without requiring the full Yelp dataset.

⚠️ **WARNING**  
Due to GitHub file size limits, the original Yelp Academic Dataset files (each ~100MB to 5GB) are not included in the repository. If you wish to run the entire pipeline, you must download these manually from [https://www.yelp.com/dataset](https://www.yelp.com/dataset).

---

## Running the project

There are six main R scripts in the project, labeled 1 through 6:

### Scripts 1–3:
- `1-preprocessing.R`  
- `2-filtering.R`  
- `3-feature-engineering.R`

These scripts require the **original Yelp Academic Dataset**. You can download the dataset from:  
👉 [https://www.yelp.com/dataset](https://www.yelp.com/dataset)

Once downloaded, place the `.json` files (especially `business`, `review`, `tip`, and `user`) in the same directory as the scripts. These scripts perform the full preprocessing pipeline to create a cleaned dataset for analysis.

### Scripts 4–6:
- `4-survival-analysis.R`  
- `5-modeling.R`  
- `6-export-results.R`

These scripts can be run **without downloading the JSON files**, using the pre-processed dataset `yelp_survival.rds` included in the repository. This allows you to jump directly into the survival analysis and modeling phases.

---

## How to use

1. Open `yelp-churn.Rproj` in RStudio to load the working environment.
2. Choose whether to:
   - Run all scripts (1–6) with the full Yelp dataset, **or**
   - Run scripts 4–6 using only `yelp_survival.rds`
3. Output (plots, tables, model metrics) will be printed to console or saved from within the scripts.

---

## Notes

- All scripts assume the working directory is set to the root of the project (handled automatically by opening the `.Rproj` file).
- To avoid saving files, you can comment out any `write.csv()` or `ggsave()` lines in the code.

---

## License

This project is for academic purposes only and is not affiliated with Yelp Inc.
