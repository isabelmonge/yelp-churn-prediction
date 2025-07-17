# install and connect to DuckDB
if (!requireNamespace("duckdb", quietly = TRUE)) install.packages("duckdb")
library(duckdb) # database connection + SQL

db_file <- "yelp_restaurants.duckdb"
con     <- dbConnect(duckdb::duckdb(), db_file)

# enable JSON extension so we can read JSON files
dbExecute(con, "INSTALL json;")
dbExecute(con, "LOAD json;")

# define file paths for each JSON file
json_dir <- "~/Desktop/TFM"   # change this if your files are in a different folder

biz_json <- file.path(json_dir, "yelp_academic_dataset_business.json")
usr_json <- file.path(json_dir, "yelp_academic_dataset_user.json")
rev_json <- file.path(json_dir, "yelp_academic_dataset_review.json")
tip_json <- file.path(json_dir, "yelp_academic_dataset_tip.json")

# helper function to run SQL queries using different file locations
sql_exec <- function(sql, path) dbExecute(con, sql, params = list(path))

# create BUSINESSES table with all restaurants from the business data
sql_exec("
  CREATE OR REPLACE TABLE businesses AS
  SELECT
    j ->> 'business_id' AS business_id,
    j ->> 'name'        AS name,
    j ->> 'city'        AS city,
    j ->> 'state'       AS state,
    j ->> 'postal_code' AS postal_code,
    CAST(j ->> 'latitude'  AS DOUBLE)        AS latitude,
    CAST(j ->> 'longitude' AS DOUBLE)        AS longitude,
    CAST(j ->> 'stars'     AS DECIMAL(3,2))  AS stars,
    CAST(j ->> 'review_count' AS INTEGER)    AS review_count,
    (j ->> 'is_open') = '1'                  AS is_open,
    j -> 'categories'                        AS categories
  FROM read_json_auto(?) AS j
  WHERE (j ->> 'categories') ILIKE '%Restaurants%';
", biz_json)

# create USERS table
sql_exec("
  CREATE OR REPLACE TABLE users AS
  SELECT
    j ->> 'user_id'  AS user_id,
    j ->> 'name'     AS name,
    CAST(j ->> 'yelping_since' AS DATE)         AS yelping_since,
    CAST(j ->> 'review_count'  AS INTEGER)      AS review_count,
    CAST(j ->> 'average_stars' AS DECIMAL(3,2)) AS average_stars,
    CAST(j ->> 'fans'          AS INTEGER)      AS fans,
    CAST(j ->> 'useful'        AS INTEGER)      AS useful_user_sent,
    CAST(j ->> 'funny'         AS INTEGER)      AS funny_user_sent,
    CAST(j ->> 'cool'          AS INTEGER)      AS cool_user_sent,
    CAST(j ->> 'compliment_hot'     AS INTEGER) AS compliment_hot,
    CAST(j ->> 'compliment_more'    AS INTEGER) AS compliment_more,
    CAST(j ->> 'compliment_profile' AS INTEGER) AS compliment_profile,
    CAST(j ->> 'compliment_cute'    AS INTEGER) AS compliment_cute,
    CAST(j ->> 'compliment_list'    AS INTEGER) AS compliment_list,
    CAST(j ->> 'compliment_note'    AS INTEGER) AS compliment_note,
    CAST(j ->> 'compliment_plain'   AS INTEGER) AS compliment_plain,
    CAST(j ->> 'compliment_cool'    AS INTEGER) AS compliment_cool,
    CAST(j ->> 'compliment_funny'   AS INTEGER) AS compliment_funny,
    CAST(j ->> 'compliment_writer'  AS INTEGER) AS compliment_writer,
    CAST(j ->> 'compliment_photos'  AS INTEGER) AS compliment_photos,
    j -> 'friends' AS friends_json,
    j -> 'elite'   AS elite_json
  FROM read_json_auto(?) AS j;
", usr_json)

# create USER FRIENDS table
dbExecute(con, "
  CREATE OR REPLACE TABLE user_friends AS
  SELECT
    u.user_id,
    trim(f) AS friend_id
  FROM users AS u
  CROSS JOIN UNNEST(
    string_split(CAST(u.friends_json AS VARCHAR), ',')
  ) AS t(f)
  WHERE u.friends_json IS NOT NULL AND f <> '';
")

# make sure each pair (user, friend) is unique
dbExecute(con, "
  ALTER TABLE user_friends
  ADD CONSTRAINT pk_user_friends PRIMARY KEY (user_id, friend_id);
")

# create ELITE YEARS table 
dbExecute(con, "
  CREATE OR REPLACE TABLE user_elite_years AS
  SELECT
    u.user_id,
    CAST(e AS INTEGER) AS elite_year
  FROM users AS u
  CROSS JOIN UNNEST(
    regexp_extract_all(CAST(u.elite_json AS VARCHAR), '[0-9]{4}')
  ) AS t(e)
  WHERE u.elite_json IS NOT NULL;
")

# make sure each pair (user, elite_year) is unique
dbExecute(con, "
  ALTER TABLE user_elite_years
  ADD CONSTRAINT pk_user_elite PRIMARY KEY (user_id, elite_year);
")

# create REVIEWS table, and only keep reviews from businesses that are restaurants
sql_exec("
  CREATE OR REPLACE TABLE reviews AS
  SELECT
    j ->> 'review_id'   AS review_id,
    j ->> 'user_id'     AS user_id,
    j ->> 'business_id' AS business_id,
    CAST(j ->> 'stars'  AS INTEGER)         AS stars,
    CAST(j ->> 'date'   AS DATE)            AS date,
    j ->> 'text'                            AS text,
    CAST(j ->> 'useful' AS INTEGER)         AS useful_per_review,
    CAST(j ->> 'funny'  AS INTEGER)         AS funny_per_review,
    CAST(j ->> 'cool'   AS INTEGER)         AS cool_per_review
  FROM read_json_auto(?) AS j
  INNER JOIN businesses b
    ON b.business_id = (j ->> 'business_id');
", rev_json)

# add indexes to speed up review queries
dbExecute(con, "CREATE INDEX idx_reviews_user_date ON reviews(user_id, date);")
dbExecute(con, "CREATE INDEX idx_reviews_business  ON reviews(business_id);")

# create TIPS table
sql_exec("
  CREATE OR REPLACE TABLE tips AS
  SELECT
    row_number() OVER ()                     AS tip_id,
    j ->> 'user_id'       AS user_id,
    j ->> 'business_id'   AS business_id,
    j ->> 'text'          AS text,
    CAST(j ->> 'date' AS DATE)              AS date,
    CAST(j ->> 'compliment_count' AS INTEGER) AS compliment_count
  FROM read_json_auto(?) AS j
  INNER JOIN businesses b
    ON b.business_id = (j ->> 'business_id');
", tip_json)

# add indexes to speed up tip queries
dbExecute(con, "CREATE INDEX idx_tips_user_date ON tips(user_id, date);")
dbExecute(con, "CREATE INDEX idx_tips_business  ON tips(business_id);")

# drop helper columns we no longer need
dbExecute(con, "ALTER TABLE users DROP COLUMN friends_json;")
dbExecute(con, "ALTER TABLE users DROP COLUMN elite_json;")

# drop businesses table — we only needed it to filter reviews and tips
dbExecute(con, "DROP TABLE IF EXISTS businesses;")

# preview the final tables
print(dbGetQuery(con, "SELECT * FROM users LIMIT 15"))
print(dbGetQuery(con, "SELECT * FROM user_friends LIMIT 15"))
print(dbGetQuery(con, "SELECT * FROM user_elite_years LIMIT 15"))
print(dbGetQuery(con, "SELECT * FROM reviews LIMIT 15"))
print(dbGetQuery(con, "SELECT * FROM tips LIMIT 15"))

# disconnect from the database and save it
dbDisconnect(con, shutdown = TRUE)
cat("\n✅ Load finished — database saved as", db_file, "\n")
