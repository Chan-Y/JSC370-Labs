Lab 05 - Data Wrangling
================
Chan Yu

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs
# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd
# Step 3
# Happens on github
# Step 4
git init
git add README.Rmd
git commit -m "First commit"
# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")
# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )
# Step 3: Happens on Github
# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')
# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

``` r
library(dtplyr) # translator between dplyr (tidyverse) and data.table
library(dplyr)
library(data.table)
library(leaflet)
library(ggplot2)
```

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]
# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])
# Dropping NAs
stations <- stations[!is.na(USAF)]
# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

``` r
if (!file.exists("met_all.gz")) {
 download.file(
      url = "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz",
      destfile = "met_all.gz",
      method   = "libcurl",
      timeout  = 60
      ) 
}
met <- data.table::fread("met_all.gz")
```

3.  Merge the data as we did during the lecture.

``` r
met <- merge(
  x = met,
  y = stations,
  by.x = "USAFID",
  by.y = "USAF",
  all.x = TRUE,
  all.y = FALSE
)
# Make it lazy
met_lz <- lazy_dt(met, immutable = FALSE)

# MERGE OPT2 Same as left join in tidyverse
# left_join(x = met, y = stations, by = c("USAFID"="USAF"))
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
# average for each station
met_avg_lz <- met_lz %>% 
  group_by(USAFID) %>% 
  summarise(
    # tmp = mean(temp, na.rm = TRUE),
    # wind.sp = mean(wind.sp, na.rm = TRUE),
    # atm.press = mean(atm.press, na.rm = TRUE)
    # <===equivalent===>
    across(
      c(temp, wind.sp, atm.press),
      function(x) mean(x, na.rm = TRUE)
    )
  ) 
# find median of temp, wind.sp, atm.press
met_med_lz <- met_avg_lz %>% 
  summarise(across(
    2:4,
    function(x) quantile(x, probs = .5, na.rm = TRUE)
  )) 
met_med_lz
```

    ## Source: local data table [1 x 3]
    ## Call:   `_DT1`[, .(temp = (function (x) 
    ## mean(x, na.rm = TRUE))(temp), wind.sp = (function (x) 
    ## mean(x, na.rm = TRUE))(wind.sp), atm.press = (function (x) 
    ## mean(x, na.rm = TRUE))(atm.press)), keyby = .(USAFID)][, .(temp = (function (x) 
    ## quantile(x, probs = 0.5, na.rm = TRUE))(temp), wind.sp = (function (x) 
    ## quantile(x, probs = 0.5, na.rm = TRUE))(wind.sp), atm.press = (function (x) 
    ## quantile(x, probs = 0.5, na.rm = TRUE))(atm.press))]
    ## 
    ##    temp wind.sp atm.press
    ##   <dbl>   <dbl>     <dbl>
    ## 1  23.7    2.46     1015.
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

``` r
# Station closer to median temperature
temp_id <- met_avg_lz %>%
  mutate(
    d = abs(temp - met_med_lz %>% pull(temp))
  ) %>%
  arrange(d) %>%
  slice(1) %>%
  pull(USAFID)

# Station closer to median wind.sp
wsp_id <- met_avg_lz %>%
  mutate(
    d = abs(wind.sp - met_med_lz %>% pull(wind.sp))
  ) %>%
  arrange(d) %>%
  slice(1) %>%
  pull(USAFID)

# Station closer to median atm.press
atm_id <- met_avg_lz %>%
  mutate(
    d = abs(atm.press - met_med_lz %>% pull(atm.press))
  ) %>%
  arrange(d) %>%
  slice(1) %>%
  pull(USAFID)


cat(
  "ID with median ...",
  "\n\t temperature: ", temp_id,
  "\n\t wind speed: ", wsp_id,
  "\n\t atm pressure: ", atm_id
)
```

    ## ID with median ... 
    ##   temperature:  725515 
    ##   wind speed:  720929 
    ##   atm pressure:  723200

``` r
met_lz %>% 
  select(USAFID, lon, lat) %>% 
  distinct() %>% 
  filter(USAFID %in% c(temp_id, wsp_id, atm_id))
```

    ## Source: local data table [5 x 3]
    ## Call:   unique(`_DT1`[, .(USAFID, lon, lat)])[USAFID %in% c(temp_id, 
    ##     wsp_id, atm_id)]
    ## 
    ##   USAFID   lon   lat
    ##    <int> <dbl> <dbl>
    ## 1 720929 -92.0  45.5
    ## 2 723200 -85.2  34.4
    ## 3 723200 -85.2  34.3
    ## 4 723200 -85.2  34.4
    ## 5 725515 -96.8  40.3
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

The three stations do not coincide. The median station for temperature
has ID 725515, the station for median wind speed has ID 720929, and for
atmospheric pressure, it has ID 723200.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
# average for each station
met_avg_lz <- met_lz %>% 
  group_by(USAFID) %>% 
  summarise(across(
      c(temp, wind.sp, atm.press, lat, lon),
      function(x) mean(x, na.rm = TRUE)
    )) 

# average for each station with its lat, lon, state
met_avg_lz <- 
  left_join(met_avg_lz, stations, by = c("USAFID"="USAF"))

# median of temp, wind.sp, atm.press per state
met_st_med_lz <- met_avg_lz %>% 
  group_by(STATE) %>% 
  summarise(across(
    2:4,
    function(x) quantile(x, probs = .5, na.rm = TRUE)
  ))

# average for each station with it's state median
# by met_avg_lz left join met_st_med_lz on state
met_avg_lz <- 
  left_join(met_avg_lz, met_st_med_lz, by = "STATE") 
```

Compute the euclidean distance between state median value and each
station

``` r
udist <- function(pt1, pt2) {
  sqrt(sum((pt1 - pt2)^2))
}
# tried but failed to use function, will try later again

met_avg_lz <- met_avg_lz %>% 
  mutate(dist = sqrt(
    (temp.x-temp.y)^2 + (wind.sp.x-wind.sp.y)^2 + (atm.press.x-atm.press.y)^2 
  ))


# https://stackoverflow.com/questions/24070714/extract-row-corresponding-to-minimum-value-of-a-variable-by-group
state_rep <- met_avg_lz %>% 
  group_by(STATE) %>% 
  slice(which.min(dist))
state_rep
```

    ## Source: local data table [46 x 12]
    ## Groups: STATE
    ## Call:
    ##   _DT3 <- setnames(setcolorder(setnames(setcolorder(`_DT2`[`_DT1`[, .(temp = <function(x)
    ##   _DT3 <-   mean(x, na.rm = TRUE)>(temp), wind.sp = <function(x) mean(x, na.rm = TRUE)>(
    ##   _DT3 <-   wind.sp), atm.press = <function(x) mean(x, na.rm = TRUE)>(atm.press), lat = <function(
    ##   _DT3 <-   x) mean(x, na.rm = TRUE)>(lat), lon = <function(x) mean(x, na.rm = TRUE)>(lon)),
    ##   _DT3 <- keyby = .(USAFID)], on = .(USAF = USAFID), allow.cartesian = TRUE], <int: 1L,
    ##   _DT3 <-   4L, 5L, 6L, 7L, ...>), "USAF", "USAFID")[, .(temp = <function(x) quantile(x,
    ##   _DT3 <-   probs = 0.5, na.rm = TRUE)>(temp), wind.sp = <function(x) quantile(x, probs = 0.5,
    ##   _DT3 <-   na.rm = TRUE)>(wind.sp), atm.press = <function(x) quantile(x, probs = 0.5,
    ##   _DT3 <-   na.rm = TRUE)>(atm.press)), keyby = .(STATE)][setnames(setcolorder(`_DT2`[
    ##   _DT3 <-   `_DT1`[, .(temp = <function(x) mean(x, na.rm = TRUE)>(temp), wind.sp = <function(
    ##   _DT3 <-     x) mean(x, na.rm = TRUE)>(wind.sp), atm.press = <function(x) mean(x, na.rm = TRUE)>(
    ##   _DT3 <-     atm.press), lat = <function(x) mean(x, na.rm = TRUE)>(lat), lon = <function(
    ##   _DT3 <-     x) mean(x, na.rm = TRUE)>(lon)), keyby = .(USAFID)], on = .(USAF = USAFID),
    ##   _DT3 <-   allow.cartesian = TRUE], <int: 1L, 4L, 5L, 6L, 7L, ...>), "USAF", "USAFID"),
    ##   _DT3 <- on = .(STATE), allow.cartesian = TRUE], <int: 5L, 6L, 7L, 8L, 9L, ...>),
    ##   _DT3 <-   <chr: "i.temp", "i.wind.sp", "i.atm.press", "temp", "wind.sp", ...>,
    ##   _DT3 <-   <chr: "temp.x", "wind.sp.x", "atm.press.x", "temp.y", "wind.sp.y", ...>)[,
    ##   _DT3 <-   `:=`(dist = sqrt((temp.x - temp.y)^2 + (wind.sp.x - wind.sp.y)^2 +
    ##   _DT3 <-     (atm.press.x - atm.press.y)^2))]
    ##   `_DT3`[`_DT3`[, .I[which.min(dist)[between(which.min(dist), -.N, 
    ##     .N)]], by = .(STATE)]$V1]
    ## 
    ##   USAFID temp.x wind.s…¹ atm.p…²   lat    lon CTRY  STATE temp.y wind.…³ atm.p…⁴
    ##    <int>  <dbl>    <dbl>   <dbl> <dbl>  <dbl> <chr> <chr>  <dbl>   <dbl>   <dbl>
    ## 1 722970   22.8     2.33   1013.  33.8 -118.  US    CA      22.7    2.57   1013.
    ## 2 722416   29.8     3.54   1012.  29.7  -98.0 US    TX      29.8    3.41   1012.
    ## 3 725395   20.4     2.36   1015.  42.3  -84.5 US    MI      20.5    2.27   1015.
    ## 4 723190   25.7     2.25   1015.  34.5  -82.7 US    SC      25.8    1.70   1015.
    ## 5 725440   22.8     2.57   1015.  41.5  -90.5 US    IL      22.4    2.24   1015.
    ## 6 723495   24.3     2.55   1014.  37.2  -94.5 US    MO      24.0    2.45   1015.
    ## # … with 40 more rows, 1 more variable: dist <dbl>, and abbreviated variable
    ## #   names ¹​wind.sp.x, ²​atm.press.x, ³​wind.sp.y, ⁴​atm.press.y
    ## 
    ## # Use as.data.table()/as.data.frame()/as_tibble() to access results

``` r
# Check whether exists multiple stations for one state
nrow(as_tibble(state_rep)) == length(unique(state_rep %>% pull(STATE)))
```

    ## [1] TRUE

In result `state_rep`, for each state, there’s only 1 weather station
listed, so no need to select by latitude.

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, examine the
  association between median temperature (y) and median wind speed (x).
  Create a scatterplot of the two variables using ggplot2. Add both a
  linear regression line and a smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.
