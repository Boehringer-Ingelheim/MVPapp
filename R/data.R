# See local/databases_v_0_2_1.R for more details and code.
# 'cdc.expand'      - SEX, AGEMO, AGEYR, WT, HT (approx. 100k virtual subjects)
# 'who.expand'      - SEX, AGEMO, AGEYR, WT, HT (approx. 80k virtual subjects)
# 'nhanes.filtered' - SEX, AGEMO, AGEYR, WT, HT (approx. 90k real subjects)


#' World Health Organization Pediatric Data
#'
#' WHO weight-for-age (https://www.who.int/childgrowth/standards/weight_for_age/en/)
#' from birth to 10 yo in boys and girls
#' Individual files are stacked together to create one overall dataframe 'who'
#' Insert citation notes here in the future
#'
#' WHO length/height for age (https://www.who.int/childgrowth/standards/height_for_age/en/)
#' from birth to 5 yo in boys and girls
#' Individual files are stacked together to create one overall dataframe 'who.ht'
#'
#' WHO length/height for age from birth to 5 yo in boys and girls (https://www.who.int/childgrowth/standards/height_for_age/en/)
#' WHO growth reference 5-19 yo in boys and girls (https://www.who.int/growthref/who2007_height_for_age/en/)
#' Individual files are stacked together to create one overall dataframe 'who.ht'
#'
#' @format ## `who.expand`
#' A data frame with 77744 rows and 6 columns SEX, AGEMO, AGEYR, WT, HT
#' (approx. 80k virtual subjects)
#' @source <https://www.who.int/childgrowth/standards/weight_for_age/en/>
"who.expand"


#' NHANES 1999-2020 (pre-pandemic, until March 2020, pre-pandemic
#' (https://wwwn.cdc.gov/nchs/nhanes/Default.aspx) 
#' ID, WT, SEX, AGEYR are retained by default.
#' Insert citation notes here in the future
#'
#' @format ## `nhanes.filtered`
#' 'data.frame':	92901 obs. of  5 variables:
#' SEX  : num  1 0 1 0 1 1 0 1 0 0 ...
#' AGEMO: num  24 924 120 588 228 708 156 132 516 180 ...
#' AGEYR: num  2 77 10 49 19 59 13 11 43 15 ...
#' WT   : num  12.5 75.4 32.9 92.5 59.2 ...
#' HT   : num  91.6 174 136.6 178.3 162 ...
#' @source <https://wwwn.cdc.gov/nchs/nhanes/Default.aspx>
"nhanes.filtered"


#' CDC weight-for-age (https://www.cdc.gov/growthcharts/percentile_data_files.htm)
#' Weight-for-age charts, 0 to 20 years, LMS parameters and selected smoothed weight percentiles in kilograms, by sex and age
#' Stature-for-age charts, 0 to 20 years, LMS parameters and selected smoothed weight percentiles in cm, by sex and age
#' Insert citation notes here in the future
#'
#' @format ## `cdc.expand`
#' data.frame':	97177 obs. of  6 variables:
#' $ SEX  : num  0 0 0 0 0 0 0 0 0 0 ...
#' $ AGEMO: num  0 0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 ...
#' $ AGEYR: num  0 0.0417 0.125 0.2083 0.2917 ...
#' $ WT   : num  2.6 3.8 4.9 4.9 6.5 7.6 7.2 8.4 8.8 7.2 ...
#' $ HT   : num  45.7 51.8 56.6 57 62.5 65.9 64.8 68.5 69.9 65.8 ...
#' $ BMI  : num  12.4 14.2 15.3 15.1 16.6 ...
#' @source <https://www.cdc.gov/growthcharts/percentile_data_files.htm>
"cdc.expand"