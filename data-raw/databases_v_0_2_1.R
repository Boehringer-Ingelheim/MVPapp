#-------------------------------------------------------------------------------
#' This is a script that loads in databases from CDC, WHO, and NHANES
#' It is a .RData object that contains the following dataframes and columns:
#'
# 'cdc.expand'      - SEX, AGEMO, AGEYR, WT, HT (approx. 100k virtual subjects)
# 'who.expand'      - SEX, AGEMO, AGEYR, WT, HT (approx. 80k virtual subjects)
# 'nhanes.filtered' - SEX, AGEMO, AGEYR, WT, HT (approx. 90k real subjects)
#
#  Note that SEX = 0 is male, 1 = female. [Originally 1 = Male, 2 = Female]
#'
#' For the best user experience, the databases are already "baked" in the .RData
#' object.
#'
#' Some example code is included below on the different databases but note that it
#' may not produce the same .RData object as-is.
#'
#-------------------------------------------------------------------------------

read_pre_baked_databases <- TRUE

# CDC weight-for-age (https://www.cdc.gov/growthcharts/percentile_data_files.htm)
# Weight-for-age charts, 0 to 20 years, LMS parameters and selected smoothed weight percentiles in kilograms, by sex and age
# Stature-for-age charts, 0 to 20 years, LMS parameters and selected smoothed weight percentiles in cm, by sex and age
# Insert citation notes here in the future

# WHO weight-for-age (https://www.who.int/childgrowth/standards/weight_for_age/en/)
# from birth to 10 yo in boys and girls
# Individual files are stacked together to create one overall dataframe 'who'
# Insert citation notes here in the future
#
# WHO length/height for age (https://www.who.int/childgrowth/standards/height_for_age/en/)
# from birth to 5 yo in boys and girls
# Individual files are stacked together to create one overall dataframe 'who.ht'
#
# WHO length/height for age from birth to 5 yo in boys and girls (https://www.who.int/childgrowth/standards/height_for_age/en/)
# WHO growth reference 5-19 yo in boys and girls (https://www.who.int/growthref/who2007_height_for_age/en/)
# Individual files are stacked together to create one overall dataframe 'who.ht'
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The 'cdc' and 'who' datasets only has the L, M, S values.
# To generate WT, we need to generate Z scores and then apply the equation:
# WT = M * ((Z * L * S) + 1)^(1/L)
# for background see https://www.cdc.gov/growthcharts/background.htm
# for equations see https://www.cdc.gov/growthcharts/percentile_data_files.htm
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# NHANES 1999-2020 (pre-pandemic, until March 2020 [pre-pandemic])
# (https://wwwn.cdc.gov/nchs/nhanes/Default.aspx)
# ID, WT, SEX, AGEYR are retained by default.
# Insert citation notes here in the future

if(read_pre_baked_databases) {
  #load("databases.RData") #handled via LazyLoading
  load("nhanes.filtered.rda")
  load("who.expand.rda")
  load("cdc.expand.rda")
  } else {

    library(readr)
    library(foreign)
    library(magrittr)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # User input settings - Note this is just a starting point
    # Further manipulation is likely required in 'custom_settings' code chunk
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    minWT           <-  0    # This is used to pre-configure the databases, change as required
    maxWT           <-  200  # This is used to pre-configure the databases, change as required
    database_path   <- file.path(".", "Databases") # usually don't need to change this

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Reading in CDC, WHO, NHANES databases
    # Note that they have different column names for age, sex and weight
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read in CDC database
    # CDC weight-for-age (https://www.cdc.gov/growthcharts/percentile_data_files.htm)
    # Weight-for-age charts, 0 to 20 years, LMS parameters and selected smoothed weight percentiles in kilograms, by sex and age
    # Stature-for-age charts, 0 to 20 years, LMS parameters and selected smoothed weight percentiles in cm, by sex and age
    # Insert citation notes here in the future
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    cdc.wt.age36  <- read.csv(file.path(database_path, "CDC", "wtageinf.csv"), header = TRUE) #0-36 months
    cdc.wt.age240 <- read.csv(file.path(database_path, "CDC", "wtage.csv"), header = TRUE)   #24-240 months, 2-20y


    cdc <- bind_rows(cdc.wt.age36, cdc.wt.age240) %>%
      rename(SEX   = Sex,
             AGEMO = Agemos) %>%
      mutate(AGEYR = AGEMO / 12) %>%
      distinct(AGEMO, SEX, .keep_all = TRUE)   #remove duplicated rows

    ### Remove AGEMO == 36.0 in order to match number of rows with cdc.ht later on
    cdc %<>%
      filter(AGEMO != 36.0)

    cdc.ht.age36  <- read.csv(file.path(database_path, "CDC", "lenageinf.csv"), header = TRUE) #0-36 months
    cdc.ht.age240 <- read.csv(file.path(database_path, "CDC", "statage.csv"), header = TRUE) #24-240 months, 2-20y

    cdc.ht <- cdc.ht.age36 %>%
      select(Sex:P97) %>%                    #contains data not available for 2-20 y subjects
      bind_rows(cdc.ht.age240) %>%
      rename(SEX   = Sex,
             AGEMO = Agemos) %>%
      mutate(AGEYR = AGEMO / 12) %>%
      distinct(AGEMO, SEX, .keep_all = TRUE) #remove duplicated rows

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read in WHO database
    # WHO weight-for-age (https://www.who.int/childgrowth/standards/weight_for_age/en/)
    # from birth to 10 yo in boys and girls
    # Individual files are stacked together to create one overall dataframe 'who'
    # Insert citation notes here in the future
    #
    # WHO length/height for age (https://www.who.int/childgrowth/standards/height_for_age/en/)
    # from birth to 5 yo in boys and girls
    # Individual files are stacked together to create one overall dataframe 'who.ht'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    who.age.0.to.13w.boys  <- read.table(file.path(database_path, "WHO", "wfa_boys_0_13_zscores.txt"), header = TRUE)
    who.age.0.to.5.boys    <- read.table(file.path(database_path, "WHO", "wfa_boys_0_5_zscores.txt"), header = TRUE)
    who.age.5.to.10.boys   <- read.table(file.path(database_path, "WHO", "wfa_boys_z_WHO2007_exp.txt"), header = TRUE)

    who.age.0.to.13w.girls <- read.table(file.path(database_path, "WHO", "wfa_girls_0_13_zscores.txt"), header = TRUE)
    who.age.0.to.5.girls   <- read.table(file.path(database_path, "WHO", "wfa_girls_0_5_zscores.txt"), header = TRUE)
    who.age.5.to.10.girls  <- read.table(file.path(database_path, "WHO", "wfa_girls_z_WHO2007_exp.txt"), header = TRUE)

    # Create months in young paeds
    who.age.0.to.13w.boys.filtered  <- who.age.0.to.13w.boys  %>%
      mutate(Month = Week/4) %>%
      filter(Month != 0, Month != 1, Month != 2, Month != 3, Month != 3.25) %>% # Remove some rows to fit better with the other 0-5 years data
      select(-Week) %>%
      select(Month, everything())

    who.age.0.to.13w.girls.filtered <- who.age.0.to.13w.girls %>%
      mutate(Month = Week/4) %>%
      filter(Month != 0, Month != 1, Month != 2, Month != 3, Month != 3.25) %>% # Remove some rows to fit better with the other 0-5 years data
      select(-Week) %>%
      select(Month, everything())

    # Selecting columns of interest (L, M, S) and join together
    who.age.5.to.10.boys.filtered  <- who.age.5.to.10.boys  %<>% select(-SD4neg, -SD4)
    who.age.5.to.10.girls.filtered <- who.age.5.to.10.girls %<>% select(-SD4neg, -SD4)

    who.age.0.to.10.boys <- bind_rows(
      who.age.0.to.13w.boys.filtered,
      who.age.0.to.5.boys,
      who.age.5.to.10.boys.filtered
    ) %>%
      arrange(Month)

    who.age.0.to.10.girls <- bind_rows(
      who.age.0.to.13w.girls.filtered,
      who.age.0.to.5.girls,
      who.age.5.to.10.girls.filtered
    ) %>%
      arrange(Month)


    who.age.0.to.10.boys %<>%
      mutate(
        AGEYR = Month/12,
        SEX   = 1
      )

    who.age.0.to.10.girls %<>%
      mutate(
        AGEYR = Month/12,
        SEX   = 2
      )

    ## Stack boys and girls together

    who <- bind_rows(
      who.age.0.to.10.boys,
      who.age.0.to.10.girls
    )

    who %<>%
      rename(AGEMO = Month)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # WHO length/height for age from birth to 5 yo in boys and girls (https://www.who.int/childgrowth/standards/height_for_age/en/)
    # WHO growth reference 5-19 yo in boys and girls (https://www.who.int/growthref/who2007_height_for_age/en/)
    # Individual files are stacked together to create one overall dataframe 'who.ht'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    who.ht.0.to.13w.boys  <- read.table(file.path(database_path, "WHO", "lhfa_boys_0_13_zscores.txt"), header = TRUE)
    who.ht.0.to.2.boys    <- read.table(file.path(database_path, "WHO", "lhfa_boys_0_2_zscores.txt"),  header = TRUE)
    who.ht.2.to.5.boys    <- read.table(file.path(database_path, "WHO", "lhfa_boys_2_5_zscores.txt"),  header = TRUE)
    who.ht.5.to.19.boys   <- read.table(file.path(database_path, "WHO", "hfa_boys_z_WHO2007_exp.txt"), header = TRUE)

    who.ht.0.to.13w.girls <- read.table(file.path(database_path, "WHO", "lhfa_girls_0_13_zscores.txt"), header = TRUE)
    who.ht.0.to.2.girls   <- read.table(file.path(database_path, "WHO", "lhfa_girls_0_2_zscores.txt"),  header = TRUE)
    who.ht.2.to.5.girls   <- read.table(file.path(database_path, "WHO", "lhfa_girls_2_5_zscores.txt"),  header = TRUE)
    who.ht.5.to.19.girls  <- read.table(file.path(database_path, "WHO", "hfa_girls_z_WHO2007_exp.txt"), header = TRUE)

    # Create months in young paeds
    who.ht.0.to.13w.boys.filtered  <- who.ht.0.to.13w.boys  %>%
      mutate(Month = Week/4) %>%
      filter(Month != 0, Month != 1, Month != 2, Month != 3, Month != 3.25) %>% # Remove some rows to fit better with the other 0-5 years data
      select(-Week) %>%
      select(Month, everything())

    who.ht.0.to.13w.girls.filtered <- who.ht.0.to.13w.girls %>%
      mutate(Month = Week/4) %>%
      filter(Month != 0, Month != 1, Month != 2, Month != 3, Month != 3.25) %>% # Remove some rows to fit better with the other 0-5 years data
      select(-Week) %>%
      select(Month, everything())

    who.ht.0.to.2.boys.filtered <- who.ht.0.to.2.boys %>% filter(Month != 24) # Remove some rows to fit better with the other 2-5 years data
    who.ht.0.to.2.girls.filtered <- who.ht.0.to.2.girls %>% filter(Month != 24) # Remove some rows to fit better with the other 2-5 years data

    # Remove some columns to fit better with the other data
    who.ht.5.to.19.boys.filtered  <- who.ht.5.to.19.boys  %>% select(-SD5neg, -SD4neg, -SD4) %>% rename(SD = StDev)
    who.ht.5.to.19.girls.filtered <- who.ht.5.to.19.girls %>% select(-SD5neg, -SD4neg, -SD4) %>% rename(SD = StDev)

    # Selecting columns of interest (L, M, S - see SAP for equation) and join together
    who.ht.0.to.19.boys <- bind_rows(
      who.ht.0.to.13w.boys.filtered,
      who.ht.0.to.2.boys.filtered,
      who.ht.2.to.5.boys,
      who.ht.5.to.19.boys.filtered
    ) %>%
      arrange(Month)

    who.ht.0.to.19.girls <- bind_rows(
      who.ht.0.to.13w.girls.filtered,
      who.ht.0.to.2.girls.filtered,
      who.ht.2.to.5.girls,
      who.ht.5.to.19.girls.filtered
    ) %>%
      arrange(Month)


    who.ht.0.to.19.boys %<>%
      mutate(
        AGEYR = Month/12,
        SEX   = 1
      )

    who.ht.0.to.19.girls %<>%
      mutate(
        AGEYR = Month/12,
        SEX   = 2
      )

    ## Stack boys and girls together

    who.ht <- bind_rows(
      who.ht.0.to.19.boys,
      who.ht.0.to.19.girls
    )

    who.ht %<>%
      rename(AGEMO = Month)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read in and merge NHANES data
    # NHANES 1999-2020 (pre-pandemic, until March 2020)
    # (https://wwwn.cdc.gov/nchs/nhanes/Default.aspx)
    # ID, WT, SEX, AGEYR are retained by default.
    # Insert citation notes here in the future
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    nhanes.demogs.A <- read.xport(file.path(database_path, "NHANES", "DEMO_A_NHANES_1999-2000_demographics.xpt"))
    nhanes.demogs.B <- read.xport(file.path(database_path, "NHANES", "DEMO_B_NHANES_2001-2002_demographics.xpt"))
    nhanes.demogs.C <- read.xport(file.path(database_path, "NHANES", "DEMO_C_NHANES_2003-2004_demographics.xpt"))
    nhanes.demogs.D <- read.xport(file.path(database_path, "NHANES", "DEMO_D_NHANES_2005-2006_demographics.xpt"))
    nhanes.demogs.E <- read.xport(file.path(database_path, "NHANES", "DEMO_E_NHANES_2007-2008_demographics.xpt"))
    nhanes.demogs.F <- read.xport(file.path(database_path, "NHANES", "DEMO_F_NHANES_2009-2010_demographics.xpt"))
    nhanes.demogs.G <- read.xport(file.path(database_path, "NHANES", "DEMO_G_NHANES_2011-2012_demographics.xpt"))
    nhanes.demogs.H <- read.xport(file.path(database_path, "NHANES", "DEMO_H_NHANES_2013-2014_demographics.xpt"))
    nhanes.demogs.I <- read.xport(file.path(database_path, "NHANES", "DEMO_I_NHANES_2015-2016_demographics.xpt"))
    nhanes.demogs.P <- read.xport(file.path(database_path, "NHANES", "DEMO_P_NHANES_2017-2020_pre_pandemic_demographics.xpt"))

    nhanes.weight.A <- read.xport(file.path(database_path, "NHANES", "BMX_A_NHANES_1999-2000_body_measures.xpt"))
    nhanes.weight.B <- read.xport(file.path(database_path, "NHANES", "BMX_B_NHANES_2001-2002_body_measures.xpt"))
    nhanes.weight.C <- read.xport(file.path(database_path, "NHANES", "BMX_C_NHANES_2003-2004_body_measures.xpt"))
    nhanes.weight.D <- read.xport(file.path(database_path, "NHANES", "BMX_D_NHANES_2005-2006_body_measures.xpt"))
    nhanes.weight.E <- read.xport(file.path(database_path, "NHANES", "BMX_E_NHANES_2007-2008_body_measures.xpt"))
    nhanes.weight.F <- read.xport(file.path(database_path, "NHANES", "BMX_F_NHANES_2009-2010_body_measures.xpt"))
    nhanes.weight.G <- read.xport(file.path(database_path, "NHANES", "BMX_G_NHANES_2011-2012_body_measures.xpt"))
    nhanes.weight.H <- read.xport(file.path(database_path, "NHANES", "BMX_H_NHANES_2013-2014_body_measures.xpt"))
    nhanes.weight.I <- read.xport(file.path(database_path, "NHANES", "BMX_I_NHANES_2015-2016_body_measures.xpt"))
    nhanes.weight.P <- read.xport(file.path(database_path, "NHANES", "BMX_P_NHANES_2017-2020_pre_pandemic_body_measures.xpt"))

    # Merge NHANES data
    nhanes.demogs <- bind_rows(nhanes.demogs.A,
                               nhanes.demogs.B,
                               nhanes.demogs.C,
                               nhanes.demogs.D,
                               nhanes.demogs.E,
                               nhanes.demogs.F,
                               nhanes.demogs.G,
                               nhanes.demogs.H,
                               nhanes.demogs.I,
                               nhanes.demogs.P) %>%
      select(ID = SEQN,
             SEX = RIAGENDR,
             AGEYR = RIDAGEYR)

    nhanes.weight <- bind_rows(nhanes.weight.A,
                               nhanes.weight.B,
                               nhanes.weight.C,
                               nhanes.weight.D,
                               nhanes.weight.E,
                               nhanes.weight.F,
                               nhanes.weight.G,
                               nhanes.weight.H,
                               nhanes.weight.I,
                               nhanes.weight.P) %>%
      select(ID = SEQN,
             WT = BMXWT,
             HT = BMXHT)

    nhanes <- left_join(nhanes.demogs, nhanes.weight, by = "ID")

    nhanes %<>%
      filter(complete.cases(.)) %>%
      mutate(AGEMO = AGEYR * 12) %>% # Note AGEMO will not be accurate for young paeds since it is rounded to the nearest year
      select(-ID)

    nhanes.filtered <- nhanes %>%
      filter(WT >= minWT,
             WT <  maxWT)

    ### Re-ordering of columns
    nhanes.filtered %<>% select(SEX, AGEMO, AGEYR, WT, HT)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The 'cdc' and 'who' datasets only has the L, M, S values.
    # To generate WT, we need to generate Z scores and then apply the equation:
    # WT = M * ((Z * L * S) + 1)^(1/L)
    # for background see https://www.cdc.gov/growthcharts/background.htm
    # for equations see https://www.cdc.gov/growthcharts/percentile_data_files.htm
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Define a function that expands each row N times, randomise Z scores for each row,
    # and then derive the WT based on Z, L, M, S
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    generate_WT <- function(data, repeat_each_row = 300, z_mean = 0, z_sd = 1, seednum = 20230110) {
      set.seed(seednum)
      data.expanded <- data %>% slice(rep(row_number(), repeat_each_row))
      data.expanded %<>%
        mutate(Z  = rnorm(nrow(data.expanded), mean = z_mean, sd = z_sd))

      # weights for z > 2.2 start to become unrealistically large, especially for older subjects, so truncate  to 2.2:
      # no apparent issue with z's that are very negative. [Note - not filtered by default]
      data.expanded %<>%
        mutate(#Z  = case_when(Z > 2.2 ~ 2.2, TRUE    ~ Z),
          WT = round(M * ((Z * L * S) + 1)^(1/L), digits = 1) # round to 1 dp
        ) %>%
        select(AGEMO, AGEYR, WT, SEX)
      return(data.expanded)
    }

    cdc.expand <- generate_WT(cdc, repeat_each_row = 200)
    cdc.ht.expand <- generate_WT(cdc.ht, repeat_each_row = 200) %>% rename(HT = WT)  # Re-name to fit function, ugly hack

    who.expand <- generate_WT(who)
    who.ht.expand <- generate_WT(who.ht) %>% rename(HT = WT)

    ### Col bind CDC WT and HT
    cdc.expand <- dplyr::bind_cols(cdc.expand, cdc.ht.expand %>% select(HT))
    cdc.expand %<>% filter(WT >= minWT, WT < maxWT) # Filter after joining
    cdc.expand %<>% filter(HT > 0)

    ### Col bind WHO WT and HT
    who.ht.expand %<>% filter(AGEMO <= 120) # There are no WHO weight data beyond 10yr, trim to fit
    who.expand <- dplyr::bind_cols(who.expand, who.ht.expand %>% select(HT))
    who.expand %<>% filter(WT >= minWT, WT < maxWT) # Filter after joining
    who.expand %<>% filter(HT > 0)

    ### Re-ordering of columns
    cdc.expand %<>% select(SEX, AGEMO, AGEYR, WT, HT)
    who.expand %<>% select(SEX, AGEMO, AGEYR, WT, HT)

    ### Create BMI and pre-filter out non-sensible values, this will be repeated later
    cdc.expand %<>% mutate(BMI = WT / (HT/100)^2)
    cdc.expand %<>% filter(BMI >= 10 & BMI <= 50)

    who.expand %<>% mutate(BMI = WT / (HT/100)^2)
    who.expand %<>% filter(BMI >= 10 & BMI <= 50)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Recode SEX == 1 (Male) to 0, and SEX == 2 (Female) to 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cdc.expand %<>%
      mutate(
        SEX = case_when(
          SEX == 1 ~ 0,
          SEX == 2 ~ 1,
          TRUE ~ SEX
        ))

    who.expand %<>%
      mutate(
        SEX = case_when(
          SEX == 1 ~ 0,
          SEX == 2 ~ 1,
          TRUE ~ SEX
        ))

    nhanes.filtered %<>%
      mutate(
        SEX = case_when(
          SEX == 1 ~ 0,
          SEX == 2 ~ 1,
          TRUE ~ SEX
        ))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The three pre-configured sources of data with column names available are:
    #
    # 'cdc.expand'      - SEX, AGEMO, AGEYR, WT, HT
    # 'who.expand'      - SEX, AGEMO, AGEYR, WT, HT
    # 'nhanes.filtered' - SEX, AGEMO, AGEYR, WT, HT
    #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Uncomment below to save the configured databases
    ##save(cdc.expand, nhanes.filtered, who.expand, file = "databases.RData")
    #save(cdc.expand, file = "cdc.expand.rda") # use_data is much more compact
    #save(who.expand, file = "who.expand.rda")
    #save(nhanes.filtered, file = "nhanes.filtered.rda")

    #usethis::use_data(cdc.expand, overwrite = TRUE)
    #usethis::use_data(who.expand, overwrite = TRUE)
    #usethis::use_data(nhanes.filtered, overwrite = TRUE)

    ## Recompress all .rda files in the "data" directory
    # tools::resaveRdaFiles(paths = "data", compress = "xz")

} # end of read_pre_baked_databases

