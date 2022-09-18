#' In this script there are functions for splitting
#' and cleaning data.
#' Data must be exactly divisible by 24 h
#' Data must (preferably) not contained NA
#' Data must not contained isHDD == FALSE

if (!exists("hdd_threshold")) source("00_settings.R", encoding = 'UTF-8')

# 1. Load / read data -----------------------------------------

load("./data/tableau_res.Rdata")


# 2. Split Train / Test ---------------------------------------

df_train <- df.res[1:8784, ] # Schaltjahre

remove(df.res)

df_train$P.urb.pu  <- NULL
df_train$P.sub.obs <- NULL
df_train$P.sub.pu  <- NULL
df_train$t.hh      <- NULL
df_train$isHDD     <- NULL



# 3. Cleaning --------------------------------------------------

#  Set names: if measurements include Temperature

if (ncol(df_train) == 3) {
  
  colnames(df_train) <- c("datetime_idx", "P_obs", "T_obs")
  
}else{
  
  colnames(df_train) <- c("datetime_idx", "P_obs")
  
}

# Filter 0 [kW] days, i.e. isHDD = FALSE  

df_train$is_HDD <- is_HDD(df_train, hdd_threshold)

df_train <- subset(df_train, df_train$is_HDD == TRUE)


# FILTER observations with NA

na_dates <- unique(as.Date(df_train$datetime_idx[which(is.na(df_train$P_obs))]))

df_train <- df_train[-which(as.Date(df_train$datetime_idx) %in% na_dates), ] 

row.names(df_train) <- seq(1, nrow(df_train), 1)

remove(na_dates)

# FILTER Summer Time --> Pheat = 0 kW

# from 2016-05-23 00:00:00 idx 4921

# to:  2016-10-07 23:00:00 idx 6048

df_train <- df_train[-seq(4921, 6048, 1), ] 

row.names(df_train) <- seq(1, nrow(df_train), 1)

# save(df_train, file = "./data_examples/urb_20151012_20161011_UTC.Rdata")


