

###Functions created by Jacqui Levy for ECCC WBNP and FWAP to process data for the Water Survey of Canada###
##See associated .Rmd files for methods and use
###contact: jacqui.s.levy@gmail.com###

##Sept 2024###

####### 1) Tidying functions#####

#function calculates the day of the water year using a tidyhydat data frame
#with STATION_NUMBER and waterYear columns.
#First use add_water_year() from add_water_year.R to get the waterYear

calc_day_of_wyear <- function(data){

  grouped_df <- {{data}} %>%
    group_by(STATION_NUMBER, waterYear) %>%
    mutate(day_of_year = row_number()) %>%
    ungroup()

  counts <- grouped_df %>%
    group_by(STATION_NUMBER, waterYear) %>%
    summarize(count = n())

  wrong_num_groups <- counts %>% filter(count !=365 & count != 366)
  if(nrow(wrong_num_groups) > 0){
    warning("The following groups do not have 365 or 366 values:\n",
            paste0("STATION_NUMBER: ", wrong_num_groups$STATION_NUMBER,
                   ", waterYear: ", wrong_num_groups$waterYear,
                   ", Values: ", wrong_num_groups$count, collapse = "\n"))
  }
  rm(counts, wrong_num_groups)
  return(grouped_df)

}


#function calculates the week number of each year for each STATION_NUMBER and waterYear
calc_weeks <- function(data){

  df <- {{data}} %>%
    group_by(STATION_NUMBER, waterYear) %>%
    dplyr::mutate(weeks = rep(1:(ceiling(n()/7)), each = 7)[1:n()]) %>%
    mutate(weeks = if_else(weeks == 53, 52, weeks)) %>% #make the one day week 53 be week 52. FYI then there are 9 days in week 52 in leap years.
    ungroup()
}


####### 2) Missing Data functions#####


#remove a week for a station number and year, if there are x or more missing days in that week (7 or less). FYI there could be days missing at the end of one week and the beginning of the next, but the function doesn't check for that
rem_missing_wks <- function(x, days){

  df <- {{x}}

  toremove <- df %>%
    group_by(STATION_NUMBER, waterYear, weeks) %>%
    select(-c(Date, Parameter, day_of_year, month)) %>%
    mutate(week_nas = sum(is.na(Value))) %>%
    filter(week_nas > {{days}}) %>%
    distinct()%>%
    ungroup()

  df_removed <- anti_join(df, toremove, by = c('STATION_NUMBER', 'waterYear', 'weeks'))

  return(df_removed)
}

#remove a month in a station number and year, if there are x or more missing days in that month. FYI there could be days missing at the end of one month and the beginning of the next, but the function doesn't check for that
rem_missing_months <- function(x, days){

  df <- {{x}} %>% mutate(month = month(Date))

  toremove <- df %>%
    group_by(STATION_NUMBER, waterYear, month) %>%
    select(-c(Date, Parameter, day_of_year, weeks)) %>%
    mutate(month_nas = sum(is.na(Value))) %>%
    filter(month_nas > {{days}}) %>%
    distinct() %>%
    ungroup()

  df_removed <- anti_join(df, toremove, by = c('STATION_NUMBER', 'waterYear', 'month'))

  return(df_removed)
}


#removes months with missing days >x and then removes years with any number of missing months.
rem_missing_years <- function(x, days){
  df <- {{x}}
  df_mnths_removed <- rem_missing_months(df, {{days}})

  toremove <- df_mnths_removed %>%
    group_by(STATION_NUMBER, waterYear) %>%
    summarize(month = n_distinct(month)) %>%
    filter(month < 12) %>%
    ungroup()

  df_removed <- anti_join(df_mnths_removed, toremove, by = c('STATION_NUMBER', 'waterYear'))

  return(df_removed)

}


#function to remove years that have > x consecutive NA values (ie 3 days in a row with no data)
calc_rle <- function(data, days) {
  na_rows <- with(rle(is.na({{data}}$Value)), rep(values & lengths > {{days}}, lengths))
  years_to_remove <- unique({{data}}$waterYear[na_rows])
  output <- {{data}}[!{{data}}$waterYear %in% years_to_remove, ]
  return(output)
}

####### 3) Annual Data functions#####

#yearly mean and median and std for each station and waterYear
calc_mn_md_annual <- function(data){
  input_data <- {{data}}
  results <- input_data %>%
    dplyr::group_by(STATION_NUMBER, waterYear) %>%
    dplyr::summarize(annual_mean = round(mean(Value), 3), annual_median = round(median(Value),3), annual_sd = sd(Value)) %>% ungroup()
  min <- min(year(input_data$Date))
  max <- max(year(input_data$Date))
  yrs_all <- min:max

  results <-results %>% complete(waterYear = yrs_all)
  return(results)
}

#monthly mean and median and std for each station and waterYear
calc_mn_md_month <- function(data){
  input_data <- {{data}}
  results <- input_data %>%
    mutate(month = factor(lubridate::month(Date, label = TRUE, abbr = TRUE), levels = month.abb)) %>%
    dplyr::group_by(STATION_NUMBER, waterYear, month) %>%
    dplyr::summarize(monthly_mean = round(mean(Value),3), monthly_median = round(median(Value), 3), monthly_std = round(sd(Value), 3)) %>%
    pivot_wider(names_from = month, values_from = c(monthly_mean, monthly_median, monthly_std), names_prefix = "") %>%   select(STATION_NUMBER, waterYear, monthly_mean_Jan, monthly_mean_Feb, monthly_mean_Mar, monthly_mean_Apr, monthly_mean_May, monthly_mean_Jun, monthly_mean_Jul, monthly_mean_Aug, monthly_mean_Sep, monthly_mean_Oct, monthly_mean_Nov, monthly_mean_Dec, monthly_median_Jan, monthly_median_Feb, monthly_median_Mar, monthly_median_Apr, monthly_median_May, monthly_median_Jun, monthly_median_Jul, monthly_median_Aug, monthly_median_Sep, monthly_median_Oct, monthly_median_Nov, monthly_median_Dec, monthly_std_Jan, monthly_std_Feb, monthly_std_Mar, monthly_std_Apr, monthly_std_May, monthly_std_Jun, monthly_std_Jul, monthly_std_Aug, monthly_std_Sep, monthly_std_Oct, monthly_std_Nov, monthly_std_Dec)  %>%
    ungroup()
  min <- min(year(input_data$Date))
  max <- max(year(input_data$Date))
  yrs_all <- min:max
  results <-results %>% complete(waterYear = yrs_all)
  return(results)
}

#weekly mean and median and std for each station and waterYear
calc_mn_md_week <- function(x){
  df <- {{x}}
  results <- df %>%
    group_by(STATION_NUMBER, weeks) %>%
    summarize(weekly_mean = round(mean(Value),1), weekly_median = round(median(Value), 1))
  return(results)
}

#yearly mean and median for each station and waterYear for seasonal data
calc_mn_md_seasonal <- function(data){
  input_data <- {{data}}
  results <- input_data %>%
    dplyr::filter(month(Date) %in% c(3,4,5,6,7,8,9,10)) %>%
    dplyr::group_by(STATION_NUMBER, waterYear) %>%
    dplyr::summarize(seasonal_mean = round(mean(Value), 3), seasonal_median = round(median(Value),3), seasonal_sd = round(sd(Value)),3) %>% ungroup()
  min <- min(year(input_data$Date))
  max <- max(year(input_data$Date))
  yrs_all <- min:max
  results <-results %>% complete(waterYear = yrs_all)
  return(results)
}

####### 4) Center of Mass/Magnitude#####


center_of_mass <- function(data){

  df <- data %>%
    group_by(STATION_NUMBER, waterYear) %>%
    mutate(sum_runoff = sum(Value)) %>%
    mutate(runoff_50  = sum_runoff*.5) %>%
    mutate(cuml_sum = cumsum(Value))

  result <- df %>%group_by(STATION_NUMBER, waterYear) %>%


    do({
      df_subset <- .
      perc_cumsum <- df_subset[which.min(abs(df_subset$runoff_50-df_subset$cuml_sum)),]
      perc_cumsum <- df_subset[which(df_subset$cuml_sum > df_subset$runoff_50)[1], ]

      perc_cumsum <- data.frame(perc_cumsum)
    })
  return(result)
}




####### 5) B dates function#####

#Finds the first b date, last b date, first b value, last b value,
#and the duration of the open water and ice effected seasons.
#Make sure there is a full, year-round dataset with minimal missing data.

b_dates <- function(df){

  input_df <- {{df}}
  output_df_firstB <- input_df %>%
    group_by(STATION_NUMBER, waterYear) %>%
    arrange(STATION_NUMBER, waterYear, Date) %>%
    filter(Symbol == "B") %>%
    slice(1) %>%
    ungroup() %>%
    select(STATION_NUMBER, waterYear, first_B_Date = Date, first_B_Value = Value, first_B_doy = day_of_year)


  output_df_lastB <- input_df %>%
    group_by(STATION_NUMBER, waterYear) %>%
    arrange(STATION_NUMBER, waterYear, Date) %>%
    filter(Symbol == "B") %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(STATION_NUMBER, waterYear, last_B_Date = Date, last_B_Value = Value, last_B_doy = day_of_year)

  output_df <- merge(output_df_firstB, output_df_lastB, on = c(STATION_NUMBER, waterYear))

  output_df <- output_df %>%
    group_by(STATION_NUMBER, waterYear) %>%
    mutate(duration_ice = last_B_doy - first_B_doy) %>%
    mutate(duration_ow = 365 - duration_ice)
  return(output_df)

}


#######6) Ice variables#####

#Used to calculate the freeze-up dates, ice break-up dates, and continuous ice coverage
#df must have col names: "day_of_year", "Value", "Symbol", "Date", "waterYear"
#Date must be in yyyy-mm-dd format for all ice variables functions

#GROUP 1 FUNCTION - ICE COVER

Group_1_ice_cover <- function(data) {
  # Function returns a df for the length of ice coverage, per water year
  # Works with one station, multiple years
  # For multiple stations, split-apply-combine station

  lst <- list()
  data <- {{data}}
  # Iterate over unique water years
  for (i in unique(data$waterYear)) {
    # Subset data for the current water year
    year_data <- data[data$waterYear == i, ]
    # Find the length of ice coverage
    length_B_date <- max(rle(year_data$Symbol == "B")$lengths)
    # Append length to list
    lst[[as.character(i)]] <- length_B_date
  }

  # Create dataframe with waterYear and ice coverage
  Ice_coverage_wy <- data.frame(waterYear = as.integer(names(lst)), B_date_length = unlist(lst))
  #Ice_coverage_wy <- Ice_coverage_wy %>% rownames_to_column("Year")
  rownames(Ice_coverage_wy) <- NULL
  return(Ice_coverage_wy)
}




#GROUP 2 FUNCTION - FREEZE AND THAW DATES

Group_2_freeze_thaw <- function(data) {
  #This function calculates the freeze and thaw dates and their flow values, using the longest consecutive run of "B" Symbols in the df

  start_date_lst <- list()
  end_date_lst <- list()
  start_flow_lst <- list()
  end_flow_lst <- list()
  start_doy_lst <- list()
  end_doy_lst <- list()
  waterYear_lst <- list()

  for (i in unique({{data}}$waterYear)){

    df_subset <- {{data}}[{{data}}$waterYear == i,]

    #calc rle for B symbol
    rle_m = rle(df_subset$Symbol == "B")

    #find index for max run of B symbols
    max_run_index <- which.max(rle_m$lengths)

    #find end start and index of max run of B symbols
    end <- cumsum(rle_m$lengths)[max_run_index]
    start <- end - rle_m$lengths[max_run_index] +1

    #find date at end, start index of the max run
    date_end = df_subset$Date[end]
    date_start = df_subset$Date[start]

    #find flow at end, start index
    flow_end = df_subset$Value[end]
    flow_start = df_subset$Value[start]

    #find doy at end, start index
    doy_end = df_subset$day_of_year[end]
    doy_start = df_subset$day_of_year[start]

    #append dates to the list
    start_date_lst[[i]] <- date_start
    end_date_lst[[i]] <- date_end

    #append flows to the list
    start_flow_lst[[i]] <- flow_start
    end_flow_lst[[i]] <- flow_end

    #append doy to the list
    start_doy_lst[[i]] <- doy_start
    end_doy_lst[[i]] <- doy_end

    waterYear_lst[[i]] <- rep(i, length(date_start))

  }

  Freeze_Date <- as.Date(unlist(start_date_lst))
  Thaw_Date <- as.Date(unlist(end_date_lst))
  Flow_Freeze <- unlist(start_flow_lst)
  Flow_Thaw <- unlist(end_flow_lst)
  Freeze_DOY <-unlist(start_doy_lst)
  Thaw_DOY <- unlist(end_doy_lst)
  waterYear <- unlist(waterYear_lst)

  df <- data.frame(waterYear, Freeze_Date,Freeze_DOY,Flow_Freeze,Thaw_Date,Thaw_DOY, Flow_Thaw)

  # Ice_coverage_dates_flow <- rownames_to_column(df, "waterYear")
  return(df)

}



####### 7) Freshet functions#####

#GROUP 3 FUNCTION - ONSET OF FRESHET

#a complex and lengthy function that calculates freshet using the burns method
#ie when a day is 1.5 times the 16 day running mean
#the rolling mean here is set to start on March 1

Group_3_freshet <- function(data) {
  index <- 0
  f_index <- 16
  date_lst <- list()
  flow_lst <- list()
  stn_nu <- list()
  doy_lst <- list()

  for (i in unique({{data}}$waterYear)) { #first loop
    #subset data by year, resetting at each year
    index = 0
    f_index = 16
    df_subset <- {{data}}[{{data}}$waterYear == i,]
    df_subset <- df_subset %>%
      #data tidying:delete dates before Feb 12, so rolling mean calc starts on March 1
      mutate(Date = as.Date(Date)) %>%
      #filter(month(Date) %in% c(3,4,5,6))
      mutate(new_col = format(Date,"%m-%d")) %>%
      filter(month(Date) >= 2 & month(Date) < 7) %>%
      filter(!(new_col %in% c("02-01", "02-02", "02-03", "02-04", "02-05", "02-06", "02-07", "02-08", "02-09", "02-10", "02-11")))

    #calc rolling 16 day mean
    rollmn <- rollmean(df_subset$Value, k = 16, width = 16)
    #rollmn <- as.data.frame(rollmn)

    for (j in rollmn) { #second loop
      #increment index
      index = index + 1
      f_index = f_index + 1

      #find rolling mean value at the index and multiply by 1.5
      rollmnvalue <- rollmn[index] #roll mean = 0.81
      rollmnvalue1.5 <- rollmnvalue*1.5 #1.215

      #get the flow value at that index
      flowvalue <- df_subset$Value[f_index] #flow = 0.654

      if (flowvalue > rollmnvalue1.5 & f_index < 123 ) { #third loop
        #append date, flow value to a list using the index numbers
        dt <- df_subset$Date[f_index]
        fl <- df_subset$Value[f_index]
        st <- df_subset$STATION_NUMBER[f_index]
        doy <- df_subset$day_of_year[f_index]
        # print(dt)
        #  print(fl)
        #  print("end")
        date_lst[[i]] <- dt
        flow_lst[[i]] <- fl
        stn_nu[[i]] <- st
        doy_lst[[i]] <- doy
        Freshet_Date <- as.Date(unlist(date_lst))
        Freshet_Flow <- unlist(flow_lst)
        Station_Number <- unlist(stn_nu)
        Freshet_Dayofyear <- unlist(doy_lst)
        df <- cbind.data.frame(Station_Number, Freshet_Date, Freshet_Dayofyear, Freshet_Flow)
        break
      }

    }

  }
  Freshet_dates_flow <- rownames_to_column(df, "waterYear")
  return(Freshet_dates_flow)
}


#FRESHET PERCENTILES METHODS
#calculates total open water runoff, and sets freshet when the daily running total for runoff reaches
#10% of the total runoff. This number can be adjusted in the function below.
freshet_percentile <- function(data){

  df <- data %>%
    group_by(STATION_NUMBER, waterYear) %>%
    mutate(sum_runoff = sum(Value)) %>%
    mutate(runoff_10 = sum_runoff*.1) %>% #adjust percentage here
    mutate(cuml_sum = cumsum(Value))

  result <- df %>%group_by(STATION_NUMBER, waterYear) %>%


    do({
      df_subset <- .
      perc_cumsum <- df_subset[which.min(abs(df_subset$runoff_10-df_subset$cuml_sum)),]
      perc_cumsum <- data.frame(perc_cumsum)
    })
  return(result)
}



####### 8) Mann Kendall#####

#Performs mann-kendall test for one variable at a time but multiple "STATION_NUMBER"s in a dataframe.
#No missing data or years, use expand() to make sure there is a full set of dates for each variable
#parameter = col_variable to calculate MK test (must be annual variable)
#start = start year

library(Kendall)
calc_MK <- function(data, parameter, start) {
    plst <- list()
    stn_list <- list()
    for (i in unique({{data}}$STATION_NUMBER)) {
    #subset the data by stn number
    df_subset <- {{data}}[{{data}}$STATION_NUMBER == i,]
    #get the stn number for each iteration and append to a list
    #stn_num <- i
    stn_list[[i]] <- i

    col_var <- df_subset %>% pull({{parameter}})
    #df subset to a ts object and run MK analysis
    TS <- ts(col_var, frequency = 1, start = c({{start}}, 1))
    MK <- MannKendall(TS)
    #append pvalue to a list
    pval <- as.numeric(MK$sl)
    plst[[i]] <- pval
    #unlist, rename etc
    df <- as.data.frame(unlist(plst))
    names(df) <- "P_Value"
    final_MK <- rownames_to_column(df, "STATION_NUMBER") %>%
      mutate(Interpretation = case_when(P_Value <= .05 ~ "Signficant", .default =  "Not Significant"))
  }
  return(final_MK)
  }



####### 9) Percent Change IHA values#####

# Calculate percent change using pre and post data, using IHA values

calc_percent_change <- function(data_pre, data_post, stn, year_col){

  #calculates percent change from IHA pre and post stns, once tidying is complete.
  #stn = i if looping through multiple stns

  years_post <- {{data_post}}[[year_col]]

  IHA_medians_pre <- {{data_pre}} %>%
    filter(STATION_NUMBER == {{stn}}) %>%
    select(-c(STATION_NUMBER))

  IHA_pst <- {{data_post}} %>%
    filter(STATION_NUMBER == {{stn}})%>%
    select(-c(STATION_NUMBER, {{year_col}}))

  IHA_pre_expand_rows <- IHA_medians_pre[rep(1, nrow(IHA_pst)),]

  output <- ((IHA_pst - IHA_pre_expand_rows) /IHA_pre_expand_rows ) * 100

  percent_change <- merge(years_post, output, by.x = 0, by.y = 0) %>%
    rename("Year" = "x")
}
