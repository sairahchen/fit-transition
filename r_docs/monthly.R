#----

# Script name: monthly.R

# Author: Sairah Lai Fa Chen

# Date created: 2022-12-08

#----

# load monthly indicators
monthly <- read.csv2("./data/monthly.csv", sep = "\t")

# transformations ----


# time transformations 
monthly$month <- paste0("01-", monthly$month)
monthly$month <- as.Date(monthly$month, "%d-%m-%Y") # set month variable to date format, set date to first day of each month

monthly$t <- 1:nrow(monthly) # assign numbers to months as t

# create percent vars ----

monthly <- monthly %>%
  mutate(totalTestVol = rejected_test + invalid_test + pos_test + neg_test,
         rejectedProp = rejected_test/totalTestVol,
         invalidProp = invalid_test/totalTestVol,
         posProp = pos_test/totalTestVol,
         negProp = neg_test/totalTestVol,
         retestProp = (rejected_test + invalid_test)/totalTestVol,
         phase = case_when(month <= "2020-03-01" ~ "Jun 2017 - Mar 2020",
                           month >= "2020-04-01" & month <= "2021-03-01" ~ "Apr 2020 - Mar 2021",
                           month >= "2021-04-01" ~ "Apr 2021 - Jul 2022")) # invitation letters resumed 2021-04





# mean and sd for test results in table using gtsummary::----

table_1 <- 
  monthly %>% 
  select(totalTestVol, 
         pos_test,
         rejected_test,
         invalid_test,
         neg_test,
         phase
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = phase,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      totalTestVol ~ "Total test volume", 
      pos_test ~ "Positive test volume",
      rejected_test ~ "Rejected test volume",
      invalid_test ~ "Invalid test volume",
      neg_test ~ "Negative test volume",
      phase ~ "Phase"),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_1 %>%
  as_flex_table() %>%
  add_header_row(values = c("", "Phase"), colwidths = c(1, 3)) %>%
  add_header_lines(values = "Fecal test results") %>%
  bold(bold = TRUE, i = 2, part = "header") %>%
  flextable::save_as_docx(path="./output/table_test_result.docx")



# plots ----

# function for scatter plot
plot_monthly_indicators <- function(dataframe, indicator, indicatorName){
  
  p <- ggplot(monthly, aes(month, indicator)) + 
    geom_point() + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    annotate("rect", xmin=as.Date("2020-04-01"), xmax=as.Date("2021-03-01"), ymin=0, ymax=Inf,
             alpha = .1,fill = "blue") +
    scale_x_date(date_breaks = "2 months", date_labels = "%m-%y") +
    xlab("Month-Year (mm/yy)") +
    ylab(indicatorName)
  
print(p)
}

total_vol_plot <- plot_monthly_indicators(monthly, monthly$totalTestVol, "Total test volume")
positivity_plot <- plot_monthly_indicators(monthly, monthly$posProp, "Proportion fecal tests with positive result")
retest_plot <- plot_monthly_indicators(monthly, monthly$retestProp, "Proportion fecal tests requiring retest")




# Define segments with dummy vars ----


# Pre-covid = 0, post-covid = 1

monthly <- monthly %>%
  mutate(segment = case_when(month <= "2020-03-01" ~ 0,
                             month >= "2021-04-01" ~ 1,
                             TRUE ~ NA_real_), # define months in washout period as missing, so as to exclude from regression
         t_segment2 = case_when(t-24 >= 0 ~ t-24, # define t=0 of segment 2 as 2020-03-01 (month 24 from start of study)
                                t-24 < 0 ~ 0)
  )


# Segmented regression, 2 segments, washout period, poisson

positivity_model_1 <- glm(pos_test ~ offset(log(totalTestVol)) + # offset with denominator of positive test proportion
                            t + # month since beginning of study
                            segment + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                            t_segment2*segment, # interaction between month since end of pre-covid segment and dummy var (slope of segment 2)
                          family=poisson, monthly)





  





