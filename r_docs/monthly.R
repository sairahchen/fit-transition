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

monthly$t <- 0:(nrow(monthly)-1) # assign numbers to months as t

# create percent vars ----

monthly <- monthly %>%
  mutate(totalTestVol = rejected_test + invalid_test + pos_test + neg_test,
         rejectedProp = rejected_test/totalTestVol,
         invalidProp = invalid_test/totalTestVol,
         posProp = pos_test/totalTestVol,
         negProp = neg_test/totalTestVol,
         retestProp = (rejected_test + invalid_test)/totalTestVol,
         phase = case_when(month <= "2019-06-01" ~ "Jun 2017 - Jun 2019",
                           month >= "2019-07-01" & month <= "2020-03-01" ~ "Jul 2019 - Mar 2020",
                           month >= "2020-04-01" & month <= "2021-03-01" ~ "Apr 2020 - Mar 2021",
                           month >= "2021-04-01" ~ "Apr 2021 - Jul 2022"), # invitation letters resumed 2021-04
         phase = factor(phase, levels = c("Jun 2017 - Jun 2019", "Jul 2019 - Mar 2020", "Apr 2020 - Mar 2021", "Apr 2021 - Jul 2022" ) )
         ) 





# mean and sd for test results in table using gtsummary::----

table_1 <- 
  monthly %>% 
  select(totalTestVol, 
         pos_test,
         posProp,
         rejected_test,
         rejectedProp,
         invalid_test,
         invalidProp,
         neg_test,
         negProp,
         phase
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = phase,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    #digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      totalTestVol ~ "Total test volume", 
      pos_test ~ "Positive test volume",
      posProp ~ "Prop pos",
      rejected_test ~ "Rejected test volume",
      rejectedProp ~ "Rej prop",
      invalid_test ~ "Invalid test volume",
      invalidProp ~ "Inv prop",
      neg_test ~ "Negative test volume",
      negProp ~ "Neg prop",
      phase ~ "Phase"),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_1 %>%
  as_flex_table() %>%
  add_header_row(values = c("", "Phase"), colwidths = c(1, 4)) %>%
  add_header_lines(values = "Fecal test results") %>%
  bold(bold = TRUE, i = 2, part = "header") %>%
  flextable::save_as_docx(path="./output/monthly.docx")



# plots ----

# function for scatter plot
plot_monthly_indicators <- function(indicator, indicatorName){
  
  p <- ggplot(monthly, aes(month, indicator)) + 
    geom_point() + 
    #scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    annotate("rect", xmin=as.Date("2017-06-01"), xmax=as.Date("2019-06-01"), ymin=0, ymax=Inf,
             alpha = .1,fill = "purple") +
    annotate("rect", xmin=as.Date("2019-07-01"), xmax=as.Date("2020-03-01"), ymin=0, ymax=Inf,
             alpha = .1,fill = "blue") +
    annotate("rect", xmin=as.Date("2020-04-01"), xmax=as.Date("2021-03-01"), ymin=0, ymax=Inf,
             alpha = .1,fill = "dark gray") +
    annotate("rect", xmin=as.Date("2021-04-01"), xmax=as.Date("2022-07-01"), ymin=0, ymax=Inf,
             alpha = .1,fill = "green") +
    scale_x_date(date_breaks = "2 months", date_labels = "%m-%y") +
    xlab("Month-Year (mm-yy)") +
    ylab(indicatorName)
  
print(p)
}

total_vol_plot <- plot_monthly_indicators(monthly$totalTestVol, "Total test volume")
positivity_plot <- plot_monthly_indicators(monthly$posProp, "Proportion fecal tests with positive result")
retest_plot <- plot_monthly_indicators(monthly$retestProp, "Proportion fecal tests requiring retest")

monthly_grid <- grid.arrange(total_vol_plot,
                             positivity_plot,
                             retest_plot,
                             nrow=1)

ggsave("./output/total_vol_plot.tiff", plot = total_vol_plot, width = 14, height = 6)
ggsave("./output/positivity_plot.tiff", plot = positivity_plot, width = 14, height = 6)
ggsave("./output/retest_plot.tiff", plot = retest_plot, width = 14, height = 6)


# Define segments with dummy vars ----


# Segment version 1: Pre-covid = 0, post-covid = 1

monthly <- monthly %>%
  mutate(segment_v1 = case_when(month <= "2020-03-01" ~ 0,
                             month >= "2021-04-01" ~ 1,
                             TRUE ~ NA_real_), # define months in washout period as missing, so as to exclude from regression
         t_segment2_v1 = case_when(t-33 >= 0 ~ t-33, # define t=0 of segment 2 as 2020-03-01 (33rd month from start of study)
                                t-33 < 0 ~ 0)
  )


    # Segmented regression, segment version 1, 2 segments, washout period, poisson

positivity_model_1 <- glm(pos_test ~ offset(log(totalTestVol)) + # offset with denominator of positive test proportion
                            t + # month since beginning of study
                            segment_v1 + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                            t_segment2_v1*segment_v1, # interaction between month since end of pre-covid segment and dummy var (slope of segment 2)
                          family=poisson, monthly)




# Segment version 2: FOBT = 0, FIT = 1

monthly <- monthly %>%
  mutate(segment_v2 = case_when(month <= "2019-06-01" ~ 0,
                                month >= "2019-07-01" ~ 1,
                                month >= "2020-04-01" & month <= "2021-03-01" ~ NA_real_,
                                TRUE ~ NA_real_), # define months in washout period as missing, so as to exclude from regression
         t_segment2_v2 = case_when(t-25 >= 0 ~ t-25, # define t=0 of segment 2 as 2019-07-01 (25th month from start of study)
                                   t-25 < 0 ~ 0)
  )

    # Segmented regression, segment version 2, 2 segments, washout period, poisson

positivity_model_2 <- glm(pos_test ~ offset(log(totalTestVol)) + # offset with denominator of positive test proportion
                            t + # month since beginning of study
                            segment_v2 + # dummy var for FOBT FIT (intercept change between segments 1 and 2)
                            t_segment2_v2*segment_v2, # interaction between month since beginning of FIT and dummy var (slope of segment 2)
                          family=poisson, monthly)


