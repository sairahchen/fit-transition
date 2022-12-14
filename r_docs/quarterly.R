#----

# Script name: quarterly.R

# Author: Sairah Lai Fa Chen

# Date created: 2022-12-08

#----


quarterly <- read.csv2("./data/quarterly.csv", sep = "\t")

# Cleaning 
quarterly$quarter <- gsub("Q", "", quarterly$quarter)
quarterly <- rename(quarterly, abnormal_fecal_result = abnormal_fecal._result)
quarterly$overdue <- as.numeric(gsub(" ", "", quarterly$overdue))
quarterly$screening_eligible <- as.numeric(gsub(" ", "", quarterly$screening_eligible))
quarterly$no_cs_6mth <- as.numeric(gsub(" ", "", quarterly$no_cs_6mth))
quarterly$abnormal_fecal_result <- as.numeric(gsub(" ", "", quarterly$abnormal_fecal_result))


# Create new date vars

quarterly <- quarterly %>%
  mutate(lowerQuarterYear = substr(year, 1,4),
         upperQuarterYear = substr(year, 6, 9),
         date = case_when(quarter == 4 ~ paste0("Jan-Mar", "-", upperQuarterYear),
                          quarter == 1 ~ paste0("Apr-Jun", "-", lowerQuarterYear),
                          quarter == 2 ~ paste0("Jul-Sep", "-", lowerQuarterYear),
                          quarter == 3 ~ paste0("Oct-Dec", "-", lowerQuarterYear)
                          ),
         t = row_number()-1,
         phase = case_when(t <= 7 ~ "Jul 2017 - Jun 2019",
                           t >= 8 & t <= 10 ~ "Jul 2019 - Mar 2020",
                           t >= 11 & t <= 14 ~ "Apr 2020 - Mar 2021",
                           t >= 15 ~ "Apr 2021 - Jun 2022"),
         phase = factor(phase, levels = c("Jul 2017 - Jun 2019",
                                          "Jul 2019 - Mar 2020",
                                          "Apr 2020 - Mar 2021",
                                          "Apr 2021 - Jun 2022"
                                          ))
  )

quarterly$date <- fct_reorder(quarterly$date, quarterly$t)

# Create new indicator vars

quarterly <- quarterly %>%
  mutate(overdueProp = overdue/screening_eligible,
         lostfuProp = no_cs_6mth/abnormal_fecal_result)

# function for scatter plot
plot_quarterly_indicators <- function(indicator, indicatorName){
  
  p <- ggplot(quarterly, aes(date,indicator)) + 
    geom_point() + 
  #  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    annotate("rect", xmin="Jul-Sep-2017", xmax="Apr-Jun-2019", ymin=0, ymax=Inf,
             alpha = .1,fill = "purple") +
    annotate("rect", xmin="Jul-Sep-2019", xmax="Jan-Mar-2020", ymin=0, ymax=Inf,
             alpha = .1,fill = "blue") +
    annotate("rect", xmin="Apr-Jun-2020", xmax="Jan-Mar-2021", ymin=0, ymax=Inf,
            alpha = .1,fill = "dark gray") +
    annotate("rect", xmin="Apr-Jun-2021", xmax="Apr-Jun-2022", ymin=0, ymax=Inf,
             alpha = .1,fill = "green") +
    xlab("Quarter intervals according to calendar month and year") +
  ylab(indicatorName)
  
  print(p)
}

overdue_plot <- plot_quarterly_indicators(quarterly$overdueProp, "Proportion overdue for screening")
lostfu_plot <- plot_quarterly_indicators(quarterly$lostfuProp, "Proportion without colonoscopy after 6 months of positive fecal result")
wait_plot <- plot_quarterly_indicators(quarterly$X75p_wait_days, "75th percentile of wait time (days) until colonoscopy after positive fecal result")


ggsave("./output/overdue_plot.tiff", plot = overdue_plot, width = 18, height = 6)
ggsave("./output/lostfu_plot.tiff", plot = lostfu_plot, width = 18, height = 6)
ggsave("./output/wait_plot.tiff", plot = wait_plot, width = 18, height = 6)


# mean and sd for test results in table using gtsummary::----

table_2 <- 
  quarterly %>% 
  select(screening_eligible,
         overdue,
         overdueProp,
         no_cs_6mth,
         lostfuProp,
         X75p_wait_days,
         phase
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = phase,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = list(overdueProp ~ c(3, 3),
                  lostfuProp ~ c(3,3)),                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      screening_eligible ~ "Number of screening eligible",
      overdue ~ "Overdue for screening",
      overdueProp ~ "Proportion overdue for screening",
      no_cs_6mth ~ "Lost to follow-up",
      lostfuProp ~ "Proportion lost to follow-up", 
      X75p_wait_days ~ "Wait time (75th p. days)",
      phase ~ "Phase"),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_2 %>%
  as_flex_table() %>%
  add_header_row(values = c("", "Phase"), colwidths = c(1, 4)) %>%
  add_header_lines(values = "Follow-up indicators") %>%
  bold(bold = TRUE, i = 2, part = "header") %>%
  flextable::save_as_docx(path="./output/quarterly.docx")




# Segmented regression ----


# Define dummy variable according to segments, Pre-covid = 0, post-covid = 1

quarterly <- quarterly %>%
  mutate(segment = case_when(t <= 10 ~ 0,
                             t >= 15 ~ 1,
                             TRUE ~ NA_real_), # define quarter in washout period as missing, so as to exclude from regression
         t_segment2 = case_when(t-10 >= 0 ~ t-10, # define t=0 of segment 2 as Jul 2017 - Mar 2020 (10th quarter from start of study)
                                t-10 < 0 ~ 0)
  )



# Segmented regression, 2 segments, washout period, poisson

overdue_model_1 <- glm(overdue ~ offset(log(screening_eligible)) + # offset with denominator of positive test proportion
                            t + # quarter since beginning of study
                            segment + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                            t_segment2*segment, # interaction between quarter since end of pre-covid segment and dummy var (slope of segment 2)
                          family=poisson, quarterly)


lostfu_model_1 <- glm(no_cs_6mth ~ offset(log(abnormal_fecal_result)) + # offset with denominator of positive test proportion
                         t + # quarter since beginning of study
                         segment + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                         t_segment2*segment, # interaction between quarter since end of pre-covid segment and dummy var (slope of segment 2)
                       family=poisson, quarterly)

wait_model_1 <- glm(X75p_wait_days ~ # offset(log(??)) + # do we need an offset for percentile?
                         t + # quarter since beginning of study
                         segment + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                         t_segment2*segment, # interaction between quarter since end of pre-covid segment and dummy var (slope of segment 2)
                       family=poisson, quarterly)



# Define dummy variable according to segments, FOBT = 0, FIT = 1

quarterly <- quarterly %>%
  mutate(segment_v2 = case_when(t <= 7 ~ 0,
                             t >= 8 ~ 1,
                             t >= 11 & t <= 14 ~ NA_real_, 
                             TRUE ~ NA_real_), # define quarter in washout period as missing, so as to exclude from regression
         t_segment2_v2 = case_when(t-8 >= 0 ~ t-8, # define t=0 of segment 2 as Jul 2019-Sep 2019 (8th quarter from start of study)
                                t-8 < 0 ~ 0)
  )

    # Segmented regression, 2 segments, washout period, poisson

overdue_model_2 <- glm(overdue ~ offset(log(screening_eligible)) + # offset with denominator of positive test proportion
                         t + # quarter since beginning of study
                         segment_v2 + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                         t_segment2_v2*segment_v2, # interaction between quarter since end of pre-covid segment and dummy var (slope of segment 2)
                       family=poisson, quarterly)


lostfu_model_2 <- glm(no_cs_6mth ~ offset(log(abnormal_fecal_result)) + # offset with denominator of positive test proportion
                        t + # quarter since beginning of study
                        segment_v2 + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                        t_segment2_v2*segment_v2, # interaction between quarter since end of pre-covid segment and dummy var (slope of segment 2)
                      family=poisson, quarterly)

wait_model_2 <- glm(X75p_wait_days ~ # offset(log(??)) + # do we need an offset for percentile?
                      t + # quarter since beginning of study
                      segment_v2 + # dummy var for pre and post covid (intercept change between segments 1 and 2)
                      t_segment2_v2*segment_v2, # interaction between quarter since end of pre-covid segment and dummy var (slope of segment 2)
                    family=poisson, quarterly)

