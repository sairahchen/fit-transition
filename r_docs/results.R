#----

# Script name: results.R

# Description: Compiling results across all outcomes

# Author: Sairah Lai Fa Chen

# Date created: 2022.12.14

#----

source("./r_docs/monthly.R")
source("./r_docs/quarterly.R")


# Function to compile each regression output into a table with exponentiated estimates

result_prep <- function(result){
  result_table <- broom::tidy(result, conf.int=TRUE)
  result_table %<>% mutate(exp.estimate = exp(estimate),
                           exp.conf.low = exp(conf.low),
                           exp.conf.high = exp(conf.high))
  return(result_table)
  
}


# List of regression output

result_list <- list(positivity_model_1,
                    overdue_model_1,
                    lostfu_model_1,
                    wait_model_1)

lapply(result_list, result_prep)


