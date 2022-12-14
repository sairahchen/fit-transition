#----

# Script name: results.R

# Description: Compiling results across all outcomes

# Author: Sairah Lai Fa Chen

# Date created: 2022.12.14

#----

source("./r_docs/monthly.R")
source("./r_docs/quarterly.R")


# Function to compile each regression output into a table with exponentiated estimates

result_prep <- function(output){
  output_table <- broom::tidy(output, conf.int=TRUE)
  output_table %<>% mutate(exp.estimate = exp(estimate),
                           exp.conf.low = exp(conf.low),
                           exp.conf.high = exp(conf.high))

  return(output_table)
  
}


# List of regression output

output_list <- list(positivity_model_1,
                    overdue_model_1,
                    lostfu_model_1,
                    wait_model_1)

result_tables <- lapply(output_list, result_prep)

# Give model names to result list elements 
        # NB: must be in same order as output_list
names(result_tables) <- c(deparse(substitute(positivity_model_1)),
                          deparse(substitute(overdue_model_1)),
                          deparse(substitute(lostfu_model_1)),
                          deparse(substitute(wait_model_1))
                          )

