
# install.packages("httr2")
# install.packages("dplyr")

library(httr2)
library(dplyr)


#############################################################################################################
### As provided - should you need an api_key, please contact us using https://contact.perinatal.org.uk/ 
#############################################################################################################

api_key = ""

#############################################################################################################
### Ethnicities with coefficients for centile calculation 
#############################################################################################################

ethnicity_reference <- request(
  "https://bcc.perinatal.org.uk/reference/ethnicities"
) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  as.data.frame()

#############################################################################################################
### REQUIRED COLUMNS = ID, mat_height, mat_weight, ethnicity, parity, sex, gest_days,  weight, outcome 
### See User Guide Here: 
### Please comment / delete input data below for your own data in same format
#############################################################################################################

input_data <- data.frame(
  id = c(1,2,3),
  mat_height = c(153,170,200),
  mat_weight = c(60,78,200),
  ethnicity = c("England","Argentina","Spain"),
  parity = c(0,1,2),
  sex = c("M","F","U"),
  gest_days = c(275,262,240),
  weight = c(300,2500,4000),
  outcome = c("LB","FD","LB"))


calculate_centiles <- function(
    data
) {
  
  start_time <- Sys.time()
  
  starts <- seq(1, nrow(data), by = 10000)
  results <- vector("list", length(starts))
  
  for (i in seq_along(starts)) {
    
    first_row <- starts[i]
    last_row <- min(first_row + 10000 - 1, nrow(data))
    
    cat(
      "Processing batch", i, "of", length(starts),
      "- rows", first_row, "to", last_row, "\n"
    )
    
    batch_start <- Sys.time()
    
    batch <- data[first_row:last_row, , drop = FALSE]
    
    response <- request("https://bcc.perinatal.org.uk/centile") |>
      req_headers("x-api-key" = api_key) |>
      req_body_json(batch) |>
      req_timeout(300) |>
      req_perform()
    
    results[[i]] <- response |>
      resp_body_json(simplifyVector = TRUE) |>
      as.data.frame()
    
    batch_seconds <- as.numeric(
      difftime(Sys.time(), batch_start, units = "secs")
    )
    
    cat(
      "Batch completed in",
      floor(batch_seconds / 60), "minutes and",
      round(batch_seconds %% 60, 1), "seconds\n\n"
    )
  }
  
  combined_results <- bind_rows(results)
  
  duration_seconds <- as.numeric(
    difftime(Sys.time(), start_time, units = "secs")
  )
  
  cat(
    "All batches completed in",
    floor(duration_seconds / 60), "minutes and",
    round(duration_seconds %% 60, 1), "seconds\n"
  )
  
  return(combined_results)
}

#############################################################################################################
### OUTPUT
#############################################################################################################

final_data <- calculate_centiles(data = input_data)

### Please submit any feedback to data@perinatal.org.uk 

