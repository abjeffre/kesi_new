##############################################################
############ OUTLIER DETECTION PROCESS #######################

# Load in all functions

# Things to define
# 1.  Which colmuns and in which dataframe the user wants to clean.  (Eg.  d$self, "profits")
# 2.  Define any sub categories which will cut up the profits data (E.g. "main_cat")
# 3.  Define minimum number of entries in the category for the outlier detection alogrithm to work on
# 4.  Define the upper and lower bounds for the outlier detection - the default is 3 standard deviations above normal. 
# 5.  Which colmuns the user wants to be able to modify via the cleaning script (E.g. "profits", "hours", "main")
# 6.  The name of the data-frame although this could be extracted.

# Process
# 1. Load and apply all previous data. (load_newest_data)
# 2. Check perform outlier detection (outlier_detection_by_category)
# 3. Process the outliers by propting the user with desired action (process_outlier_data)

# Arguments, 
# 1. directory - where the data is stored 
# 2. prefix - prefix for the name of the loaded data
# 3. data - the data frame where the values are located
# 4. value (Numeric Value to be checked)
# 5. category - A single category that will group the values by in order to ensure that proper categories are being compared for outlier detection
# 6. columns_to_change - A vector of columns that the user can directly modify via the prompted script. 
# 6. cut = Z-score bound
# 7. low_cut - Manual override for Z-score
# 8. high_cut - Manual override forZ-score
#

# TO DO: 
# use hard indexing rather than soft indexing. 

###################################################################
################# FUNCTIONS #######################################


### LOAD NEWEST FILE 

load_newest_file <- function(directory, prefix) {
  # List files in the directory
  files <- list.files(directory, full.names = TRUE)
  
  # Filter files with the specified prefix
  matching_files <- grep(paste0(prefix, "_"), files, value = TRUE)
  
  if (length(matching_files) == 0) {
    print("No matching files found. - NULL Returned")
    imported_data = NULL
  }
  else{
  # Get the modification time of each file
  file_times <- file.info(matching_files)$mtime
  
  # Find the index of the newest file
  newest_index <- which.max(file_times)
  print(matching_files[newest_index])
  imported_data <- readRDS(matching_files[newest_index])
  # Return the path to the newest file
  }
  return(imported_data)
}

############# Integrate already fixed data ####################

outlier_data_integrator <- function(data, imported_data){
  change_cols <-select(imported_data, starts_with("change_"))
  import_cols <- names(change_cols)
  col_names_import <- c(import_cols, "approved", "justification")
  col_names_data <- unlist(lapply(import_cols, function(x) strsplit(x, "change_")[[1]][2]))
  # Apply changes from the saved data
  cnt <- 1
  for (col in import_cols) {
    ind=which(!is.na(change_cols[, col]))  
    if(length(ind) > 0){
      for (i in ind) {
        values <- strsplit(change_cols[i ,col], "_")
        data[i, col_names_data[cnt]] =  ifelse(is.numeric(data[,col_names_data[cnt]]), as.numeric(values[[1]][2]), values[[1]][2])
      }
    }
    cnt <- cnt +1
  }
  
  # Note that data and imported data are likely to have diffrent lengths
  for(col in col_names_import){
    data[, col] <- NA
    data[1:nrow(imported_data), col] <- imported_data[, col] 
  }
  return(data)
}



# Function to detect outliers in a numeric vector
# Arguments:
#   - x: Numeric vector for which outliers need to be detected.
#   - cut: Number of standard deviations from the mean to consider as a cut-off for outliers. Default is 3.
#   - low_cut: Lower cut-off value. If provided, overrides the cutpoint_low calculation.
#   - high_cut: Upper cut-off value. If provided, overrides the cutpoint_high calculation.
outlier_detect <- function(value, cut = 3, low_cut = NULL, high_cut = NULL, ignore_zero = FALSE) {
  # Calculate standard deviation and mean of the input vector
  sd_x <- sd(value, na.rm = TRUE)
  mean_x <- mean(value, na.rm = TRUE)
  
  # Calculate cut-off points for outliers
  cutpoint_low <- ifelse(is.null(low_cut), mean_x - sd_x * cut, low_cut)
  cutpoint_high <- ifelse(is.null(high_cut), mean_x + sd_x * cut, high_cut)
  
  # Identify indices of values exceeding the cut-off points
  high_indices <- which(value > cutpoint_high)
  if(ignore_zero == FALSE){
    low_indices <- which(value < cutpoint_low)
  }else{
    low_indices <- which(value < cutpoint_low & value > 0)
  }
  
  # Return a list containing indices of high and low outliers
  return(list(high = high_indices, low = low_indices))
}

############ OUTLIER DETECTION BY CATEGORY ################
outlier_detection_by_category <- function(data, category = NULL, value, min_entries = 5, low = NULL, high = NULL, ignore_zero = FALSE ) {
  # Extract and sort the counts of each category
  cat_counts <- if(is.null(category)) {
    nrow(data)
  } else {
    table(data[,category])
  }
  cat_names <- rev(dimnames(cat_counts)[[1]])
  # Get category names where there are more than 'min_entries' entries
  cat_check <- names(cat_counts[cat_counts > min_entries])
  
  # Initialize a list to store outlier detection results for each category
  check_result <- list()
  cnt <- 1
  
  # Loop over categories with more than 'min_entries' entries
  for (cat in cat_check) {
    # Get indices for the current category
    ind <- which(data[,category] == cat)
    # Perform outlier detection on profits for the current category
    temp <- outlier_detect(data[ind, value], low = low, high = high, ignore_zero = ignore_zero)
    temp$high <- ind[temp$high]
    temp$low <- ind[temp$low]
    # Store outlier detection results in the list
    check_result[[cnt]] = temp
    cnt = cnt + 1
  }
  
  if (is.null(category)) {
    # Perform outlier detection for the entire dataset
    ind <- seq_len(nrow(data))
    temp <- outlier_detect(data[ind ,value], low = low, high = high)
    temp$high <- ind[temp$high]
    temp$low <- ind[temp$low]
    check_result[[cnt]] = temp
  }
  
  # Rename the list elements for better identification
  #names(check_result) 
  if(is.null(category)){
    names(check_result) <- "whole data" 
  }else{
    names(check_result) <- c(cat_check)
  }
  #<- ifelse(is.null(category), "whole_data", c(cat_check, "other"))
  #print(c(cat_check))
  return(check_result)
}

################# PROCESS OUTLIERS ########################

process_outlier_data <- function(data, check, columns_to_change, prefix) {
  # This checks to see the column including the modifcations have already been added, 
  # If not then they should be added.
  for (col in columns_to_change) {
    new_col <- paste0("change_", col)
    if(!any(new_col == colnames(data))){
      data[,paste0("change_", col)] <- rep(NA, nrow(data))
    }
  }
  
  # This checks to see if justifcation or approved are in the data and if not it adds them
  for(col in c("approved", "justification")){
    if(!any(col == colnames(data))){
      data[, col] <- rep(NA, nrow(data))
    }
  }
  cnt <- 1
  # Loop over 'check' vector
  for (i in check) {
    # Skip if already approved
    if (!is.na(data$approved[i]) && tolower(data$approved[i]) == "approved") {
      next
    }
    info = data[i, c("names", columns_to_change, "_index", "hhm_ID" )]
    print(data[which(data$hhm_ID==info$hhm_ID),])
    print(info)
    print(paste("Working on", cnt, "of", length(check)))
    # Prompts for each specified column
    for (col in columns_to_change) {
      cat(paste("Change", col, "? (Enter new", col, "or press Enter to skip): "), sep = " ")
      new_val <- readline(prompt = "")
      if (nzchar(new_val)) {
        old_val <- data[[col]][i]
        data[[col]][i] <- new_val
        data[[paste0("change_", col)]][i] <- paste(old_val, new_val, sep = "_")
      }
    }
    
    # Prompt for justification
    cat("Provide a note justifying your decision: ")
    justification <- readline(prompt = "")
    data$justification[i] <- justification
    
    # Mark as approved
    cat("approved?: ")
    approve <- readline(prompt = "")
    data$approved[i] <- ifelse(approve == "y", "approved", "no")
    
    saveRDS(data, file = paste0("data/outlier_detection/", prefix, "_", gsub(":","-", as_datetime(Sys.time(), tz = "CET")),".RDS"))
    
    cnt = cnt+1
  }
  
  # Create a dataframe for changes and justifications
  changes_df <- data.frame(
    setNames(lapply(columns_to_change, function(col) data[[paste0("change_", col)]]), paste0("change_", columns_to_change)),
    approved = data$approved,
    justification = data$justification
  )
  
  # Return the modified dataframe and changes dataframe
  return(list(data = data, changes_df = changes_df))
}



###### Combined function ########

out_detect <- function(directory,
                       prefix,
                       data,
                       value,
                       category,
                       columns_to_change,
                       min_entries,
                       low_cut,
                       high_cut,
                       ignore_zero,
                       filter = NULL
                       ){
  
  imported_data <- load_newest_file(directory, prefix)
  if(!is.null(imported_data)) data<-outlier_data_integrator(data, imported_data)
  check<-outlier_detection_by_category(data, category, value, min_entries, low, high, ignore_zero)
  check<-unlist(check)
  if(!is.null(filter)) check <- check[which(check %in% filter)]
  process_outlier_data(data, check, columns_to_change, prefix)  
  
}

##################################################################
######## Define Parameters for Different Lists ###################
out_detect_param <- list()

# Self 
out_detect_param[["self"]]<-list(
  prefix = "self",
  directory = "data/outlier_detection",
  data = d$self,
  category = "main",
  value = "profits",
  min_entries = 5,
  low = 1000,
  high = NULL,
  #filter = which(d$self$`_submission_submitted_by`== "bakarimakame"),
  columns_to_change = c("profits", "hours", "main_cat"),
  ignore_zeros = TRUE
)


# Wage
out_detect_param[["wage"]]<-list(
  prefix = "wage",
  directory = "data/outlier_detection",
  data = d$wage,
  category = "main",
  value = "income",
  min_entries = 5,
  low = 1000,
  high = NULL,
  columns_to_change = c("income", "hours", "payperiod", "main_cat")
)

###########################################
############# SIMPLE USAGE ################

out_detect( prefix = "self",
            directory = "data/outlier_detection",
            data = d$self,
            category = "main",
            value = "profits",
            min_entries = 5,
            low = 1000,
            high = NULL,
            #filter = which(d$self$`_submission_submitted_by`== "bakarimakame"),
            columns_to_change = c("profits", "hours", "main_cat"),
            ignore_zero = TRUE)