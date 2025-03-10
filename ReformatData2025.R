library(dplyr)
library(tidyr)
library(readxl)
library(tools)

process_experiment_data <- function(base_dir, output_dir) {
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Get all date folders
  date_folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  
  if(length(date_folders) == 0) {
    stop("No date folders found in the base directory")
  }
  
  # Function to find subject file in a date folder
  find_subject_file <- function(folder) {
    files <- list.files(folder, pattern = "2_subjects\\.xls$", full.names = TRUE)
    if(length(files) == 0) {
      warning(paste("No subjects file found in folder:", folder))
      return(NULL)
    }
    return(files[1])
  }
  
  # Function to process a single file
  process_single_file <- function(file_path) {
    if(is.null(file_path)) return(NULL)
    
    # Extract date from folder path
    date_folder <- basename(dirname(file_path))
    
    # Read raw data
    raw_data <- read.table(file = file_path, sep = "\t", header = TRUE)
    
    Renamed_Data<- raw_data 
    for (i in 0:15){
      colnames(Renamed_Data)[13+i]= print(paste0("set",i+1,"outsideoption"))
    }
    
    #apply subject number to each set 1-16
    for (i in 1:16){
      Renamed_Data[,print(noquote(paste0("set",i,"subject")))]=Renamed_Data$Subject
    }
    
    #drop unwanted variables
    Renamed_Data <- subset(Renamed_Data, select = -outsideoption)
    session <- Renamed_Data[1,1]
    #Isolate columns to transform
    columns_to_transform <- Renamed_Data %>%
      select(starts_with("set"))
    ##
    
    
    
    ##
    # Use pivot_longer to reshape the data
    reshaped_data <- columns_to_transform%>%
      pivot_longer(cols = everything(), names_to = "col_name") %>%
      separate(col_name, into = c("set", "variable"), sep = "(?<=\\d)(?=\\D)") %>%
      pivot_wider(names_from = variable, values_from = value)
    
    #drop unwanted variables
    reshaped_data <- subset(reshaped_data, select = -c(outsideoption.1))
    
    #unnest
    reshaped_data_exploded <- reshaped_data %>%
      unnest( cols = names(reshaped_data)[2:ncol(reshaped_data)], .drop=TRUE)
    
    #drop more unwanted variables (set0 remnants, an old name for set1 that slipped through but isn't of importance)
    reformatted_data <- reshaped_data_exploded %>%
      filter(set != "set0")
reformatted_data$subject_unique_id <- paste(session, "00", reformatted_data$subject, sep="")
    return(reformatted_data)
    }
  subject_files <- sapply(date_folders, find_subject_file)
  subject_files <- sapply(date_folders, find_subject_file)
  
  # Debug print
  print("Subject files found:")
  print(subject_files)
  
  # Get only the file paths, removing the names
  valid_files <- subject_files[!sapply(subject_files, is.null)]
  valid_files <- unname(valid_files)  # Remove names from vector
  
  print("Processing these files:")
  print(valid_files)
  
  # Process each file individually with error handling
  all_data <- list()
  for(i in seq_along(valid_files)) {
    print(paste("Processing file", i, ":", valid_files[i]))
    tryCatch({
      result <- process_single_file(valid_files[i])
      all_data[[i]] <- result
    }, error = function(e) {
      print(paste("Error processing file:", valid_files[i]))
      print(paste("Error message:", e$message))
    })
  }
  
  # Remove NULL entries
  all_data <- all_data[!sapply(all_data, is.null)]
  
  # Combine all data
  if(length(all_data) > 0) {
    experimental_data <- bind_rows(all_data)
    
    # Export the combined data
    output_file <- file.path(output_dir, "combined_experimental_data.csv")
    write.csv(experimental_data, output_file, row.names = FALSE)
    
    return(experimental_data)
  } else {
    stop("No data was successfully processed")
  }
}


# Usage example:
 experimental_data <- process_experiment_data(
   base_dir = "C:/Users/willi/Desktop/Research/Data and Processing/RawDatai",
   output_dir = "C:/Users/willi/Desktop/Research/Data and Processing/ReformattedDatai"
 )
 ###############################################################################
#add number of searches and outside option indicator
 #number of searches
 experimental_data$Number_Of_Searches<- rowSums(experimental_data[,c("reveal1","reveal2","reveal3","reveal4","reveal5","reveal6","reveal7")])
 #outside option variable, and accrued_search_cost are out of use names, use _accrued search cost and an option of 0 to indicate outside option
 #drop these unwanted variables
 
 experimental_data$option= 0
 for (i in 1:7) {
   new_col_name <-paste("select",i, sep="")
   
   experimental_data$option[experimental_data[, new_col_name] ==1] <- i
 }
 #drop out of use variables
 experimental_data <- experimental_data %>% select(-accrued_search_cost, -outsideoption)
 ##############################################################################
 #assign hurdle rates based on ID
 # Function that assigns numbers based on ID patterns
 assign_number_by_first_digits <- function(ids, pattern_values) {
   # Convert IDs to character
   ids_char <- as.character(ids)
   
   # Initialize result vector with NA
   result <- rep(NA, length(ids))
   
   # For each pattern-value pair, assign the value where pattern matches
   for(pattern in names(pattern_values)) {
     first_digits <- substr(ids_char, 1, nchar(pattern))
     result[first_digits == pattern] <- pattern_values[pattern]
   }
   
   return(result)
 }
 
 experimental_data$hurdle_rate <- "0"
 experimental_data$hurdle_rate <- as.character(experimental_data$hurdle_rate)
   pattern_groups <- c(
     "240326" = "0", "240409" = "0", "250204" = "0",
     "250130" = "95", "250203" = "95", "240325" = "95", "240403" = "95", "240410" = "95",
     "250205" = "125", "250206" = "125", "240327" = "125", "240402" = "125"
   )
 
 # Apply to dataframe
 experimental_data$hurdle_rate <- assign_number_by_first_digits(experimental_data$subject_unique_id, pattern_groups)
 
 ###############################################################################
 #Add lottery information:
 RawData_1_30 <- read.table(file = "C:\\Users\\willi\\Desktop\\Research\\Data and Processing\\RawDatai\\1 30 Low H No Search\\250130_1436_2_globals.xls", 
                                            sep = "\t", header=TRUE)
 #create set-row option-column reformatted dataframe
 #drop first 23 columns(other global parameters)
 Lotteries_mod_1 <- RawData_1_30[, 24:ncol(RawData_1_30)]
 #reshape sets using procedure from above
 # Use pivot_longer to reshape the data
 reshaped_lotteries <- Lotteries_mod_1%>%
   pivot_longer(cols = everything(), names_to = "col_name") %>%
   separate(col_name, into = c("set", "variable"), sep = "(?<=\\d)(?=\\D)") %>%
   pivot_wider(names_from = variable, values_from = value)
 #unnest
 reshaped_lotteries_exploded <- reshaped_lotteries %>%
   unnest( cols = names(reshaped_lotteries)[2:ncol(reshaped_lotteries)], .drop=TRUE)
 #in this new dataframe, set1 is listed 7 times, this is for the seven realizations lotteries can take
 # these realizations being (30,70,90,100,110,130,170)
 #Calculate relevent distribution moments (Mean and Variance)
 #Add value column
 lottery_values <- rep(c(30,70,90,100,110,130,170), length.out = nrow(reshaped_lotteries_exploded))
 reshaped_lotteries_exploded$lottery_values<-lottery_values
 
 group_size <- 7
 
 # Calculate the sum of every 7 rows in the specified column
 result1 <- tapply(reshaped_lotteries_exploded$option1, rep(1:(nrow(reshaped_lotteries_exploded)/group_size), each=group_size, length.out=nrow(reshaped_lotteries_exploded)), sum)
 result2 <- tapply(reshaped_lotteries_exploded$option2, rep(1:(nrow(reshaped_lotteries_exploded)/group_size), each=group_size, length.out=nrow(reshaped_lotteries_exploded)), sum)
 result3 <- tapply(reshaped_lotteries_exploded$option3, rep(1:(nrow(reshaped_lotteries_exploded)/group_size), each=group_size, length.out=nrow(reshaped_lotteries_exploded)), sum)
 result4 <- tapply(reshaped_lotteries_exploded$option4, rep(1:(nrow(reshaped_lotteries_exploded)/group_size), each=group_size, length.out=nrow(reshaped_lotteries_exploded)), sum)
 result5 <- tapply(reshaped_lotteries_exploded$option5, rep(1:(nrow(reshaped_lotteries_exploded)/group_size), each=group_size, length.out=nrow(reshaped_lotteries_exploded)), sum)
 result6 <- tapply(reshaped_lotteries_exploded$option6, rep(1:(nrow(reshaped_lotteries_exploded)/group_size), each=group_size, length.out=nrow(reshaped_lotteries_exploded)), sum)
 result7 <- tapply(reshaped_lotteries_exploded$option7, rep(1:(nrow(reshaped_lotteries_exploded)/group_size), each=group_size, length.out=nrow(reshaped_lotteries_exploded)), sum)
 result=rbind(result1,result2)
 result=rbind(result,result3)
 result=rbind(result,result4)
 result=rbind(result,result5)
 result=rbind(result,result6)
 result=rbind(result,result7)
 result<as.data.frame(result)
 #mean
 # List of column names for which you want to calculate weighted averages
 columns_to_average <- c("option1", "option2", "option3", "option4", "option5", "option6", "option7")
 num_sets <- 16
 
 # Number of options
 num_options <- 7
 
 # Create "set" column
 set <- rep(paste0(1:num_sets), each = num_options)
 
 # Create "option" column
 option <- rep(paste0(1:num_options), times = num_sets)
 
 # Combine into a dataframe
 lottery_stats_mean <- data.frame(set = set, option = option)
 
 # Print the dataframe
 print(lottery)
 #######################################
 # Create an empty dataframe to store the results
 lottery_stats_mean <- data.frame(group = numeric(), mean = numeric(), column_name = character())
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   avg <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(mean = sum(get(col_name) * lottery_values) / sum(get(col_name))) %>%
     mutate(column_name = col_name)
   
   lottery_stats_mean <- bind_rows(lottery_stats_mean, avg)
 }
 
 # Reset the row names
 rownames(lottery_stats_mean) <- NULL
 colnames(lottery_stats_mean)[1] <- "set"
 colnames(lottery_stats_mean)[3] <- "option"
 
 
 #now calculate variances
 lottery_stats_var <- data.frame(group = numeric(), var = numeric(), column_name = character())
 
 
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   avg <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(var = sum(get(col_name) * (lottery_values - sum(get(col_name) * lottery_values) / sum(get(col_name)))^2) / sum(get(col_name))) %>%
     mutate(column_name = col_name)
   
   lottery_stats_var <- bind_rows(lottery_stats_var, avg)
 }
 
 # Reset the row names
 rownames(lottery_stats_var) <- NULL
 colnames(lottery_stats_var)[1] <- "set"
 colnames(lottery_stats_var)[3] <- "option"
 #remerge
 lottery_stats<- merge(lottery_stats_mean, lottery_stats_var, by=c("set","option"))
 #calculate percent of each lottery
 lottery_stats_prob <- data.frame(group = numeric(), prob = numeric(), column_name = character())
 
 
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   avg <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(prob = sum(get(col_name) ) )%>%
     mutate(column_name = col_name)
   
   lottery_stats_prob <- bind_rows(lottery_stats_prob, avg)
 }
 
 # Reset the row names
 rownames(lottery_stats_prob) <- NULL
 colnames(lottery_stats_prob)[1] <- "set"
 colnames(lottery_stats_prob)[3] <- "option"
 #remerge
 lottery_stats<- merge(lottery_stats, lottery_stats_prob, by=c("set","option"))
 
 #now calculate expected profit investor no hurdle
 lottery_stats_payoff_investor_no_h <- data.frame(group = numeric(), payoff_investor_no_h = numeric(), column_name = character())
 
 
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   avg <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(payoff_investor_no_h = sum(get(col_name) * (lottery_values - 30 - .05*lottery_values))) %>%
     mutate(column_name = col_name)
   
   lottery_stats_payoff_investor_no_h <- bind_rows(lottery_stats_payoff_investor_no_h, avg)
 }
 
 # Reset the row names
 rownames(lottery_stats_payoff_investor_no_h) <- NULL
 colnames(lottery_stats_payoff_investor_no_h)[1] <- "set"
 colnames(lottery_stats_payoff_investor_no_h)[3] <- "option"
 #remerge
 lottery_stats<-merge(lottery_stats, lottery_stats_payoff_investor_no_h, by=c("set","option"))
 #now calculate expected profit manager no hurdle
 lottery_stats_payoff_manager_no_h <- data.frame(group = numeric(), payoff_manager_no_h = numeric(), column_name = character())
 
 
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   avg <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(payoff_manager_no_h = sum(get(col_name) * (30+.05*lottery_values))) %>%
     mutate(column_name = col_name)
   
   lottery_stats_payoff_manager_no_h <- bind_rows(lottery_stats_payoff_manager_no_h, avg)
 }
 
 # Reset the row names
 rownames(lottery_stats_payoff_manager_no_h) <- NULL
 colnames(lottery_stats_payoff_manager_no_h)[1] <- "set"
 colnames(lottery_stats_payoff_manager_no_h)[3] <- "option"
 #remerge
 lottery_stats<-merge(lottery_stats, lottery_stats_payoff_manager_no_h, by=c("set","option"))
 #now calculate expected profit investor low hurdle
 lottery_stats_payoff_investor_low_h <- data.frame(group = numeric(), payoff_investor_low_h = numeric(), column_name = character())
 
 
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   avg <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(payoff_investor_low_h = sum(
       case_when(
         lottery_values < 90 ~ get(col_name) * (lottery_values - 15),
         lottery_values >= 90 ~ get(col_name) * (lottery_values - 30 - lottery_values * 0.05)
       )
     )) %>%
     mutate(column_name = col_name)
   
   lottery_stats_payoff_investor_low_h <- bind_rows(lottery_stats_payoff_investor_low_h, avg)
 }
 
 # Reset the row names
 rownames(lottery_stats_payoff_investor_low_h) <- NULL
 colnames(lottery_stats_payoff_investor_low_h)[1] <- "set"
 colnames(lottery_stats_payoff_investor_low_h)[3] <- "option"
 #remerge
 lottery_stats<-merge(lottery_stats, lottery_stats_payoff_investor_low_h, by=c("set","option"))
 #now calculate expected profit manager low hurdle
 lottery_stats_payoff_manager_low_h <- data.frame(group = numeric(), payoff_manager_low_h = numeric(), column_name = character())
 
 
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   avg <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(payoff_manager_low_h = sum(
       case_when(
         lottery_values < 90 ~ get(col_name) * (15),
         lottery_values >= 90 ~ get(col_name) * ( 30 + lottery_values * 0.05)
       )
     )) %>%
     mutate(column_name = col_name)
   
   lottery_stats_payoff_manager_low_h <- bind_rows(lottery_stats_payoff_manager_low_h, avg)
 }
 
 # Reset the row names
 rownames(lottery_stats_payoff_manager_low_h) <- NULL
 colnames(lottery_stats_payoff_manager_low_h)[1] <- "set"
 colnames(lottery_stats_payoff_manager_low_h)[3] <- "option"
 #remerge
 lottery_stats<-merge(lottery_stats, lottery_stats_payoff_manager_low_h, by=c("set","option"))
 #now calculate expected profit investor high hurdle
 lottery_stats_payoff_investor_high_h <- data.frame(group = numeric(), payoff_investor_high_h = numeric(), column_name = character())
 
 
 
 # Loop through each column and calculate payoffs to investors under high h
 for (col_name in columns_to_average) {
   lottery_stats_payoff_investor_high_h_i <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(payoff_investor_high_h = sum(
       case_when(
         lottery_values < 130 ~ get(col_name) * (lottery_values - 15),
         lottery_values >= 130 ~ get(col_name) * (lottery_values - 30 - lottery_values * 0.05)
       )
     ) ) %>%
     mutate(column_name = col_name)
   
   lottery_stats_payoff_investor_high_h <- bind_rows(  lottery_stats_payoff_investor_high_h,lottery_stats_payoff_investor_high_h_i)
 }
 lottery_stats_payoff_investor_high_h
 # Reset the row names
 rownames(lottery_stats_payoff_investor_high_h) <- NULL
 colnames(lottery_stats_payoff_investor_high_h)[1] <- "set"
 colnames(lottery_stats_payoff_investor_high_h)[3] <- "option"
 #remerge
 lottery_stats<-merge(lottery_stats, lottery_stats_payoff_investor_high_h, by=c("set","option"))
 #now calculate expected profit manager high hurdle
 lottery_stats_payoff_manager_high_h <- data.frame(group = numeric(), payoff_manager_high_h = numeric(), column_name = character())
 
 
 # Loop through each column and calculate the weighted average
 for (col_name in columns_to_average) {
   lottery_stats_payoff_manager_high_h_i <- reshaped_lotteries_exploded %>%
     mutate(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     group_by(group) %>%
     summarize(payoff_manager_high_h = sum(
       case_when(
         lottery_values < 130 ~ get(col_name) *( 15),
         lottery_values >= 130 ~ get(col_name) * ( 30 + lottery_values * 0.05)
       )
     ) ) %>%
     mutate(column_name = col_name)
   
   lottery_stats_payoff_manager_high_h <- bind_rows(lottery_stats_payoff_manager_high_h, lottery_stats_payoff_manager_high_h_i)
 }
 
 # Reset the row names
 rownames(lottery_stats_payoff_manager_high_h) <- NULL
 colnames(lottery_stats_payoff_manager_high_h)[1] <- "set"
 colnames(lottery_stats_payoff_manager_high_h)[3] <- "option"
 #remerge
 lottery_stats<-merge(lottery_stats, lottery_stats_payoff_manager_high_h, by=c("set","option"))
 
 #drop "option" text from option row
 lottery_stats$option <- gsub("option","",lottery_stats$option)
 #Indicate maximums and result from random choice
 columns_to_test <- colnames(lottery_stats)[3:ncol(lottery_stats)]
 lottery_stats_i<-lottery_stats
 lottery_stats_ii<-lottery_stats
 for (col_name in columns_to_test) {
   lottery_stats <- lottery_stats %>%
     group_by(group = rep(1:(n() %/% 7 + 1), each = 7, length.out = n())) %>%
     mutate(across(all_of(col_name), list(
       max = ~max(.),
       avg = ~mean(.),
       diff = ~max(.) - mean(.)
     ), .names = "{.col}_{.fn}")) %>%
     ungroup()
 }
 
 summary(lottery_stats)
 
 
 #write.csv(lottery_stats,"C:\\Users\\willi\\Desktop\\Research\\Data and Processing\\ReformattedData\\lottery_stats.csv")
 ################################################################################
 #add lottery information to experimental data
 experimental_data$set <- as.numeric(gsub("set", "", experimental_data$set))
 Prepped_Entrepreneur_Data <- merge(experimental_data,lottery_stats, by = c("set","option"), all.x=TRUE)
 
 