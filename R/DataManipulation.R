## ----simple, echo=TRUE-------------------------------


#divide dataset into zones
dm.general_splicer <- function(data, name, column) {
  # check (zone) column in question is present
  if (!column %in% colnames(data)) {
    stop("The dataset does not contain a ", column, " column.")
  }
  
  # find all unique values from column
  unique_vals <- unique(data[[column]])

  subset_collection <- list()

  # create separate datasets for each unique value in column
  for (q in unique_vals) {
    # subset the data
    subset_data <- data[data[[column]] == q, ]

    subset_data_name <- paste0(name, q)

    # append new subset to collection
    subset_collection[[subset_data_name]] <- subset_data

  }

  message("Datasets created for each ", column, " value!")
  message("Empty datasets are not generated! (except for NA apparently)")
  
  return(subset_collection)
}

#divide dataset up by data + zone
dm.date_splicer <- function(dataset_names, column){
  subset_collection <- list()
  #iterate through each dataset
  for (name in names(dataset_names)){
    #collect the first part of old dataset name for new name
    new_name <- paste0(substr(name, start = 6, stop = 10), "_")
    #call splicing function to divide dataset and use new naming convention
    spliced_data = dm.general_splicer(dataset_names[[name]], new_name, column)
    for( subset_data_name in names(spliced_data)) {
      # append new subset to collection
      subset_collection[[subset_data_name]] <- spliced_data[[subset_data_name]]
    }
  }
  return(subset_collection)
}

#divide dateset up into time + date + zone
dm.time_splicer <- function(dataset_names, column){
  subset_collection <- list()
  #iterate through each dataset
  for (name in names(dataset_names)){
    #collect the first part of old dataset name for new name
    new_name <- paste0(substr(name, start = 1, stop = 8), "_")
    #call splicing function to divide dataset and use new naming convention
    spliced_data = dm.general_splicer(dataset_names[[name]], new_name, column)
    for( subset_data_name in names(spliced_data)) {
      # append new subset to collection
      subset_collection[[subset_data_name]] <- spliced_data[[subset_data_name]]
    }
  }
  return(subset_collection)
}


#Divide data up into distinct events (zone + data + sample time + quadrat)
dm.event_splicer <- function(dataset_names, column){
  subset_collection <- list()
  #iterate through each dataset
  for (name in names(dataset_names)){
    #collect the first part of old dataset name for new name
    new_name <- paste0(substr(name, start = 1, stop = 10), "_")
    #call splicing function to divide dataset and use new naming convention
    spliced_data = dm.general_splicer(dataset_names[[name]], new_name, column)
    for( subset_data_name in names(spliced_data)) {
      # append new subset to collection
      subset_collection[[subset_data_name]] <- spliced_data[[subset_data_name]]
    }
  }
  return(subset_collection)
}

# remove data with questionable measurements
dm.data_filter <- function(dataset_names, column1, column2, val1, val2){
  subset_collection <- list()
  
  #iterate through each dataset
  for (name in names(dataset_names)){
    #collect the first part of old dataset name for new name
    new_name <- paste0("fltr_", name)
    #filter the data for column1
    data = dataset_names[[name]]
    filtered_data0 <- data[data[[column1]]!= val1,]
    
    #refilter for column 2
    filtered_data <- filtered_data0[filtered_data0[[column2]] != val2,]
    
    subset_collection[[new_name]] <- filtered_data
  }
  return(subset_collection)
}

#find the maximum value of a column and generate an adjacent column with the normalised data
dm.normaliser <- function(data, old_col, new_col, dataset_name){
  #ensure column is numeric
  
  data[[old_col]] = as.numeric(data[[old_col]])
  #ignore empty columns
  if(length(data[[old_col]]) == 0){
    return(NULL)
  }
  #check data entries exist
  has_value <- FALSE %in% is.na(data[[old_col]])
  #find max value in column and exclude NA
  if(has_value == TRUE){
    max <- max(data[[old_col]], na.rm = TRUE)
    #deal with special case where 0 is max value (all entries must therefore be zero)
    if(max == 0){
      normalised <- (data[[old_col]] <- 1)
    }
    else{
      #checks if value in column is na or not
      isna_check <- is.na(data[[old_col]])
      #normalise the data in new column, ignoring NA
      normalised <- ifelse(isna_check,
                           NA,data[[old_col]] / max)
    }
  }
  else{
    normalised <- (data[[old_col]] <- NA)
  }
  
  
  
  #specify where new column is added
  
  #determine original column positon
  original_position <- which(colnames(data) == old_col)
  #insert new column
  data <- cbind(data[, 1:original_position, drop = FALSE], setNames(data.frame(normalised), new_col), data[, (original_position + 1):ncol(data), drop = FALSE])
  
  return(data)
}


# loop the normaliser function for set of datasets
dm.general_normaliser <- function(dataset_names, old_col, new_col){
  subset_collection <- list()
  #iterate through each dataset
  for (name in names(dataset_names)){
    #run normaliser function for the desired column
    if(length(dataset_names[[name]]) > 0){
      normalised_data <- dm.normaliser(dataset_names[[name]], old_col, new_col, name)
      subset_collection[[name]] <- normalised_data
    }
  }
  return(subset_collection)
}

#generate a rank column for data
dm.ranker <- function(data, old_col, new_col, dataset_name,rank_type){
  #ensure column is numeric
  data[[old_col]] = as.numeric(data[[old_col]])
  #find ranking system
  if(!rank_type){
    rank_coef <- 1
  }
  else{
    rank_coef <- -1
  }
  
  #rank the data in new column, ignoring NA
  normalised <- ifelse(!is.na(data[[old_col]]), 
                       rank(rank_coef*data[[old_col]], ties.method = "average"), 
                       NA)
  #specify where new column is added
  
  #determine original column positon
  original_position <- which(colnames(data) == old_col)
  #insert new column
  data <- cbind(data[, 1:original_position, drop = FALSE], setNames(data.frame(normalised), new_col), data[, (original_position + 1):ncol(data), drop = FALSE])

  return(data)
}

#ranker(fltr_zone1_12_n_e2, "rel_len", "len_rank", "fltr_zone1_12_n_e2")
#fltr_zone1_12_n_e2

#Loop this function for all datasets in a list
dm.general_ranker <- function(dataset_names, old_col, new_col, rank_type){
  subset_collection <- list()
  #iterate through each dataset
  for (name in names(dataset_names)){
    #run normaliser function for the desired column
    ranked_data <- dm.ranker(dataset_names[[name]], old_col, new_col, name, rank_type)
    subset_collection[[name]] <- ranked_data
  }
  return(subset_collection)
}

#grouping filtered events into new dataset list based on time of day
dm.event_time_compiler <- function(all_datasets, prefix, suffix, middle){
  all_dataset_names <- names(all_datasets)
  
  name_filter <- paste0(prefix, middle, suffix)
  filtered_datasets <- all_datasets[grep(name_filter, names(all_datasets))]  

  return(filtered_datasets)

}


dm.smoosher <- function(datasets){
  ds_names <- names(datasets)
  columns <- colnames(datasets[[ds_names[1]]])
  big_data <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(big_data) = columns
  for(name in names(datasets)){
    big_data <- rbind(big_data,datasets[[name]])
  }
  return(big_data)
}