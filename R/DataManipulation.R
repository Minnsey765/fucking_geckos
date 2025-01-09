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

#input dataset list and ranking system and normalise + rank all appropriate columns
dm.norm_ranker <- function(datasets, rank_sys){
  #normalise first
  #normalise for length_cm and add to large dataset
  norm_list <- dm.general_normaliser(datasets, "length_cm", "rel_len")
  
  #repeat for svl_cm
  norm_list <- dm.general_normaliser(norm_list, "svl_cm", "rel_svl")
  
  #repeat for width_cm
  norm_list <- dm.general_normaliser(norm_list, "width_cm", "rel_wid")
  
  #repeat for len_vol_cm3
  norm_list <- dm.general_normaliser(norm_list, "len_vol_cm3", "rel_len_vol")
  
  #repeat for svl_vol_cm3
  norm_list <- dm.general_normaliser(norm_list, "svl_vol_cm3", "rel_svl_vol")
  
  #repeat for disp_light_cm
  norm_list <- dm.general_normaliser(norm_list, "disp_light_cm", "rel_disp")
  
  #repeat for dist_light_cm
  norm_list <- dm.general_normaliser(norm_list, "dist_light_cm", "rel_dist")
  
  #rank second
  #rank rel_len and add to large dataset
  norm_list <- dm.general_ranker(norm_list, "rel_len", "len_rank", rank_sys["rel_len"])
  
  #repeat for rel_svl
  norm_list <- dm.general_ranker(norm_list, "rel_svl", "svl_rank", rank_sys["rel_svl"])
  
  #repeat for rel_wid
  norm_list <- dm.general_ranker(norm_list, "rel_wid", "wid_rank", rank_sys["rel_wid"])
  
  #repeat for rel_len_vol
  norm_list <- dm.general_ranker(norm_list, "rel_len_vol", "len_vol_rank", rank_sys["rel_len_vol"])
  
  #repeat for rel_svl_vol
  norm_list <- dm.general_ranker(norm_list, "rel_svl_vol", "svl_vol_rank", rank_sys["rel_svl_vol"])
  
  #repeat for rel_disp
  norm_list <- dm.general_ranker(norm_list, "disp_light_cm", "disp_rank", rank_sys["disp_light_cm"])
  
  #repeat for rel_dist
  norm_list <- dm.general_ranker(norm_list, "dist_light_cm", "dist_rank", rank_sys["dist_light_cm"])
  
  return(norm_list)
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

#add column next to quadrat with quadrat type as this makes analysis easier later on
dm.quad_type <- function(data, old_col, name){
  #generate new col of quadrat type (e or c)
  new_col <- substr(data[[old_col]], 1,1)
  
  #change contents of new_col to 'expr' or 'ctrl'
  for(i in 1:length(new_col)){
    if(new_col[i] == "e"){
      new_col[i] <- "exp"
    }
    else{
      new_col[i] <- "ctrl"
    }
  }
  #specify where new column is added
  
  #determine original column positon
  original_position <- which(colnames(data) == old_col)
  #insert new column
  data <- cbind(data[, 1:original_position, drop = FALSE], setNames(data.frame(new_col), name), data[, (original_position + 1):ncol(data), drop = FALSE])
  return(data)
}


#remove outliars from big dataset
dm.outliar <- function(data, column){
  #summary object contains all relevant information
  summary <- summary(as.numeric(data[[column]]))
  #use 1.5*IQR as outliar determination
  IQR <- summary[[5]] - summary[[2]]
  u_limit <- summary[[5]] + 1.5*IQR
  l_limit <- summary[[2]] - 1.5*IQR
  #filter the data greater than 1.5*IQR above 3rd quartile into a vector
  filtered <- replace(as.numeric(data[[column]]), as.numeric(data[[column]]) > u_limit, NA)
  #filter the data less than 1.5*IQR below 1st quartile
  filtered <- replace(filtered, filtered < l_limit, NA)
  
  #replace new vector where old column was
  data[[column]] <- filtered
  return(data)
}

#replace any values that require length_cm or svl_cm with NA if they have been removed
dm.replacer <- function(data, column){
  #initalise target column
  target_col = ""
  #specify target column based on input column
  if(column == "length_cm"){
    target_col <- "len_vol_cm3"
    #locate index of values with NA
    index <- which(is.na(data[[column]]))
    #replace values in target column with this index with NA
    for(i in index){
      data[[target_col]][i] <- NA
    }
  }
  else if(column == "svl_cm"){
    target_col <- "svl_vol_cm3"
    #locate index of values with NA
    index <- which(is.na(data[[column]]))
    #replace values in target column with this index with NA
    for(i in index){
      data[[target_col]][i] <- NA
    }
  }
  
  return(data)
}

#mass remove outliars combining above functions
dm.outliar.rm <- function(data, columns){
  #initialise filtered data as data
  filtered_data <- data
  #loop through and remove outliars from each column and overwrite dataset
  for(column in columns){
    #remove outliars from relevant main columns
    filtered_data <- dm.outliar(filtered_data, column)
    #remove outliars from columns dependent on main columns
    filtered_data <- dm.replacer(filtered_data, column)
  }
  return(filtered_data)
}

#function or raw_data2 that converts all numeric values to numeric data and nas to NAs
dm.cleaner <- function(data,columns){
  for(column in columns){
    data[[column]] <- as.numeric(data[[column]])
  }
  return(data)
}

#function to take and add a column with logorithms of a column
dm.log_col <- function(data, column, name){
  #make vector of logorithmed data
  options(digits=10)
  new_col <- as.numeric(data[[column]])
  new_col <- log(new_col)
  #change -Inf values to -1
  for(i in 1:length(new_col)){
    #NA conditon must come first for some reason
    if(is.na(new_col[i])){
      new_col[i] <- NA
    }
    else if(new_col[i]=="-Inf"){
      new_col[i] <- -1
    }
  }
  #determine original column positon
  original_position <- which(colnames(data) == column)
  #insert new column
  data <- cbind(data[, 1:original_position, drop = FALSE], setNames(data.frame(new_col), name), data[, (original_position + 1):ncol(data), drop = FALSE])
  return(data)
}