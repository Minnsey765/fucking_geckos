## ----simple, echo=TRUE-------------------------------

#divide dataset into zones
general_splicer <- function(data, name, column) {
  # check (zone) column in question is present
  if (!column %in% colnames(data)) {
    stop("The dataset does not contain a ", column, " column.")
  }
  
  # find all unique values from column
  unique_vals <- unique(data[[column]])
  
  # create separate datasets for each unique value in column
  for (q in unique_vals) {
    # subset the data
    
    subset_data <- data[data[[column]] == q, ]
    
    # assign subsetted data a variable name based on unique value
    assign(paste0(name, q), subset_data, envir = .GlobalEnv)
  }
  
  message("Datasets created for each ", column, " value!")
  message("Empty datasets are not generated! (except for NA apparently)")
}

# DEPRECATED No longer needed as no longer search Global Env
# function to collect datasets with a specific prefix into a list
collect_datasets_to_list <- function(prefix, suffix) {
  # Get all objects in the global environment
  all_objects <- ls(envir = .GlobalEnv)
  
  #ending may not need to be specified
  if (suffix == "none"){
    # Filter objects that start with the given prefix
    dataset_names <- grep(paste0(prefix), all_objects, value = TRUE)
  } else {
    dataset_names <- grep(paste0(prefix, ".*", suffix), all_objects, value = TRUE)
  }
  # Create a named list of these datasets
  dataset_list <- lapply(dataset_names, function(name) get(name, envir = .GlobalEnv))
  names(dataset_list) <- dataset_names
  
  return(dataset_list)
}