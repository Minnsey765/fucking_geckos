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
  print(length(subset_collection))

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




