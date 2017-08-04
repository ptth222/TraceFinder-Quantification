


## Read xlsx or csv files.
read.xlsx_or_csv <- function(filepath, skip = 0, header = FALSE, col_names = TRUE, stringsAsFactors = FALSE, sheet = 1){
  
  if(grepl(".csv", filepath)){
    return(read.csv(filepath, header = header, sep = ",", skip = skip, stringsAsFactors = stringsAsFactors))
  } else if(grepl(".xlsx|.xlsm", filepath)){
    return(as.data.frame(read_excel(filepath, sheet = sheet, col_names = col_names, skip = skip), stringsAsFactors = stringsAsFactors))
  } else {
    stop("Error in read.xlsx_or_csv. File is not .csv, .xlsx, or .xlsm.")
  }
}

## This function generates the sequence data.frame needed to perform the sequence quantitation and produce the column.
## It takes the sequence_file_path given to the program by the user, and strips it down to what is necessary for quantitation.
gen_sequence_data <- function(sequence_data, tf_sample_id){
  
  ids <- sequence_data[!grepl("BLANK", sequence_data[, "File Name"], ignore.case = TRUE),]
  
  seq_data <- data.frame(id = ids, stringsAsFactors = FALSE)
  seq_data$position <- as.numeric(rownames(seq_data))
  
  std_id <- seq_data$id[grep("stds", seq_data$id, ignore.case = TRUE)]
  sample_data <- seq_data[!seq_data$id %in% std_id,]
  
  for (i in 1:length(sample_data$id))
  {
    if(grepl("ICMS", sample_data$id[i], ignore.case = TRUE)){
      
      sample_data$id[i] <- substr(sample_data$id[i], 1, nchar(sample_data$id[i]) - 6)
      
    } else if(grepl("FTMS", sample_data$id[i], ignore.case = TRUE)){
      
      sample_data$id[i] <- substr(sample_data$id[i], 1, nchar(sample_data$id[i]) - 8)
      
    } else if(grepl("NMR", sample_data$id[i], ignore.case = TRUE)){
      
      sample_data$id[i] <- substr(sample_data$id[i], 1, nchar(sample_data$id[i]) - 5)
      
    }
  }
  
  seq_data$id[as.numeric(rownames(sample_data))] <- sample_data$id
  
  ## the unique Sample IDs from galaxy data.
  samples <- unique(tf_sample_id)
  ## Removes the time stamp from the sample ID
  ## Creates a boolean for whether the samples in the seq data are in the galaxy data
  seq_data$exists <- seq_data$id %in% samples
  ## Returns the sequence data.frame
  return(seq_data)
}