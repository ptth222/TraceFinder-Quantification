## These are all of the functions that check to make sure the input data is formatted correctly and has other consistencies.

#################
## Standards Mix
#################

standards_read_check <- function(metadata_file_path){

  try_result <- try(CompoundNamesAndFormulasSorted <- read.xlsx_or_csv(metadata_file_path, header = TRUE, stringsAsFactors = FALSE, skip = 20, sheet = "ICMS_StdComp_RefDB_Submission"))
  
  ## Check to see if there were any errors when reading in.
  if(class(try_result) == "try-error"){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Standards Mixture Read Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("Error when reading in the \"ICMS_StdComp_RefDB_Submission\" sheet in the meta data file:\n\n", try_result[1]),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }

  return(CompoundNamesAndFormulasSorted)
}


standards_empty_check <- function(CompoundNamesAndFormulasSorted){
  
  ## Check to see if there is data in the file.
  if(nrow(CompoundNamesAndFormulasSorted) == 0){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Standards Mixture Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "The \"ICMS_StdComp_RefDB_Submission\" sheet in the meta data file is empty. \nPlease supply a non-empty sheet.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
    
}


standards_column_check <- function(CompoundNamesAndFormulasSorted){
  ## Make sure it has the correct columns. If it doesn't have all of the columns then give the user a message box and quit the program.
  if(!all(c("CompoundName", "Concentration_uM") %in% colnames(CompoundNamesAndFormulasSorted))){
    
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Standards Mixture Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "The \"ICMS_StdComp_RefDB_Submission\" sheet in the meta data file did not have the correct column names. \nThere should be column names for \"CompoundName\" and \"Concentration_uM\". \nThese names are case sensitive.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
  CompoundNamesAndFormulasSorted <- CompoundNamesAndFormulasSorted[!is.na(CompoundNamesAndFormulasSorted$CompoundName),]
  CompoundNamesAndFormulasSorted <- CompoundNamesAndFormulasSorted[,c("CompoundName", "Concentration_uM")]
  
  return(CompoundNamesAndFormulasSorted)
}


## Check to make sure that every CompoundName has data in every column. This function assumes that there are no NA values for the 
## CompoundName column since they are removed with an earlier function. It also assumes that the data only has the columns we need
## since an earlier function also strips off any extra columns.
standards_values_check <- function(CompoundNamesAndFormulasSorted){
  if(any(is.na(CompoundNamesAndFormulasSorted))){
    
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Standards Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "The \"ICMS_StdComp_RefDB_Submission\" sheet in the meta data file does not have data for every CompoundName. \nMake sure every column has values for every CompoundName.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
}


##################
## Meta Data
##################

metadata_read_check <- function(metadata_file_path){

  try_result <- try(meta_data <- read.xlsx_or_csv(metadata_file_path, col_names = TRUE, stringsAsFactors = FALSE, sheet = "ICMS_MetaData_Submission", skip = 20))
  
  ## Check to see if there were any errors when reading in.
  if(class(try_result) == "try-error"){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Meta Data Read Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("Error when reading in the \"ICMS_MetaData_Submission\" sheet in the meta data file:\n\n", try_result[1]),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
  return(meta_data)

}


metadata_empty_check <- function(meta_data){
  
  ## Check to see if there is data in the file.
  if(nrow(meta_data) == 0){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Meta-Data Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "The \"ICMS_MetaData_Submission\" sheet in the meta data file is empty. \nPlease supply a non-empty sheet.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
}


metadata_column_check <- function(meta_data){

  ## Make sure meta data has the correct columns. If it doesn't have all of the columns then give the user a message box and quit the program.
  if(!all(c("SampleID", "Protein_mg", "ICMS_split_ratio", "ReconstitutionVolume_uL", "InjectionVolume_uL") %in% colnames(meta_data))){
    
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Meta-Data Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "The \"ICMS_MetaData_Submission\" sheet in the meta data file did not have the correct column names. \nThere should be column names for \"SampleID\", \"Protein_mg\", \"ICMS_split_ratio\", \"ReconstitutionVolume_uL\", and \"InjectionVolume_uL\". \nThese names are case sensitive.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
  meta_data <- meta_data[!is.na(meta_data$SampleID),]
  
  ## Keep only the columns we need.
  meta_data <- meta_data[,c("SampleID", "Protein_mg", "ICMS_split_ratio", "ReconstitutionVolume_uL", "InjectionVolume_uL")]
  
  meta_data$SampleID <- gsub(" ", "", meta_data$SampleID)
  
  return(meta_data)
}

## Check to make sure that every SampleID has data in every column. This function assumes that there are no NA values for the 
## SampleID column since they are removed with an earlier function. It also assumes that the data only has the columns we need
## since an earlier function also strips off any extra columns.
metadata_values_check <- function(meta_data){
  if(any(is.na(meta_data))){
    
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Meta-Data Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "The \"ICMS_MetaData_Submission\" sheet in the meta data file does not have data for every SampleID. \nMake sure every column has values for every SampleID.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
}


#########################
## Sequence Data
#########################


sequence_read_check <- function(metadata_file_path){

  try_result <- try(sequence_data <- read.xlsx_or_csv(metadata_file_path, col_names = TRUE, stringsAsFactors = FALSE, sheet = "ICMS_SequenceData_Submission", skip = 20))
  
  ## Check to see if there were any errors when reading in.
  if(class(try_result) == "try-error"){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Sequence Data Read Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("Error when reading in the \"ICMS_SequenceData_Submission\" sheet in the meta data file:\n\n", try_result[1]),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
  colnames(sequence_data) <- gsub(" ", "", colnames(sequence_data)) 

  return(sequence_data)
}


sequence_empty_check <- function(sequence_data){

## Check to see if there is data in the file.
if(nrow(sequence_data) == 0){
  tt <- tktoplevel()
  message_font <- tkfont.create(family = "Times New Roman", size = 14)
  tkwm.title(tt, "Sequence Data Error")
  tkgrid(ttklabel(tt, image = error_icon),
         ttklabel(tt, text = "The \"ICMS_SequenceData_Submission\" sheet in the meta data file is empty. \nPlease supply a non-empty sheet.",
                  font = message_font), padx = 20, pady = 20)
  close_box <- function(){
    tkdestroy(tt)
  }
  tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
  tkwait.window(tt)
  stop()
}
  
}


sequence_column_check <- function(sequence_data){

## Make sure sequence data has the correct columns. If it doesn't have all of the columns then give the user a message box and quit the program.
if(!("FileName" %in% colnames(sequence_data))){
    
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Sequence Data Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "The \"ICMS_SequenceData_Submission\" sheet in the meta data file did not have the correct column names. \nThere should be column names for \"File Name\". \nThese names are case sensitive.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
  sequence_list <- sequence_data$FileName[!is.na(sequence_data$FileName)]
  
  sequence_data <- data.frame(x = sequence_list, stringsAsFactors = FALSE)
  colnames(sequence_data) <- "File Name"
  
  return(sequence_data)
}


##########################
## TraceFinder Reports for Every Sample in MetaData
##########################

metadata_in_TF_list <- function(meta_data, SampleNames){

  ## Get the sample names that are in the meta data sheet, but there are not TraceFinder reports for.
  meta_diff <- setdiff(meta_data$SampleID, SampleNames)
  
  ## Check to make sure that every meta data sample ID has a matching tracefinder report.
  if(length(meta_diff) != 0 ){
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Meta Data Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("There is not a matching TraceFinder report for every sample in the \"ICMS_MetaData_Submission\" sheet in the meta data file.\nReports for:\n\n", paste(meta_diff, collapse = "\n"), "\n\ncould not be found.", sep =""),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }

}


##########################
## Every Sample in MetaData has a TF Report
##########################

TF_list_in_metadata <- function(meta_data, SampleNamesNoStds){
  ## Get the sample names that there are TF reports for but are not in the meta data sheet.
  TF_diff <- setdiff(SampleNamesNoStds, meta_data$SampleID)
  
  ## Check that every TraceFinder report has an entry for meta data.
  if(length(TF_diff) != 0 ){
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Meta Data Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("There is not a matching entry in the \"ICMS_MetaData_Submission\" sheet in the meta data file for every TraceFinder report.\nEntries for:\n\n", paste(TF_diff, collapse = "\n"), "\n\ncould not be found in the meta data.", sep = ""),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
}


##########################
## Every Sample in Sequence Data has a TF Report
##########################

TF_list_in_sequence_data <- function(sequence_data, SampleNames){
  
  ## Find the sample names that there are TraceFinder reports for but aren't in the sequence data.
  sequence_diff <- setdiff(SampleNames, sequence_data$id)
  
  if(length(sequence_diff) != 0){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Sequence Data Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("There is not a matching entry in the \"ICMS_SequenceData_Submission\" sheet in the meta data file for every TraceFinder report.\nEntries for:\n\n", paste(sequence_diff, collapse = "\n"), "\n\ncould not be found in the sequence data.", sep = ""),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
}


##########################
## Every Standard Compound is in the TF Reports
##########################

standards_in_TF_reports <- function(CompoundNamesAndFormulasSorted, TF_compounds){
  
  ## Check that every compound in the standards mix is in the TraceFinder reports.
  standard_diff <- setdiff(CompoundNamesAndFormulasSorted$CompoundName, unique(TF_compounds))
  
  if(length(standard_diff) != 0){
    abort_flag <- FALSE
    
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Standard Mix Warning")
    tkgrid(ttklabel(tt, image = warning_icon),
           ttklabel(tt, text = paste("The following standard compounds in the \"ICMS_StdComp_RefDB_Submission\" \nsheet of the meta data file were not found in the TraceFinder reports:\n\n", paste(standard_diff, collapse = "\n"), "\n\nContinue quantification?", sep = ""),
                    font = message_font), padx = 20, pady = 20)
    continue <- function(){
      tkdestroy(tt)
    }
    abort <- function(){
      assign("abort_flag", TRUE, envir = .GlobalEnv)
      tkdestroy(tt)
    }
    tt1 <- tkframe(tt)
    tkgrid(tkbutton(tt1, text='Continue', command = continue, background = "SlateGray1"), tkbutton(tt1, text='Abort', command = abort, background = "SlateGray1"), padx = 20, pady = 15)
    tkgrid(tt1, columnspan = 2)
    tkwait.window(tt)
    
    if(abort_flag){
      stop()
    }
  }

}


###########################
## TraceFinder reports checks
###########################

report_read_check <- function(TF_File){
  
  try_result <- try(TempMatrix <- read.xlsx2(TF_File, 1, startRow=45))
  
  ## Check to see if there were any errors when reading in.
  if(class(try_result) == "try-error"){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "TraceFinder Report Read Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("Error when reading in the TraceFinder report ", TF_File, "  .\n\n", try_result[1]),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
  return(TempMatrix)
}


report_empty_check <- function(TempMatrix, TF_File){
  
  ## Check to see if there is data in the file.
  if(nrow(TempMatrix) == 0){
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "TraceFinder Report Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("The TraceFinder report ", TF_File, " is empty. \nPlease supply a non-empty report."),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
}


report_column_check <- function(TempMatrix, TF_File){
  ## Make sure it has the correct columns. If it doesn't have all of the columns then give the user a message box and quit the program.
  if(!all(c("Target.Compounds", "Formula", "Peak.Area") %in% colnames(TempMatrix))){
    
    ## Create a message box.
    tt <- tktoplevel()
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "TraceFinder Report Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = paste("The TraceFinder report ", TF_File, " did not have the correct column names, or the column names are not on row 45. \nThere should be column names for \"Target Compounds\", \"Formula\" and \"Peak Area\". \nThese names are case sensitive."),
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    stop()
  }
  
  TempMatrix <- TempMatrix[!is.na(TempMatrix$Target.Compound),]
  
  return(TempMatrix)
}


##########################
## TraceFinder Reports All Have the Same Type of Labeling
##########################


TF_labeling_check <- function(TF_labeling_type){

  ## Make sure all TF files have the same labeling and set labeling to that type.
  if(any(TF_labeling_type$Labeling != TF_labeling_type$Labeling[1])){
    
    tt <- tktoplevel()
    tkfocus(tt)
    message_font <- tkfont.create(family = "Times New Roman", size = 14)
    tkwm.title(tt, "Labeling Error")
    tkgrid(ttklabel(tt, image = error_icon),
           ttklabel(tt, text = "Not all TraceFinder files have the same type of labeling.\nPlease submit TraceFinder files with the same type of labeling.",
                    font = message_font), padx = 20, pady = 20)
    close_box <- function(){
      tkdestroy(tt)
    }
    tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
    tkwait.window(tt)
    
    stop()
  }
    
}