########### This script extracts XIC peakAreas from TraceFinder 33 sample reports
########### Author Marc . O. Warmoes
########### Date: Jan 2017
############################################################################################################################
############################################################################################################################
library(tcltk)
library(tcltk2)
library(xlsx)
library(matrixStats)
library(httr)
library(gridBase)
library(grid)
library(assertthat)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)

source("/PC_Xfer/Quantification Script/Helper Functions.R")
source("/PC_Xfer/Quantification Script/User Interface(1).R")
source("/PC_Xfer/Quantification Script/Compile Data For Galaxy.R")
source("/PC_Xfer/Quantification Script/Error Checking.R")
source("/PC_Xfer/Quantification Script/Talk to Galaxy.R")
source("/PC_Xfer/Quantification Script/Functions After Galaxy.R")


## Tell httr not to worry about certificates.
httr::set_config( config( ssl_verifypeer = 0L ) )


###########################
## Create Opening GUI for the User
###########################

Labelling <- user_interface()



################################################################################################################
################################################################################################################
## Read in the files the user just input and make sure they have the correct information before proceeding.
################################################################################################################
################################################################################################################


## Standards Checks
CompoundNamesAndFormulasSorted88 <- standards_read_check(metadata_file_path)
standards_empty_check(CompoundNamesAndFormulasSorted88)
CompoundNamesAndFormulasSorted <- standards_column_check(CompoundNamesAndFormulasSorted88)


## MetaData Checks
meta_data <- metadata_read_check(metadata_file_path)
metadata_empty_check(meta_data)
meta_data <- metadata_column_check(meta_data)


## Sequence Data Checks
sequence_data <- sequence_read_check(metadata_file_path)
sequence_empty_check(sequence_data)
sequence_data <- sequence_column_check(sequence_data)


TF_FileList <- TF_reports_file_paths


## Get folder above the folder with the TraceFinder reports.
PathWithTFfiles=strsplit(TF_FileList[1], "/")[[1]][1:(length(strsplit(TF_FileList[1], "/")[[1]])-2)]
PathWithTFfiles=paste(PathWithTFfiles,collapse = "/")


## Set working directory to this folder.
setwd(PathWithTFfiles)


## Create a list that is the same size as the list of reports to hold sample names.
SampleNames=TF_FileList 


## Truncate the beginning of the report names to get sample names.
for (i in 1:length(TF_FileList))
       {
  SampleNames[i]=strsplit(TF_FileList[i],"/")[[1]][length(strsplit(TF_FileList[i],"/")[[1]])]
  
  if(grepl("stds|refsample", SampleNames[i], ignore.case = TRUE)){
    
    SampleNames[i] <- SampleNames[i] %>% gsub(".*Finder_","", .) %>% substr(.,1,nchar(.) - 20)
    
  } else{
    
    SampleNames[i] <- SampleNames[i] %>% gsub(".*Finder_","", .) %>% substr(.,1,nchar(.) - 26)
    
  }
}

## Get a list of sample names without the standards in the list.
SampleNamesNoStds <- SampleNames[!grepl("stds", SampleNames, ignore.case = TRUE)]




######################################
## Check to make sure the meta data samples, TraceFinder reports, and sequence data samples match.
######################################

## Check that all samples in the meta data file have a matching TF report.
metadata_in_TF_list(meta_data, SampleNames)

## Check that all TF reports have an entry in the meta data file.
TF_list_in_metadata(meta_data, SampleNamesNoStds)

## Generate sequence data to check it against the the sample names.
sequence_data <- gen_sequence_data(sequence_data, SampleNames)

## Check that every TF report has an entry in the sequence data.
TF_list_in_sequence_data(sequence_data, SampleNames)

## Read in the first report's data.
TempMatrix=read.xlsx2(TF_FileList[1], 1,startRow=45)

## Remove the labeling off the end of the compound names in the TraceFinder reports so that
## they can be compared with the names in the standard reference database.
TF_compounds <- gsub("\\[(.*)\\]", "", TempMatrix$Target.Compound)

## Check that every standard compound in the database has a match in the TF report.
standards_in_TF_reports(CompoundNamesAndFormulasSorted, TF_compounds)



##################################
## All checks passed, so start calculating what we need.
##################################

## Read in TF reports and put the peak areas into a matrix.
temp_return <- read_TF_reports(TF_FileList, TempMatrix)
PeakAreas <- temp_return$PeakAreas
TF_labeling_type <- temp_return$TF_labeling_type

## Check that all the TraceFinder reports have the same labeling.
TF_labeling_check(TF_labeling_type)

## If the program made it past the error check then all the TF files have the same labeling, so set the Labelling to it.
Labelling <- TF_labeling_type$Labeling[1]


## Give the user a message so they know the program is working.
tt <- tktoplevel()
tkfocus(tt)
message_font <- tkfont.create(family = "Times New Roman", size = 14)
tkwm.title(tt, "Program Progress")
tkgrid(ttklabel(tt, text = "Building file to submit to Galaxy.", font = message_font), padx = 20, pady = 20)



## Add row and column names to peak areas matrix.
rownames(PeakAreas)<-TempMatrix$Target.Compounds[1:dim(PeakAreas)[1]]
colnames(PeakAreas)<-SampleNames
class(PeakAreas) <- "numeric"

## Sort peak areas matrix by names.
Indexing=sort(rownames(PeakAreas),index.return = TRUE)
PeakAreas<-PeakAreas[Indexing$ix,]
TempMatrix <- TempMatrix[Indexing$ix,]

## Put chemical formulas for compounds into a form that Galaxy likes.
CompoundNamesAndFormulasForStripping <- correct_chemical_formulas(PeakAreas, TempMatrix)



ForStripping <- build_final_matrix(Labelling, CompoundNamesAndFormulasForStripping, SampleNames, PeakAreas)


## Destroy the box with the message about building the file for Galaxy.
tkdestroy(tt)



## TODO Can make each http request a function instead of the copy pasted stuff below.
## Use eval(parse(text = variable_with_R_code)) to execute R code stored in a variable.
## Using that you can give the httr code directly as a parameter and the error message as another.


###########################
## Loop, asking for a username and password until the user quits or the request to Galaxy is successful.
###########################
repeat{
  
  ## Create pop up box to get username and password from user.
  temp_return <- get_galaxy_login()
  
  username <- temp_return$username
  password <- temp_return$password
  
  ## Send HTTP request to log in to Galaxy.
  r <- galaxy_login(username, password)
  
}


#############################
## Request history from Galaxy. If this request is not made then Galaxy won't accept new files.
############################

r <- galaxy_history(username, password)


#############################
## Upload the file created previously to Galaxy.
#############################

## Create a message box to let the user know what is going on.
tt <- tktoplevel()
tkfocus(tt)
message_font <- tkfont.create(family = "Times New Roman", size = 14)
tkwm.title(tt, "Galaxy Progress")
tkgrid(ttklabel(tt, text = "Uploading file to Galaxy.", font = message_font), padx = 20, pady = 20)

setwd(PathWithTFfiles)

## The file to be submitted to Galaxy must be written to a file first. 
## I don't know of another way to make the matrix tab separated like it needs to be without writing to a file.
write.table(ForStripping, file="Send_to_Galaxy_for_NA_correction.txt",row.names = FALSE, sep="\t",quote=FALSE)

filename <- "Send_to_Galaxy_for_NA_correction.txt"

## Create the body of the POST request to upload the file.
body <- list(history_id = "null", tool_id = "upload1", inputs = '{"dbkey":"?","file_type":"tabular","files_0|type":"upload_dataset","files_0|space_to_tab":null,"files_0|to_posix_lines":"Yes"}', "files_0|file_data" = upload_file("Send_to_Galaxy_for_NA_correction.txt"))

r <- galaxy_upload(username, password, body)


####################################
## Once the file is uploaded we have to keep asking Galaxy if it recieved it, and when it is ready to
## be used in tools.
####################################

## Get the history_id, id, and state of the file upload from the response to the POST.
content <- content(r)
history_id <- content$outputs[[1]]$history_id
id <- content$outputs[[1]]$id
state <- content$outputs[[1]]$state

## Build the url for the GET request that will check the status of the uploaded file.
url <- paste("https://galaxy.cesb.uky.edu/api/histories/", history_id, "/contents", sep = "")

r <- galaxy_upload_status(username, password, url, history_id, state)

## Destroy the message box that told the user about uploading the file.
tkdestroy(tt)

##################################
## Configure the NA correction tool to run on the uploaded file.
##################################

r <- galaxy_tool_configure(username, password, Labelling)


#################################
## Keep asking Galaxy if the file is ready for download until it is ready.
#################################

## Create a message box to let the user know what is going on.
tt <- tktoplevel()
tkfocus(tt)
message_font <- tkfont.create(family = "Times New Roman", size = 14)
tkwm.title(tt, "Galaxy Progress")
tkgrid(ttklabel(tt, text = "Waiting for NA correction to complete.", font = message_font), padx = 20, pady = 20)

## Get the id and state of the file being created by Galaxy.
content <- content(r)
id <- content$outputs[[1]]$id
state <- content$outputs[[1]]$state

r <- galaxy_NA_corrected_status(username, password, url, state)


## TODO It may be worth while to make another request for the errors when doing NA correction.
## Leaving this out for now, but may be good later.

## Destroy the message box.
tkdestroy(tt)


#################################
## Once the NA corrected file is ready, download it from Galaxy.
#################################

## Build the url to download the newly created file from. 
url <- paste("https://galaxy.cesb.uky.edu/datasets/", id, "/display?to_ext=txt", sep = "")


r <- galaxy_download(username, password, url)


## Probably don't need to write the file out. Can use it directly, but it is easier to interface with
## the existing code this way and it keeps data for posterity.

## Set working directory to the folder above the folder that contains the tracefinder files.
setwd(PathWithTFfiles)
## Write the file out.
write(content(r, "text"), file = "NA_corrected_file_from_Galaxy.txt")

galaxy_data <- read.delim("NA_corrected_file_from_Galaxy.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

plot_samples <- data.frame(Signal = galaxy_data$Renormalized[galaxy_data$Compound == "DSS" & galaxy_data$C_isomers == 0], 
                           SampleID = galaxy_data$SamplID[galaxy_data$Compound == "DSS" & galaxy_data$C_isomers == 0], stringsAsFactors = FALSE)

DSS_plot <- ggplot(data = plot_samples, aes(x = SampleID, y = Signal)) +
  geom_bar(stat="identity", fill = "#0000BB") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Internal Standard Signal")

ggsave(filename = "Internal Standard Signal Barplot.jpeg", plot = DSS_plot, device = "jpeg")



## Get the file IDs aka the unique galaxy data samplIDs
file_id <- unique(galaxy_data$SamplID)
## Declare stdmix_id and sample_id for functions
stdmix_id <- file_id[grep("std", file_id, ignore.case = TRUE)]
sample_id <- file_id[!file_id %in% stdmix_id]
## Remove blanks from std_compound_refdb
std_compound_refdb <- CompoundNamesAndFormulasSorted[!(CompoundNamesAndFormulasSorted$CompoundName == ""),]

## Do the quantitation for the data inputted by user and returned from galaxy for tracefinder files
galaxy_quantified_data <- gen_umol_g_protein_columns(galaxy_data, std_compound_refdb, sequence_data, meta_data, stdmix_id, sample_id, Labelling, swap_compound = TRUE)
## Add legend
galaxy_quantified_data_legend <- gen_galaxy_file_legend(galaxy_quantified_data)

## Keep making a new save dialogue box until the user types in a file name or decides not to save.
response <- tclVar("no")
saved_filename <- ""
while(nchar(saved_filename) == 0 && tclvalue(response) == "no"){
  saved_filename <- tclvalue(tkgetSaveFile(title = "Save Quantified Data As (.csv)"))
  if(nchar(saved_filename) == 0){
    response <- tkmessageBox(title = "File Not Saved", 
                             message = "Quantification file was not saved. Continue without saving?", 
                             icon = "error", type = "yesno")
  } else {
    
    ## Remove .csv file extension if it exists.
    saved_filename <- gsub(".csv", "", saved_filename)
    write.csv(galaxy_quantified_data_legend, file=paste0(saved_filename,".csv"),row.names = FALSE)
  }
}