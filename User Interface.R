## Function that makes the opening GUI for the user.

user_interface <- function(){

  ## Make the first GUI for the user. Ask for the labeling type, reconstitution volume, injection volume, 
  ## TF reports, standards database, and metadata file.
  tt <- tktoplevel()
  tkfocus(tt)
  tkwm.title(tt, "Quantification from Tracefinder Data")
  tkgrid(ttklabel(tt, text = "Upload Files Here" ), columnspan = 4)
  tkgrid(ttklabel(tt,text="    " ))
  
  ###
  ## Add dropdown for labelling type.
  ###
  tkgrid(tklabel(tt, text = "Type of Labelling: "), columnspan = 4)
  labeling_types <- c("Choose One", "C13", "C13N15")
  chosen_labeling <- tclVar("Choose One")
  dropdown <- ttkcombobox(tt, values = labeling_types, textvariable = chosen_labeling, state="readonly") 
  tkgrid(dropdown, columnspan = 4)
  tkgrid(ttklabel(tt,text="    " ))
  
  
  ####
  ## Add in the widgets to get the Tracefinder sample reports.
  ## Create a label, entry box to display the chosen filepath, and button to select files.
  ####
  reports_label <- tklabel(tt, text="Tracefinder Reports: ")
  reports_entry_variable <- tclVar("Select TraceFinder Reports")
  reports_entry <- tkentry(tt, background = "white", textvariable = reports_entry_variable, width = 100, state = "readonly")
  reports_button <- tkbutton(tt, text = "Open")
  
  ## Make a label that is blank just to take up space and position widgets in the grid.
  space_label1 <- tklabel(tt)
  
  ## Grid widgets into the window.
  tkgrid(reports_label, reports_entry, space_label1, reports_button)
  tkgrid.configure(reports_label, sticky = "e")
  tkgrid.configure(reports_entry, columnspan = 2, sticky = "we")
  tkgrid.remove(space_label1)
  
  TF_reports_file_paths <<- ""
  
  ## Function the button runs when pressed.
  change_report_text <- function() {
    assign("TF_reports_file_paths", tk_choose.files(caption = "Select All Tracefinder Reports", multi = TRUE),
           envir = .GlobalEnv)
    temp_entry_var <- tclVar(TF_reports_file_paths[1])
    tkconfigure(reports_entry, textvariable = temp_entry_var)
  }
  tkconfigure(reports_button, command = change_report_text)
  
  
  ####
  ## Add in the widgets to get the metadata file that contains the sample volume and grams of protein.
  ####
  metadata_label <- tklabel(tt, text="Metadata File: ")
  metadata_entry <- tkentry(tt, background = "white", textvariable = tclVar("Select Metadata File"), width = 100, state = "readonly")
  metadata_button <- tkbutton(tt, text = "Open")
  
  ## Make a label that is blank just to take up space and position widgets in the grid.
  space_label2 <- tklabel(tt)
  
  ## Grid widgets into the window.
  tkgrid(metadata_label, metadata_entry, space_label2, metadata_button)
  tkgrid.configure(metadata_label, sticky = "e")
  tkgrid.configure(metadata_entry, columnspan = 2, sticky = "we")
  tkgrid.remove(space_label2)
  
  metadata_file_path <<- ""
  
  change_metadata_text <- function() {
    assign("metadata_file_path", tk_choose.files(caption = "Select Metadata File", multi = TRUE),
           envir = .GlobalEnv)
    temp_entry_var <- tclVar(metadata_file_path)
    tkconfigure(metadata_entry, textvariable = temp_entry_var)
  }
  tkconfigure(metadata_button, command = change_metadata_text)
  
  tkgrid(ttklabel(tt,text="    " ))
  
  
  ## Add a variable to capture whether the done button was pressed or the x.
  test <- tclVar("")
  tclvalue(test) <- 0
  
  ####
  ## Add button to close the window.
  ####
  file_check <- function(){
    
    if(identical(TF_reports_file_paths, character(0)) || identical(metadata_file_path, character(0)) || 
       TF_reports_file_paths == "" || metadata_file_path == ""){
      
      tkconfigure(error_label, text ="Error. Files not selected for every field.", foreground = "red")
    }
    else if(tclvalue(chosen_labeling) == "Choose One"){
      tkconfigure(error_label, text ="Error. Please choose a type of labeling.", foreground = "red")
    }
    else{
      tclvalue(test) <- 1
    }
  }
  tkgrid(tkbutton(tt, text='Done', command = file_check), columnspan = 4)
  error_label <- tklabel(tt)
  tkgrid(error_label, columnspan = 4)
  tkbind(tt, "<Destroy>", function() tclvalue(test)<-2)
  tkwait.variable(test)
  testval <- as.integer(tclvalue(test))
  tkdestroy(tt)
  
  if(testval == 2) {stop()}
  
  return(tclvalue(chosen_labeling))

}