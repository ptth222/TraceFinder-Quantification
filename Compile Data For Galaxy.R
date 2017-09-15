## These functions compile the TraceFinder data into the correct for to submit to Galaxy for NA correction.


read_TF_reports <- function(TF_FileList, TempMatrix){
  
  ## Create a matrix to hold peak area values from each report.
  PeakAreas<-matrix(NA, nrow = dim(TempMatrix)[1]-1, ncol = length(TF_FileList ))
  
  ## Create a list to keep track of the labeling type of each input TraceFinder file.
  TF_labeling_type <- as.data.frame(TF_FileList, stringsAsFactors = FALSE)
  TF_labeling_type$Labeling <- "NA"
  
  ## Loop through all of the reports and pull out the peak areas from each report into
  ## the PeakAreas matrix.
  for (i in 1:length(TF_FileList))
  {
    TempMatrix <- report_read_check(TF_FileList[i])
    report_empty_check(TempMatrix, TF_FileList[i])
    TempMatrix <- TempMatrix[1:(dim(TempMatrix)[1]-1),]
    TempMatrix <- report_column_check(TempMatrix, TF_FileList[i])
    
    ## Determine the labeling from the pattern in brackets at the end of the first compound name.
    ## For example 13-BPG[C+0] or 13-BPG[C+0_N+0]
    if(any(grepl("\\[C\\+[[:digit:]]_N\\+[[:digit:]]\\]", TempMatrix$Target.Compounds))){
      TF_labeling_type$Labeling[i] <- "C13N15"
    } else if(any(grepl("\\[C\\+[[:digit:]]\\]", TempMatrix$Target.Compounds))){
      TF_labeling_type$Labeling[i] <- "C13"
    } else {
      
      tt <- tktoplevel()
      tkfocus(tt)
      message_font <- tkfont.create(family = "Times New Roman", size = 14)
      tkwm.title(tt, "Labeling Error")
      tkgrid(ttklabel(tt, image = error_icon),
             ttklabel(tt, text = "Could not determine labeling from TraceFinder compound names.",
                      font = message_font), padx = 20, pady = 20)
      close_box <- function(){
        tkdestroy(tt)
      }
      tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
      tkwait.window(tt)
      
      stop()
    }
    
    PeakAreas[,i]= matrix(TempMatrix$Peak.Area)
    gc()
  }
  
  return(list(PeakAreas=PeakAreas, TF_labeling_type=TF_labeling_type))
}


##################################
## Put Chemical Formulas from TraceFinder into a Form that Galaxy Likes
##################################


correct_chemical_formulas <- function(PeakAreas, TempMatrix){
  
  ## Create a matrix with the same number of rows as the data (PeakAreas) and 8 columns.
  CompoundNamesAndFormulasForStripping <- as.data.frame(matrix("0", nrow = dim(PeakAreas)[1], ncol = 8), stringsAsFactors = FALSE)
  
  ## Name the columns.
  colnames(CompoundNamesAndFormulasForStripping)=c("CompoundName",
                                                   "Formula",
                                                   "C_isomers",
                                                   "H_isomers",
                                                   "N_isomers",
                                                   "O_isomers",
                                                   "P_isomers",
                                                   "S_isomers")
  ######## Name the rows.
  CompoundNamesAndFormulasForStripping$CompoundName <- as.character(rownames(PeakAreas))
  
  ## Copy the formulas from the TraceFinder data.
  CompoundNamesAndFormulasForStripping$Formula <- as.character(TempMatrix$Formula)
  
  ## Copy the formula down to all of the isomers.
  for(i in 2:length(CompoundNamesAndFormulasForStripping$Formula)){
    if(CompoundNamesAndFormulasForStripping$Formula[i] == ""){
      CompoundNamesAndFormulasForStripping$Formula[i] <- CompoundNamesAndFormulasForStripping$Formula[i-1]
    }
  }
  
  
  ## For each row in the data change the chemical formula to add in 1's to the formula name
  ## and set the number of isomers correctly.
  for (i in 1:(dim(CompoundNamesAndFormulasForStripping)[1]))
  {
    
    ## Create a variable to hold the formula of the current row.
    ## ex. C5H8O4
    form=CompoundNamesAndFormulasForStripping[i,2]
    ## Make a list where each number is the position of each letter in the formula,
    ## and the last number is the number of total characters plus 1.
    ## ex. C5H8O4 generates 1 3 5 7 C is at 1, H at 3, O and 5 and there are 6 characters
    ## in the formula.
    ups = c(gregexpr("[[:upper:]][[:lower:]]*", form)[[1]], nchar(form) + 1)
    ## Seperate each element and its number of atoms into a list.
    ## ex. C5 H8 O4
    seperated = sapply(1:(length(ups)-1), function(x) substr(form, ups[x], ups[x+1] - 1)) 
    ## Strip off the number of atoms from each limit and put it in another list.
    ## ex. C H O
    elements =  gsub("[[:digit:]]", "", seperated) 
    ## Strip off the elements and put just the number of atoms into another list.
    ## ex. 5 8 4     Elements with 1 atom will be a blank space.
    nums = gsub("[[:alpha:]]", "", seperated)
    ## Create a boolean vector where elements with a number of atoms greater than 1
    ## become FALSE and elements with a number of atoms equal to 1 become TRUE.
    ## ex C5H8O4 becomes FALSE FALSE FALSE CH8O4 becomes TRUE FALSE FALSE
    Adjust=(nums=="")
    ## Use the boolean vector just created to replace the blank spaces with a 1.
    nums[Adjust]=1
    ## Create a blank string.
    newform=""
    
    ## Rebuild the formula. This will look the same if all the elements have a 
    ## number of atoms greater than 1. This will add in a 1 next to the elements
    ## that don't.
    for (j in 1:length(seperated))
    { 
      newform=paste (newform,elements[j], sep = "")
      newform=paste (newform,nums[j], sep = "")
    }
    
    ## Set the formula in the matrix equal to the newly built formula.
    CompoundNamesAndFormulasForStripping[i,2]=newform
    
    ## Set the number of isomers for each row.
    ## If the formula contains a "C" then set the C_isomer column equal to 
    ## the number of carbon atoms in the formula.
    ## Do this for each isomer element, Hydrogen, Nitrogen, etc.
    if (sum(match(elements,"C",nomatch=0))==1)
    {
      CompoundNamesAndFormulasForStripping[i,3]=nums[match(elements,"C",nomatch=0)>0]
    } else {
      CompoundNamesAndFormulasForStripping[i,3] <- "NA"
    }
    if (sum(match(elements,"H",nomatch=0))==1)
    {
      CompoundNamesAndFormulasForStripping[i,4]=nums[match(elements,"H",nomatch=0)>0]
    } else {
      CompoundNamesAndFormulasForStripping[i,4] <- "NA"
    }
    if (sum(match(elements,"N",nomatch=0))==1)
    {
      CompoundNamesAndFormulasForStripping[i,5]=nums[match(elements,"N",nomatch=0)>0]
    }  else {
      CompoundNamesAndFormulasForStripping[i,5] <- "NA"
    }
    if (sum(match(elements,"O",nomatch=0))==1)
    {
      CompoundNamesAndFormulasForStripping[i,6]=nums[match(elements,"O",nomatch=0)>0]
    } else {
      CompoundNamesAndFormulasForStripping[i,6] <- "NA"
    }
    if (sum(match(elements,"P",nomatch=0))==1)
    {
      CompoundNamesAndFormulasForStripping[i,7]=nums[match(elements,"P",nomatch=0)>0]
    }  else {
      CompoundNamesAndFormulasForStripping[i,7] <- "NA"
    }
    if (sum(match(elements,"S",nomatch=0))==1)
    {
      CompoundNamesAndFormulasForStripping[i,8]=nums[match(elements,"S",nomatch=0)>0]
    } else {
      CompoundNamesAndFormulasForStripping[i,8] <- "NA"
    }
  }
  
  return(CompoundNamesAndFormulasForStripping)

}



###################################
## Put the peak areas and corrected chemical formulas into the final form to submit to Galaxy
##################################

build_final_matrix <- function(Labelling, CompoundNamesAndFormulasForStripping, SampleNames, PeakAreas){

  ## If the labelling is only C13 then make a matrix with 5 columns, otherwise if the 
  ## labelling is C13 and N15 then make a matrix with 6 columns.
  if (Labelling=="C13")
  {
    ## Create a matrix with no values.
    ForStripping=matrix(nrow=0, ncol = 5)
    ## Name the columns.
    colnames(ForStripping)=c("Compound","Mol_Formula","C_isomers","SamplID","Intensity")
    ## Create a new matrix and fill it with 0's.
    TempForStripping=matrix("0",nrow=1, ncol = 5)
  }
  if (Labelling=="C13N15")
  {
    ForStripping=matrix(nrow=0, ncol = 6)
    colnames(ForStripping)=c("Compound","Mol_Formula","C_isomers","N_isomers","SamplID","Intensity")
    TempForStripping=matrix("0",nrow=1, ncol = 6)
  }
  
  
  ## For each column in the data.
  for (i in 1:(dim(PeakAreas)[2]))
  {
    ## For each row in the data.
    for (j in 1:(dim(CompoundNamesAndFormulasForStripping)[1]))
    {
      # if (PeakAreas[j,i]>0)
      #  {
      ## Strip off the labeling from the row name and put it in the temporary matrix.
      TempForStripping[,1] <- strsplit(CompoundNamesAndFormulasForStripping[j,1],"\\[")[[1]][1]
      ## Put the chemical formula in the temporary matrix.
      TempForStripping[,2] <- CompoundNamesAndFormulasForStripping[j,2]
      ## Get just the labeling from the row name and put it in a temporary variable.
      ## ex. [C+0] becomes C+0 in the variable.
      TempIsotoplogue <- regmatches(CompoundNamesAndFormulasForStripping[j,1], regexpr("\\[.+\\]", CompoundNamesAndFormulasForStripping[j,1]))
      TempIsotoplogue <- gsub("\\[|\\]", "", TempIsotoplogue)

      ## If there is no labeling at all on the row name then set the variable to 
      ## C+0 or C+0_N+0 depending on which labelling has been done.
      if (identical(TempIsotoplogue, character(0)))
      {
        if (Labelling=="C13"){
          
          TempIsotoplogue <- "C+0"
          
        } else if(Labelling == "C13N15"){
          
          TempIsotoplogue <- "C+0_N+0"
          
        }
      }
      
      if (Labelling=="C13")
      {
        ## Get just the number from the labeling and put it in the temp matrix.
        TempForStripping[,3]=strsplit(TempIsotoplogue,"+",fixed = TRUE)[[1]][2]
        ## Put the full column name (sample name) in the temp matrix.
        TempForStripping[,4]=SampleNames[i]
        ## Put the peak area value in the temp matrix.
        TempForStripping[,5]=PeakAreas[j,i]
        ## Copy the temporary matrix without row and column names to the temp matrix
        ## with row and column names.
        
        ## Not filtering out peack values of 0 anymore so no data is lost.
        ## Add only rows with peak areas greater than 0 or isomers of 0.
        #if(TempForStripping[,3] == 0 || TempForStripping[,5] > 0){
        ForStripping=rbind(ForStripping,TempForStripping)
        #}
        ## Zero out the temp matrix. 
        TempForStripping=matrix("0",nrow=1, ncol = 5)
      }   
      
      ## Same as for C13 labelling but also add in the Nitrogen labeling to the matrix.
      if (Labelling=="C13N15")
      {
        
        TempForStripping[,3]=strsplit(strsplit(TempIsotoplogue,"_",fixed = TRUE)[[1]][1],"+",fixed = TRUE)[[1]][2]
        TempForStripping[,4]=strsplit(strsplit(TempIsotoplogue,"_",fixed = TRUE)[[1]][2],"+",fixed = TRUE)[[1]][2]
        if (is.na(TempForStripping[, 4]))
        {
          TempForStripping[,4]=0
        }

        TempForStripping[,5]=SampleNames[i]
        TempForStripping[,6]=PeakAreas[j,i]
        ## Not filtering out peak values of 0 anymore so no data is lost.
        ## Add only rows with peak areas greater than 0 or isomers of 0.
        #if((TempForStripping[,3] == 0 && TempForStripping[,4] == 0) || TempForStripping[,6] > 0){
        ForStripping=rbind(ForStripping,TempForStripping)
        # }
        TempForStripping=matrix("0",nrow=1, ncol = 6)
      } 
      
      
    }
    
    # }
  }
  
  return(ForStripping)

}