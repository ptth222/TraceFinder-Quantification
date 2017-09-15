## These are all of the functions that are called to quantify the data after it has came back from Galaxy.




###########################
## Function Definitions
###########################

## This function sets the Renormalized column to the intensity column for compounds that don't contain both carbon and nitrogen, or carbon
## depending on the labeling.
Set_Renormalized_for_Unlabeled_Compounds <- function(galaxy_data, Labelling){
  
  galaxy_data$Renormalized[!grepl("C[[:digit:]]", galaxy_data$Mol_Formula) & !(Labelling == "13C15N" & grepl("N[[:digit:]]", galaxy_data$Mol_Formula))] <- 
    as.numeric(galaxy_data$Intensity[!grepl("C[[:digit:]]", galaxy_data$Mol_Formula) & !(Labelling == "13C15N" & grepl("N[[:digit:]]", galaxy_data$Mol_Formula))])
  
  return(galaxy_data)

}


## Not being used currently, but this function will allow for the Renormalized Intensities to be corrected
## by the internal standard "DSS."  This function generates the Internal Standards corrected intensity column
## for potential use in quantitation.  It requires the galaxy data and internal standard ID.
gen_is_correction_column <- function(galaxy_data, is_id = "DSS"){
  ## Gets only the data for the DSS compound.
  is <- galaxy_data[grep(is_id, galaxy_data$Compound),]
  ## Filters to only 0 C_isomers data. 
  is <- is[which(is$C_isomers == 0)]
  ## Generates a correction factor that is the renormalized IS intensity / mean of all of the IS intensities.
  corr_factor <- is$Renormalized / mean(is$Renormalized)
  ## Gives the appropriate sample ID to each of the correction factors.
  names(corr_factor) <- is$SamplID
  ## Makes a numeric column called RenormalizedISCorrected.
  galaxy_data$RenormalizedIScorrected <- 0
  ## For each of the Sample ID's, populate the RenormalizedISCorrected column with the sample specific IS corrected intensities, 
  ## for all of the compound intensities in the data.
  for(id in is$SampleID){
    galaxy_data$RenormalizedIScorrected[galaxy_data$SamplID == id] <- galaxy_data$Renormalized[galaxy_data$SamplID == id] / corr_factor[id]
  }
  ## Return the new dataset with the added column.
  return(galaxy_data)
}




## This function swaps the intensity values for the Lactate compound C_isomer = 0 and the Lactate compound C_isomer = 3.
## It also takes into consideration the type of labelling to control for N_isomer = 0.  This data is then used to do quantification.
swap_compound_cisomer0_intensity <- function(galaxy_data, swap_compound = "Lactate", Labelling = Labelling, swap_cisomer = 3){
  ## Generate a temporary dataset to not overwrite galaxy_data.
  tmp_data <- galaxy_data
  if(Labelling == "C13"){
    ## Get the list of Lactate with C isomers equal to 3.
    temp_3_isomer <- tmp_data[tmp_data$Compound == swap_compound & tmp_data$C_isomers == swap_cisomer,]
    ## Replace each Renormalized value of Lactate with C isomer of 0 with its C isomer of 3 value.
    for(i in 1:nrow(temp_3_isomer)){
      tmp_data$Renormalized[tmp_data$Compound == swap_compound & tmp_data$C_isomers == 0 & tmp_data$SamplID == temp_3_isomer$SamplID[i]] <-
        temp_3_isomer$Renormalized[i]
    }
    
    quantification_data <- tmp_data[tmp_data$C_isomers == 0,]
  } else if(Labelling == "C13N15"){
    
    temp_3_isomer <- tmp_data[tmp_data$Compound == swap_compound & tmp_data$C_isomers == swap_cisomer & tmp_data$N_isomers == 0,]
    for(i in 1:nrow(temp_3_isomer)){
      tmp_data$Renormalized[tmp_data$Compound == swap_compound & tmp_data$C_isomers == 0 & tmp_data$N_isomers == 0 & tmp_data$SamplID == temp_3_isomer$SamplID[i]] <-
        temp_3_isomer$Renormalized[i]
    }
    
    quantification_data <- tmp_data[tmp_data$C_isomers == 0 & tmp_data$N_isomers == 0,]
  }
  ## Return the quantification data for gen_nm_ratio_column function to use.
  return(quantification_data)
}



## This function generates the matrix so that the standard mixes average intensity per compound can be calculated.
## It requires: galaxy data, stdmix_ids, the std compound names, what type of labelling, and the swap compound boolean.
## It also utilizes the swap_compound_cisomer0_intensity function above, and returns the matrix with all of the standard mixes 
## quantification data.
gen_stdmix_intensity_matrix <- function(galaxy_data, stdmix_id, stdcompound_names, Labelling = Labelling, swap_compound = swap_compound){
  ## Generate a matrix with compound names as the rows and std mix id's as the columns
  matrix <- matrix(0, nrow = length(stdcompound_names), ncol = length(stdmix_id))
  colnames(matrix) <- stdmix_id
  rownames(matrix) <- stdcompound_names
  ## For each id in the std mix id list:
  for(id in stdmix_id){
    ## Get galaxy data for the std mix id 
    id_data <- galaxy_data[galaxy_data$SamplID == id,]
    ## Filter to only the standard compound verified data
    id_stdcompounds_data <- id_data[id_data$Compound %in% stdcompound_names,]
    ## Swap compound hardcoded as TRUE currently
    if(swap_compound){
      ## get quantification data for the std mix ID compound data
      quantification_data <- swap_compound_cisomer0_intensity(id_stdcompounds_data, swap_compound = "Lactate", Labelling, swap_cisomer = 3)
      ## Compound IDs only for the verified standard compounds
      compound_id <- quantification_data$Compound
      ## Populate the matrix with the Renormalized intensity for the C-isomer = 0 quantification data
      ## for the compounds present in the std mix and the column associated with the std mix ID.
      matrix[compound_id, id] <- as.numeric(quantification_data$Renormalized)
    } else{
      ## Just does the same thing but swaps the C-isomer = 0 with C-isomer = 0 if FALSE, which will return 
      ## necessary quantification data for matrix.
      quantification_data <- swap_compound_cisomer0_intensity(id_stdcompounds_data, Labelling, swap_cisomer = 0)
      compound_id <- quantification_data$Compound
      matrix[compound_id, id] <- quantification_data$Renormalized
    }
  }
  ## Returns the matrix for averaging for the std mix IDs.
  return(matrix)  
}

## This function takes the previous functions and gets the column for the averaged method of quantitation.  
## This is not the same as the sequence column, which is calculated later.  This function requires the previous functions
## to be performed in order to manipulate the data into the format necessary to do the calculation.  Because this is performed the same
## way each time, it can be hardcoded into a function like this one.  This function requires the galaxy data, std compound reference db,
## std mix ID's, sample ID's, labelling and swap compound = TRUE.
gen_uM_ratio_column <- function(galaxy_data, std_compound_refdb, stdmix_id, sample_id, Labelling = Labelling, swap_compound = swap_compound){
  ## Get the unique std compound names from the reference database column CompoundName.
  stdcompound_names <- unique(std_compound_refdb$CompoundName)
  ## Generates a matrix using the previous function gen_stdmix_intensity_matrix.
  matrix <- gen_stdmix_intensity_matrix(galaxy_data, stdmix_id, stdcompound_names, Labelling, swap_compound = swap_compound)
  ## Creates a new data.frame where the necessary information for the standard compounds quantitation will be stored. 
  ## This includes the average intensities per compound for the standard mixes, the concentrations in the standard mixes.
  stdmix_compound_avgs_data <- data.frame(avg_intensity = rowMeans(matrix),
                                          concentration_uM = std_compound_refdb$Concentration_uM, stringsAsFactors = FALSE)
  ## Generates a new numeric column called Quantified_nM_ratio and renders column values to 0.
  galaxy_data$Quantified_uM_ratio <- 0
  ## For each std compound ID in standard compound names list:
  for(id in stdcompound_names){
    ## Get the galaxy data for the standard compound ID
    compound_data <- galaxy_data[galaxy_data$Compound == id,]
    ## change the quantified_nM_ratio column for the compound data indeces to = the Renormalized intensities / std mix average intensity for the compoound
    ## and multiply it by the std mix concentration (nM) for that compound.
    galaxy_data[rownames(compound_data),"Quantified_uM_ratio"] <- compound_data$Renormalized / stdmix_compound_avgs_data[id,"avg_intensity"] *
      stdmix_compound_avgs_data[id,"concentration_uM"]
  }
  ## After we do that for all standard compounds we return the galaxy data with the updated Quantified_nM_ratio column.
  return(galaxy_data)
}

## This function takes into account a sample's position between standard mixes.  
## The function returns a quantitation column using the sequence between two standard mixes for each sample in 
## the galaxy data.   It requires the galaxy data generated already by gen_nm_ratio_column, the std compound
## reference DB, and the sequence data.frame generated by the function above.
gen_uM_sequence_ratio_column <- function(galaxy_data, std_compound_refdb, sequence_data, Labelling){
  ## Create a new column in galaxy data called Quantified_nM_sequence_ratio and render 0.
  galaxy_data$Quantified_uM_sequence_ratio <- 0
  ## get the std mix ID's from the sequence data
  std_id <- sequence_data$id[grep("stds", sequence_data$id, ignore.case = TRUE)]
  ## For loop for the amount of standards in the sequence data do:
  for(i in 1:(length(std_id)-1)){
    ## First standard for calculation
    std_1 <- sequence_data[sequence_data$id == std_id[i],]
    ## Second standard for calculation
    std_2 <- sequence_data[sequence_data$id == std_id[i+1],]
    ## Checking to make sure there are samples between the standards
    if(std_1$position+1 != std_2$position){
      ## The positions associated with the samples between the two standards
      between_pos <- seq(std_1$position+1, std_2$position-1)
      ## the sample ID's for the positions between the two standards from sequence_data
      sample_id <- sequence_data[sequence_data$position %in% between_pos,]
      ## If the first standard mix in the sequence pair of interest was not included by the user,
      ## then give them an error message and try the next pair.
      if(!std_1$exists) {
        ## Only give the user a message if they included sample information the standard could be used to quantify.
        ## The idea is that if there is no TraceFinder report for the standard or for the samples then the user probably
        ## didn't include them because they don't care about them and not because they forgot to include them. The chance of leaving out a standard 
        ## TF report and all of the samples it could quantify is small.
        if(any(sample_id$exists)){
          tt <- tktoplevel()
          tkfocus(tt)
          message_font <- tkfont.create(family = "Times New Roman", size = 12)
          tkwm.title(tt, "Sequence Quantification Warning")
          tkgrid(ttklabel(tt, image = warning_icon),
                 ttklabel(tt, text = paste("The TraceFinder file for standard mix ", std_1$id, " was not found.\nSamples:\n\n", paste(sample_id$id, collapse = "\n"), "\n\ncould not be quantified.", sep = ""), 
                          font = message_font), padx = 20, pady = 20)
          close_box <- function(){
            tkdestroy(tt)
          }
          tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
          tkwait.window(tt)
        }
        
        next()
      }
      
      ## If the second standard mix in the sequence pair of interest was not included by the user, 
      ## then give them an error message and try the next pair.
      if(!std_2$exists) {
        ## Only give the user a message if they included sample information the standard could be used to quantify.
        ## The idea is that if there is no TraceFinder report for the standard or for the samples then the user probably
        ## didn't include them because they don't care about them and not because they forgot to include them. The chance of leaving out a standard 
        ## TF report and all of the samples it could quantify is small.
        if(any(sample_id$exists)){
          tt <- tktoplevel()
          tkfocus(tt)
          message_font <- tkfont.create(family = "Times New Roman", size = 12)
          tkwm.title(tt, "Sequence Quantification Warning")
          tkgrid(ttklabel(tt, image = warning_icon),
                 ttklabel(tt, text = paste("The TraceFinder file for standard mix ", std_2$id, " was not found.\nSamples:\n\n", paste(sample_id$id, collapse = "\n"), "\n\ncould not be quantified.", sep = ""), 
                          font = message_font), padx = 20, pady = 20)
          close_box <- function(){
            tkdestroy(tt)
          }
          tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
          tkwait.window(tt)
        }
        
        next()
      }
      ## If any of the samples in the sequence don't have a matching TraceFinder report, 
      ## then tell the user and remove them from the list to be quantified.
      if(!all(sample_id$exists)){
        ## Get the samples that don't have TraceFinder reports.
        non_exist <- sample_id$id[!sample_id$exists]
        
        ## Remove them from sample_id.
        sample_id <- sample_id[sample_id$exists,]
        
        tt <- tktoplevel()
        tkfocus(tt)
        message_font <- tkfont.create(family = "Times New Roman", size = 12)
        tkwm.title(tt, "Sequence Quantification Warning")
        tkgrid(ttklabel(tt, image = warning_icon),
               ttklabel(tt, text = paste("The TraceFinder files for samples:\n\n", paste(non_exist, collapse = "\n"), "\n\nwere not found.\nThey could not be quantified.", sep = ""), 
                        font = message_font), padx = 20, pady = 20)
        close_box <- function(){
          tkdestroy(tt)
        }
        tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
        tkwait.window(tt)
        
      }
      ## If there are no samples between the standard mix pair then the code will error, so skip to the next pair.
      if(length(sample_id$id) == 0){next()}
      ## Make a list of the two standard mix names
      stds <- c(std_1$id, std_2$id)
      ## So we don't overwrite galaxy_data incorrectly
      tmp_data <- galaxy_data
      ## Filter to only data for our std mixes and between samples
      tmp_data <- tmp_data[tmp_data$SamplID %in% c(sample_id$id, stds),]
      
      
      # ## Filter down to only C_isommer = 0 data or C_isomer = 0 and N_isomer = 0
      # if(Labelling =="C13"){
      #   
      #   tmp_data <- tmp_data[tmp_data$C_isomers == 0,]
      #   
      # } else if(Labelling == "C13N15"){
      #   
      #   tmp_data <- tmp_data[tmp_data$C_isomers == 0 & tmp_data$N_isomers == 0,]
      # }
      # 
      
      ## get the unique Compounds for the between samples 
      std_compounds <- unique(tmp_data$Compound[tmp_data$SamplID %in% sample_id$id])
      ## Filter down to only the verified standard compounds to look for in data.
      std_compounds <- std_compounds[std_compounds %in% std_compound_refdb$CompoundName]
      ## For each standard compound in the between position samples standard compounds list:
      for (comp_id in std_compounds){
        ## get only the data for the standard compound
        data <- tmp_data[tmp_data$Compound %in% comp_id,]
        
        ## Filter down to only C_isommer = 0 data or C_isomer = 0 and N_isomer = 0
        if(Labelling =="C13"){
          
          if(comp_id == "Lactate"){
            
            data <- data[data$C_isomers == 3,]
          } else{
            
            data <- data[data$C_isomers == 0,]
          }
          
          
        } else if(Labelling == "C13N15"){
          
          if(comp_id == "Lactate"){
            
            data <- data[data$C_isomers == 3 & data$N_isomers == 0,]
          } else{
            
            data <- data[data$C_isomers == 0 & data$N_isomers == 0,]
          }
        }
        
        if(nrow(data) == 0){
          
          tt <- tktoplevel()
          tkfocus(tt)
          message_font <- tkfont.create(family = "Times New Roman", size = 12)
          tkwm.title(tt, "Sequence Quantification Warning")
          tkgrid(ttklabel(tt, image = warning_icon),
                 ttklabel(tt, text = paste0("The compound \n\n", comp_id, "\n\ndid not have an entry in the TraceFinder reports for its unlabeled isotopologue (C+0, C+0_N+0). \nIt could not be quantified."), 
                          font = message_font), padx = 20, pady = 20)
          close_box <- function(){
            tkdestroy(tt)
          }
          tkgrid(tkbutton(tt, text='Okay', command = close_box), columnspan = 2)
          tkwait.window(tt)
          
          next()
        }
        
        ## Subtract the Renormalized intensity for first standard mix from second standard mix intensity for the standard compound
        std_compound_change <- data$Renormalized[data$SamplID == std_2$id] - data$Renormalized[data$SamplID == std_1$id]
        ## Take the standard mix difference and divide it by the amount of samples between standard mix, and add to first standard mix intensity
        corrected_std_signal <- std_compound_change/(std_2$position - std_1$position - 1) + data$Renormalized[data$SamplID == std_1$id]
        ## Generate dataset to correct by removing timestamp and matching galaxy data sample ID with between position sample IDs
        corrected_data <- galaxy_data[galaxy_data$SamplID %in% sample_id$id,]
        ## Filter corrected data to standard compound in for loop
        corrected_data <- corrected_data[corrected_data$Compound %in% comp_id,]
        ## Populate the Quantified_nM_sequence_ratio column for between position sample ID's with Renormalized intensities for all compound isomers
        ## divided by the corrected standard signal above and multiplied by the standard concentration in uM
        corrected_data$Quantified_uM_sequence_ratio[corrected_data$SamplID %in% sample_id$id] <- corrected_data$Renormalized[corrected_data$SamplID %in% sample_id$id] / corrected_std_signal * 
          std_compound_refdb$Concentration_uM[std_compound_refdb$CompoundName == comp_id]
        ## Populate the Quantified_uM_sequence_ratio column for the indeces in the corrected data with the proper corrected data Quantification
        galaxy_data[rownames(corrected_data),"Quantified_uM_sequence_ratio"] <- corrected_data$Quantified_uM_sequence_ratio
      }
    }
  }
  ## Return modified galaxy data
  return(galaxy_data)
}
## This is the final wrapper function that uses all previous functions to produce the final quantitation spreadsheet that the user saves and uses.
## This function returns the umol_g_protein_columns and requires all user input as well as hard codes swap_compound = TRUE.
gen_umol_g_protein_columns <- function(galaxy_data, std_compound_refdb, sequence_data, meta_data, stdmix_id, sample_id, Labelling, swap_compound = TRUE){
  ## Use gen_uM_ratio_column function to produce galaxy data with this column and the galaxy data that gen_uM_sequence_ratio_column requires.
  galaxy_data <- gen_uM_ratio_column(galaxy_data, std_compound_refdb, stdmix_id, sample_id, Labelling, swap_compound = swap_compound)
  ## Generate sequence based quantification "ratio" to use in final quantitation for the sequence data
  galaxy_data <- gen_uM_sequence_ratio_column(galaxy_data, std_compound_refdb, sequence_data, Labelling)
  ## Add necessary columns to do quantitation calculation and render them 0.
  galaxy_data$reconstitution_volume_uL <- 0 
  galaxy_data$injection_volume_uL <- 0
  galaxy_data$protein_mg <- 0
  galaxy_data$icms_split_ratio <- 0
  galaxy_data$Amount_ProteinAdj_uMol_g_protein_RatioBased <- 0
  galaxy_data$Amount_ProteinAdj_uMol_g_protein_SequenceBased <- 0
  
  
  ## Make a list of sample IDs that is the intersection of the sample IDs in the meta data and galaxy data.
  ## This ensures that you get a list of sample IDs that are in both data sets.
  
  sample_ids <- unique(galaxy_data$SamplID)[as.logical(match(unique(galaxy_data$SamplID), unique(meta_data$SampleID), nomatch = 0))]
  
  ## For each intersecting sample ID
  for(id in sample_ids){
    ## get galaxy data for the sample ID
    sample_data <- galaxy_data[galaxy_data$SamplID == id,]
    ## get meta data for the sample ID
    sample_meta <- meta_data[meta_data$SampleID == id,]
    ## fill the values for the sample data with the values in the meta data for quantitation calculation
    sample_data$protein_mg <- sample_meta$Protein_mg
    sample_data$icms_split_ratio <- sample_meta$ICMS_split_ratio
    sample_data$reconstitution_volume_uL <- sample_meta$ReconstitutionVolume_uL
    sample_data$injection_volume_uL <- sample_meta$InjectionVolume_uL
    ## Do the calculation for the average standard mixes intensity "RatioBased" quantitation
    sample_data$Amount_ProteinAdj_uMol_g_protein_RatioBased <- sample_data$Quantified_uM_ratio * sample_data$reconstitution_volume_uL /
      sample_data$icms_split_ratio / sample_data$protein_mg / 1000
    ## Do the calculation for the standard mix / sample sequence quantitation
    sample_data$Amount_ProteinAdj_uMol_g_protein_SequenceBased <- sample_data$Quantified_uM_sequence_ratio * sample_data$reconstitution_volume_uL /
      sample_data$icms_split_ratio / sample_data$protein_mg / 1000
    ## Populate the galaxy data for the sample data rows with all of the pertinent info for the sample
    galaxy_data[rownames(sample_data),] <- sample_data
  }
  ## Return modified final galaxy dataset ready to have legend printed and exported for user.
  return(galaxy_data)  
}
## Puts the galaxy file legend onto final galaxy file.
gen_galaxy_file_legend <- function(galaxy_data){
  galaxy_data$CommentQuantification<- ""
  galaxy_data$CommentQuantification[1]="Legend"
  galaxy_data$CommentQuantification[2]="Compound: name of assigned metabolite, noStd means assigment was NOT verified with standard compound"
  galaxy_data$CommentQuantification[3]="Mol_Formula: molucular formula od assigned compound"
  galaxy_data$CommentQuantification[4]="C_isomers and/or N_isomers: additional Da units that were measured in a targeted manner for the compound in question"
  galaxy_data$CommentQuantification[5]="Intensity: raw XIC intensity data"
  galaxy_data$CommentQuantification[6]="Renormalized: Natural abundance Corrected XIC intensity data"
  galaxy_data$CommentQuantification[7]="IS_CorrFactor = Correction factor based on internal standard DSS"
  galaxy_data$CommentQuantification[8]="Quantified_nM_ratio = Isotoplogue concentration calculated for the reconstituted ICMS vial using the stds mixture concentration for each standard, calculated by taking \"(Ratio Sample Isotoplogue/avarage(Ratio M+0 External standard Isotoplogues)) x Concentration standard\". Renormalized column was used for the NA corrected XIC peak areas, When multiple batches for the stds mixes were used, batch information is provided in a seperate file"
  galaxy_data$CommentQuantification[9]="Quantified_nM_sequence_ratio = Isotoplogue concentration calculated for the reconstituted ICMS vial using the stds mixture concentration for each standard, calculated by taking \"sample intensity/(std_1 + ((std_2 - std_1)/# of samples) * standard concentration\"."
  galaxy_data$CommentQuantification[10]="Quantified_nM_CalCurve = UNDER development, do not use,Isotoplogue concentration calculated for the reconstituted ICMS vial, calculated by taking based on a fit to a calibration curve"
  galaxy_data$CommentQuantification[11]="Quantified_nM_CalCurveLog = UNDER development, do not use,Isotoplogue concentration calculated for the reconstituted ICMS vial, calculated by taking based on a fit to a calibration curve in log space"
  galaxy_data$CommentQuantification[12]="ICMS_split_ratio = Fraction of Polar extract that was injected for ICMS , equals g polar vial used divided by g polar ext "
  galaxy_data$CommentQuantification[13]="Protein_mg = mg protein?of protein from metadata file used for adjusting concentration for protein amount "
  galaxy_data$CommentQuantification[14]="SliceNumber= SliceNumber of sample"
  galaxy_data$CommentQuantification[15]="ReconsitutionVolume_uL = volume by which ICMS vial was reconsituted in uL, usually this is 20uL"
  galaxy_data$CommentQuantification[16]="InjectionVolume_uL= Volume of the Reconsitution Volume that was injected for ICMS in uL, usually this is 10uL"
  galaxy_data$CommentQuantification[17]="Amount_ProteinAdj_uMol_g_protein_RatioBased = amount of quantified isotoplogue in sample in umol/g protein?protein, calculated as Quantified_uM x (RecVol/1000/SplitRatio/mg protein)"
  galaxy_data$CommentQuantification[18]="Amount_ProteinAdj_uMol_g_protein_SequenceBased = amount of quantified isotoplogue in sample in umol/g protein?protein, calculated as Quantified_uM x (RecVol/1000/SplitRatio/mg protein)"
  #galaxyData$CommentQuantification[19]="11) Amount_ProteinAdj_nMol_g_protein_CalCurveBased = same as 11 but calculated with Quantified_uM_CalCurve"
  #galaxyData$CommentQuantification[21]="12) Amount_ProteinAdj_nMol_g_protein_LogCalCurveRatioBased = same as 11 but calculated with Quantified_uM_CalCurveLog"
  return(galaxy_data)
}




############################
## End Definitions
############################