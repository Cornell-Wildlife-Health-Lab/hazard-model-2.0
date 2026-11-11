#_______________________________________________________________________________
#
# Hazard Model 2.0
#
# AUTHOR: Brenda Hanley
# ERROR HANDLING: Brenda Hanley
# MODEL LOGGING: Brenda Hanley
# QAQC:
#
# Location: Cornell Wildlife Health Lab
# Licence: MIT
#
# This code corresponds to the work by Thompson et al.
#
# This code was written under:
# R version 4.4.1 (2024-06-14 uct) -- "Race for Your Life"
# Copyright (C) 2024 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64
#
#_______________________________________________________________________________

# Load packages. ------------
library(dplyr)

# Reusable Functions
html_tag_line <- function(text, tag = "p") {
	# Given a string and a simple string representing an html tag, this
	# function surrounds the text with the tag.
	paste0("<", tag, ">", text, "</", tag, ">")
}

add_item_to_json_array=function(file_path, new_item) {
    # This is a bespoke function that adds a string representing a JavaScript
    # Object to the attachments.json file containing an array listing the model
    # outputs. Although this function has error handling for a missing file and
    # improperly formed file, the existence of the file and a list enclosed in
    # brackets in that file are expected.

    # Check if the file exists.
    if (!file.exists(file_path)) {
        # Write to error log and exit script with an error.
        line=paste0("<h4>ERROR</h4><p>Error: File '", file_path, "' not found.</p>")
        write(line,file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Read the file content.
    file_content=readChar(file_path, file.info(file_path)$size)

    # Check if the file is empty.
    if (nchar(file_content) == 0) {
        line=paste0("<h4>ERROR</h4><p>Error: File '", file_path, "' is empty.</p>")
        write(line,file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Remove the last closing bracket.
    file_content=substr(file_content, 1, nchar(file_content) - 1)

    # Create a function for adding double quotes around text.
    double_quote=function(x) {paste0('"', x, '"')}

    # Create the new item as a JSON string using shQuote with double_quote.
    new_item_json=paste0(
        "{",
        paste(
            sapply(names(new_item), double_quote),
            sapply(as.character(new_item), double_quote),
            sep=":", collapse=","
        ),
        "}"
    )

    # Add comma, new item, and closing bracket to the file content
    file_content=paste0(file_content, ",", new_item_json, "]")

    # Write the updated data back to the file
    writeLines(file_content, file_path, sep="")
}

# Model log file started with Python data processing script. -------
	model_log_filepath=file.path("", "data","attachments","info.html")
	json_attachment_file=file.path("", "data", "attachments.json")

# Continue the log started with the python script. ----------
	line='<h4>Model Execution</h4>'
	write(line,file=model_log_filepath,append=TRUE)

# Info about Required Data. ------------
# There are required files that will always be imported:
# 1. A csv file of selected set of risk factors and weights.
# 2. A csv file of sub-administrative areas (to get the standard data frame). 
# 3. A csv file of data totals (to get data totals on all user-selected factors). 
# 4. A csv file of adjusted distances to closest known infection by subadmin unit.

# 1. WEIGHTS TABLE -----------------------------------------------
	
# Read in the (Required) Weights file. 
	Weights_filepath=file.path("","data","Weights.csv")
	Weights=readr::read_csv(Weights_filepath,show_col_types = FALSE)
	#Weights=readr::read_csv("Weights.csv") 

	# Formatting the weights table.  
	Weights=as.data.frame(cbind(Weights$RiskFactor,Weights$UserSelection,Weights$WeightNear,Weights$SDWeightNear,Weights$WeightFar,Weights$SDWeightFar))
  colnames(Weights)=c("RiskFactor","UserSelection","WeightNear","SDWeightNear","WeightFar","SDWeightFar")
  
  # Get the number of risk factors selected. 
  NumberRisksSelected=sum(as.numeric(Weights$UserSelection))

  # Zero out weights of risk factors that are irrelevant in model.
	Weights[Weights$UserSelection=="0",]=0
	
	# Check. Make sure that there are no NAs in the weights table as it could cause problems in shadow math. 
	Weights[is.na(Weights)]=0
	
	# REVIEW: When no risk factors are selected (all UserSelection=0, so all weights are 0), 
	# the model now proceeds with baseline-only output instead of terminating. 
	# BaselineRisk is still computed per area from EffectiveDistance; TotalRisk is 0; 
	# TotalHazard equals BaselineRisk; e_Baseline is 1 (baseline fully explains hazard); 
	# and per-factor elasticities (e_<Factor>, ew_<Factor>) are 0 (then nulled out in 
	# output.json by output_processing.py based on UserSelection=0). 
	# A log message informs the user that this is a baseline-only run. 
	# 
	# Previously, this block checked whether any weight was non-zero and terminated 
	# the model otherwise. The modeler requested that baseline-only runs be a valid 
	# output mode, so the terminator has been removed. 

	# Check whether any risk factor has non-zero weights (i.e., any factor is "active"). 
	NotZero=Weights[rowSums(Weights!=0)>0,]
	NotZeroDim=as.numeric(nrow(NotZero))
	if (NotZeroDim==0){
	    line="No risk factors were selected for this model run. The model will produce baseline-only hazard results, which reflect proximity to known CWD-positive areas but do not incorporate any risk-factor contributions. To include risk factors, return to the CWD Data Warehouse and select one or more."
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
	  } # End if no risk factors are selected.
	
	Cleaned_Weights=Weights
	
	# The final version of the weights is now named Cleaned_Weights.
	
# 2. SUBADMIN -----------------------------------------------------
	
	# Read in the (Required) SubAdmin file. 
	Subadmin_filepath=file.path("","data","Subadmin.csv")
	Subadmin=readr::read_csv(Subadmin_filepath,show_col_types = FALSE) 
	#Subadmin=readr::read_csv("Subadmin.csv") 
	
	# Formatting the subadmin file. 
	Subadmin=as.data.frame(cbind(Subadmin$SubAdminID,Subadmin$FullName))
	colnames(Subadmin)=c("SubAdminID","FullName")
	
	# Get the number of subadmin units. 
	FullNumberAreas=as.numeric(nrow(Subadmin))
	
# 3. ADJUSTED DISTANCE FILE --------------------------------------
	
	# Read in the (Required) effective distance file. 	
	EffectiveDistance_filepath=file.path("","data","EffectiveDistance.csv")
		EffectiveDistance=readr::read_csv(EffectiveDistance_filepath,show_col_types = FALSE) 
	#EffectiveDistance=readr::read_csv("EffectiveDistance.csv")
	
	# Formatting the movement file. 
		Cleaned_EffectiveDistance=as.data.frame(cbind(EffectiveDistance$SubAdminID,EffectiveDistance$EffectiveDistance))
	colnames(Cleaned_EffectiveDistance)=c("SubAdminID","EffectiveDistance")
	
	# Check. Make sure that there are no NAs in the EffectiveDistance file. 
	Cleaned_EffectiveDistance[is.na(Cleaned_EffectiveDistance)]=0
	
	# Check. Make sure all EffectiveDistances are positive. 
	Cleaned_EffectiveDistance[Cleaned_EffectiveDistance<0]=0
	
	# The cleaned EffectiveDistance file is now named Cleaned_EffectiveDistance.
	
# 4. DATA TOTALS FILE ---------------------------------------------
	
	# Read in the (Required) Data Totals file. 
	DataTotals_filepath=file.path("","data","DataTotals.csv")
	DataTotals=readr::read_csv(DataTotals_filepath,show_col_types = FALSE) 
	# DataTotals=readr::read_csv("DataTotals.csv") 

	# Formatting the data totals. 
	Cleaned_DataTotals=as.data.frame(cbind(
	  DataTotals$SubAdminID,
	  DataTotals$FullName,
	  DataTotals$MineralLicks,
	  DataTotals$WaterBodies,
	  DataTotals$Feedgrounds,
	  DataTotals$Guzzlers,
	  DataTotals$BaitingStations,
	  DataTotals$AgriculturalPractices,
	  DataTotals$CaptiveCervidFacilities,
	  DataTotals$RehabilitationFacilities,
	  DataTotals$Taxidermists,
	  DataTotals$Processors,
	  DataTotals$RenderingFacilities,
	  DataTotals$Incinerators,
	  DataTotals$FoodBanks,
	  DataTotals$Landfill,
	  DataTotals$Dumping,
	  DataTotals$Sheds,
	  DataTotals$Roadkill,
	  DataTotals$LocalPractices,
	  DataTotals$FreeRangingCervidDispersal,
	  DataTotals$SeasonalMigration))
	  colnames(Cleaned_DataTotals)=
	    c("SubAdminID",
	    "FullName",
	    "MineralLicks",
	    "WaterBodies",
	    "Feedgrounds",
	    "Guzzlers",
	    "BaitingStations",
	    "AgriculturalPractices",
	    "CaptiveCervidFacilities",
	    "RehabilitationFacilities",
	    "Taxidermists",
	    "Processors",
	    "RenderingFacilities",
	    "Incinerators",
	    "FoodBanks",
	    "Landfill",
	    "Dumping",
	    "Sheds",
	    "Roadkill",
	    "LocalPractices",
	    "FreeRangingCervidDispersal",
	    "SeasonalMigration")
	  
	# Get the number of subamin areas with data. 
	NumSubAreas=nrow(Cleaned_DataTotals)
	
	# At this point, if the risk factor is not relevant, the weight is zero. 
	# However, there could be instances where NA exists in the data of an irrelevant risk factor. 
	# We need to take care of those to ensure that NA is used only for missing data within relevant risk factors. 
	for (i in 1:20){if (Cleaned_Weights$UserSelection[i]==0){Cleaned_DataTotals[,i+2]=rep(0,dim(DataTotals)[1])}}
	
	# Check. If the user selects only one risk factor, and that risk factor 
	# has a NA-valued data in all subamin areas, then stop the code. 
	if (NumberRisksSelected==1){
	  NumberOfNAs=as.numeric(sum(is.na(Cleaned_DataTotals)))
	  if (NumberOfNAs==NumSubAreas){
	    # Terminate the code. 
	    line="We're sorry. You selected a single risk factor, but that risk factor does not 
	      have data for any sub-admin area. Return to the CWD Data Warehouse and input 
	    additional data, or select a new set of risk factors."
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	    # Quit the session.
	    quit(status=70)}}
	
	# Check. If the user selects many risk factors, but all selected risk factors
	# only have NAs for data, then terminate the code. 
	if (NumberRisksSelected>1){
	  NumberOfNAs=as.numeric(sum(is.na(Cleaned_DataTotals)))
	  if (NumberOfNAs==NumberRisksSelected*NumSubAreas){
	    # Terminate the code. 
	    line="We're sorry. You selected a set of risk factors, but those risk factors do not 
	   have data for any sub-admin area. Return to the CWD Data Warehouse and input 
	    additional data, or select a new set of risk factors."
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	    # Quit the session.
	    quit(status=70)}}
	
	# At this point, an NA only exists if there is missing data in a relevant risk factor.
	# However, at least one sub-admin area has non-NA data, and not all sub-admin areas 
	# have NAs in every risk factor. At least one sub-admin area can be mapped. 
	
	# Now we want to scale the count factors. Our goal is to have all count columns with non-zero entries sum to one. 
	# Note that these count risk factors start at column 3 and run through column 20. 
	# Note: We will deal with the last two (#21 dispersal and #22 seasonal migration) next. 
	Cleaned_DataTotals=as.data.frame(Cleaned_DataTotals)
	for (h in 3:20){
	  x=sum(as.numeric(Cleaned_DataTotals[,h]),na.rm=TRUE)
	  if (x!=0) {
	    Cleaned_DataTotals[,h]=as.numeric(Cleaned_DataTotals[,h])/sum(as.numeric(Cleaned_DataTotals[,h]),na.rm=TRUE)}}
	
	# We will now scale dispersal (column #21). 
	# Dispersal matters fully (i.e., has a value of 1) if a single animal can arrive from the nearest known infection. 
	# Thus, if dispersal is greater than or equal to the effective distance, then the cleaned dispersal value will be set to 1.
	# Whereas if dispersal is less than the effective distance, then the value will be scaled to the proportion of the distance covered. 
	for (k in 1:NumSubAreas){
	  y=as.numeric(Cleaned_DataTotals[k,21])
	  if (is.na(y)){Cleaned_DataTotals[k,21]=NA}else{
	    # If the hosts can go farther than the distance to the nearest, then the risk factor is set to maximum. 
	    if (y>=as.numeric(Cleaned_EffectiveDistance$EffectiveDistance[k])){Cleaned_DataTotals[k,21]=1
	    }else{
	      # If the host only go a portion of the distance to nearest, then the risk factor is set to the proportion of the distance.
	      if (as.numeric(Cleaned_EffectiveDistance$EffectiveDistance[k])==0){Cleaned_DataTotals[k,21]=0}else{Cleaned_DataTotals[k,21]=as.numeric(Cleaned_DataTotals[k,21])/as.numeric(Cleaned_EffectiveDistance$EffectiveDistance[k])}
	      }
	  }}
	# Now that the units are consistent, we will scale the entire vector to one. 
	# REVIEW: Guard against division by zero. When the column sum is 0 (e.g., all values 
	# are 0 or NA because no dispersal data were provided), dividing produces NaN, 
	# which later cascades through is.na() checks and causes every area to be dropped. 
	# In that case, leave the column as-is (it is already all 0s or NAs). 
	dispersal_sum=sum(as.numeric(Cleaned_DataTotals[,21]),na.rm=TRUE)
	if (dispersal_sum>0){
	  Cleaned_DataTotals[,21]=as.numeric(Cleaned_DataTotals[,21])/dispersal_sum
	}
	
	# Finally, we will scale seasonal migration (column #22).  
	# Seasonal migration matters fully (i.e., has a value of 1) if the herd's movements touch the nearest known infection.
	# Thus, if the seasonal migration is greater than the distance to the nearest positive, then the value will be scaled to 1. 
	# Whereas if seasonal migration is less than the distance to the nearest positive, then the value will be scaled to the proportion of the distance. 
	for (k in 1:NumSubAreas){
	  y=as.numeric(Cleaned_DataTotals[k,22])
	  if (is.na(y)){Cleaned_DataTotals[k,22]=NA}else{
	    # If the herds move farther than the distance to the nearest, then the risk factor is set to maximum.
	    if (y>=as.numeric(Cleaned_EffectiveDistance$EffectiveDistance[k])){Cleaned_DataTotals[k,22]=1
	    }else{
	      # If the herds only go a portion of the distance to nearest, then the risk factor is set to the proportion of the distance.
	      if(as.numeric(Cleaned_EffectiveDistance$EffectiveDistance[k])==0){Cleaned_DataTotals[k,22]=0}else{Cleaned_DataTotals[k,22]=as.numeric(Cleaned_DataTotals[k,22])/as.numeric(Cleaned_EffectiveDistance$EffectiveDistance[k])}
	      }
	  }}
	# Now that the units are consistent, we will scale the entire vector to one. 
	# REVIEW: Guard against division by zero. When the column sum is 0 (e.g., all values 
	# are 0 or NA because no seasonal migration data were provided), dividing produces NaN, 
	# which later cascades through is.na() checks and causes every area to be dropped. 
	# In that case, leave the column as-is (it is already all 0s or NAs). 
	migration_sum=sum(as.numeric(Cleaned_DataTotals[,22]),na.rm=TRUE)
	if (migration_sum>0){
	  Cleaned_DataTotals[,22]=as.numeric(Cleaned_DataTotals[,22])/migration_sum
	}
	
	# The complete scaled data table is now named Cleaned_DataTotals.  

# FOLD DATA STREAMS TOGETHER ------------------------------------------------
	
# Join cleaned effective distance data to SubAdmin ID. 	 	
	SubAdmin_Distance=left_join(Subadmin,Cleaned_EffectiveDistance,by=c("SubAdminID"))	
	
# Now join the cleaned data total table to SubAdmin ID. 
	SubAdmin_Data=left_join(SubAdmin_Distance,Cleaned_DataTotals,by=c("SubAdminID","FullName"))

# COMPUTE QUANTITIES FOR EACH OF THE i SPATIAL CELLS. ------------------

	# Initialize data storage for the i spatial cells. 
	BaselinePerCell=rep(0,FullNumberAreas)
	TotalPerCell=rep(0,FullNumberAreas)
	HazardPerCell=rep(0,FullNumberAreas)
	
	# Initialize data storage for the j risk factors in the i cells.
	# Measures of center. (Note: Could be mean or median, whatever is selected/passed by user).
	eBPerCell=rep(0,FullNumberAreas)
	eRFPerCell=matrix(0,FullNumberAreas,20)
	eWPerCell=matrix(0,FullNumberAreas,20)
	
	# Measures of center. 
	# Loop through all i spatial cells to find the measures of center. 
	for (i in 1:FullNumberAreas){
	  
	  Distance=as.numeric(SubAdmin_Data$EffectiveDistance)
	  
	  # Compute the Baseline Per Spatial Cell. 
	  FRAME=read.csv("/data/CWD_Transition_Probability.csv")
	  model=lm(transition_prob~dist,data=FRAME)
	  distance=data.frame(dist=Distance[i])
	  BaselinePerCell[i]=as.numeric(max(0,predict(model,newdata=distance)))
	  
	  # Compute the Total Risk Per Spatial Cell. 
	  DataCountMatrix=SubAdmin_Data[,4:23]
	  
	  # Get the weights to be used per spatial cell. 
	  UsedWeights=c()
	  if(Cleaned_EffectiveDistance$EffectiveDistance[i]==0){UsedWeights=Weights$WeightNear}else{UsedWeights=Weights$WeightFar}
	  
	  	  WeightVector=as.numeric(UsedWeights) # 20 element vector with all numbers.
	  
	  # REVIEW: Only consider SELECTED risk factors (those with non-zero weight) when 
	  # checking for missing data and computing the weighted sum. Previously, this 
	  # block checked all 20 columns for NA, so a missing value in an UNSELECTED risk 
	  # factor would poison the sum (because 0 * NA = NA in R) and the entire area 
	  # would be marked as insufficient. Now, NA in an unselected factor is ignored 
	  # because that factor contributes nothing to the sum anyway. NA in a selected 
	  # factor still produces TotalPerCell = NA so the downstream logic can decide 
	  # how to handle it. 
	  SelectedIdx=which(WeightVector!=0)
	  if (length(SelectedIdx)==0){
	    # No selected risk factors contribute to this area (e.g., all selected factors 
	    # have zero weight for the relevant near/far case). Risk contribution is 0. 
	    TotalPerCell[i]=0
	  }else{
	    SelectedRow=as.numeric(DataCountMatrix[i,SelectedIdx])
	    if (any(is.na(SelectedRow))){
	      TotalPerCell[i]=NA
	    }else{
	      TotalPerCell[i]=as.numeric(sum(WeightVector[SelectedIdx]*SelectedRow))
	    }
	  }

	  # Compute the Total Hazard Per Cell. 
	  if (is.na(TotalPerCell[i])){HazardPerCell[i]=NA}else{HazardPerCell[i]=BaselinePerCell[i]+BaselinePerCell[i]*TotalPerCell[i]}
    
	  # Compute the elasticity of the baseline. 
	  if(is.na(HazardPerCell[i])){eBPerCell[i]=NA}else{
	    if(HazardPerCell[i]==0){eBPerCell[i]=0}else{eBPerCell[i]=(BaselinePerCell[i]/HazardPerCell[i])*(1+sum(WeightVector*DataCountMatrix[i,]))}}
    
	  # Compute the elasticity of each of the j risk factors in cell i.
	  for (j in 1:20){
	    if (is.na(HazardPerCell[i])){eRFPerCell[i,j]=NA}else{
	    if(HazardPerCell[i]==0){eRFPerCell[i,j]=0}else{eRFPerCell[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	  } # End j. 
    
	  # Compute the elasticity of each of the j weights in cell j. 
	  for (j in 1:20){ 
	    if(is.na(HazardPerCell[i])){eWPerCell[i,j]=NA}else{
	    if(HazardPerCell[i]==0){eWPerCell[i,j]=0}else{eWPerCell[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	  } # End j. 
  } # End i areas. 
	
# PREPARE THE OUTPUT FRAME. -------------------------------
	OutputHazards=cbind(Subadmin,BaselinePerCell,TotalPerCell,HazardPerCell,eBPerCell,eRFPerCell,eWPerCell)
	colnames(OutputHazards)=c("SubAdminID","FullName","BaselineRisk","TotalRisk","TotalHazard",
	                               "e_Baseline",
	                               "e_MineralLicks","e_WaterBodies",
	                               "e_Feedgrounds","e_Guzzlers","e_BaitingStations",
	                               "e_AgriculturalPractices","e_CaptiveCervidFacilities",
	                               "e_RehabilitationFacilities","e_Taxidermists","e_Processors",
	                               "e_RenderingFacilities","e_Incinerators","e_FoodBanks",
	                               "e_Landfill","e_Dumping","e_Sheds","e_Roadkill","e_LocalPractices",
	                          "e_FreeRangingCervidDispersal","e_SeasonalMigration",
	                               "ew_MineralLicks","ew_WaterBodies",
	                               "ew_Feedgrounds","ew_Guzzlers","ew_BaitingStations",
	                               "ew_AgriculturalPractices","ew_CaptiveCervidFacilities",
	                               "ew_RehabilitationFacilities","ew_Taxidermists","ew_Processors",
	                               "ew_RenderingFacilities","ew_Incinerators","ew_FoodBanks",
	                               "ew_Landfill","ew_Dumping","ew_Sheds","ew_Roadkill","ew_LocalPractices",
	                               "ew_FreeRangingCervidDispersal","ew_SeasonalMigration")
	
	# Determine which sub-admin areas had insufficient data.
	omitted_areas=OutputHazards[!complete.cases(OutputHazards),]

	# If only a few had insufficient data, report it out. 
	if (dim(omitted_areas)[1]>=1){
	  # Let user know that data frame generation was successful. 
	  line="The dataframe was successfully generated with the set of subadministrative areas that met the data requirements."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  line="However, some subadministrative areas had insufficient data and were therefore removed from this analysis."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  line="Specifically, this analysis did not consider:"
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  line=print(omitted_areas$FullName)
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
	  line="You can proceed with the partial outputs, or you can return to the CWD Data Warehouse and input more data, or select a new set of risk factors."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)}
	
	# Check to see that at least one subadmin area has sufficient data. If not, kill the code. 
	if (dim(omitted_areas)[1]==FullNumberAreas){
	  line="We're sorry. All subadministative areas had insufficient data for this analysis. Return to the CWD Data Warehouse and input more data, or select a new set of risk factors."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  # Quit the session.
	  quit(status=70)}
	
	# Check to see if all areas were eligible for analysis. 
	if (dim(omitted_areas)[1]==0){
	  # Let user know that data frame generation was successful. 
	  line="The data frame was successfully generated for all subadministrative areas."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)}
	
# WRITE THE OUTPUT FILE. -----------------
	OutputHazards_CompleteCases=OutputHazards[complete.cases(OutputHazards),]
	  # Write the file to the attachments directory so it can be picked up by the next script
	  output_csv_path = file.path("", "data", "attachments", "OutputHazards.csv")
	  write.csv(OutputHazards_CompleteCases, output_csv_path, row.names = FALSE)