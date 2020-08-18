#setwd("C:\\Users\\iank0618\\Box\\TC RMAs\\TC RMA List\\J82")

# VC Edits!
# Automatic naming

library("plyr")
library("openxlsx")
#library("xlsx")

### fill these out ###

root = "C:/Users/vc0416/Desktop/Under Work/TC Scans J144 Temp/20200706"
#root = "//crete/teams/CoreTeams/Radium/PreProductionIntegration/ThermocyclerLEDInvestigation/InHouse PEEK Data/J0390 P735 F51/20200619"
setwd(root)


USE_LID = TRUE # set to true for multicore, false for peek sheets

PEEK_SHEET = c(26000, 3450, 1350, 790, 1140) #fill these out if using a PEEK sheet
#PEEK_SHEET = c(35000, 4850, 2700, 1870, 2700) #fill these out if using a PEEK sheet, MAR Values



input_PEEK = "RFUTest_0008_PEEKLid.csv"
input_background = "RFUTest_0009_BackgroundSheet.csv" 
input_barcode = "LidBarcode.txt" # Only used if cannot find it in the file
ZeroPadBarcode = TRUE

output_file = paste0(basename(root), ".xlsx") #name the output file something that denotates whether or not you used a PEEK sheet or lid
if (USE_LID) {
  output_file = paste0("PEEKLid_", output_file)
} else {
  output_file = paste0("PEEKSheet_", output_file)
}
#lids are NOT background subtracted
#PEEK sheets ARE background subtracted


###defining a function to take in a single file and return the averaged fluorescence per well###
average_wells <- function(well_data){
  
  well_data <- well_data[order(well_data$Dye) , ]
  
  FAM <- subset(well_data, Dye == 0) 
  FAM_ave = ddply(FAM,~Well,summarise,Mean=mean(RFU))
  
  HEX <- subset(well_data, Dye == 1) 
  HEX_ave = ddply(HEX,~Well,summarise,Mean=mean(RFU))
  
  ROX <- subset(well_data, Dye == 2) 
  ROX_ave = ddply(ROX,~Well,summarise,Mean=mean(RFU))
  
  RED647 <- subset(well_data, Dye == 3) 
  RED647_ave = ddply(RED647,~Well,summarise,Mean=mean(RFU))
  
  RED677 <- subset(well_data, Dye == 4) 
  RED677_ave = ddply(RED677,~Well,summarise,Mean=mean(RFU))
  
  
  
  stats <-data.frame(FAM_ave,
                     HEX_ave,
                     ROX_ave,
                     RED647_ave,
                     RED677_ave)
  
  stats = data.frame(stats$Well, stats$Mean, stats$Mean.1, stats$Mean.2, stats$Mean.3, stats$Mean.4)
  names(stats) <- c("Well", "FAM Mean", "HEX Mean", "ROX Mean", "RED 647 Mean", "RED 677 Mean")
  
  return(stats)
}

fluormeter_med <- function(stats){
  flurometer1 = stats[1:30,]
  FAM_median = median(flurometer1$`FAM Mean`)
  HEX_median = median(flurometer1$`HEX Mean`)
  ROX_median = median(flurometer1$`ROX Mean`)
  RED647_median = median(flurometer1$`RED 647 Mean`)
  RED677_median = median(flurometer1$`RED 677 Mean`)
  
  fluorometer1 = data.frame(FAM_median,HEX_median,ROX_median,RED647_median,RED677_median)
  
  
  flurometer2 = stats[31:60,]
  FAM_median = median(flurometer2$`FAM Mean`)
  HEX_median = median(flurometer2$`HEX Mean`)
  ROX_median = median(flurometer2$`ROX Mean`)
  RED647_median = median(flurometer2$`RED 647 Mean`)
  RED677_median = median(flurometer2$`RED 677 Mean`)
  
  fluorometer2 = data.frame(FAM_median,HEX_median,ROX_median,RED647_median,RED677_median)
  
  medians <-rbind(fluorometer1,fluorometer2)
  
  label = data.frame(1:2)
  names(label) = "Fluorometer"
  
  medians = data.frame(label,medians)
  return(medians)
}

read_barcode <- function(barcode){
  ## takes in a barcode, as a string, and returns expected values for PEEK sheet ##
  FAM = as.numeric(substr(barcode, 3,6)) * 10
  HEX = as.numeric(substr(barcode, 7,11))
  ROX = as.numeric(substr(barcode, 12,15))
  #RED647 = as.numeric(substr(barcode, 17,19))
  RED647 = as.numeric(substr(barcode, 16,19))
  RED677 = as.numeric(substr(barcode, 20,23))
  
  expected_vals = c(FAM, HEX,ROX, RED647, RED677)
  return(expected_vals)
}


check_vals <- function(vals1, vals2, background_sub_wells, background_sub_medians){
  
  for (i in 1:5) {
    
    #calculate percentages for fluormeter 1
    background_sub_wells[1:30, i + 1] = ((background_sub_wells[1:30, i + 1] - vals1[i]) / vals1[i] )* 100
    background_sub_medians[1,i + 1] = ((background_sub_medians[1, i + 1] - vals1[i]) / vals1[i] )* 100
    
    #calculate percentages for fluormeter 2
    background_sub_wells[31:60, i + 1] = ((background_sub_wells[31:60, i + 1] - vals2[i]) / vals2[i] )* 100
    background_sub_medians[2,i + 1] = ((background_sub_medians[2,i+1] - vals2[i]) / vals2[i] )* 100
  }
  
  
  list <- list(background_sub_medians, background_sub_wells) #combine both into a list
  percent_diff <- do.call(rbind.fill, list) #bind them
  
  return(percent_diff)
  
}

###Code starts here###

if (file.exists(output_file)) {
  file.remove(output_file) #remove output file incase it already exists
}

##create a PEEK dataset then process PEEK wells and PEEK fluorometers##
VersionCheck = readLines(input_PEEK, n = 10)
if (grepl("Bank No", VersionCheck[1])) {
  text_dat = readLines(input_barcode, n=2)
  # Force correct barcodes due to zero added (SSW bug)
  if ( ZeroPadBarcode ) {
  text_dat[1] = paste0(str_sub(text_dat[1],start = 1, end = 6), "0", str_sub(text_dat[1],start = 7))
  text_dat[2] = paste0(str_sub(text_dat[2],start = 1, end = 6), "0", str_sub(text_dat[2],start = 7))
  }
  barcode1 = read_barcode(text_dat[1])
  barcode2 = read_barcode(text_dat[2])
  PEEKSkip = 0
  BackSkip = 0
} else {
  text_dat = readLines(input_PEEK , n = 4, skip = 3)
  barcode1 = read_barcode(substr(text_dat[3], 24,46))
  barcode2 = read_barcode(substr(text_dat[4], 24,46))
  PEEKSkip = 5
  BackSkip = 3
}

PEEK_dataset <- read.table(input_PEEK, header=TRUE, sep=";", fill = TRUE, skip = PEEKSkip)

PEEK_dataset = data.frame(PEEK_dataset$Color, PEEK_dataset$Well.No, PEEK_dataset$RFU)
names(PEEK_dataset) <- c("Dye", "Well", "RFU")
PEEK_dataset <- PEEK_dataset[order(PEEK_dataset$Well) , ]

PEEK_wells = average_wells(PEEK_dataset)
PEEK_medians = fluormeter_med(PEEK_wells)

##do the same thing for the background data##
background_dat <- read.table(input_background, header=TRUE, sep=";", fill = TRUE, skip = BackSkip)
background_dat = data.frame(background_dat$Color, background_dat$Well.No, background_dat$RFU)
names(background_dat) <- c("Dye", "Well", "RFU")
background_dat <- background_dat[order(background_dat$Well) , ]

background_wells = average_wells(background_dat)
background_medians = fluormeter_med(background_wells)

##


background_sub_wells = data.frame( c(1:60), (PEEK_wells[,2:6] - background_wells[,2:6])) 
names(background_sub_wells) <- c("Well", "FAM Mean", "HEX Mean", "ROX Mean", "RED 647 Mean", "RED 677 Mean")

background_sub_medians = fluormeter_med(background_sub_wells)



if(USE_LID == TRUE){
  vals1 = barcode1
  vals2 = barcode2

}
if(USE_LID == FALSE){
  vals1 = PEEK_SHEET
  vals2 = PEEK_SHEET
}



percent_diff_30_subtracted <- check_vals(vals1, vals2, background_sub_wells, background_sub_medians)
percent_diff_30 <- check_vals(vals1, vals2, PEEK_wells, PEEK_medians)




list <- list(background_medians, background_wells) #combine both into a list
background <- do.call(rbind.fill, list) #bind them


list <- list(PEEK_medians, PEEK_wells) #combine both into a list
PEEK <- do.call(rbind.fill, list) #bind them




# Write the first data set in a new workbook
#write.xlsx(PEEK, file = output_file,
#           sheetName = "Raw PEEK", append = FALSE,row.names = FALSE)
# Add a second data set in a new worksheet
#write.xlsx(background, file = output_file, 
#           sheetName="Raw Background", append=TRUE,row.names = FALSE)
# Add a third data set
#write.xlsx(percent_diff_30, file = output_file, 
#           sheetName="Thirty Percent Diff", append=TRUE,row.names = FALSE)

#write.xlsx(percent_diff_30_subtracted, file = output_file, 
#           sheetName="Thirty Percent Diff Subtracted", append=TRUE,row.names = FALSE)


wb <- createWorkbook()
addWorksheet(wb, "Raw PEEK")
if ( !USE_LID ) { addWorksheet(wb, "Raw Background") }
addWorksheet(wb, "Percent Diff")
if ( !USE_LID ) { addWorksheet(wb, "Percent Diff Subtracted") }
addWorksheet(wb, "Barcodes")


writeData(wb, "Raw PEEK", PEEK, startCol = 1, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE)

if ( !USE_LID ) {
writeData(wb, "Raw Background", background, startCol = 1, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE)
}

writeData(wb,  "Percent Diff", percent_diff_30, startCol = 1, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE)

if ( !USE_LID ) {
writeData(wb, "Percent Diff Subtracted", percent_diff_30_subtracted, startCol = 1, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE)
}


barcode_label = c('LED Color', 'FAM', 'HEX', 'ROX', 'RED647', 'RED677')

writeData(wb, "Barcodes", barcode_label, startCol = 1, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE)
writeData(wb, "Barcodes", 'Barcode 1', startCol = 2, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE)
writeData(wb, "Barcodes", 'Barcode 2', startCol = 3, startRow = 1, xy = NULL,
          colNames = TRUE, rowNames = FALSE)
writeData(wb, "Barcodes", vals1, startCol = 2, startRow = 2, xy = NULL,
          colNames = TRUE, rowNames = FALSE)
writeData(wb, "Barcodes", vals2, startCol = 3, startRow = 2, xy = NULL,
          colNames = TRUE, rowNames = FALSE)




conditionalFormatting(wb, "Percent Diff",  cols = 2:6, rows = 2:3, rule = ">=30", style = NULL)
conditionalFormatting(wb, "Percent Diff", 2:6, 2:3, rule = "<=-30", style = NULL)

conditionalFormatting(wb, "Percent Diff", 8:12, 4:63, rule = ">=30", style = NULL)
conditionalFormatting(wb, "Percent Diff", 8:12, 4:63, rule = "<=-30", style = NULL)

if ( !USE_LID ) {
conditionalFormatting(wb, "Percent Diff Subtracted",  cols = 2:6, rows = 2:3, rule = ">=30", style = NULL)
conditionalFormatting(wb, "Percent Diff Subtracted", 2:6, 2:3, rule = "<=-30", style = NULL)

conditionalFormatting(wb, "Percent Diff Subtracted", 8:12, 4:63, rule = ">=30", style = NULL)
conditionalFormatting(wb, "Percent Diff Subtracted", 8:12, 4:63, rule = "<=-30", style = NULL)
}


saveWorkbook(wb,output_file)



                  
                  
                  