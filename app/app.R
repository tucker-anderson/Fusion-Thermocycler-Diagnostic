library("shiny")
library("shinyjs")
library("shinyFeedback")
library("stringr")
library("plyr")
library("openxlsx")
library("knitr")
library("RPostgreSQL")

# TODO Integrate openxlsx into download handler better, remove dependency on temporary local file
# TODO Vectorize some GUI calculations to speed up table generation
# TODO Save report/data, SN and Thermocycler SN into backend database
########################################################################################
#----------------------------UI DEFINITIONS-----------------------------------------
########################################################################################
version <- "v1.0.0.2"

ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  
  # App title
  titlePanel("Panther Fusion Thermocycler Diagnostic Tool"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      fixedRow(
        column(4,
          # Input: Input Thermocycler serial number for record keeping.
          textInput("pantherSN", "Panther Serial #", placeholder = "2090000101")
        ),
        column(4,
          # Input: Input Thermocycler serial number for record keeping.
          textInput("thermocyclerSN", "Thermocycler Serial #", placeholder = "J0001D16D0")
        ),
        column(4,
         # Input: Input Thermocycler part number for record keeping.
         textInput("thermocyclerPN", "Thermocycler Part #", placeholder = "ASY-10119, RM-ASY-09123")
        )
      ),
      
      # Input: Select a background file
      fileInput("bgFile", "Select Background Scan File",
                width = "100%",
                multiple = FALSE,
                accept = c("text/csv",
                           # "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Select a file 
      fileInput("peekFile", "Select Peek Scan File",
                width = "100%",
                multiple = FALSE,
                accept = c("text/csv",
                           # "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Select Lid Presence
      fixedRow(
        # column(2),
        column(6, radioButtons("lid", "Lid Present",
                 choices = c("No Lid (Peek sheet used)" = FALSE, "Integrated Lid" = TRUE),
                 inline = TRUE,
                 selected = TRUE),
               offset = 1
               )
        ),
      fixedRow(
        column(4,
          verticalLayout(
            # Input: Select peek Lid Values
            textInput("peek1", "Peek Sheet FAM:", placeholder = "e.g. 10000"),
            textInput("peek2", "Peek Sheet HEX:", placeholder = "e.g. 2000"),
            textInput("peek3", "Peek Sheet ROX:", placeholder = "e.g. 3000"),
            textInput("peek4", "Peek Sheet RED647:", placeholder = "e.g. 400"),
            textInput("peek5", "Peek Sheet RED677:", placeholder = "e.g. 5000")
          ),
          offset = 1
        ),
        column(6,
          verticalLayout(
            # Input: Select Barcode values if lid is present. Autopopulated on peek file upload if barcode information present.
            textInput("barcode1", "Lid Barcode 1:", placeholder = "e.g. 10000000000000000000000"),
            # NULL,
            # min = 10000000000000000000000, 
            # max = 19999999999999999999999),
            textInput("barcode2", "Lid Barcode 2:", placeholder = "e.g. 20000000000000000000000")
          )
        )
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Action Button to run Script
      disabled(actionButton("calculate", "Calculate")),
      
      # Output: Downloader to export excel report
      disabled(downloadButton("download", "Download Excel Report"))
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Tabs w/ plot, summary, and table
      tabsetPanel(id = "tabs", type = "tabs",
                  tabPanel("Peek & Background",
                    tabsetPanel(id = "summary_tabs", type = "tabs",
                     tabPanel("Background Subtracted Peek",
                              tableOutput("bgSubPeekTable"),
                              tableOutput("bgSubPeekFLTable")
                     ),
                     tabPanel("Background Subtracted Peek % Difference",
                              tableOutput("bgSubPercentDiffTable"),
                              tableOutput("bgSubPercentDiffFLTable")
                     )
                    ),
                  ),
                  tabPanel("Background",
                   tabsetPanel(id = "bg_tabs", type = "tabs",
                     tabPanel("Raw Background",
                       tableOutput("bgTable"),
                       tableOutput("bgFLTable"),
                     )
                    ),
                  ),
                  tabPanel("Peek",
                     tabsetPanel(id = "peek_tabs", type = "tabs",
                      tabPanel("Raw Peek",
                       tableOutput("peekTable"),
                       tableOutput("peekFLTable"),
                      ),
                      tabPanel("Peek % Difference",
                        tableOutput("percentDiffTable"),
                        tableOutput("percentDiffFLTable"),
                      )
                    ),
                  ),
                  tabPanel("Report",
                     tableOutput("reportTable"),
                  )
                
      ),
      conditionalPanel(
        condition = "input.tabs != 'Report'",
        radioButtons("Color", "Dye Color:", inline = TRUE, 
                                  c("FAM" = 0,
                                    "HEX" = 1,
                                    "ROX" = 2,
                                    "RED647" = 3,
                                    "RED677" = 4))
      ),
      conditionalPanel(
        condition = "input.tabs != 'Peek & Background' && input.tabs != 'Report' && input$peek_tabs != 'Peek % Difference'",
        radioButtons("Agg", "Well Aggregation Type:", inline = TRUE, 
                                   c("Mean" = "mean",
                                     "Max" = "max",
                                     "Min" = "min",
                                     "Std Dev" = "sd"))
      ),
    ),
  ),
  # App version
  absolutePanel(paste("Version: ", version), style="color:grey; font-size:10px")
)

########################################################################################
#----------------------------SERVER DEFINITION------------------------------------------
########################################################################################

server <- function(input, output, session) {
  isDatabase <- FALSE
  
  if (isDatabase) {
    db <- 'tc_diagnostic'
    host_db <- 'localhost'
    db_port <- '5432'
    db_user <- 'shiny_user'
    db_password <- 'shiny'
    
    con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)
  }
  
  isPantherSN <- reactiveVal(FALSE)
  isThermocyclerSN <- reactiveVal(FALSE)
  isThermocyclerPN <- reactiveVal(FALSE)
  isPeekFile <- reactiveVal(FALSE)
  isBackgroundFile <- reactiveVal(FALSE)

  peekFilemd5 <- reactiveVal("")
  bgFilemd5 <- reactiveVal("")
  
########################################################################################
#----------------------------FUNCTION DEFINITIONS---------------------------------------
########################################################################################

  ######################################################################################
  # function to extract data frame from SSW scan files
  # parameter is SSW scan file 
  # return a data frame of extracted data
  generate_data <- function(input) {
    skip <- str_which(readLines(input), ".*Bank No.*") - 1
    
    data_table <- read.table(input, header = TRUE, sep = ";", fill = TRUE, skip = skip, quote = "'")
    
    data_set <- data.frame(data_table$Color, data_table$Well.No, data_table$RFU)
    names(data_set) <- c("Dye", "Well", "RFU")
    data_set <- data_set[order(data_set$Well) , ]
    
    return(data_set)
  }
  
  ######################################################################################
  # function to take in scan file data from ssw
  # parameter is extracted well data (from generate_data function)
  # return the average of each fluorometer color in a data frame
  average_wells <- function(well_data){
    well_data <- well_data[order(well_data$Dye) , ]
    
    FAM <- subset(well_data, Dye == 0) 
    FAM_ave = ddply(FAM,~Well,summarise,Mean = mean(RFU))
    
    HEX <- subset(well_data, Dye == 1) 
    HEX_ave = ddply(HEX,~Well,summarise,Mean = mean(RFU))
    
    ROX <- subset(well_data, Dye == 2) 
    ROX_ave = ddply(ROX,~Well,summarise,Mean = mean(RFU))
    
    RED647 <- subset(well_data, Dye == 3) 
    RED647_ave = ddply(RED647,~Well,summarise,Mean = mean(RFU))
    
    RED677 <- subset(well_data, Dye == 4) 
    RED677_ave = ddply(RED677,~Well,summarise,Mean = mean(RFU))
    
    stats <- data.frame(FAM_ave,
                       HEX_ave,
                       ROX_ave,
                       RED647_ave,
                       RED677_ave)
    
    stats = data.frame(stats$Well, stats$Mean, stats$Mean.1, stats$Mean.2, stats$Mean.3, stats$Mean.4)
    names(stats) <- c("Well", "FAM Mean", "HEX Mean", "ROX Mean", "RED 647 Mean", "RED 677 Mean")
    
    return(stats)
  }

  ######################################################################################
  # function to take in scan file data from ssw
  # parameter is extracted well data (from generate_data function)
  # return the median of each fluorometer color in a data frame
  fluorometer_med <- function(stats){
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
    
    medians <- rbind(fluorometer1,fluorometer2)
    
    label = data.frame(1:2)
    names(label) = "Fluorometer"
    
    medians = data.frame(label,medians)

    return(medians)
  }
  
  ######################################################################################
  # Function to take in a peek barcode, as a single string
  # parameter is single string from peek lid barcode
  # return expected values for peek sheet as a vector of ints for each color
  read_barcode <- function(barcode){
    FAM <- as.numeric(substr(format(barcode, scientific = FALSE), 3, 6)) * 10
    HEX <- as.numeric(substr(format(barcode, scientific = FALSE), 7, 10))
    ROX <- as.numeric(substr(format(barcode, scientific = FALSE), 11, 14))
    RED647 <- as.numeric(substr(format(barcode, scientific = FALSE), 15, 18))
    RED677 <- as.numeric(substr(format(barcode, scientific = FALSE), 19, 22))
    
    expected_vals = c(FAM, HEX,ROX, RED647, RED677)

    return(expected_vals)
  }
  
  # ######################################################################################
  # # Function to calculate percent delta between peek calibration values and bg subtracted peek scan and normalize against this value
  # # parameters are peek values (from bc or peek lid) and bg subtracted peek scan wells and median values
  # # return percent difference between peek and bg sub peek scan wells as a list
  # val_norm <- function(vals1, vals2, wells, medians){
  #   normalized_wells <- wells
  #   normalized_medians <- medians
  #   # for each fluorometer color
  #   for (i in 1:5) {
  #     # handle if barcode / peek is zero for this fluorometer
  #     if (vals1[i] == 0 || vals2[i] == 0) {
  #       normalized_wells[1:30, i + 1] = 0
  #       normalized_medians[1,i + 1] = 0
  #       
  #       normalized_wells[31:60, i + 1] = 0
  #       normalized_medians[2,i + 1] = 0
  #     }
  #     else {
  #       #calculate percentages for fluorometer 1
  #       normalized_wells[1:30, i + 1] = ((wells[1:30, i + 1] - vals1[i]) / vals1[i] ) * 100
  #       normalized_medians[1,i + 1] = ((medians[1, i + 1] - vals1[i]) / vals1[i] ) * 100
  #       
  #       #calculate percentages for fluorometer 2
  #       normalized_wells[31:60, i + 1] = ((wells[31:60, i + 1] - vals2[i]) / vals2[i] ) * 100
  #       normalized_medians[2,i + 1] = ((medians[2,i + 1] - vals2[i]) / vals2[i]) * 100
  #     }
  #   }
  #   
  #   normalized <- rbind.fill(list(normalized_medians, normalized_wells))
  #   
  #   return(normalized)
  # }

  ######################################################################################
  # Function to check SSW scan filetype
  # parameters are scan file, and string of scan type (same as displayed in file)
  # return boolean of if input type matches detected scan file type 
  check_filetype <- function(input_scan, input_type) {
    input_line <- readLines(input_scan, n = 1)
    scan_line <- str_subset(input_line, "Scan")
    
    scan_type <- str_extract(scan_line, ".+ Scan")
    
    if (scan_type == input_type) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  
  ######################################################################################
  # Function to attempt to get barcodes from peek input file
  # parameter is peek SSW scan file
  # return barcodes as vector of strings. If they cannot be found, return empty vector
  get_barcodes <- function(input_peek){
    input_lines <- readLines(input_peek)
    barcode_lines <- str_subset(input_lines, "Barcode")

    #No barcodes in file
    if (length(barcode_lines) == 0) {
      return(vector())
    }

    barcodes <- str_extract(barcode_lines, "[0-9]{22,}")
    barcode_1 <- barcodes[1]
    barcode_2 <- barcodes[2]
    
    #handle if barcodes were improperly lengthened in SSW export
    if (nchar(barcode_1) == 23) {
      barcode_1 <- paste0(str_sub(barcode_1,1,6), str_sub(barcode_1,8))
    } 
    if (nchar(barcode_2) == 23) {
      barcode_2 <- paste0(str_sub(barcode_2,1,6), str_sub(barcode_2,8))
    } 
    return(c(barcode_1, barcode_2))
  }
  
  ######################################################################################
  # Function to return correct peek values dependent upon if lid is present or not
  # parameter is vectors of barcodes, peek values and a boolean of lid presence
  # return peek vals as vector of numerics If they cannot be found, return empty vector
  check_peek <- function(barcodes, peek_values, is_lid) {
    if (is_lid == TRUE) {
      vals1 <- read_barcode(barcodes[1])
      vals2 <- read_barcode(barcodes[2])
    }
    else {
      vals1 <- as.numeric(c(peek_values[1], peek_values[2], peek_values[3], peek_values[4], peek_values[5]))
      vals2 <- as.numeric(c(peek_values[1], peek_values[2], peek_values[3], peek_values[4], peek_values[5]))
    }
    
    vals <- list(vals1, vals2)
    return(vals)

  }
  
  ######################################################################################
  # Function to attempt to get manual peek lid values from peek input file
  # parameter is peek SSW scan file
  # return peek lid values as vector of strings. If they cannot be found, return empty vector
  get_peek_values <- function(input_peek){
    input_lines <- readLines(input_peek)
    peek_lines <- str_subset(input_lines, "Peek.*Fluorometer")
    
    #No peek lines in file
    if (length(peek_lines) == 0) {
      return(vector())
    }
    
    # extract peek lid values if present
    FAM_1 <- str_match(peek_lines, "FAM: ([0-9]+)")[1,2]
    HEX_1 <- str_match(peek_lines, "HEX: ([0-9]+)")[1,2]
    ROX_1 <- str_match(peek_lines, "ROX: ([0-9]+)")[1,2]
    RED646_1 <- str_match(peek_lines, "RED646: ([0-9]+)")[1,2]
    RED677_1 <- str_match(peek_lines, "RED677: ([0-9]+)")[1,2]
    FAM_2 <- str_match(peek_lines, "FAM: ([0-9]+)")[2,2]
    HEX_2 <- str_match(peek_lines, "HEX: ([0-9]+)")[2,2]
    ROX_2 <- str_match(peek_lines, "ROX: ([0-9]+)")[2,2]
    RED646_2 <- str_match(peek_lines, "RED646: ([0-9]+)")[2,2]
    RED677_2 <- str_match(peek_lines, "RED677: ([0-9]+)")[2,2]
    
    # no peek lid scanned. SSW uses string of four zeroes for peek placeholder
    if (FAM_1 == "0000" | FAM_2 == "0000") {
      return(c("0", "0", "0", "0", "0", "No Peek Lid Data included in file, please manually update."))
    }
    
    # peek values should be the same 
    if (identical(c(FAM_1, HEX_1, ROX_1, RED646_1, RED677_1), c(FAM_2, HEX_2, ROX_2, RED646_2, RED677_2))) {
      return(c(FAM_1, HEX_1, ROX_1, RED646_1, RED677_1, ""))
    }
    # but if they aren't who knows, most likely barcode scan but peek values still included in files
    else {
      return(c(FAM_1, HEX_1, ROX_1, RED646_1, RED677_1, "Peek Lid data included in scan file does not match between fluorometers, please manually update."))
    }
  }
  
  ######################################################################################
  # function to extract data frame from SSW scan files, return shaped differently than generate_data function
  # parameter is SSW scan file
  # return a data frame of extracted data
  generate_data_visual <- function(data, color, fun="mean", fl_fun="median", FL=FALSE) {
    data_set <- data
    
    data_set <- data_set[data_set$Dye == color, ]
    
    data_set$Bank <- floor((data_set$Well - 1) / 5) + 1
    data_set$Bank.Well <- (data_set$Well - 1) %% 5 + 1
    
    placeholderBank <- c(0,0,0,0,0)
    data_reshaped <- data.frame(Bank.Well = 1:5, Bank1 = placeholderBank,
                                Bank2 = placeholderBank,
                                Bank3 = placeholderBank,
                                Bank4 = placeholderBank,
                                Bank5 = placeholderBank,
                                Bank6 = placeholderBank,
                                Bank7 = placeholderBank,
                                Bank8 = placeholderBank,
                                Bank9 = placeholderBank,
                                Bank10 = placeholderBank,
                                Bank11 = placeholderBank,
                                Bank12 = placeholderBank)
    
    for (i in 1:12) {
      bank <- paste0("Bank", i)
      for (j in 1:5) {
        well <- j
        if (fun == "mean") {
          data_reshaped[well, bank] <- mean(data_set[ which(data_set$Bank == i & data_set$Bank.Well == j & data_set$Dye == color),]$RFU)
        }
        if (fun == "min") {
          data_reshaped[well, bank] <- min(data_set[ which(data_set$Bank == i & data_set$Bank.Well == j & data_set$Dye == color),]$RFU)
        }
        if (fun == "max") {
          data_reshaped[well, bank] <- max(data_set[ which(data_set$Bank == i & data_set$Bank.Well == j & data_set$Dye == color),]$RFU)
        }
        if (fun == "sd") {
          data_reshaped[well, bank] <- sd(data_set[ which(data_set$Bank == i & data_set$Bank.Well == j & data_set$Dye == color),]$RFU)
        }
        if (fun == "median") {
          data_reshaped[well, bank] <- median(data_set[ which(data_set$Bank == i & data_set$Bank.Well == j & data_set$Dye == color),]$RFU)
        }
      }
    }
    
    if (FL == TRUE) {
      fl1 <- unlist(data_reshaped[,2:7], use.names=FALSE)
      fl2 <- unlist(data_reshaped[,8:13], use.names=FALSE)
      
      placeholderFL <- c(0,0)
      data_reshaped_fl <- data.frame(Fluorometer = 1:2, Fluorescence = placeholderFL)
      
      if (fl_fun == "mean") {
        data_reshaped_fl[, "Fluorescence"] <- c(mean(fl1), mean(fl2))
      }
      if (fl_fun == "min") {
        data_reshaped_fl[, "Fluorescence"] <- c(min(fl1), min(fl2))
      }
      if (fl_fun == "max") {
        data_reshaped_fl[, "Fluorescence"] <- c(max(fl1), max(fl2))
      }
      if (fl_fun == "sd") {
        data_reshaped_fl[, "Fluorescence"] <- c(sd(fl1), sd(fl2))
      }
      if (fl_fun == "median") {
        data_reshaped_fl[, "Fluorescence"] <- c(median(fl1), median(fl2))
      }
      return(data_reshaped_fl)
    }
    return(data_reshaped)
    # if (FL == TRUE) {
    # 
    #   data_set$Fluorometer <- floor((data_set$Well - 1) / 31) + 1
    #   data_set <- data_set[, .SD[which.mean(RFU)], by = .(Dye, Well)]
    # 
    #   placeholderFL <- c(0,0)
    #   data_reshaped <- data.frame(Fluorometer = 1:2, Fluorescence = placeholderFL)
    #   
    #   for (i in 1:2) {
    #     if (fun == "mean") {
    #       data_reshaped[i, "Fluorescence"] <- mean(data_set[ which(data_set$Fluorometer == i & data_set$Dye == color),]$RFU)
    #     }
    #     if (fun == "min") {
    #       data_reshaped[i, "Fluorescence"] <- min(data_set[ which(data_set$Fluorometer == i & data_set$Dye == color),]$RFU)
    #     }
    #     if (fun == "max") {
    #       data_reshaped[i, "Fluorescence"] <- max(data_set[ which(data_set$Fluorometer == i & data_set$Dye == color),]$RFU)
    #     }
    #     if (fun == "sd") {
    #       data_reshaped[i, "Fluorescence"] <- sd(data_set[ which(data_set$Fluorometer == i & data_set$Dye == color),]$RFU)
    #     }
    #     if (fun == "median") {
    #       data_reshaped[i, "Fluorescence"] <- median(data_set[ which(data_set$Fluorometer == i & data_set$Dye == color),]$RFU)
    #     }
    #   }
    #   return(data_reshaped)
    # }
  }
    
  ######################################################################################
  # Function to extract data frame from SSW scan files, return shaped differently than generate_data function
  # parameters are input from generate_data (peek file, bg file, barcodes, peek, if lid is present) 
  # return a data frame of extracted data
  generate_bg_sub_peek <- function(peek_wells, bg_wells) {
    # peek_means <- data.frame("Dye" = integer(0), "Well" = integer(0), "RFU" = integer(0))
    # bg_means <- data.frame("Dye" = integer(0), "Well" = integer(0), "RFU" = integer(0))
    bg_sub_wells <- data.frame("Dye" = integer(0), "Well" = integer(0), "RFU" = integer(0))

    for (dye in unique(peek_wells$Dye)) {
      for (well in 1:60) {
        bg_sub <- mean(peek_wells[ which(peek_wells$Dye == dye & peek_wells$Well == well),]$RFU) - mean(bg_wells[ which(bg_wells$Dye == dye & bg_wells$Well == well),]$RFU)

        bg_sub_wells[nrow(bg_sub_wells)+1,] <- c(dye, well, bg_sub)
      }
    }
    return(bg_sub_wells)
  }
  
  ######################################################################################
  # Function to extract data frame from SSW scan files, return shaped differently than generate_data function
  # parameters are input from generate_data (peek file, bg file, barcodes, peek, if lid is present) 
  # return a data frame of extracted data
  generate_percent_diff <- function(wells, vals, fun=mean) {
    vals1 <- vals[[1]]
    vals2 <- vals[[2]]
    # peek_means <- data.frame("Dye" = integer(0), "Well" = integer(0), "RFU" = integer(0))
    # bg_means <- data.frame("Dye" = integer(0), "Well" = integer(0), "RFU" = integer(0))
    percent_diff <- data.frame("Dye" = integer(0), "Well" = integer(0), "RFU" = integer(0))
    
    for (dye in unique(wells$Dye)) {
      for (well in 1:60) {
        fl <- floor((well - 1) / 31) + 1
        if (vals1[fl] == 0 || vals2[fl] == 0) {
          percent_diff[nrow(percent_diff)+1,] <- c(dye, well, 0.0)
        }
        else {
          diff <- ((fun(wells[ which(wells$Dye == dye & wells$Well == well),]$RFU) - vals[[fl]][dye+1]) / vals[[fl]][dye+1]) * 100

          percent_diff[nrow(percent_diff)+1,] <- c(dye, well, diff)
        }
      }
    }
    
    return(percent_diff)
  }
  
  ######################################################################################
  # Function to calculate percent delta between peek calibration values and  peek scan and normalize against this value
  # parameters are peek values (from bc or peek lid) and bg subtracted peek scan wells and median values
  # return percent difference between peek scan and peek values as wells
  generate_percent_diff_wells <- function(wells, vals) {
    vals1 <- vals[[1]]
    vals2 <- vals[[2]]
    # percent_diff <- val_norm(vals1, vals2, peek_wells, peek_medians)
    

    # val_norm <- function(vals1, vals2, wells, medians){
    normalized_wells <- wells
    # for each fluorometer color
    for (i in 1:5) {
      # handle if barcode / peek is zero for this fluorometer
      if (vals1[i] == 0 || vals2[i] == 0) {
        normalized_wells[1:30, i + 1] = 0
        # normalized_medians[1,i + 1] = 0
        
        normalized_wells[31:60, i + 1] = 0
        # normalized_medians[2,i + 1] = 0
      }
      else {
        #calculate percentages for fluorometer 1
        normalized_wells[1:30, i + 1] = ((wells[1:30, i + 1] - vals1[i]) / vals1[i] ) * 100
        # normalized_medians[1,i + 1] = ((medians[1, i + 1] - vals1[i]) / vals1[i] ) * 100
        
        #calculate percentages for fluorometer 2
        normalized_wells[31:60, i + 1] = ((wells[31:60, i + 1] - vals2[i]) / vals2[i] ) * 100
        # normalized_medians[2,i + 1] = ((medians[2,i + 1] - vals2[i]) / vals2[i]) * 100
      }
    }
    
    return(normalized_wells)
  }
  
  ######################################################################################
  # Function to calculate percent delta between peek calibration values and  peek scan and normalize against this value
  # parameters are peek values (from bc or peek lid) and bg subtracted peek scan wells and median values
  # return percent difference between peek scan and peek values as fluorometers
  generate_percent_diff_fl <- function(fl, vals) {
    vals1 <- vals[[1]]
    vals2 <- vals[[2]]
    
    normalized_fl <- fl
    
    # for each fluorometer color
    for (i in 1:5) {
      # handle if barcode / peek is zero for this fluorometer
      if (vals1[i] == 0 || vals2[i] == 0) {
        # normalized_wells[1:30, i + 1] = 0
        normalized_fl[1,i + 1] = 0
        
        # normalized_wells[31:60, i + 1] = 0
        normalized_fl[2,i + 1] = 0
      }
      else {
        #calculate percentages for fluorometer 1
        # normalized_wells[1:30, i + 1] = ((wells[1:30, i + 1] - vals1[i]) / vals1[i] ) * 100
        normalized_fl[1,i + 1] = ((fl[1, i + 1] - vals1[i]) / vals1[i] ) * 100
        
        #calculate percentages for fluorometer 2
        # normalized_wells[31:60, i + 1] = ((wells[31:60, i + 1] - vals2[i]) / vals2[i] ) * 100
        normalized_fl[2,i + 1] = ((fl[2,i + 1] - vals2[i]) / vals2[i]) * 100
      }
    }
    
    return(normalized_fl)
  }

  ######################################################################################
  # Function to generate excel workbook with summary of peek and bg scans
  # parameters are input from shiny UI (peek file, bg file, barcodes, peek, if lid is present) 
  # return excel spreadsheet
  generate_workbook <- function(input_peek, input_bg, barcodes, peek_values, is_lid) {
    #first perform some calculations
    peek_dataset <- generate_data(input_peek[["datapath"]])
    peek_wells <- average_wells(peek_dataset)
    peek_medians <- fluorometer_med(peek_wells)
    
    bg_dataset <- generate_data(input_bg[["datapath"]])
    bg_wells <- average_wells(bg_dataset)
    bg_medians <- fluorometer_med(bg_wells)

    bg_sub_wells = average_wells(generate_bg_sub_peek(peek_dataset, bg_dataset))
    bg_sub_medians = fluorometer_med(bg_sub_wells)
    
    vals <- check_peek(barcodes, peek_values, is_lid)
    vals1 <- vals[[1]]
    vals2 <- vals[[2]]
    # if (is_lid == TRUE) {
    #   vals1 <- read_barcode(barcodes[1])
    #   vals2 <- read_barcode(barcodes[2])
    # }
    # else {
    #   vals1 <- as.numeric(c(peek_values[1], peek_values[2], peek_values[3], peek_values[4], peek_values[5]))
    #   vals2 <- as.numeric(c(peek_values[1], peek_values[2], peek_values[3], peek_values[4], peek_values[5]))
    # }
    # percent_diff_30_subtracted <- generate_percent_diff(bg_sub_wells, bg_sub_medians, barcodes, peek_values, is_lid)
    # percent_diff_wells <- generate_percent_diff_wells(peek_wells, barcodes, peek_values, is_lid)
    # percent_diff_fls <- generate_percent_diff_fl(peek_medians, barcodes, peek_values, is_lid)
    # percent_diff_subtracted_wells <- generate_percent_diff_wells(bg_sub_wells, barcodes, peek_values, is_lid)
    # percent_diff_subtracted_fls <- generate_percent_diff_fl(bg_sub_medians, barcodes, peek_values, is_lid)
    percent_diff_wells <- generate_percent_diff_wells(peek_wells, vals)
    percent_diff_fls <- generate_percent_diff_fl(peek_medians, vals)
    percent_diff_subtracted_wells <- generate_percent_diff_wells(bg_sub_wells, vals)
    percent_diff_subtracted_fls <- generate_percent_diff_fl(bg_sub_medians, vals)

    peek <- rbind.fill(list(peek_medians, peek_wells))
    background <- rbind.fill(list(bg_medians, bg_wells)) 
    bg_sub <- rbind.fill(list(bg_sub_medians, bg_sub_wells))
    percent_diff <- rbind.fill(list(percent_diff_fls, percent_diff_wells))
    percent_diff_sub <- rbind.fill(list(percent_diff_subtracted_fls, percent_diff_subtracted_wells))
    
    #generate workbook
    wb <- createWorkbook()
    addWorksheet(wb, "Raw Peek")
    addWorksheet(wb, "Raw Background") 
    addWorksheet(wb, "Background Subtracted")
    addWorksheet(wb, "Percent Diff")
    addWorksheet(wb, "Percent Diff Subtracted") 
    addWorksheet(wb, "Barcodes")
    
    writeData(wb, "Raw Peek", peek, startCol = 1, startRow = 1, xy = NULL,
              colNames = TRUE, rowNames = FALSE)
    writeData(wb, "Raw Background", background, startCol = 1, startRow = 1, xy = NULL,
                colNames = TRUE, rowNames = FALSE)
    writeData(wb, "Background Subtracted", bg_sub, startCol = 1, startRow = 1, xy = NULL,
                colNames = TRUE, rowNames = FALSE)
    writeData(wb,  "Percent Diff", percent_diff, startCol = 1, startRow = 1, xy = NULL,
              colNames = TRUE, rowNames = FALSE)
    writeData(wb, "Percent Diff Subtracted", percent_diff_sub, startCol = 1, startRow = 1, xy = NULL,
                colNames = TRUE, rowNames = FALSE)
    
    #FORMAT AS NUMBER
    s <- createStyle(numFmt = "0.00")
    addStyle(wb, "Raw Peek", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Raw Peek", style = s, rows = 4:63, cols = 8:12, gridExpand = TRUE)
    addStyle(wb, "Raw Background", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Raw Background", style = s, rows = 4:63, cols = 8:12, gridExpand = TRUE)
    addStyle(wb, "Background Subtracted", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Background Subtracted", style = s, rows = 4:63, cols = 8:12, gridExpand = TRUE)
    addStyle(wb, "Percent Diff", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Percent Diff", style = s, rows = 4:63, cols = 8:12, gridExpand = TRUE)
    addStyle(wb, "Percent Diff Subtracted", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Percent Diff Subtracted", style = s, rows = 4:63, cols = 8:12, gridExpand = TRUE)

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
    
    if (is_lid == FALSE) {
      conditionalFormatting(wb, "Percent Diff Subtracted",  cols = 2:6, rows = 2:3, rule = ">=30", style = NULL)
      conditionalFormatting(wb, "Percent Diff Subtracted", 2:6, 2:3, rule = "<=-30", style = NULL)
      
      conditionalFormatting(wb, "Percent Diff Subtracted", 8:12, 4:63, rule = ">=30", style = NULL)
      conditionalFormatting(wb, "Percent Diff Subtracted", 8:12, 4:63, rule = "<=-30", style = NULL)
    }
    return(wb)
  }

  ######################################################################################
  # function to update database with Panther SN
  # parameters are database connection and Panther SN as string 
  # return nothing
  update_database_panther_sn <- function(conn, pantherSN) {
    dbSendQuery(conn, paste0("INSERT INTO public.panther_info (panther_sn) VALUES ('", pantherSN, "') ON CONFLICT (panther_sn) DO NOTHING"))
  }
  
  ######################################################################################
  # Function to update database with Fusion SN
  # parameters are database connection and Fusion SN as string 
  # return nothing
  update_database_tc_sn <- function(conn, tcSN) {
    #ensure input is all uppercase to avoid casing conflicts in database
    tcSN <- toupper(tcSN)
    dbSendQuery(conn, paste0("INSERT INTO public.tc_info (tc_sn) VALUES ('", tcSN, "') ON CONFLICT (tc_sn) DO NOTHING"))
  }

########################################################################################
#---------------------------------EVENT OBSERVERS---------------------------------------
########################################################################################

  ######################################################################################
  # Event Observers for lid presence. Disable numeric fields if no lid selected
  observe({
    if (input$lid == TRUE) {
      disable("peek1")
      disable("peek2")
      disable("peek3")
      disable("peek4")
      disable("peek5")
      enable("barcode1")
      enable("barcode2")
    }                               
    else {
      enable("peek1")
      enable("peek2")
      enable("peek3")
      enable("peek4")
      enable("peek5")
      disable("barcode1")
      disable("barcode2")
    }
  })
  
  ######################################################################################
  # Event observers for SN text input
  observeEvent(input$pantherSN, {
    if (length(input$pantherSN) == 0) {
      hideFeedback("pantherSN")
      isPantherSN(FALSE)
    }
    else if (input$pantherSN == "NA") {
      hideFeedback("pantherSN")
      isPantherSN(TRUE) 
      showFeedbackSuccess("pantherSN", color = "#337ab7")
    }
    else if (!grepl("^209[0-9]{7}$", input$pantherSN) & !grepl("^912[0-9]{7}$", input$pantherSN)) {     
      hideFeedback("pantherSN")
      showFeedbackWarning(
        inputId = "pantherSN",
        text = "SN does not match known format(s)!"
      )
      isPantherSN(FALSE)
    }
    else {
      hideFeedback("pantherSN")
      isPantherSN(TRUE)
      showFeedbackSuccess("pantherSN", color = "#337ab7")
    }
  })
  
  observeEvent(input$thermocyclerSN, {
    if (length(input$thermocyclerSN) == 0) {
      hideFeedback("thermocyclerSN")
      isThermocyclerSN(FALSE)
    }
    else if (input$thermocyclerSN == "NA") {
      hideFeedback("thermocyclerSN")
      isThermocyclerSN(TRUE) 
      showFeedbackSuccess("thermocyclerSN", color = "#337ab7")
    }
    else if (!grepl("^J[0-9]{4}[A-Z][0-9]{2}[A-Z][0-9]$", input$thermocyclerSN)) { #eg J0001D16D0
      hideFeedback("thermocyclerSN")
      showFeedbackWarning(
        inputId = "thermocyclerSN",
        text = "SN does not match known format(s)!"
      )
      isThermocyclerSN(FALSE)
    }
    else {
      hideFeedback("thermocyclerSN")
      isThermocyclerSN(TRUE)
      showFeedbackSuccess("thermocyclerSN", color = "#337ab7")
    }
  })
  
  observeEvent(input$thermocyclerPN, {
    if (length(input$thermocyclerPN) == 0) {
      hideFeedback("thermocyclerPN")
      isThermocyclerPN(FALSE)
    }
    else if (input$thermocyclerPN == "NA") {
      hideFeedback("thermocyclerPN")
      isThermocyclerPN(TRUE) 
      showFeedbackSuccess("thermocyclerPN", color = "#337ab7")
    }
    else if (!grepl("^[A-Z]*-?ASY-[0-9]{5}$", input$thermocyclerPN)) { #eg ASY-07574, RM-ASY-09123, RASY-10119
      hideFeedback("thermocyclerPN")
      showFeedbackWarning(
        inputId = "thermocyclerPN",
        text = "PN does not match known format(s)!"
      )
      isThermocyclerPN(FALSE)
    }
    else {
      hideFeedback("thermocyclerPN")
      isThermocyclerPN(TRUE)
      showFeedbackSuccess("thermocyclerPN", color = "#337ab7")
    }
  })
  
  ######################################################################################
  # Event Observers for peek File upload
  # Attempt to verify that file is a peek scan and not background and populate barcodes etc.
  observeEvent(input$peekFile, {
    is_peek <- check_filetype(input$peekFile[["datapath"]], "Peek Lid Scan")
    is_bg <- check_filetype(input$peekFile[["datapath"]], "Background Scan")
    is_pm_bg <- check_filetype(input$peekFile[["datapath"]], "Panther Main BG Scan")
    if (is_peek) {
      showNotification("Peek Scan File detected.")
      updateTabsetPanel(session, "tabs", selected = "Peek")
      isPeekFile(TRUE)
      peekFilemd5(toString(tools::md5sum(input$peekFile[["datapath"]])))
      
      barcodes <- get_barcodes(input$peekFile[["datapath"]])
      peek <- get_peek_values(input$peekFile[["datapath"]])

      # Update lid presence option automatically based on barcode detection.
      if (length(barcodes) > 0) {
        updateRadioButtons(session, "lid", selected = TRUE)
        updateNumericInput(session, "barcode1", value = barcodes[1])
        updateNumericInput(session, "barcode2", value = barcodes[2])
        #if peek values are also provided in file, then also put them in fields
        if (length(peek) > 0) {
          updateNumericInput(session, "peek1", value = peek[1])
          updateNumericInput(session, "peek2", value = peek[2])
          updateNumericInput(session, "peek3", value = peek[3])
          updateNumericInput(session, "peek4", value = peek[4])
          updateNumericInput(session, "peek5", value = peek[5])
        }
        # otherwise translate barcode into peek value fields
        else {
          bc_vals = read_barcode(barcodes[1])
          updateNumericInput(session, "peek1", value = bc_vals[1])
          updateNumericInput(session, "peek2", value = bc_vals[2])
          updateNumericInput(session, "peek3", value = bc_vals[3])
          updateNumericInput(session, "peek4", value = bc_vals[4])
          updateNumericInput(session, "peek5", value = bc_vals[5])
        }
      }
      # if no barcodes and only peeklid values are in file, update peek and switch to no lid mode
      else if (length(peek) > 0) {
        updateRadioButtons(session, "lid", selected = FALSE)
        reset("barcode1")
        reset("barcode2")
        updateNumericInput(session, "peek1", value = peek[1])
        updateNumericInput(session, "peek2", value = peek[2])
        updateNumericInput(session, "peek3", value = peek[3])
        updateNumericInput(session, "peek4", value = peek[4])
        updateNumericInput(session, "peek5", value = peek[5])
      }
      # no barcodes or peek values detected in file, alert user
      else {
        alert("Peek file has no barcodes / peek lid values. Please manually select information.")
      }
    }
    else if (is_bg | is_pm_bg) {
      alert("Background Scan File detected. Please upload a Peek scan file.")
      reset("peekFile")
      isPeekFile(FALSE)
    }
    else {
      alert("Unknown Scan File detected. Please upload correct scan file.")
      reset("peekFile")
      isPeekFile(FALSE)
    }
  })
  
  # Attempt to verify that file is a background scan and not peek
  observeEvent(input$bgFile, {
    is_peek <- check_filetype(input$bgFile[["datapath"]], "Peek Lid Scan")
    is_bg <- check_filetype(input$bgFile[["datapath"]], "Background Scan")
    is_pm_bg <- check_filetype(input$bgFile[["datapath"]], "Panther Main BG Scan")
    if (is_peek) {
      showNotification("Peek Scan File detected.")
      alert("Peek Scan File detected. Please upload a Background scan file.")
      reset("bgFile")
      isBackgroundFile(FALSE)
    }
    else if (is_bg | is_pm_bg) {
      showNotification("Background Scan File detected.")
      updateTabsetPanel(session, "tabs", selected = "Background")
      isBackgroundFile(TRUE)
      bgFilemd5(toString(tools::md5sum(input$bgFile[["datapath"]])))
    }
    else {
      alert("Unknown Scan File detected. Please upload correct scan file.")
      reset("bgFile")
      isBackgroundFile(FALSE)
    }
  })

  ######################################################################################
  # Event Observers for tab navigation
  # when navigated to, some data should be calculated/displayed
  observeEvent({input$peek_tabs == input$`Raw Peek`
    input$Color
    input$Agg}, {
    if (length(input$peekFile) != 0) {
      peek_dataset <- generate_data(input$peekFile[["datapath"]])
      peek_visual <- generate_data_visual(peek_dataset, color = input$Color, fun = input$Agg)
      output$peekTable <- renderTable(peek_visual)
      peek_visual_FL <- generate_data_visual(peek_dataset, color = input$Color, fun = input$Agg, FL=TRUE)
      output$peekFLTable <- renderTable(peek_visual_FL)
    }
  })

  observeEvent({input$peek_tabs == input$`Peek % Difference`
    input$Color}, {
      if (length(input$peekFile) != 0) {
        peek_dataset <- generate_data(input$peekFile[["datapath"]])
        vals <- check_peek(c(input$barcode1, input$barcode2), c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5), input$lid)
        percent_diff <- generate_percent_diff(peek_dataset, vals)

        percent_diff_visual <- generate_data_visual(percent_diff, color = input$Color)
        output$percentDiffTable <- renderTable(percent_diff_visual)
        percent_diff_visual_FL <- generate_data_visual(percent_diff, color = input$Color, FL=TRUE)
        output$percentDiffFLTable <- renderTable(percent_diff_visual_FL)
      }
    })

  observeEvent({input$tabs == input$`Raw Background`
    input$Color
    input$Agg}, {
    if (length(input$bgFile) != 0) {
      bg_dataset <- generate_data(input$bgFile[["datapath"]])
      bg_visual <- generate_data_visual(bg_dataset, color = input$Color, fun = input$Agg)
      output$bgTable <- renderTable(bg_visual)
      bg_visual_FL <- generate_data_visual(bg_dataset, color = input$Color, fun = input$Agg, FL=TRUE)
      output$bgFLTable <- renderTable(bg_visual_FL)
    }
  })
  
  observeEvent({input$tabs == input$`Peek & Background` && input$summary_tabs == input$`Background subtracted Peek`
    input$Color}, {
      if (length(input$bgFile) != 0 & length(input$peekFile) != 0) {
        peek_dataset <- generate_data(input$peekFile[["datapath"]])
        bg_dataset <- generate_data(input$bgFile[["datapath"]])
        bg_sub_wells <- generate_bg_sub_peek(peek_dataset, bg_dataset)
        
        bg_sub_visual <- generate_data_visual(bg_sub_wells, color = input$Color)
        output$bgSubPeekTable <- renderTable(bg_sub_visual)
        bg_sub_visual_FL <- generate_data_visual(bg_sub_wells, color = input$Color, fun ="mean", FL=TRUE)
        output$bgSubPeekFLTable <- renderTable(bg_sub_visual_FL)
      }
    })

  observeEvent({input$summary_tabs == input$`Background Subtracted Peek % Difference`
    input$Color}, {
      if (length(input$bgFile) != 0 & length(input$peekFile) != 0) {
        peek_dataset <- generate_data(input$peekFile[["datapath"]])
        bg_dataset <- generate_data(input$bgFile[["datapath"]])
        vals <- check_peek(c(input$barcode1, input$barcode2), c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5), input$lid)

        bg_sub_wells <- generate_bg_sub_peek(peek_dataset, bg_dataset)
        bg_sub_percent_diff <- generate_percent_diff(bg_sub_wells, vals)
        
        bg_sub_percent_diff_visual <- generate_data_visual(bg_sub_percent_diff, color = input$Color)
        output$bgSubPercentDiffTable <- renderTable(bg_sub_percent_diff_visual)
        bg_sub_percent_diff_visual_FL <- generate_data_visual(bg_sub_percent_diff, color = input$Color, FL=TRUE)
        output$bgSubPercentDiffFLTable <- renderTable(bg_sub_percent_diff_visual_FL)
      }
    })

  ######################################################################################
  # Event Observers for diagnostic calculation and download
  observeEvent(input$calculate, {
    # summary_visual <- generate_summary(input$peekFile[["datapath"]],
    #                                    input$bgFile[["datapath"]],
    #                                    c(input$barcode1, input$barcode2),
    #                                    c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5),
    #                                    input$lid)
    # output$summaryTable <- renderTable(summary_visual)
    
    # wb <- generate_workbook(input$peekFile, input$bgFile,
    #                         c(input$barcode1, input$barcode2),
    #                         c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5),
    #                         input$lid)
    # 
    # filename <- "./reports/ThermocyclerDiagnosticReport.xlsx"
    # 
    # if (file.exists(filename)) {
    #   file.remove(filename)
    # }
    # 
    # saveWorkbook(wb, filename)

    enable("download")
    updateTabsetPanel(session, "tabs", selected = "Summary")

    reset("bgFile")
    isBackgroundFile(FALSE)
    reset("peekFile")
    isPeekFile(FALSE)
    
    disable("calculate")
    
    if (isDatabase) {
      update_database_panther_sn(con, input$pantherSN)
      update_database_tc_sn(con, input$thermocyclerSN)
      #TODO update db with TC PN as well
    }
  })
  
  # only enable calculate if both files uploaded correctly & SNs input in correct format
  observe({
    if (isPeekFile() && isBackgroundFile() && isThermocyclerSN() && isThermocyclerSN() && isPantherSN()) {
      enable("calculate")
    }
    else {
      disable("calculate")
    }
  })
  
########################################################################################
#------------------------------OUTPUT DEFINITIONS---------------------------------------
########################################################################################

  # download button to extract xlsx diagnostic file from server
  output$download <- downloadHandler(
      filename = "ThermocyclerDiagnosticReport.xlsx",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        wb <- generate_workbook(input$peekFile, input$bgFile,
                                c(input$barcode1, input$barcode2),
                                c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5),
                                input$lid)
        
        filepath <- "./reports/ThermocyclerDiagnosticReport.xlsx"
        # filepath <- file.path(tempdir(), "ThermocyclerDiagnosticReport.xlsx")
        
        if (file.exists(filepath)) {
          file.remove(filepath)
        }
        
        # tempReport <- file.path(tempdir(), "ThermocyclerDiagnosticReport.xlsx")
        saveWorkbook(wb, filepath)
        file.copy("./reports/ThermocyclerDiagnosticReport.xlsx", file)
        # file.copy("ThermocyclerDiagnosticReport.xlsx", tempReport, overwrite = TRUE)
      }
  )
}

shinyApp(ui = ui, server = server)
