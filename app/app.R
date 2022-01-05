library("shiny")
library("shinyjs")
library("shinyFeedback")
library("stringr")
library("plyr")
library("openxlsx")
library("htmltools")
library(openssl)
library(DBI)


# TODO Integrate openxlsx into download handler better, remove dependency on temporary local file
# TODO Vectorize some GUI calculations to speed up table generation
################################################################################
#----------------------------UI DEFINITIONS-------------------------------------
################################################################################
version <- "v2.0.0.0"

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
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select a peek file 
      fileInput("peekFile", "Select Peek Scan File",
                width = "100%",
                multiple = FALSE,
                accept = c("text/csv",
                           # "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Select Lid Presence
      fixedRow(
        # column(2),
        column(10, radioButtons("lid", "Lid Present",
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
      
      fixedRow(
        column(4, numericInput("bgMax", "Background Max", value = 1.80, min = 0.0, max = 5.0, step = 0.1), offset = 1),
        column(4, numericInput("peekMin", "Peek Min", value = 0.60, min = 0.0, max = 1.0, step = 0.1), offset = 1),
        column(4, numericInput("ledMin", "LED Min", value = 0.40, min = 0.0, max = 1.0, step = 0.1), offset = 1),
        column(4, numericInput("peekDecay", "Peek Decay", value = 0.30, min = 0.0, max = 1.0, step = 0.1), offset = 1)
      ),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Action Button to run Script
      disabled(actionButton("calculate", "Calculate")),
      
      # Output: excel Downloader to export excel report
      disabled(downloadButton("excel_download", "Download Excel Report")),
      
      # Output: Downloader to export excel report
      disabled(downloadButton("html_download", "Download HTML Report")),
      
      
      fixedRow(
        # column(2),
        column(6, checkboxInput("dev", "Developer Options",
                               value = FALSE),
               offset = 0
        )
      ),
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Tabs w/ plot, summary, and table
      tabsetPanel(id = "tabs", type = "tabs",
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
                  tabPanel(value="peek_and_bg", "Peek & Background",
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
                  tabPanel("Report",
                     htmlOutput("report"),
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
        condition = "input.tabs != 'Peek & Background' && input.tabs != 'Report' && input.peek_tabs != 'Peek % Difference'",
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

################################################################################
#----------------------------SERVER DEFINITION----------------------------------
################################################################################

server <- function(input, output, session) {
  isDatabase <- TRUE
  isDev <- FALSE
  
  isPantherSN <- reactiveVal(FALSE)
  isThermocyclerSN <- reactiveVal(FALSE)
  isThermocyclerPN <- reactiveVal(FALSE)
  isPeekFile <- reactiveVal(FALSE)
  isBackgroundFile <- reactiveVal(FALSE)

  isAccepted <- FALSE
  
  if (isDev) {
    updateTextInput(session, "pantherSN", value = "NA")
    updateTextInput(session, "thermocyclerSN", value = "NA")
    updateTextInput(session, "thermocyclerPN", value = "NA")
    
    updateCheckboxInput(session, "dev", value = TRUE)
  }
  
################################################################################
#----------------------------FUNCTION DEFINITIONS-------------------------------
################################################################################

  ##############################################################################
  # function to set dev options for ease of unit testing.
  setDev <- function() {
    input$dev = TRUE
    updateCheckboxInput(
      # session = session,
      "DEV",
      value = TRUE
    )
  }
  
  ##############################################################################
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

  ##############################################################################
  # function to start / stop timestamp from SSW scan files
  # parameter is SSW scan file 
  # return a string of timestamp for ingestion into postgres
  get_scan_timestamp <- function(input, n=1) {
    skip <- str_which(readLines(input), ".*Bank No.*") - 1
    
    data_table <- read.table(input, header = TRUE, sep = ";", fill = TRUE, skip = skip, quote = "'")
    timestamps <- data_table$SystemTime

    if (n == 0) {
      timestamp <- timestamps[1]
    }
    else if (n < 0) {
      timestamp <- rev(timestamps)[abs(n)]
    }
    else {
      timestamp <- timestamps[n]
    }
    strtime <- strftime(timestamp, "%Y-%m-%d   %H:%M:%S", usetz=FALSE)

    #if poorly formatted file, set to start of time...  
    if (length(strtime) == 0) {
      strtime <- strftime("1960-01-01   00:00:00", "%Y-%m-%d   %H:%M:%S", usetz=FALSE)
    }
    
    return(strtime)
  }
  
  ##############################################################################
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
    
    stats = data.frame(stats$Well, 
                      floor((stats$Well - 1) / 5) + 1, 
                      (stats$Well - 1) %% 5 + 1,
                      stats$Mean, stats$Mean.1, stats$Mean.2, stats$Mean.3, stats$Mean.4)
    names(stats) <- c("Overall Well", "Bank", "Bank Well", "FAM Mean", "HEX Mean", "ROX Mean", "RED 647 Mean", "RED 677 Mean")
    
    return(stats)
  }

  ##############################################################################
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
  
  ##############################################################################
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
  
  # ##############################################################################
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

  ##############################################################################
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
  
  ##############################################################################
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
  
  ##############################################################################
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
  
  ##############################################################################
  # Function to attempt to get manual peek lid values from peek input file
  # parameter is peek SSW scan file
  # return peek lid values as vector of strings. If they cannot be found, return empty vector
  get_peek_values <- function(input_peek, both_fl=FALSE){
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

    if (both_fl == TRUE) {
      return(c(FAM_1, HEX_1, ROX_1, RED646_1, RED677_1, FAM_2, HEX_2, ROX_2, RED646_2, RED677_2))
    }
    
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
 
  ##############################################################################
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
    
  ##############################################################################
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
  
  ##############################################################################
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
  
  ##############################################################################
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
        normalized_wells[1:30, i + 3] = 0
        # normalized_medians[1,i + 1] = 0
        
        normalized_wells[31:60, i + 3] = 0
        # normalized_medians[2,i + 1] = 0
      }
      else {
        #calculate percentages for fluorometer 1
        normalized_wells[1:30, i + 3] = ((wells[1:30, i + 3] - vals1[i]) / vals1[i] ) * 100
        # normalized_medians[1,i + 1] = ((medians[1, i + 1] - vals1[i]) / vals1[i] ) * 100
        
        #calculate percentages for fluorometer 2
        normalized_wells[31:60, i + 3] = ((wells[31:60, i + 3] - vals2[i]) / vals2[i] ) * 100
        # normalized_medians[2,i + 1] = ((medians[2,i + 1] - vals2[i]) / vals2[i]) * 100
      }
    }
    
    return(normalized_wells)
  }
  
  ##############################################################################
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

  ##############################################################################
  # Function to generate excel workbook with summary of peek and bg scans
  # parameters are input from shiny UI (peek file, bg file, barcodes, peek, if lid is present) 
  # return excel spreadsheet
  generate_workbook <- function(input) {
    input_peek <- input$peekFile
    input_bg <- input$bgFile
    barcodes <- c(input$barcode1, input$barcode2)
    peek_values <- c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5)
    is_lid <- input$lid
    is_dev <- input$dev
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
    addStyle(wb, "Raw Peek", style = s, rows = 4:63, cols = 10:14, gridExpand = TRUE)
    addStyle(wb, "Raw Background", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Raw Background", style = s, rows = 4:63, cols = 10:14, gridExpand = TRUE)
    addStyle(wb, "Background Subtracted", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Background Subtracted", style = s, rows = 4:63, cols = 10:14, gridExpand = TRUE)
    addStyle(wb, "Percent Diff", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Percent Diff", style = s, rows = 4:63, cols = 10:14, gridExpand = TRUE)
    addStyle(wb, "Percent Diff Subtracted", style = s, rows = 2:3, cols = 2:6, gridExpand = TRUE)
    addStyle(wb, "Percent Diff Subtracted", style = s, rows = 4:63, cols = 10:14, gridExpand = TRUE)

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
    
    conditionalFormatting(wb, "Percent Diff", 10:14, 4:63, rule = ">=30", style = NULL)
    conditionalFormatting(wb, "Percent Diff", 10:14, 4:63, rule = "<=-30", style = NULL)
    
    if (is_lid == FALSE) {
      conditionalFormatting(wb, "Percent Diff Subtracted",  cols = 2:6, rows = 2:3, rule = ">=30", style = NULL)
      conditionalFormatting(wb, "Percent Diff Subtracted", 2:6, 2:3, rule = "<=-30", style = NULL)
      
      conditionalFormatting(wb, "Percent Diff Subtracted", 10:14, 4:63, rule = ">=30", style = NULL)
      conditionalFormatting(wb, "Percent Diff Subtracted", 10:14, 4:63, rule = "<=-30", style = NULL)
    }
    
    if (is_dev == FALSE) {
      removeWorksheet(wb,"Percent Diff Subtracted")
      removeWorksheet(wb, "Background Subtracted")
    }
    
    return(wb)
  }

  ##############################################################################
  # Function to generate dataframe of bg failures (as per manufacturing spec)
  # parameters are input from shiny UI and bg manufacturing threshold 
  # return dataframe
  get_bg_failures <- function(input, threshold) {
    bg_dataset <- generate_data(input$bgFile[["datapath"]])
    threshold <- as.double(threshold)

    placeholderBank <- c("","","","","")
    bg_failures <- data.frame(placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank)
    for (i in 0:4) {
      bg_visual <- generate_data_visual(bg_dataset, color = i, fun = "mean", FL = FALSE)[,-1]
      bg_visual_fl <- generate_data_visual(bg_dataset, color = i, fl_fun= "median", FL = TRUE)[,-1]
      bg_visual[1:5, 1:6] <- bg_visual[1:5, 1:6] / bg_visual_fl[1]
      bg_visual[1:5, 7:12] <- bg_visual[1:5, 7:12] / bg_visual_fl[2]
      
      c <- switch(i+1, "FAM", "HEX", "ROX", "647", "677")
      
      for (j in 1:12) {
        bg_failures[,j] <- ifelse(bg_visual[,j] > threshold, 
                                    str_c(bg_failures[,j], c, ": ", formatC(bg_visual[,j], 2, format = "f"), "\n"), 
                                    str_c(bg_failures[,j], ""))
      }
    }
    return(bg_failures)
  }

  ##############################################################################
  # Function to generate dataframe of peek failures (as per manufacturing spec)
  # parameters are input from shiny UI and peek manufacturing threshold 
  # return dataframe
  get_peek_failures <- function(input, threshold) {
    peek_dataset <- generate_data(input$peekFile[["datapath"]])
    threshold <- as.double(threshold)

    placeholderBank <- c("","","","","")
    peek_failures <- data.frame(placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank)
    for (i in 0:4) {
      peek_visual <- generate_data_visual(peek_dataset, color = i, fun = "mean", FL = FALSE)[,-1]
      peek_visual_fl <- generate_data_visual(peek_dataset, color = i, fl_fun= "mean", FL = TRUE)[,-1]
      peek_visual[1:5, 1:6] <- (peek_visual_fl[1] - peek_visual[1:5, 1:6]) / peek_visual_fl[1]
      peek_visual[1:5, 7:12] <- (peek_visual_fl[2] - peek_visual[1:5, 7:12]) / peek_visual_fl[2]
      
      c <- switch(i+1, "FAM", "HEX", "ROX", "647", "677")
      
      for (j in 1:12) {
        peek_failures[,j] <- ifelse(peek_visual[,j] > threshold, 
                                    str_c(peek_failures[,j], c, ": ", formatC(peek_visual[,j], 2, format = "f"), "\n"), 
                                    str_c(peek_failures[,j], ""))
      }
    }
    return(peek_failures)
  }

  ##############################################################################
  # Function to generate dataframe of peek decay failures (as per Ian's spec)
  # parameters are input from shiny UI and peek manufacturing threshold 
  # return dataframe
  get_decay_failures <- function(input, threshold) {
    peek_dataset <- generate_data(input$peekFile[["datapath"]])
    vals <- check_peek(c(input$barcode1, input$barcode2), c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5), input$lid)
    percent_diff <- generate_percent_diff(peek_dataset, vals)

    threshold <- as.double(threshold)

    placeholderBank <- c("","","","","")
    decay_failures <- data.frame(placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank,
                                placeholderBank)
    for (i in 0:4) {
      percent_diff_visual <- generate_data_visual(percent_diff, color = i)[,-1]
      # peek_visual <- generate_data_visual(peek_dataset, color = i, fun = "mean", FL = FALSE)[,-1]
      # peek_visual[1:5, 1:6] <- (peek_visual_fl[1] - peek_visual[1:5, 1:6]) / peek_visual_fl[1]
      # peek_visual[1:5, 7:12] <- (peek_visual_fl[2] - peek_visual[1:5, 7:12]) / peek_visual_fl[2]
      
      c <- switch(i+1, "FAM", "HEX", "ROX", "647", "677")
      
      for (j in 1:12) {
        decay_failures[,j] <- ifelse(abs(percent_diff_visual[,j]) >= (threshold*100), 
                                    str_c(decay_failures[,j], c, ": ", formatC(percent_diff_visual[,j], 2, format = "f"), "\n"), 
                                    str_c(decay_failures[,j], ""))
      }
    }
    return(decay_failures)
  }

    ##############################################################################
  # Function to generate dataframe of peek decay failures (as per Ian's spec)
  # parameters are input from shiny UI and peek manufacturing threshold 
  # return dataframe
  get_decay_fl_failures <- function(input, threshold) {
    peek_dataset <- generate_data(input$peekFile[["datapath"]])
    vals <- check_peek(c(input$barcode1, input$barcode2), c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5), input$lid)
    percent_diff <- generate_percent_diff(peek_dataset, vals)

    threshold <- as.double(threshold)

    placeholderFl <- c("")
    decay_fl_failures <- data.frame(placeholderFl,
                                placeholderFl)
    for (i in 0:4) {
      peek_visual_fl <- generate_data_visual(percent_diff, color = i, fun = "median", FL = TRUE)[,-1]
      
      c <- switch(i+1, "FAM", "HEX", "ROX", "647", "677")
      
      for (j in 1:2) {
        decay_fl_failures[1,j] <- ifelse(peek_visual_fl[j] >= (threshold*100), 
                                    str_c(decay_fl_failures[1,j], c, ": ", formatC(peek_visual_fl[j], 2, format = "f"), "\n"), 
                                    str_c(decay_fl_failures[1,j], ""))
      }
    }
    return(decay_fl_failures)
  }
  #   for (i in 0:4) {
  #     percent_diff_visual <- generate_data_visual(percent_diff, color = i)[,-1]
  #     # peek_visual <- generate_data_visual(peek_dataset, color = i, fun = "mean", FL = FALSE)[,-1]
  #     # peek_visual[1:5, 1:6] <- (peek_visual_fl[1] - peek_visual[1:5, 1:6]) / peek_visual_fl[1]
  #     # peek_visual[1:5, 7:12] <- (peek_visual_fl[2] - peek_visual[1:5, 7:12]) / peek_visual_fl[2]
      
  #     c <- switch(i+1, "FAM", "HEX", "ROX", "647", "677")
      
  #     for (j in 1:12) {
  #       decay_failures[,j] <- ifelse(abs(percent_diff_visual[,j]) >= (threshold*100), 
  #                                   str_c(decay_failures[,j], c, ": ", formatC(percent_diff_visual[,j], 2, format = "f"), "\n"), 
  #                                   str_c(decay_failures[,j], ""))
  #     }
  #   }
  #   return(decay_failures)
  # }

  ##############################################################################
  # Function to generate dataframe of LED failures (as per manufacturing spec)
  # parameters are input from shiny UI and LED manufacturing threshold 
  # return dataframe
  get_led_failures <- function(input, threshold) {
    peek_dataset <- generate_data(input$peekFile[["datapath"]])
    vals <- check_peek(c(input$barcode1, input$barcode2), c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5), input$lid)
    threshold <- as.double(threshold)

    placeholderFl <- c("")
    led_failures <- data.frame(placeholderFl,
                                placeholderFl)
    for (i in 0:4) {
      peek_visual_fl <- generate_data_visual(peek_dataset, color = i, fun = "mean", FL = TRUE)[,-1]
      peek_visual_fl[1] <- peek_visual_fl[1] / vals[[1]][i+1]
      peek_visual_fl[2] <- peek_visual_fl[2] / vals[[2]][i+1]
      
      c <- switch(i+1, "FAM", "HEX", "ROX", "647", "677")
      
      for (j in 1:2) {
        led_failures[1,j] <- ifelse(peek_visual_fl[j] < threshold, 
                                    str_c(led_failures[1,j], c, ": ", formatC(peek_visual_fl[j], 2, format = "f"), "\n"), 
                                    str_c(led_failures[1,j], ""))
      }
    }
    return(led_failures)
  }

  generate_html_report <- function(input, tester='Unknown') {
    peek_failures <- get_peek_failures(input, input$peekMin)
    bg_failures <- get_bg_failures(input, input$bgMax)
    led_failures <- get_led_failures(input, input$ledMin)
    decay_failures <- get_decay_failures(input, input$peekDecay)
    decay_fl_failures <- get_decay_fl_failures(input, input$peekDecay)
    
    acceptance <- ifelse(any(str_detect(bg_failures, ": "),
                        str_detect(peek_failures, ": "),
                        str_detect(led_failures, ": "),
                        str_detect(decay_failures, ": "),
                        str_detect(decay_fl_failures, ": ")),
                        "FAIL", "PASS")
                    
    if (acceptance == "PASS") {
      isAccepted <- TRUE
    }
    else {
      isAccepted <- FALSE
    }
    
    report <- htmlTemplate("report-template.html",
                 tool_version = version,
                 panther_sn = input$pantherSN,
                 tc_sn = input$thermocyclerSN,
                 tc_pn = input$thermocyclerPN,
                 tc_firmware = "Unknown",
                 bg_1_1 = bg_failures[1,1],
                 bg_1_2 = bg_failures[1,2],
                 bg_1_3 = bg_failures[1,3],
                 bg_1_4 = bg_failures[1,4],
                 bg_1_5 = bg_failures[1,5],
                 bg_1_6 = bg_failures[1,6],
                 bg_1_7 = bg_failures[1,7],
                 bg_1_8 = bg_failures[1,8],
                 bg_1_9 = bg_failures[1,9],
                 bg_1_10 = bg_failures[1,10],
                 bg_1_11 = bg_failures[1,11],
                 bg_1_12 = bg_failures[1,12],
                 bg_2_1 = bg_failures[2,1],
                 bg_2_2 = bg_failures[2,2],
                 bg_2_3 = bg_failures[2,3],
                 bg_2_4 = bg_failures[2,4],
                 bg_2_5 = bg_failures[2,5],
                 bg_2_6 = bg_failures[2,6],
                 bg_2_7 = bg_failures[2,7],
                 bg_2_8 = bg_failures[2,8],
                 bg_2_9 = bg_failures[2,9],
                 bg_2_10 = bg_failures[2,10],
                 bg_2_11 = bg_failures[2,11],
                 bg_2_12 = bg_failures[2,12],
                 bg_3_1 = bg_failures[3,1],
                 bg_3_2 = bg_failures[3,2],
                 bg_3_3 = bg_failures[3,3],
                 bg_3_4 = bg_failures[3,4],
                 bg_3_5 = bg_failures[3,5],
                 bg_3_6 = bg_failures[3,6],
                 bg_3_7 = bg_failures[3,7],
                 bg_3_8 = bg_failures[3,8],
                 bg_3_9 = bg_failures[3,9],
                 bg_3_10 = bg_failures[3,10],
                 bg_3_11 = bg_failures[3,11],
                 bg_3_12 = bg_failures[3,12],
                 bg_4_1 = bg_failures[4,1],
                 bg_4_2 = bg_failures[4,2],
                 bg_4_3 = bg_failures[4,3],
                 bg_4_4 = bg_failures[4,4],
                 bg_4_5 = bg_failures[4,5],
                 bg_4_6 = bg_failures[4,6],
                 bg_4_7 = bg_failures[4,7],
                 bg_4_8 = bg_failures[4,8],
                 bg_4_9 = bg_failures[4,9],
                 bg_4_10 = bg_failures[4,10],
                 bg_4_11 = bg_failures[4,11],
                 bg_4_12 = bg_failures[4,12],
                 bg_5_1 = bg_failures[5,1],
                 bg_5_2 = bg_failures[5,2],
                 bg_5_3 = bg_failures[5,3],
                 bg_5_4 = bg_failures[5,4],
                 bg_5_5 = bg_failures[5,5],
                 bg_5_6 = bg_failures[5,6],
                 bg_5_7 = bg_failures[5,7],
                 bg_5_8 = bg_failures[5,8],
                 bg_5_9 = bg_failures[5,9],
                 bg_5_10 = bg_failures[5,10],
                 bg_5_11 = bg_failures[5,11],
                 bg_5_12 = bg_failures[5,12],
                 peek_1_1 = peek_failures[1,1],
                 peek_1_2 = peek_failures[1,2],
                 peek_1_3 = peek_failures[1,3],
                 peek_1_4 = peek_failures[1,4],
                 peek_1_5 = peek_failures[1,5],
                 peek_1_6 = peek_failures[1,6],
                 peek_1_7 = peek_failures[1,7],
                 peek_1_8 = peek_failures[1,8],
                 peek_1_9 = peek_failures[1,9],
                 peek_1_10 = peek_failures[1,10],
                 peek_1_11 = peek_failures[1,11],
                 peek_1_12 = peek_failures[1,12],
                 peek_2_1 = peek_failures[2,1],
                 peek_2_2 = peek_failures[2,2],
                 peek_2_3 = peek_failures[2,3],
                 peek_2_4 = peek_failures[2,4],
                 peek_2_5 = peek_failures[2,5],
                 peek_2_6 = peek_failures[2,6],
                 peek_2_7 = peek_failures[2,7],
                 peek_2_8 = peek_failures[2,8],
                 peek_2_9 = peek_failures[2,9],
                 peek_2_10 = peek_failures[2,10],
                 peek_2_11 = peek_failures[2,11],
                 peek_2_12 = peek_failures[2,12],
                 peek_3_1 = peek_failures[3,1],
                 peek_3_2 = peek_failures[3,2],
                 peek_3_3 = peek_failures[3,3],
                 peek_3_4 = peek_failures[3,4],
                 peek_3_5 = peek_failures[3,5],
                 peek_3_6 = peek_failures[3,6],
                 peek_3_7 = peek_failures[3,7],
                 peek_3_8 = peek_failures[3,8],
                 peek_3_9 = peek_failures[3,9],
                 peek_3_10 = peek_failures[3,10],
                 peek_3_11 = peek_failures[3,11],
                 peek_3_12 = peek_failures[3,12],
                 peek_4_1 = peek_failures[4,1],
                 peek_4_2 = peek_failures[4,2],
                 peek_4_3 = peek_failures[4,3],
                 peek_4_4 = peek_failures[4,4],
                 peek_4_5 = peek_failures[4,5],
                 peek_4_6 = peek_failures[4,6],
                 peek_4_7 = peek_failures[4,7],
                 peek_4_8 = peek_failures[4,8],
                 peek_4_9 = peek_failures[4,9],
                 peek_4_10 = peek_failures[4,10],
                 peek_4_11 = peek_failures[4,11],
                 peek_4_12 = peek_failures[4,12],
                 peek_5_1 = peek_failures[5,1],
                 peek_5_2 = peek_failures[5,2],
                 peek_5_3 = peek_failures[5,3],
                 peek_5_4 = peek_failures[5,4],
                 peek_5_5 = peek_failures[5,5],
                 peek_5_6 = peek_failures[5,6],
                 peek_5_7 = peek_failures[5,7],
                 peek_5_8 = peek_failures[5,8],
                 peek_5_9 = peek_failures[5,9],
                 peek_5_10 = peek_failures[5,10],
                 peek_5_11 = peek_failures[5,11],
                 peek_5_12 = peek_failures[5,12],
                 
                 fl_1 = led_failures[1,1],
                 fl_2 = led_failures[1,2],
                 
                 decay_1_1 = decay_failures[1,1],
                 decay_1_2 = decay_failures[1,2],
                 decay_1_3 = decay_failures[1,3],
                 decay_1_4 = decay_failures[1,4],
                 decay_1_5 = decay_failures[1,5],
                 decay_1_6 = decay_failures[1,6],
                 decay_1_7 = decay_failures[1,7],
                 decay_1_8 = decay_failures[1,8],
                 decay_1_9 = decay_failures[1,9],
                 decay_1_10 = decay_failures[1,10],
                 decay_1_11 = decay_failures[1,11],
                 decay_1_12 = decay_failures[1,12],
                 decay_2_1 = decay_failures[2,1],
                 decay_2_2 = decay_failures[2,2],
                 decay_2_3 = decay_failures[2,3],
                 decay_2_4 = decay_failures[2,4],
                 decay_2_5 = decay_failures[2,5],
                 decay_2_6 = decay_failures[2,6],
                 decay_2_7 = decay_failures[2,7],
                 decay_2_8 = decay_failures[2,8],
                 decay_2_9 = decay_failures[2,9],
                 decay_2_10 = decay_failures[2,10],
                 decay_2_11 = decay_failures[2,11],
                 decay_2_12 = decay_failures[2,12],
                 decay_3_1 = decay_failures[3,1],
                 decay_3_2 = decay_failures[3,2],
                 decay_3_3 = decay_failures[3,3],
                 decay_3_4 = decay_failures[3,4],
                 decay_3_5 = decay_failures[3,5],
                 decay_3_6 = decay_failures[3,6],
                 decay_3_7 = decay_failures[3,7],
                 decay_3_8 = decay_failures[3,8],
                 decay_3_9 = decay_failures[3,9],
                 decay_3_10 = decay_failures[3,10],
                 decay_3_11 = decay_failures[3,11],
                 decay_3_12 = decay_failures[3,12],
                 decay_4_1 = decay_failures[4,1],
                 decay_4_2 = decay_failures[4,2],
                 decay_4_3 = decay_failures[4,3],
                 decay_4_4 = decay_failures[4,4],
                 decay_4_5 = decay_failures[4,5],
                 decay_4_6 = decay_failures[4,6],
                 decay_4_7 = decay_failures[4,7],
                 decay_4_8 = decay_failures[4,8],
                 decay_4_9 = decay_failures[4,9],
                 decay_4_10 = decay_failures[4,10],
                 decay_4_11 = decay_failures[4,11],
                 decay_4_12 = decay_failures[4,12],
                 decay_5_1 = decay_failures[5,1],
                 decay_5_2 = decay_failures[5,2],
                 decay_5_3 = decay_failures[5,3],
                 decay_5_4 = decay_failures[5,4],
                 decay_5_5 = decay_failures[5,5],
                 decay_5_6 = decay_failures[5,6],
                 decay_5_7 = decay_failures[5,7],
                 decay_5_8 = decay_failures[5,8],
                 decay_5_9 = decay_failures[5,9],
                 decay_5_10 = decay_failures[5,10],
                 decay_5_11 = decay_failures[5,11],
                 decay_5_12 = decay_failures[5,12],
                 
                 decay_1 = decay_fl_failures[1,1],
                 decay_2 = decay_fl_failures[1,2],
                 
                 date = as.character(date()),
                 
                 result = acceptance,

                 name = tester
    )
    return(report)
  }

  # check_wells <- function(wells, threshold) {
  #   
  # }
  # 
  # check_fl_decay <- function(dataset, threshold) {
  # 
  # }

  ##############################################################################
  # function to update database with Panther SN
  # parameters are database connection and Panther SN as string 
  # return nothing
  update_database_panther_sn <- function(conn, pantherSN) {
    # dbSendQuery(conn, paste0("CREATE TABLE IF NOT EXISTS public.panther_info
    # (
    #   id SERIAL PRIMARY KEY,
    #   panther_sn VARCHAR(10)
    # )
    # "))
    rs <- dbSendStatement(conn, paste0("INSERT INTO public.panther_info (panther_sn) VALUES ('", pantherSN, "') ON CONFLICT (panther_sn) DO NOTHING;"))
    # while(!dbHasCompleted(res)){
    # }
    dbClearResult(rs)
  }
  
  ##############################################################################
  # Function to update database with Fusion SN
  # parameters are database connection and Fusion SN and PN as string 
  # return nothing
  update_database_tc_sn <- function(conn, tcSN, tcPN) {
    # dbSendQuery(conn, paste0("CREATE TABLE IF NOT EXISTS public.tc_info
    # (
    #   id SERIAL PRIMARY KEY,
    #   tc_sn VARCHAR(10),
    #   tc_pn VARCHAR(20)
    # )
    # "))
    
    #ensure input is all uppercase to avoid casing conflicts in database
    tcSN <- toupper(tcSN)
    tcPN <- toupper(tcPN)
    rs <- dbSendStatement(conn, paste0("INSERT INTO public.tc_info (tc_sn, tc_pn) VALUES ('",paste0(tcSN,"'",",","'",tcPN),"') ON CONFLICT (tc_sn, tc_pn) DO NOTHING;"))
    # while(!dbHasCompleted(res)){
    # }
    dbClearResult(rs)
  }
  
  ##############################################################################
  # Function to update database with SSW scan files
  # parameters are database connection and scan file path, scan file type (eg bg or peek)
  # return nothing
  update_database_scan <- function(conn, pantherSN, tcSN, tcPN, fp) {
    # dbSendQuery(conn, paste0(CREATE TABLE IF NOT EXISTS public.scans(
    #     scan_id INT GENERATED ALWAYS AS IDENTITY,
    #     panther_id INT NOT NULL,
    #     tc_id INT NOT NULL,
    #     md5 VARCHAR(32) NOT NULL,
    #     is_peek BOOLEAN NOT NULL,
    #     is_bg BOOLEAN NOT NULL,
    #     is_pm_bg BOOLEAN NOT NULL,
    #     barcode_1 VARCHAR(23),
    #     barcode_2 VARCHAR(23),
    #     peek_1_fam VARCHAR(5),
    #     peek_1_hex VARCHAR(5),
    #     peek_1_rox VARCHAR(5),
    #     peek_1_646 VARCHAR(5),
    #     peek_1_677 VARCHAR(5),
    #     peek_2_fam VARCHAR(5),
    #     peek_2_hex VARCHAR(5),
    #     peek_2_rox VARCHAR(5),
    #     peek_2_646 VARCHAR(5),
    #     peek_2_677 VARCHAR(5),
    #     tc_firmware VARCHAR(10),
    #     start_timestamp TIMESTAMP NOT NULL,
    #     end_timestamp TIMESTAMP NOT NULL,
    #     file BYTEA NOT NULL,
    #     PRIMARY KEY(scan_id),
    #     UNIQUE(md5),
    #     CONSTRAINT fk_panther
    #       FOREIGN KEY(panther_id) 
    # 	  REFERENCES panther_info(panther_id),
    #     CONSTRAINT fk_tc
    #       FOREIGN KEY(tc_id) 
    # 	  REFERENCES tc_info(tc_id)
    # );
    md5 <- as.character(md5(file(fp, open = "rb")))
    file_b <- file(fp, open = "rb")

    is_peek <- check_filetype(fp, "Peek Lid Scan")
    is_bg <- check_filetype(fp, "Background Scan")
    is_pm_bg <- check_filetype(fp, "Panther Main BG Scan")

    if (is_peek == TRUE) {
      barcode_vals <- get_barcodes(fp)
      barcode_1 <- barcode_vals[1]
      barcode_2 <- barcode_vals[2]
      peek_vals <- get_peek_values(fp, both_fl=TRUE)
      peek_1_fam <- peek_vals[1]
      peek_1_hex <- peek_vals[2]
      peek_1_rox <- peek_vals[3]
      peek_1_647 <- peek_vals[4]
      peek_1_677 <- peek_vals[5]
      peek_2_fam <- peek_vals[6]
      peek_2_hex <- peek_vals[7]
      peek_2_rox <- peek_vals[8]
      peek_2_647 <- peek_vals[9]
      peek_2_677 <- peek_vals[10]
    }
    else {
      barcode_1 <- ""
      barcode_2 <- ""
      peek_1_fam <- ""
      peek_1_hex <- ""
      peek_1_rox <- ""
      peek_1_647 <- ""
      peek_1_677 <- ""
      peek_2_fam <- ""
      peek_2_hex <- ""
      peek_2_rox <- ""
      peek_2_647 <- ""
      peek_2_677 <- ""
    }

    #TODO Implement firmware upload from file or user input from GUI
    # for now, place empty value in db
    tc_firmware <- ""

    start_timestamp <- get_scan_timestamp(fp, n=1)
    end_timestamp <- get_scan_timestamp(fp, n=-1)

    panther_select <- paste0("(SELECT panther_id FROM public.panther_info WHERE panther_sn='", pantherSN, "')")
    tc_select <- paste0("(SELECT tc_id FROM public.tc_info WHERE (tc_sn, tc_pn)= ('",paste0(tcSN,"'",",","'",tcPN),"') )")

    rs <- dbSendStatement(conn, paste0("INSERT INTO public.scans (panther_id, tc_id, md5, is_peek, is_bg, is_pm_bg, barcode_1, barcode_2, peek_1_fam, peek_1_hex, peek_1_rox, peek_1_647, peek_1_677, peek_2_fam, peek_2_hex, peek_2_rox, peek_2_647, peek_2_677, tc_firmware, start_timestamp, end_timestamp, file) VALUES (",
                                paste0(panther_select, ",",
                                      tc_select, ",","'",
                                      md5,"'",",","'",
                                      is_peek,"'",",","'",
                                      is_bg,"'",",","'",
                                      is_pm_bg,"'",",","'",
                                      barcode_1,"'",",","'",
                                      barcode_2,"'",",","'",
                                      peek_1_fam,"'",",","'",
                                      peek_1_hex,"'",",","'",
                                      peek_1_rox,"'",",","'",
                                      peek_1_647,"'",",","'",
                                      peek_1_677,"'",",","'",
                                      peek_2_fam,"'",",","'",
                                      peek_2_hex,"'",",","'",
                                      peek_2_rox,"'",",","'",
                                      peek_2_647,"'",",","'",
                                      peek_2_677,"'",",","'",
                                      tc_firmware,"'",",","'",
                                      start_timestamp,"'",",","'",
                                      end_timestamp,"'",",","'",
                                      file_b),
                                      "') ON CONFLICT (md5) DO NOTHING;"))
    # while(!dbHasCompleted(res)){
    # }
    dbClearResult(rs)
  }
  ##############################################################################
  # Function to update database with SSW scan files
  # parameters are database connection and scan file path, scan file type (eg bg or peek)
  # return nothing
  update_database_report <- function(conn, input, version, acceptance, excel_file, html_file) {
    # dbSendQuery(conn, paste0(CREATE TABLE IF NOT EXISTS public.reports(
    #     report_id INT GENERATED ALWAYS AS IDENTITY,
    #     peek_id INT NOT NULL,
    #     bg_id INT NOT NULL,
    #     is_dev BOOLEAN NOT NULL,
    #     barcode_1 VARCHAR(23),
    #     barcode_2 VARCHAR(23),
    #     peek_fam VARCHAR(5),
    #     peek_hex VARCHAR(5),
    #     peek_rox VARCHAR(5),
    #     peek_647 VARCHAR(5),
    #     peek_677 VARCHAR(5),
    #     bg_max NUMERIC(3, 2) NOT NULL,
    #     peek_min NUMERIC(3, 2) NOT NULL,
    #     led_min NUMERIC(3, 2) NOT NULL,
    #     peek_decay NUMERIC(3, 2) NOT NULL,
    #     acceptance BOOLEAN NOT NULL,
    #     app_version VARCHAR NOT NULL,
    #     excel_md5 VARCHAR(32) NOT NULL,
    #     html_md5 VARCHAR(32) NOT NULL,
    #     excel_file BYTEA NOT NULL,
    #     html_file BYTEA NOT NULL,
    #     PRIMARY KEY(report_id),
    #     UNIQUE(excel_md5, html_md5),
    #     CONSTRAINT fk_peek
    #       FOREIGN KEY(peek_id) 
    # 	  REFERENCES scans(scan_id),
    #     CONSTRAINT fk_bg
    #       FOREIGN KEY(bg_id) 
    # 	  REFERENCES scans(scan_id)
    # );
    bg_md5 <- as.character(md5(file(input$bgFile[["datapath"]], open = "rb")))
    peek_md5 <- as.character(md5(file(input$peekFile[["datapath"]], open = "rb")))
    # html_file <- generate_html_report(input)
    # excel_file <- generate_workbook(input)
    # html_md5 <- as.character(md5(html_file))
    # excel_md5 <- as.character(md5(excel_file))
    # html_md5 <- ""
    # excel_md5 <- ""

    bg_select <- paste0("(SELECT scan_id FROM public.scans WHERE md5='", bg_md5, "')")
    peek_select <- paste0("(SELECT scan_id FROM public.scans WHERE md5='", peek_md5, "')")

    rs <- dbSendStatement(conn, paste0("INSERT INTO public.reports (peek_id, bg_id, is_dev, barcode_1, barcode_2, peek_fam, peek_hex, peek_rox, peek_647, peek_677, bg_max, peek_min, led_min, peek_decay, acceptance, app_version, report_ts, excel_file, html_file) VALUES (",
                                paste0(peek_select, ",",
                                      bg_select, ",","'",
                                      input$dev,"'",",","'",
                                      input$barcode1,"'",",","'",
                                      input$barcode2,"'",",","'",
                                      input$peek1,"'",",","'",
                                      input$peek2,"'",",","'",
                                      input$peek3,"'",",","'",
                                      input$peek4,"'",",","'",
                                      input$peek5,"'",",","'",
                                      input$bgMax,"'",",","'",
                                      input$peekMin,"'",",","'",
                                      input$ledMin,"'",",","'",
                                      input$peekDecay,"'",",","'",
                                      acceptance,"'",",","'",
                                      version,"'",",",
                                      "NOW()",",","'",
                                      # excel_md5,"'",",","'",
                                      # html_md5,"'",",","'",
                                      excel_file,"'",",","'",
                                      html_file),
                                      "');"))#" ON CONFLICT (excel_md5, html_md5) DO NOTHING;"))
    dbClearResult(rs)
  }

################################################################################
#---------------------------------EVENT OBSERVERS-------------------------------
################################################################################
  
  ##############################################################################
  # Event Observer for dev options. Show dev options if checkboxed
  observeEvent(input$dev, {
    toggle("lid")
    toggle("peek1")
    toggle("peek2")
    toggle("peek3")
    toggle("peek4")
    toggle("peek5")
    toggle("barcode1")
    toggle("barcode2")
    toggle("bgMax")
    toggle("peekMin")
    toggle("ledMin")
    toggle("peekDecay")
    toggle("peek_and_bg")
    
    if (input$dev == TRUE) {
      showTab(inputId = "tabs", target = "peek_and_bg")
    }
    
    if (input$dev == FALSE) {
      hideTab(inputId = "tabs", target = "peek_and_bg")
    }
    # else {
    #   hide("lid")
    #   hide("peek1")
    #   hide("peek2")
    #   hide("peek3")
    #   hide("peek4")
    #   hide("peek5")
    #   hide("barcode1")
    #   hide("barcode2")
    #   hide("bgMax")
    #   hide("peekMin")
    #   hide("ledMin")
    # }
    
  })

  ##############################################################################
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
  
  ##############################################################################
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
  
  ##############################################################################
  # Event Observers for peek File upload
  # Attempt to verify that file is a peek scan and not background and populate barcodes etc.
  observeEvent(input$peekFile, {
    is_peek <- check_filetype(input$peekFile[["datapath"]], "Peek Lid Scan")
    is_bg <- check_filetype(input$peekFile[["datapath"]], "Background Scan")
    is_pm_bg <- check_filetype(input$peekFile[["datapath"]], "Panther Main BG Scan")
    if (is_peek) {
      showNotification("Peek Scan File detected.")
      updateTabsetPanel(session, "tabs", selected = "Peek")
      updateTabsetPanel(session, "peek_tabs", selected = "Raw Peek")
      isPeekFile(TRUE)
      
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
      updateTabsetPanel(session, "bg_tabs", selected = "Raw Background")
      isBackgroundFile(TRUE)
    }
    else {
      alert("Unknown Scan File detected. Please upload correct scan file.")
      reset("bgFile")
      isBackgroundFile(FALSE)
    }
  })

  ##############################################################################
  # Event Observers for tab navigation
  # when navigated to, some data should be calculated/displayed
  observeEvent({input$tabs == input$`Raw Peek`
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

  ##############################################################################
  # Event Observers for diagnostic calculation and download
  observeEvent(input$calculate, {
    enable("excel_download")
    enable("html_download")
    updateTabsetPanel(session, "tabs", selected = "Report")
    wb <- generate_workbook(input)
    html <- generate_html_report(input)

    output$report <- renderUI(html)

    excel_filepath <- "./reports/ThermocyclerDiagnosticReport.xlsx"
    html_filepath <- "./reports/ThermocyclerDiagnosticReport.html"
    
    if (file.exists(excel_filepath)) {
      file.remove(excel_filepath)
    }
    if (file.exists(html_filepath)) {
      file.remove(html_filepath)
    }
    saveWorkbook(wb, excel_filepath)
    save_html(html, html_filepath)

    excel_md5 <- as.character(md5(file(excel_filepath, open = "rb")))
    excel_file <- file(excel_filepath, open = "rb")
    
    html_md5 <- as.character(md5(file(html_filepath, open = "rb")))
    html_file <- file(html_filepath, open = "rb")

    if (isDatabase) {
      db <- 'tc'
      host_db <- 'db'
      db_port <- '5432'
      db_user <- 'tc_shiny_user'
      db_password <- 'tc_shiny_pass'
      
      con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db, port = db_port, user = db_user, password = db_password)

      if (dbIsValid(con)) {
        update_database_panther_sn(con, input$pantherSN)
        update_database_tc_sn(con, input$thermocyclerSN, input$thermocyclerPN)
        update_database_scan(con, input$pantherSN, input$thermocyclerSN, input$thermocyclerPN, input$bgFile[["datapath"]])
        update_database_scan(con, input$pantherSN, input$thermocyclerSN, input$thermocyclerPN, input$peekFile[["datapath"]])
        update_database_report(con, input, version, isAccepted, excel_file, html_file)

        dbDisconnect(con)
        showNotification("Database updated!") 
      }
      else {
        showNotification("Database NOT updated. Check server logs.") 
      }
    }

    reset("bgFile")
    isBackgroundFile(FALSE)
    reset("peekFile")
    isPeekFile(FALSE)

    isAccepted <- TRUE
    
    disable("calculate")
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
  
################################################################################
#------------------------------OUTPUT DEFINITIONS-------------------------------
################################################################################

  # download button to extract xlsx diagnostic file from server
  output$excel_download <- downloadHandler(
      filename = "ThermocyclerDiagnosticReport.xlsx",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        # wb <- generate_workbook(input)
        
        filepath <- "./reports/ThermocyclerDiagnosticReport.xlsx"
        # filepath <- file.path(tempdir(), "ThermocyclerDiagnosticReport.xlsx")
        
        # if (file.exists(filepath)) {
        #   file.remove(filepath)
        # }
        
        # tempReport <- file.path(tempdir(), "ThermocyclerDiagnosticReport.xlsx")
        # saveWorkbook(wb, filepath)
        file.copy(filepath, file)
        # file.copy("ThermocyclerDiagnosticReport.xlsx", tempReport, overwrite = TRUE)
      }
  )
  # download button to extract html diagnostic report from server
  output$html_download <- downloadHandler(
    filename = "ThermocyclerDiagnosticReport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # report <- generate_html_report(input)
      
      filepath <- "./reports/ThermocyclerDiagnosticReport.html"
      # filepath <- file.path(tempdir(), "ThermocyclerDiagnosticReport.xlsx")
      
      # if (file.exists(filepath)) {
      #   file.remove(filepath)
      # }
      
      # tempReport <- file.path(tempdir(), "ThermocyclerDiagnosticReport.xlsx")
      # save_html(report, filepath)
      file.copy(filepath, file)
      # file.copy("ThermocyclerDiagnosticReport.xlsx", tempReport, overwrite = TRUE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
