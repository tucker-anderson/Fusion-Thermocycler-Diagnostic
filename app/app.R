library("shiny")
library("shinyjs")
library("stringr")
library("plyr")
library("openxlsx")

# TODO Integrate openxlsx into download handler better, remove dependency on temporary local file
# TODO Make report and table output more clear
# TODO Save report/data, SN and Thermocycler SN into backend database
# TODO test peek lid integration more, especially since chance to textinput
#----------------------------UI DEFINITIONS-----------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # App title
  titlePanel("Panther Fusion Thermocycler Diagnostic Tool"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      fixedRow(
        column(5,
          # Input: Input Thermocycler serial number for record keeping.
          textInput("pantherSN", "Panther Serial #", placeholder = "e.g. 2090000001")
        ),
        column(5,
          # Input: Input Thermocycler serial number for record keeping.
          textInput("thermocyclerSN", "Thermocycler Serial #", placeholder = "e.g. J0001D16D0")
        )
      ),
      
      # # Horizontal line
      # tags$hr(),
      
      # Input: Select a file 
      fileInput("peekFile", "Select PEEK Scan File",
                width = "85%",
                multiple = FALSE,
                accept = c("text/csv",
                           # "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Select Lid Presence
      radioButtons("lid", "Lid Present",
                 choices = c("No Lid (PEEK sheet used)" = FALSE, "Integrated Lid" = TRUE),
                 inline = TRUE,
                 selected = TRUE),
      fixedRow(
        column(4,
          verticalLayout(
            # Input: Select PEEK Lid Values
            textInput("peek1", "Peek Sheet FAM:", placeholder = "e.g. 10000"),
            textInput("peek2", "Peek Sheet HEX:", placeholder = "e.g. 2000"),
            textInput("peek3", "Peek Sheet ROX:", placeholder = "e.g. 3000"),
            textInput("peek4", "Peek Sheet RED647:", placeholder = "e.g. 400"),
            textInput("peek5", "Peek Sheet RED677:", placeholder = "e.g. 5000")
          )
        ),
        column(6,
          verticalLayout(
            # Input: Select Barcode values if lid is present. Autopopulated on PEEK file upload if barcode information present.
            textInput("barcode1", "Lid Barcode 1:", placeholder = "e.g. 10000000000000000000000"),
            # NULL,
            # min = 10000000000000000000000, 
            # max = 19999999999999999999999),
            textInput("barcode2", "Lid Barcode 2:", placeholder = "e.g. 20000000000000000000000")
          )
        )
      ),
        
      # # Horizontal line
      # tags$hr(),
      
      # Input: Select a background file
      fileInput("bgFile", "Select Background Scan File",
                width = "85%",
                multiple = FALSE,
                accept = c("text/csv",
                           # "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Action Button to run Script
      disabled(actionButton("calculate", "Calculate")),
      
      # Output: Downloader to export excel report
      disabled(downloadButton("download", "Download Excel Report"))
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Tabset w/ plot, summary, and table
      tabsetPanel(id = "tabs", type = "tabs",
                  tabPanel("Summary", 
                    tableOutput("summary")),
                  tabPanel("PEEK",
                   radioButtons("peekColor", "Dye Color:", inline = TRUE, 
                                c("FAM" = 0,
                                  "HEX" = 1,
                                  "ROX" = 2,
                                  "RED647" = 3,
                                  "RED677" = 4)),   
                    radioButtons("peekAgg", "Aggregation Type:", inline = TRUE, 
                                 c("Mean" = "mean",
                                   "Max" = "max",
                                   "Min" = "min",
                                   "Std Dev" = "sd")),
                    tableOutput("peekTable")),
                  tabPanel("Background", 
                   radioButtons("bgColor", "Dye Color:", inline = TRUE,
                                c("FAM" = 0,
                                  "HEX" = 1,
                                  "ROX" = 2,
                                  "RED647" = 3,
                                  "RED677" = 4)),   
                    radioButtons("bgAgg", "Aggregation Type:", inline = TRUE,
                                 c("Mean" = "mean",
                                   "Max" = "max",
                                   "Min" = "min",
                                   "Std Dev" = "sd")),
                    tableOutput("bgTable"))
      )
    )
  )
)

#----------------------------SERVER DEFINITIONS-----------------------------------------
server <- function(input, output, session) {
  
  shinyjs::runjs("$('#pantherSN').attr('maxlength', 9)")
  
  # shinyjs::hide("lid")
  # shinyjs::hide("barcode1")
  # shinyjs::hide("barcode2")
  # shinyjs::hide("peek1")
  # shinyjs::hide("peek2")
  # shinyjs::hide("peek3")
  # shinyjs::hide("peek4")
  # shinyjs::hide("peek5")
  
  
  #----------------------------FUNCTION DEFINITIONS-----------------------------------------
  ###defining a function to take in a single file and return the averaged fluorescence per well###
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
  
  ###defining a function to take in a single file and return the median of each fluorometer###
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
  
  read_barcode <- function(barcode){
    ## takes in a barcode, as a string, and returns expected values for PEEK sheet ##
    #FAM = as.numeric(substr(barcode, 3,6)) * 10
    FAM <- as.numeric(substr(format(barcode, scientific = FALSE), 3, 6)) * 10
    #HEX = as.numeric(substr(barcode, 7,11))
    HEX <- as.numeric(substr(format(barcode, scientific = FALSE), 7, 10))
    #ROX = as.numeric(substr(barcode, 12,15))
    ROX <- as.numeric(substr(format(barcode, scientific = FALSE), 11, 14))
    #RED647 = as.numeric(substr(barcode, 17,19))
    #RED647 = as.numeric(substr(barcode, 16,19))
    RED647 <- as.numeric(substr(format(barcode, scientific = FALSE), 15, 18))
    #RED677 = as.numeric(substr(barcode, 20,23))
    RED677 <- as.numeric(substr(format(barcode, scientific = FALSE), 19, 22))
    
    expected_vals = c(FAM, HEX,ROX, RED647, RED677)
    return(expected_vals)
    
  }
  
  check_vals <- function(vals1, vals2, background_sub_wells, background_sub_medians){
    
    for (i in 1:5) {
      #calculate percentages for fluormeter 1
      background_sub_wells[1:30, i + 1] = ((background_sub_wells[1:30, i + 1] - vals1[i]) / vals1[i] ) * 100
      background_sub_medians[1,i + 1] = ((background_sub_medians[1, i + 1] - vals1[i]) / vals1[i] ) * 100
      
      #calculate percentages for fluormeter 2
      background_sub_wells[31:60, i + 1] = ((background_sub_wells[31:60, i + 1] - vals2[i]) / vals2[i] ) * 100
      background_sub_medians[2,i + 1] = ((background_sub_medians[2,i + 1] - vals2[i]) / vals2[i]) * 100
    }
    
    list <- list(background_sub_medians, background_sub_wells) #combine both into a list
    percent_diff <- do.call(rbind.fill, list) #bind them
    
    return(percent_diff)
    
  }
  
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
  
  ###attempt to get barcodes from PEEK input file. If they cannot be found, return empty vector
  get_barcodes <- function(input_PEEK){
    input_lines <- readLines(input_PEEK)
    barcode_lines <- str_subset(input_lines, "Barcode")
    #No barcode lines in file
    if (length(barcode_lines) == 0) {
      return(vector())
    }
    barcodes <- str_extract(barcode_lines, "[0-9]{22,}")
    barcode_1 <- barcodes[1]
    barcode_2 <- barcodes[2]
    
    if (nchar(barcode_1) == 23) {
      barcode_1 <- paste0(str_sub(barcode_1,1,6), str_sub(barcode_1,8))
    } 
    if (nchar(barcode_2) == 23) {
      barcode_2 <- paste0(str_sub(barcode_2,1,6), str_sub(barcode_2,8))
    } 
    
    return(c(barcode_1, barcode_2))
  }
  
  VersionCheck <- function(input_PEEK, input_barcode, ZeroPadBarcode){
    VersionCheck = readLines(input_PEEK, n = 10)
    if (grepl("Bank No", VersionCheck[1])) {
      text_dat = readLines(input_barcode, n = 2)
      # Force correct barcodes due to zero added (SSW bug)
      if (ZeroPadBarcode) {
        text_dat[1] = paste0(str_sub(text_dat[1],start = 1, end = 6), "0", str_sub(text_dat[1],start = 7))
        text_dat[2] = paste0(str_sub(text_dat[2],start = 1, end = 6), "0", str_sub(text_dat[2],start = 7))
      }
      barcode1 = read_barcode(text_dat[1])
      barcode2 = read_barcode(text_dat[2])
      PEEKSkip = 0
      BackSkip = 0
    } 
    else {
      text_dat = readLines(input_PEEK , n = 4, skip = 3)
      barcode1 = read_barcode(substr(text_dat[3], 24,46))
      barcode2 = read_barcode(substr(text_dat[4], 24,46))
      PEEKSkip = 5
      BackSkip = 3
    }
    
    return(list(barcode1, barcode2, PEEKSkip, BackSkip))
  }
  
  generate_data <- function(input) {
    skip <- str_which(readLines(input), ".*Bank No.*") - 1
    
    data_table <- read.table(input, header = TRUE, sep = ";", fill = TRUE, skip = skip)
    
    data_set <- data.frame(data_table$Color, data_table$Well.No, data_table$RFU)
    names(data_set) <- c("Dye", "Well", "RFU")
    data_set <- data_set[order(data_set$Well) , ]
    
    return(data_set)
  }
  
  generate_data_visual <- function(input, color, fun = c("mean", "max", "min", "sd")) {
    
    skip <- str_which(readLines(input), ".*Bank No.*") - 1
    
    data_table <- read.table(input, header = TRUE, sep = ";", fill = TRUE, skip = skip)
    
    data_set <- data.frame(data_table$Color, data_table$Well.No, data_table$RFU)
    names(data_set) <- c("Dye", "Well", "RFU")
    data_set <- data_set[order(data_set$Well) , ]
    
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
    return(data_reshaped)
  }
  
  # generate_data_visual <- function(input) {
  #   skip <- str_which(readLines(input), ".*Bank No.*") - 1
  #   
  #   data_table <- read.table(input, header = TRUE, sep = ";", fill = TRUE, skip = skip)
  #   
  #   data_set <- data.frame(data_table$Color, data_table$Well.No, data_table$RFU)
  #   names(data_set) <- c("Dye", "Well", "RFU")
  #   data_set <- data_set[order(data_set$Well) , ]
  #   
  #   data_set <- data_set[data_set$Dye == color, ]
  #   
  #   data_set$Bank <- floor((data_set$Well - 1) / 5) + 1
  #   data_set$Bank.Well <- (data_set$Well - 1) %% 5 + 1
  #   
  #   return(data_reshaped)
  # }
  
  #----------------------------EVENT OBSERVERS-----------------------------------------
  
  # Event Observers for lid presence. Disable numeric fields if no lid selected.
  observe({
    if (input$lid == TRUE) {
      disable("peek1")
      disable("peek2")
      disable("peek3")
      disable("peek4")
      disable("peek5")
      enable("barcode1")
      enable("barcode2")
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek1').prop('disabled',true)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek2').prop('disabled',true)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek3').prop('disabled',true)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek4').prop('disabled',true)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek5').prop('disabled',true)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#barcode1').prop('disabled',false)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#barcode2').prop('disabled',false)"))
      # shinyjs::hide("barcode1")
      # shinyjs::hide("barcode2")
      # shinyjs::hide("peek1")
      # shinyjs::hide("peek2")
      # shinyjs::hide("peek3")
      # shinyjs::hide("peek4")
      # shinyjs::hide("peek5")
    }                               
    else {
      enable("peek1")
      enable("peek2")
      enable("peek3")
      enable("peek4")
      enable("peek5")
      disable("barcode1")
      disable("barcode2")
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek1').prop('disabled',false)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek2').prop('disabled',false)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek3').prop('disabled',false)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek4').prop('disabled',false)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#peek5').prop('disabled',false)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#barcode1').prop('disabled',true)"))
      # session$sendCustomMessage(type = "jsCode", list(code = "$('#barcode2').prop('disabled',true)"))
      # shinyjs::show("barcode1")
      # shinyjs::show("barcode2")
      # shinyjs::show("peek1")
      # shinyjs::show("peek2")
      # shinyjs::show("peek3")
      # shinyjs::show("peek4")
      # shinyjs::show("peek5")
    }
  })
  
  # only enable calculate if both files uploaded correctly and SNs input in correct format
  observe({
    req(input$peekFile, input$bgFile, input$thermocyclerSN, input$pantherSN)
    enable("calculate")
  })
  
  # Event Observers for PEEK File upload. Attempt to verify that file is a PEEK scan and not background and populate barcodes etc.
  observeEvent(input$peekFile, {
    is_peek <- check_filetype(input$peekFile[["datapath"]], "Peek Lid Scan")
    is_bg <- check_filetype(input$peekFile[["datapath"]], "Background Scan")
    if (is_peek) {
      showNotification("Peek Scan File detected.")
      updateTabsetPanel(session, "tabs", selected = "PEEK")
    }
    else if (is_bg) {
      alert("Background Scan File detected. Please upload a PEEK scan file.")
      reset("peekFile")
    }
    else {
      alert("Unknown Scan File detected. Please upload correct scan file.")
      reset("peekFile")
    }
    
    barcodes <- get_barcodes(input$peekFile[["datapath"]])
    updateNumericInput(session, "barcode1", value = barcodes[1])
    updateNumericInput(session, "barcode2", value = barcodes[2])
    
    # Update lid presence option automatically based on barcode detection.
    if (length(barcodes) > 0) {
      updateRadioButtons(session, "lid", selected = TRUE)
    }
    else {
      updateRadioButtons(session, "lid", selected = FALSE)
    }
  })
  
  observeEvent(input$bgFile, {
    is_peek <- check_filetype(input$bgFile[["datapath"]], "Peek Lid Scan")
    is_bg <- check_filetype(input$bgFile[["datapath"]], "Background Scan")
    if (is_peek) {
      showNotification("Peek Scan File detected.")
      alert("PEEK Scan File detected. Please upload a Background scan file.")
      reset("bgFile")
    }
    else if (is_bg) {
      showNotification("Background Scan File detected.")
      updateTabsetPanel(session, "tabs", selected = "Background")
    }
    else {
      alert("Unknown Scan File detected. Please upload correct scan file.")
      reset("bgFile")
    }
  })
  
  observeEvent({input$tabs == input$PEEK
    input$peekColor
    input$peekAgg}, {
    if (length(input$peekFile) != 0) {
      peek_visual <- generate_data_visual(input$peekFile[["datapath"]], color = input$peekColor, fun = input$peekAgg)
      output$peekTable <- renderTable(peek_visual)
    }
  })
  
  observeEvent({input$tabs == input$Background
    input$bgColor
    input$bgAgg}, {
      if (length(input$bgFile) != 0) {
        bg_visual <- generate_data_visual(input$bgFile[["datapath"]], color = input$bgColor, fun = input$bgAgg)
        output$bgTable <- renderTable(bg_visual)
      }
    })
  
  # peek_dataset <- eventReactive(input$calculate, {
  #   generate_data(input$peekFile[["datapath"]])
  # })
  # 
  # bg_dataset <- eventReactive(input$calculate, {
  #   generate_data(input$bgFile[["datapath"]])
  # })
  
  # workbook <- 
  observeEvent(input$calculate, {
    peek_dataset <- generate_data(input$peekFile[["datapath"]])
    peek_wells <- average_wells(peek_dataset)
    peek_medians <- fluorometer_med(peek_wells)
    # output$peekTable <- renderTable(peek_dataset)
    

    bg_dataset <- generate_data(input$bgFile[["datapath"]])
    bg_wells <- average_wells(bg_dataset)
    bg_medians <- fluorometer_med(bg_wells)
    # output$bgTable <- renderTable(bg_dataset)
    
    bg_sub_wells = data.frame( c(1:60), (peek_wells[,2:6] - bg_wells[,2:6])) 
    names(bg_sub_wells) <- c("Well", "FAM Mean", "HEX Mean", "ROX Mean", "RED 647 Mean", "RED 677 Mean")
    bg_sub_medians = fluorometer_med(bg_sub_wells)
    
    if (input$lid == TRUE) {
      vals1 <- read_barcode(input$barcode1)
      vals2 <- read_barcode(input$barcode2)
    }
    else {
      vals1 <- c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5)
      vals2 <- c(input$peek1, input$peek2, input$peek3, input$peek4, input$peek5)
    }
    
    percent_diff_30_subtracted <- check_vals(vals1, vals2, bg_sub_wells, bg_sub_medians)
    percent_diff_30 <- check_vals(vals1, vals2, peek_wells, peek_medians)
    
    list <- list(bg_medians, bg_wells) #combine both into a list
    background <- do.call(rbind.fill, list) #bind them

    list <- list(peek_medians, peek_wells) #combine both into a list
    peek <- do.call(rbind.fill, list) #bind them
    
    #generate workbook
    wb <- createWorkbook()
    addWorksheet(wb, "Raw PEEK")
    
    if (input$lid == FALSE) { 
      addWorksheet(wb, "Raw Background") 
    }
    
    addWorksheet(wb, "Percent Diff")
    
    if (input$lid == FALSE) { 
      addWorksheet(wb, "Percent Diff Subtracted") 
    }
    
    addWorksheet(wb, "Barcodes")
    
    
    writeData(wb, "Raw PEEK", peek, startCol = 1, startRow = 1, xy = NULL,
              colNames = TRUE, rowNames = FALSE)
    
    if (input$lid == FALSE) {
      writeData(wb, "Raw Background", background, startCol = 1, startRow = 1, xy = NULL,
                colNames = TRUE, rowNames = FALSE)
    }
    
    writeData(wb,  "Percent Diff", percent_diff_30, startCol = 1, startRow = 1, xy = NULL,
              colNames = TRUE, rowNames = FALSE)
    
    if (input$lid == FALSE) {
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
    
    if (input$lid == FALSE) {
      conditionalFormatting(wb, "Percent Diff Subtracted",  cols = 2:6, rows = 2:3, rule = ">=30", style = NULL)
      conditionalFormatting(wb, "Percent Diff Subtracted", 2:6, 2:3, rule = "<=-30", style = NULL)
      
      conditionalFormatting(wb, "Percent Diff Subtracted", 8:12, 4:63, rule = ">=30", style = NULL)
      conditionalFormatting(wb, "Percent Diff Subtracted", 8:12, 4:63, rule = "<=-30", style = NULL)
    }
    filename <- "./reports/ThermocyclerDiagnosticReport.xlsx"
    
    if (file.exists(filename)) {
      file.remove(filename)
    }
    
    saveWorkbook(wb, filename)

    enable("download")
    # return(wb)
    updateTabsetPanel(session, "tabs", selected = "Summary")
  })
  
  output$download <- downloadHandler(
      filename = function() {
        paste("ThermocyclerDiagnosticReport.xlsx", ".xlsx", sep = "")
      },
      content = function(file) {
        file.copy("./reports/ThermocyclerDiagnosticReport.xlsx", file)
      }
  )
  
  #----------------------------OUTPUT
  # output$peekTable <- renderTable(peek_dataset)
}

shinyApp(ui = ui, server = server)
