 #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#G:\My Drive\Pharma_Life\SAS\ADaM_Review.sas

library(shiny)
library(DT)
library(Hmisc)
library(dplyr)
library(jsonlite)
library(shinyFiles)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Project health"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           textInput("dirpath","Enter the path to the programs", value=""),
           actionButton("getFiles" , "Load Data "),
           shinyFilesButton("LoadSetup", "Load Setup", "Choose a setup to load", multiple = FALSE),
           shinySaveButton("SaveSetup", "Save Setup", "Save setup", filetype = list(json="json")),           
           textInput("appadpath","ADaM Approval Paths", value=""),
           actionButton("getadamapp", "Check Status"),
           textInput("appsdpath","SDTM Approval Paths", value=""),
           actionButton("getsdtmapp", "Check Status"),
           uiOutput("adamappr"),
           uiOutput("sdtmappr")
        ),
           #div("ADaM Approval Status:", uiOutput("adamappr"), style = "margin-bottom: 10px;"),
           #div("SDTM Approval Status:", uiOutput("sdtmappr"))        ),
        # Show a plot of the generated distribution
        mainPanel(
           DTOutput("datatable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  loc <- reactiveVal("")

  roots <- c(home=normalizePath("~"))
  
  shinyFileChoose(input, "LoadSetup", roots=roots, session=session, filetypes=c("json"))
  
  observeEvent(input$LoadSetup, {
    fileinfo <- parseFilePaths(roots, input$LoadSetup)
    if (nrow(fileinfo) > 0){
      filepath <- as.character(fileinfo$datapath[1])
      setup <- fromJSON(filepath)
      
      updateTextInput(session, "dirpath"  , value=setup$rootloc)
      updateTextInput(session, "appadpath", value=setup$adamapp)
      updateTextInput(session, "appsdpath", value=setup$sdtmapp)
    }
  })
  
  #Save the setup to a JSON file
  shinyFileSave(input, "SaveSetup", roots=roots, session=session, filetypes=c("json"))
  
  observeEvent(input$SaveSetup,{
    fileinfo <- parseSavePath(roots, input$SaveSetup)
    if (nrow(fileinfo) > 0){
      filepath <- as.character(fileinfo$datapath[1])
      print(filepath)
      if (!grepl("\\.json$", filepath)){
        filepath <- paste0(filepath, ".json")
      }
      setup <- list(
        rootloc = input$dirpath,
        adamapp = input$appadpath,
        sdtmapp = input$appsdpath
      )
      write_json(setup, filepath, pretty=TRUE, auto_unbox=TRUE)
    }

  })
  
  observeEvent(input$getFiles, {
    loc <- input$dirpath
    files <- list.files(loc(), full.names = TRUE)    
    print(paste0("File location:",loc))
    
    # G:\My Drive\Pharma_Life\SAS\ADaM_Review.sas
    #srcdat <- reactiveVal(readsrcfiles(paste0(loc,"ADaM_Review.sas")))
    srcdat <- reactiveVal(readsrcfiles(loc))
    
    output$datatable <- renderDT({
      srcdt <- srcdat()
      labels <- sapply(srcdt, function(x) attr(x, "label"))
      labels <- ifelse(is.na(labels) | labels == "", names(srcdt), labels)
      labels <- as.character(unname(labels))  # remove names & force char vector
      
      datatable(srcdt, colnames = labels) %>%
        formatStyle(
          c("dpratio","stratio"),
          backgroundColor = styleInterval(c(40, 85), c("tomato", "white", "tomato"))
        )
    })
    
  })
  
  observeEvent(input$getadamapp, {
    #Check the ADaM spec approval
    output$adamappr  <- renderUI({
      # path to the file you want to check
      #print(paste0("ADaM input path : ", input$appadpath))
      adamexist <- fexistchk(input$appadpath)
      status <- if (adamexist) {
        tags$span("ADaM Approval Status: Found", style = "color: green; font-weight: bold;")        
      } else {
        tags$span("ADaM Approval Status: Missing", style = "color: red; font-weight: bold;")        
      }      
    })
  })

  observeEvent(input$getsdtmapp, {
    #Check the SDTM spec approval
    output$sdtmappr  <- renderUI({
      # path to the file you want to check
      #print(paste0("SDTM input path : ", input$appsdpath))
      sdtmexist <- fexistchk(input$appsdpath)
      status <- if (sdtmexist) {
        tags$span("SDTM Approval Status: Found", style = "color: green; font-weight: bold;")        
      } else {
        tags$span("SDTM Approval Status: Missing", style = "color: red; font-weight: bold;")        
      }      

    })
  })
    
}

# Check if the approval form exists in the defined location
fexistchk <- function(loc){
  apprfound <- list.files(
    path = loc,
    pattern = "Approval_.*_signed\\.pdf$",
    ignore.case = TRUE
  )
  
  if (length(apprfound) > 0) {
    found <- TRUE
  } else {
    found <- FALSE
  }
  
  return(found)
  
}

# Read the source file and scan for specific pieces of information
readsrcfiles <- function(inloc){
#  print("Start src read")
#  print(inloc)
  # Read SAS file
# sas_file <- "G:/My Drive/Pharma_Life/SAS/ADaM_Review.sas"
  pgmhealth <- data.frame()
  dftab <- data.frame()
  # Get all the files in a directory
  flist <- list.files(inloc, pattern="*.sas$", full.names=TRUE)
#  print(flist)
  
  for (file in flist){
#    print(cat("Busy with,", file))
    sas_code <- readLines(file, warn = FALSE)
    domain <- basename(file)
    # Join into single string for multi-line regex
    sas_text <- paste(sas_code, collapse = "\n")
    
    # --- Pattern 1: Block comments /* ... */ ---
    blkpat <- stringr::str_extract_all(
      sas_text,
      stringr::regex("/\\*.*?\\*/", dotall = TRUE, ignore_case = TRUE))[[1]]
    
    # --- Pattern 2: Single-line or multi-line * ... ; comments ---
    # Rule: * must be first non-space char on the line
    linepat <- "(?m)^\\s*\\*[^;]*?;"
    # This will only grab from * to ; on the same line.
    # To allow multi-line * comments until first semicolon:
    linepat_mult <- "(?ms)^\\s*\\*.*?;"
    
    linecoms <- stringr::str_extract_all(
      sas_text,
      stringr::regex(linepat_mult, dotall = TRUE, ignore_case = TRUE))[[1]]
    
    # --- Clean up: remove false positives like table var1*var2 / out=p;
    # We'll keep * comments only if they start the line
    linecoms <- linecoms[!grepl("^\\s*table\\s", linecoms, ignore.case = TRUE)]
    
    # Combine
    all_comments <- c(blkpat, linecoms)
    
    # Count and display
    comcnt <- length(all_comments)
    
#    cat("Found", comcnt, "comments\n")
#    print(all_comments)
    
    # Remove block comments /* ... */
    nblkcoms <- stringr::str_remove_all(
      sas_code,
      stringr::regex("/\\*.*?\\*/", dotall = TRUE, ignore_case = TRUE)
    )
    
    # Remove star-style comments (* ... ;)
    # Rule: starts with * after optional spaces, until the first ;
    nlinecoms <- stringr::str_remove_all(
      nblkcoms,
      stringr::regex("(?ms)^\\s*\\*.*?;", dotall = TRUE, ignore_case = TRUE)
    )
    
    #Count the nr of lines
    pgmnline <- length(nlinecoms)

    #Count the nr of data/proc
    data_pat <- "^\\s*DATA\\s+[^;]+;"        # DATA step
    proc_pat <- "^\\s*PROC\\s+[^;]+;"        # PROC step
    
    datcnt <- 0
    procnt <- 0
    semicnt <- 0
    
    datamat <- grepl(data_pat, nlinecoms, ignore.case = TRUE)
    procmat <- grepl(proc_pat, nlinecoms, ignore.case = TRUE)    
        
    semicnt <- sum(stringr::str_count(nlinecoms,";"))
#    datcnt <- sum(stringr::str_count(nlinecoms,"data"))
#    procnt <- sum(stringr::str_count(nlinecoms,"proc"))
    datcnt <- sum(datamat)
    procnt <- sum(procmat)
    
    statcnt <- datcnt + procnt

    # Count the nr of lines with 2 or more statements per line
    multcnt <- length(which(sas_code > 1))
    #print(cnt)

    # the ratio of comments to statements
    dpratio <- round((comcnt / statcnt) * 100, digit=1)
    stratio <- round((comcnt / semicnt) * 100, digit=1)
    
    # Format: domain, nr of lines in a pgm, nr of comments, statements, multiple state lines pe
    dftab <- data.frame("domain"=domain, "pgmline"=pgmnline, "comcnt"=comcnt, "semicnt"=semicnt, 
                        "multcnt"=multcnt, "statcnt"=statcnt, "dpratio"=dpratio, "stratio"=stratio)
    pgmhealth <- bind_rows(dftab,pgmhealth)
  }  

#  print(pgmhealth)
  pgmhealth <- bind_rows(dftab,pgmhealth)
  
 #%>%
    #formatStyle(
    #  "ID",
    #  backgroundColor = styleInterval(c(5, 8), c("yellow", "white", "lightblue"))
    #)  
  
  #Assign labels
  # Assign labels to the variables
   Hmisc::label(pgmhealth$domain) <- "Program Name"
   Hmisc::label(pgmhealth$pgmline) <- "# lines in pgm"
   Hmisc::label(pgmhealth$comcnt) <- "# of Comments"
   Hmisc::label(pgmhealth$semicnt) <- "# of stat."
   Hmisc::label(pgmhealth$multcnt) <- "# mult stat line"
   Hmisc::label(pgmhealth$statcnt) <- "# of data/proc"
   Hmisc::label(pgmhealth$dpratio) <- "Coms data/proc ratio"
   Hmisc::label(pgmhealth$stratio) <- "Coms state ratio"
#   Hmisc::label(pgmhealth$stratio) <- "Coms state ratio"
#   Hmisc::label(pgmhealth$headrs) <- "Headers Ready"
   
   
  
  # Extract labels
  labels <- sapply(pgmhealth, function(x) attr(x, "label"))
  labels[is.na(labels) | labels == ""] <- names(pgmhealth)
  labels <- unname(labels)  # must be unnamed vector    

  return(pgmhealth)
}

#inloc <- "G:/My Drive/Pharma_Life/SAS/"
# G:\My Drive\Pharma_Life\SAS\

#outds <- readsrcfiles(inloc)
#View(outds)  


# Run the application 
shinyApp(ui = ui, server = server)
