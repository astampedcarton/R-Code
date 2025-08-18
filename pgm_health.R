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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Program health"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           textInput("dirpath","Enter the path to the programs", value="."),
           actionButton("getFiles", "Load data")
        ),
        # Show a plot of the generated distribution
        mainPanel(
           DTOutput("datatable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  loc <- reactiveVal("")
  
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
      
      datatable(srcdt, colnames = labels)    
    })
  })
  
}


readsrcfiles <- function(inloc){
  print("Start src read")
  print(inloc)
  # Read SAS file
# sas_file <- "G:/My Drive/Pharma_Life/SAS/ADaM_Review.sas"
  pgmhealth <- data.frame()
  dftab <- data.frame()
  # Get all the files in a directory
  flist <- list.files(inloc, pattern="*.sas$", full.names=TRUE)
  print(flist)
  
  for (file in flist){
    print(cat("Busy with,", file))
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
    semicnt <- sum(stringr::str_count(nlinecoms,";"))
    datcnt <- sum(stringr::str_count(nlinecoms,"data"))
    procnt <- sum(stringr::str_count(nlinecoms,"proc"))
    
    statcnt <- datcnt + procnt

    # Count the nr of lines with 2 or more statements per line
    multcnt <- length(which(sas_code > 1))
    #print(cnt)

    # the ratio of comments to statements
    dpratio <- round((comcnt / statcnt) * 100, digit=1)
    stratio <- round((comcnt / semicnt) * 100, digit=1)
    
    # Format: domain, nr of lines in a pgm, nr of comments, statements, multiple state lines pe
    dftab <- data.frame("domain"=domain, "pgmline"=pgmnline, "comcnt"=comcnt, 
                        "statcnt"=statcnt, "dpratio"=dpratio, "stratio"=stratio, "multcnt"=multcnt)
    pgmhealth <- bind_rows(dftab,pgmhealth)
  }  

#  print(pgmhealth)
  pgmhealth <- bind_rows(dftab,pgmhealth)
  
  #Assign labels
  # Assign labels to the variables
   Hmisc::label(pgmhealth$domain) <- "Program Name"
   Hmisc::label(pgmhealth$pgmline) <- "# of lines in program"
   Hmisc::label(pgmhealth$comcnt) <- "# of Comments"
   Hmisc::label(pgmhealth$statcnt) <- "# of statements"
   Hmisc::label(pgmhealth$dpratio) <- "Coms to data/proc ratio"
   Hmisc::label(pgmhealth$stratio) <- "Coms to state ratio"
   Hmisc::label(pgmhealth$multcnt) <- "# of multi statements per line"
  
  # Extract labels
  labels <- sapply(pgmhealth, function(x) attr(x, "label"))
  labels[is.na(labels) | labels == ""] <- names(pgmhealth)
  labels <- unname(labels)  # must be unnamed vector    

  return(pgmhealth)
}

#inloc <- "G:/My Drive/Pharma_Life/SAS/"

#outds <- readsrcfiles(inloc)
#View(outds)  


# Run the application 
shinyApp(ui = ui, server = server)
