library(haven)
library(dplyr)
library(xml2)

# Read all the datasets and check the STUYDID

xptdir <- "C:/Temp/data/"
sasdir <- "C:/Temp/data/NewTemp/"

lstxpt <- list.files(xptdir, pattern="*.xpt")
lstsas <- list.files(sasdir, pattern="*.sas7bdat")

rm(masstud)
masstud <- data.frame(src="",STUDYID="", n=NA)

#Read in all the data from the directory.
readdata <- function(dir, lstdir){
  for (df in lstdir){
    dfn <- tools::file_path_sans_ext(basename(df))
    print(dfn)
    dfs <- read_xpt(file=paste0(dir,df))[,"STUDYID",drop=FALSE] 
    assign(dfn, dfs, envir = .GlobalEnv)
  }
}

checkstudy <- function(flst){
  for (df in flst){
    dfnstudy <- paste0(tools::file_path_sans_ext(basename(df)),"_STUDY") 
    dfn <- tools::file_path_sans_ext(basename(df))
#    print(dfn)
    study <- get(dfn) %>% 
      count(STUDYID) %>% 
      group_by(STUDYID) %>% 
      mutate(src=dfn)

    assign(dfnstudy,study)
    
    masstud <- bind_rows(masstud, study)
  }
  return(masstud)
}
# Check the xpts that we have the right data
readdata(xptdir, lstxpt)
masstud <- checkstudy(lstxpt)
View(masstud)

rm(list=Filter(function(x) is.data.frame(get(x)), ls()))

# Check the sas7bdat's that we have the right data

readdata(sasdir, lstsas)
checkstudy(lstsas)

# Show the data frame 
# TODO: Needs to show it in a better format


# Extract the Study details from the define
definepath <- "C:/Temp/data/"
definename <- "define.xml"
fullpath <- paste0(definepath, definename)

deftxt <- read_xml(fullpath)

ns <- xml_ns(deftxt)

study_name <- xml_text(xml_find_first(deftxt, ".//d1:StudyName",xml_ns(deftxt)))
study_desc <- xml_text(xml_find_first(deftxt, ".//d1:StudyDescription",xml_ns(deftxt)))
study_prot <- xml_text(xml_find_first(deftxt, ".//d1:ProtocolName",xml_ns(deftxt)))

# Print the details
View(masstud)
print(masstud)

print(cat("==================================================================", "\n",
          "STUDY NAME  :" ,study_name , "\n",
          "Description :", study_desc, "\n",
          "Protocol    :", study_prot, "\n",
          "==================================================================", "\n")
      )
