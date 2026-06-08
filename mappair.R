# =====================================================
# V0.1 Hentie du Plessis 08JUNE2026
# =====================================================
#Declare the libraries that will be used.
library(readxl)
library(admiral)
library(stringr)

#parameters:
#dfct     = CodeTerm dataframe
#dfin     = Input dataframe to which the pair needs to be applied
#ctname   = Name of the CT that should be applied
#ctcode   = Name of the variable containing the code
#decodeval= Name of the variable containing the decoded value. Default is `Decoded Value`
#codeval  = Variable used to link to the CT
#pairval  = New variable name to which the Decoded value will be applied

mappair <- function(dfct, dfin, ctname, ctcode, decodeval=`Decoded Value`, codeval, pairval){
  #Check that the CT is dataframe is present and contains the needed information
  #If any of the following input parameters are missing stop the function execution
  if (missing(dfct) || is.null(dfct)){
    message("ERROR:: No Control term dataframe provided")
    return (NULL)
  } else {
    if (!ctcode %in% names(dfct)){
      message(glue::glue("ERROR:: Variable {ctcode} was not found in tthe ct data set"))
      return (NULL)
    }
    if (!decodeval %in% names(dfct)){
      message(glue::glue("ERROR:: Variable {decodeval} was not found in the ct data set"))
      return (NULL)
    }
  }
  
  #Check on the input dataframe that would become the out dataframe
  if (!!rlang::ensym(codeval) == "") {
    message(glue::glue("ERROR:: No code value was entered."))
    return (NULL)  
  } else {
    codeval_name <- rlang::as_name(rlang::ensym(codeval))
    if (!codeval_name %in% names(dfin)){
      message(glue::glue("ERROR:: Variable {decodeval} was not found in the ct data set"))
      return (NULL)
    }  
  }
  
  if (!!rlang::ensym(pairval) == "") {
    message(glue::glue("ERROR:: No pair value was entered."))
    return (NULL)  
  }
  
  #Only proceed if the input parameters passes the initial checks.
  
  if (is.data.frame(dfct) && is.data.frame(dfin)) {
    if (exists('dfct') && exists('dfin')) {
      #Filter for the applicable CTNAME
      #Convert it to a consistent case before we do anything else to reduce errors 
      dfct$Name <- stringr::str_to_lower(dfct$Name)
      dfct1 <- dfct[dfct$Name == stringr::str_to_lower(ctname),]
      
      if (nrow(dfct1)==0){
        message(glue::glue("ERROR:: Entered ctname {ctname} was not found in the {dfct}"))
      }

      print(glue::glue("Value for ctcode: {ctcode}"))
      
      #Rename to match whats in the CT
      dfin <- dfin %>% dplyr::rename(!!rlang::ensym(ctcode) := !!rlang::ensym(codeval))
      
      #Merge the input data frame and CT together
      #Assign the decode value to the pair variable and remove the unneeded vars
      dfin <- admiral::derive_vars_merged_lookup(dataset=dfin,
                                                 dataset_add = dfct1,
                                                 by_vars=exprs(!!rlang::sym(ctcode)),
                                                 new_vars = exprs(!!rlang::ensym(decodeval)),
                                                 print_not_mapped = TRUE) |>
        dplyr::mutate(!!rlang::ensym(codeval) := !!rlang::ensym(ctcode), 
                      !!rlang::ensym(pairval) := !!rlang::ensym(decodeval)) |>
        dplyr::select(-!!rlang::ensym(decodeval), -!!rlang::ensym(ctcode))
        
      
      return (dfin)
    } else {
      message(glue::glue("NOTE:: Entered ct {dfct} or input data frame {dfin} was not found"))
      return (NULL)
    }
    
  } else {
    message(glue::glue("Entered ct {dfct} or input data frame {dfin} is not a data frame"))
    return (NULL)
  }
}

#Example of use
ct <- readxl::read_xlsx("C:/Temp/Projects2/Specs.xlsx", "Codelists")
lb <- data.frame(PARAMCD=c("CHEM","HEM","TRIG","CHEM","HEM","CHem"))

ct$DecodeValue <- ct$`Decoded Value`

lb2 <- mappair(dfct=ct, dfin=lb, ctname="ADLBCAT", ctcode="Term", 
               decodeval="DecodeValue", 
               codeval = PARAMCD, 
               pairval = PARAM)

