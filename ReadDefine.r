# -----------------------------------------------------------------------------
# This is a utility program to Read the CDISC Define.xml
# The code consists of 3 parts.
# Step 1: Reads the ItemGroupDef information.
# Step 2: Reads the ItemDefs information.
# Step 3: Reads the CodeList information.
# All this information is combine into one dataframe that can be used to output
# the info into the desired output method
# dplyr and xml2 are needed for the code to run
# -----------------------------------------------------------------------------
# Version 1
# Date: 08MAR2025
# -----------------------------------------------------------------------------

library(xml2)
library(dplyr)

#Code created with the assistance of ChatGPT
#Update the path to point to the specific location;
definepath <- "G:/My Drive/Pharma_Life/CDISC/Define/DefineV215/examples/Define-XML-2-1-ADaM/adam/"
definename <- "defineV21-ADaM.xml"
fullpath <- paste0(definepath, definename)

#Read the Define.
#note that if there are duplicate xmlns with the same path the function will 
#not read it

deftxt <- read_xml(fullpath)

#Step 1 of 3 ----
#Extract the Item groupDef information
item_groups <- xml_find_all(deftxt, "//d1:ItemGroupDef", ns)

results_list <- list()
for (i in seq_along(item_groups)) {
  item_group <- item_groups[i]  #Corrected

  # Extract attributes
  item_info <- tibble(
    OID = xml_attr(item_group, "OID"),
    Name = xml_attr(item_group, "Name"),
    SASDatasetName = xml_attr(item_group, "SASDatasetName"),
    Purpose = xml_attr(item_group, "Purpose"),
    StandardOID = xml_attr(item_group, "d1:StandardOID", ns)
  )

  # Extract Description
  description <- xml_text(xml_find_first(item_group, ".//d1:Description/d1:TranslatedText", ns))
  item_info <- item_info %>% mutate(Description = description)

  # Extract all ItemRef nodes for this ItemGroupDef
  item_refs <- xml_find_all(item_group, ".//d1:ItemRef", ns)
  item_ref_df <- tibble(
    ItemOID = xml_attr(item_refs, "ItemOID"),
    OrderNumber = xml_attr(item_refs, "OrderNumber"),
    Mandatory = xml_attr(item_refs, "Mandatory"),
    MethodID = xml_attr(item_refs, "MethodOID")
  ) %>% mutate(OID = item_info$OID)  # Link to parent

  # Extract leaf information (file reference)
  leaf <- xml_find_first(item_group, ".//d1:leaf", ns)
  leaf_info <- tibble(
    LeafID = xml_attr(leaf, "ID"),
    FileReference = xml_attr(leaf, "d2:href", ns),
    Title = xml_text(xml_find_first(leaf, ".//d1:title", ns))
  ) %>% mutate(OID = item_info$OID)  # Link to parent

  # Extract MethodDef information into a dataframe
  method_nodes <- xml_find_all(item_group, "//d1:MethodDef", ns)
  method_df <- tibble(
    MethodID = xml_attr(method_nodes, "OID"),
    Name = xml_attr(method_nodes, "Name"),
    Type = xml_attr(method_nodes, "Type"),
    MethodDescr = xml_text(xml_find_first(method_nodes, ".//d1:Description/d1:TranslatedText", ns))
  )
  # Combine data from the different parts in Item Group
  combined_df <- left_join(item_info, item_ref_df, by = "OID") %>%
    left_join(leaf_info, by = "OID") %>%
    left_join(method_df, by = "MethodID")

  results_list[[i]] <- combined_df
}

# Merge all results into a single dataframe
itemgrp_df <- bind_rows(results_list)

#Step 2 of 3 ----
#Read the ItemDefs essentially what's in the datasets or should be in the datasets
itemdef_groups <- xml_find_all(deftxt, "//d1:ItemDef", ns)
length(itemdef_groups)
# Initialize an empty list to store results
defs_list <- list()
for (i in seq_along(itemdef_groups)) {
  item_def <- itemdef_groups[i]  

  item_def_df <- tibble(
    ItemOID = xml_attr(item_def, "OID"),
    Name = xml_attr(item_def, "Name"),
    SASFieldName = xml_attr(item_def, "SASFieldName"),
    DataType = xml_attr(item_def, "DataType"),
    Length = xml_attr(item_def, "Length")
  ) 

  #Extract the Var label
  Refdesc <- xml_text(xml_find_first(item_def, ".//d1:Description/d1:TranslatedText", ns))
  item_def_df <- item_def_df %>% mutate(VarLabel = Refdesc)

  #Extract the Code list of the variable
  itemcls <- xml_find_first(item_def, ".//d1:CodeListRef", ns)
  itemcl <- xml_attr(itemcls, "CodeListOID")

  item_def_df <- item_def_df %>% mutate(CodeListOID = itemcl)

  defs_list[[i]] <- item_def_df
}
# Merge all Item Defs into a single dataframe
combdef_df <- bind_rows(defs_list)

#Combine with the Itemdefgroups
items_comb_df1 <- left_join(itemgrp_df, combdef_df, by = c("ItemOID" = "ItemOID"))

#Step 3 of 3 ----
# Extract ALL CodeList nodes
codelist_nodes <- xml_find_all(deftxt, "//d1:CodeList", ns)

# Initialize an empty list to store results
codelist_list <- list()

# Loop through each CodeList node
for (i in seq_along(codelist_nodes)) {
  codelist <- codelist_nodes[i]

  # Extract CodeList attributes
  codelist_oid <- xml_attr(codelist, "OID")
  codelist_name <- xml_attr(codelist, "Name")
  codelist_datatype <- xml_attr(codelist, "DataType")

  # Extract all CodeListItem nodes within CodeList
  codelist_items <- xml_find_all(codelist, ".//d1:CodeListItem", ns)

  # Extract details of CodeListItem nodes
  item_df <- tibble(
    CodeListOID = codelist_oid,
    CodeListName = codelist_name,
    DataType = codelist_datatype,
    CodedValue = xml_attr(codelist_items, "CodedValue"),
    OrderNumber = xml_attr(codelist_items, "OrderNumber"),
    DecodedText = xml_text(xml_find_first(codelist_items, ".//d1:Decode/d1:TranslatedText", ns))
  )

  # Append to list
  codelist_list[[i]] <- item_df
}

# Merge all results into a single dataframe
codelist_df <- bind_rows(codelist_list)

#Combine with the Codelist with the ItemDefs
#Set the order for display
define_df <- left_join(items_comb_df1, codelist_df, by = "CodeListOID",
                       relationship = "many-to-many") %>%
  select(-Name.x, -StandardOID, -Name) %>%
  rename(CodelistOrder = OrderNumber.y, VarOrder = OrderNumber.x,
         MethodType = Type, VarType = DataType.x) %>%
  select(SASDatasetName, Description, Purpose, SASFieldName, VarLabel, VarType,
         Length, VarOrder, CodeListOID,
         CodedValue, DecodedText, MethodType, MethodDescr)

View(define_df)