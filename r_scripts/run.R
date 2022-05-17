#########
# SETUP #
#########

##Run this section first every time you open the script

{
#Install required packages if not already present (first time running this may take some time)
packages <- c("data.table", "jsonlite","rstudioapi", "pdftools", "tesseract")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packages)
invisible(suppressPackageStartupMessages(lapply(packages, require, character.only=T)))

#Set working directory
setwd(dirname(dirname(getActiveDocumentContext()$path)))

#Pull in functions from scripts
invisible(lapply(list.files("r_scripts/functions", "[.]R$", full.names = T, recursive = T), function(x) source(x)))
}

##

############
# DOWNLOAD #
############

##Download Article IV documents and save them in folders by IMF-specified country ISO code.##

#download_dir is the folder where you want the PDFs to be saved. It will be created if it doesn't exist.
#years is the IMF-specified years of the documents. Set a range or set as 'NULL' to capture all years.
#isos is the IMF-specified country ISO code of the documents. Set a selection (e.g. c("GBR", "USA", "KEN")), or leave as 'NULL' to capture all years.

imf_aiv_dl(download_dir = "pdfs", years = 2018:2022, isos = NULL, report_times = T)

##

########################
# TABLE IDENTIFICATION #
########################

##Create search terms for tables we are interested in finding##

#Specify key terms for table titles we want to extract
#We are combining terms which indicate government or public sector, and those which indicate accounts or aggregates
#Standalone terms pick up a few cases where table names are not picked up by our general terms
government_terms <- c("Government", "Public Sector", "National")
account_terms <- c("Operation", "Financ", "Account", "Fiscal", "Budget", "Aggregates")
standalone_terms <- c("Consolidated General Government", "Consolidated Government", "Fiscal Developments and Projections", "Budgetary Operations", "Medium-Term Fiscal Scenario", "Fiscal Accounts", "Fiscal Aggregates", "Fiscal Operations")

#This function combines our group terms such that we produce all combinations government and accounts terms
search_terms <- term_combiner(terms_group1 = government_terms, terms_group2 = account_terms, terms_standalone = standalone_terms)


##Parse PDFs for tables matching the key terms; this function saves a table in the PDFs directory containing page information##

#pdfs_dir is the folder where the pdfs are located. The function will search through sub-folders in this directory.
#table_terms is the vector of key terms used to identify relevant tables - we get this from the previous function.
#output_name is the CSV file outputted containing the page numbers of tables found by PDF.

imf_aiv_tb(pdfs_dir = "pdfs", table_terms = search_terms, output_name = "imf_aiv_tables.csv", new_only = T, report_times = T)

##

####################
# TABLE EXTRACTION #
####################

##Extract tables from the PDFs based on the selected table pages.##

#table_file is the output file from the previous function containing a list of PDFs and the page numbers of tables to extract per PDF.
#new_only is a logical indicating whether only previously un-extracted tables should be extracted. Setting this to false will redo all extractions.
#try_OCR is a logical indicating whether to use Optical Character Recognition to extract table information which is stored as an image.

imf_aiv_ex(table_file = "pdfs/imf_aiv_tables.csv", new_only = T, try_OCR = T, report_times = T)

##
