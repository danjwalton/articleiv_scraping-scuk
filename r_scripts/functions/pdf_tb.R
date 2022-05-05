#This function parses given Article IV pdfs and identifies the page(s) with tables matching the terms given.
#Specify either a single pdf file or a pdf directory. The latter will consider all pdfs found in the directory, and if recursive = T, all pdfs found in sub-folders. 

imf_aiv_tb <- function(pdf_file = NULL, pdfs_dir = NULL, table_terms, output_name = "imf_aiv_tables.csv", new_only = T, recursive = T, report_times = F){
  
  if((is.null(pdf_file)&is.null(pdfs_dir))|(!is.null(pdf_file)&!is.null(pdfs_dir))){
    
    stop("Please specify one parameter for PDF(s): pdf_file or pdfs_path.")
  }
  
  #Install required packages if not already present
  packages <- c("data.table","rstudioapi","pdftools")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(packages)
  suppressPackageStartupMessages(lapply(packages, require, character.only=T))
  
  if(is.null(pdf_file) & !is.null(pdfs_dir)){
    
    output_dir <- pdfs_dir
    pdfs <- list.files(pdfs_dir, ".pdf", full.names = T, recursive = recursive)
    if(length(pdfs) == 0){
      
      stop("No PDFs found in the specified path.")
    }
  } else {
    
    output_dir <- dirname(pdf_file)
    pdfs <- pdf_file
  }
  
  if(file.exists(paste0(output_dir, "/", output_name)) & new_only){
    output <- fread(paste0(output_dir, "/", output_name))
    pdfs <- pdfs[!(pdfs %in% output$pdf)]
  }

  t1 <- system.time({
  message("Checking table", ifelse(length(pdfs) == 1, "", "s"), " in ", length(pdfs), " Article IV documents.")
  pb <- txtProgressBar(0, length(pdfs), style = 3)
  pdf_tables <- list()
  fails <- c()
  for(i in 1:length(pdfs)){
  
    tryCatch({
      pdf_pages <- suppressMessages(pdf_text(pdfs[i]))
      table_pages <- (1:length(pdf_pages))[unlist(lapply(pdf_pages, function(x) any(grepl(paste0(paste0("Table.?\\d+\\D?[.].*", table_terms), collapse = "|"), strsplit(x, "\n")[[1]], ignore.case = T))))]
      
      pdf_tables[[i]] <- data.table(pdf = pdfs[i], table_pages = paste0(table_pages, collapse = ","))
    }, error= function(e){
      fails <<- c(fails, pdfs[i])
      message("\nWarning: '", pdfs[i], "' cannot be parsed. Skipping.")
      })
    
    setTxtProgressBar(pb, i)
  }
  
  pdf_tables <- rbindlist(pdf_tables, fill = T)
  
  if(exists("output")){
    pdf_tables <- rbind(output, pdf_tables, fill = T)
  } else {
    output <- data.table()
  }
  })
  
  message("\n------------------------------\nPDF parsing summary:\n------------------------------\n",
          ifelse(new_only, paste0(nrow(output), " PDF" , ifelse(nrow(output) == 1, "", "s"), " already parsed.\n"), ""),
          length(pdfs) - length(fails), " PDF", ifelse(length(pdfs) - length(fails) == 1, "", "s"), " newly parsed.\n",
          nrow(pdf_tables[table_pages == ""]) - length(fails), " PDF", ifelse(nrow(pdf_tables[table_pages == ""]) - length(fails) == 1, "", "s"), " did not have any matching tables found.\n",
          length(fails), " PDF parsings failed.\n------------------------------")
  
  fwrite(pdf_tables, paste0(output_dir, "/", output_name))
  if(report_times) message("Parsing took ", round(t1[3], 2), " seconds.")
}

