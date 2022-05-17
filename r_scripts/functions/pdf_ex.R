#This function 

imf_aiv_ex <- function(table_file = "pdfs/imf_aiv_tables.csv", new_only = T, try_OCR = T, report_times = F){
  
  #Install required packages if not already present
  packages <- c("data.table","pdftools", "tesseract")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(packages)
  suppressPackageStartupMessages(lapply(packages, require, character.only=T))
  
  tables <- fread(table_file)
  
  tables_present <- tables[!is.na(table_pages)]
  
  tables_todo <- tables_present[, .(csv = substr(paste0(dirname(pdf), "/", gsub("[.]pdf", "", basename(pdf)), "_", gsub('[/\\?%*:|"<>]', "-", table_names, perl = T), ".csv"), 1, 255)), by = .(pdf, table_pages, table_names)]
  
  if(new_only){
    
    tables_existing <- list.files(dirname(table_file), ".csv", recursive = T, ignore.case = T, full.names = T)
    tables_existing <- gsub("_OCR", "", tables_existing)
    tables_todo <- tables_todo[!(csv %in% tables_existing)]
  }
  
  if(nrow(tables_todo) == 0){
    
    return(message("No tables to extract."))
  }
  
  t1 <- system.time({
  message(nrow(tables_todo), " table", ifelse(nrow(tables_todo) == 1, "", "s"), " to extract.")
  pb <- txtProgressBar(0, nrow(tables_todo), style = 3)
  ocrs <- c()
  fails <- c()
  for(i in 1:nrow(tables_todo)){
    
    pdf <- tables_todo$pdf[i]
    page <- as.numeric(tables_todo$table_pages[i])
    output_csv <- tables_todo$csv[i]
    
    tryCatch({
      
      output <- suppressMessages(data.table(strsplit(pdf_text(pdf)[page], "\n")[[1]]))
    
      if(nrow(output) < 20){
        
        if(try_OCR){
          
          page_image <- pdf_convert(pdf, page, dpi = 600, filenames = "tmp", format = "png", verbose = F, antialias = "text")
          output <- data.table(strsplit(ocr(page_image, eng = tesseract(options = c("preserve_interword_spaces" = 1))), "\n")[[1]])
          output_csv <- gsub("[.]csv", "_OCR.csv", output_csv)
          ocrs <- c(ocrs, output_csv)
        } else {
          
          next
        }
        
      }
      
    }, error = function(e) {
      
      fails <<- c(fails, output_csv)
    })
    
    output[, V1 := iconv(V1, from = "UTF-8")]
    output[, V1 := gsub("(?<=\\d) \\d+[/]", "   ", V1, perl = T)]
    fwrite(output, output_csv)
    setTxtProgressBar(pb, i)
  }
  })
  
  message("\n------------------------------\nExtraction summary:\n------------------------------\n",
          ifelse(new_only, paste0(length(tables_existing), " table" , ifelse(length(tables_existing) == 1, "", "s"), " already present.\n"), ""),
          nrow(tables_todo) - length(fails), " table", ifelse(nrow(tables_todo) - length(fails) == 1, "", "s"), " newly extracted.\n",
          length(ocrs), " table", ifelse(length(ocrs) == 1, "", "s")," extracted using OCR.\n",
          length(fails), " table extractions failed.\n------------------------------")
  
  ex_summary <<- (list(OCRs = ocrs, Failures = fails))
  if(report_times) message("Extracting took ", round(t1[3], 2), " seconds.")
}
