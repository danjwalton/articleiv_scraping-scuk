#This function downloads Article IV documents from IMF's library
#The search uses IMF's coveo rest API to find all pdf-attached results with 'Article IV Consultation' in the title
#Specifying the year(s) and/or isocode(s) of results relies on IMF's correct filling of these fields. Specifying NULL will include all results.

imf_aiv_dl <- function(download_dir = "pdfs", years = 2020:2022, isos = NULL, new_only = T, report_times = F){
  
  #Install required packages if not already present
  packages <- c("data.table","httr","jsonlite")
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(packages)
  suppressPackageStartupMessages(lapply(packages, require, character.only=T))
  
  if(!is.null(years)){
    imf_years <- paste0(paste0('%40imfyear%3D%22', years, '%22'), collapse = "%20OR%20")
  } else {
    imf_years <- "@imfyear"
  }
  
  if(!is.null(isos)){
    imf_isos <- paste0(paste0('%40imfisocode%3D%22', isos, '%22'), collapse = "%20OR%20")
  } else {
    imf_isos <- "@imfisocode"
  }
  
  t1 <- system.time({
  search_url <- "https://www.imf.org/coveo/rest/v2?sitecoreItemUri=sitecore%3A%2F%2Fweb%2F%7B5A91C724-B3F7-41A4-B7A8-8A9426E83B65%7D%3Flang%3Den%26amp%3Bver%3D12&siteName=imf"
  search_body <- function(i, results_per_page){paste0("aq=((%40title*%3D%22Article-iv-Consultation%22))%20(%40source%3D%3D(%22Coveo_web_index%20-%20PRD93-SITECORE-IMFORG%22))%20(", imf_years, ")%20(", imf_isos, ")&cq=%40filetype%3DPDF&searchHub=Article-iv-staff-reports&locale=en&maximumAge=900000&firstResult=", (i-1)*results_per_page, "&numberOfResults=", results_per_page, "&excerptLength=0&fieldsToExclude=%5B%5D&enableDidYouMean=false&sortCriteria=%40imfdate%20descending&queryFunctions=%5B%5D&rankingFunctions=%5B%5D&groupBy=%5B%5D&categoryFacets=%5B%5D&retrieveFirstSentences=false&timezone=&enableQuerySyntax=false&enableDuplicateFiltering=true&enableCollaborativeRating=false&debug=false&allowQueriesWithoutKeywords=true")}
  
  pre <- POST(search_url, add_headers(c("content-type" = "application/x-www-form-urlencoded; charset=\"UTF-8\"")), body = search_body(1,1))
  pre_result <- fromJSON(rawToChar(pre$content))
  
  message(paste0(pre_result$totalCountFiltered, " potential results found. Checking if any should be downloaded..."))
  
  results_per_page = 100
  pages <- ceiling(pre_result$totalCountFiltered/results_per_page) + 1
  #pb <- txtProgressBar(0, pages, style = 3)
  imf_results <- list()
  for(i in 1:pages){
    tmp <- POST(search_url, add_headers(c("content-type" = "application/x-www-form-urlencoded; charset=\"UTF-8\"")), body = search_body(i, results_per_page))
    tmp_result <- fromJSON(rawToChar(tmp$content))
    imf_results[[i]] <- data.table(tmp_result$results)
    #setTxtProgressBar(pb, i)
  }
  
  imf_results_dt <- rbindlist(imf_results, fill = T)
  })
  
  if(report_times) message("Checking took ", round(t1[3], 2), " seconds.")
  
  if(new_only){
    
    imf_results_existing <- gsub(".*[/]", "", list.files(download_dir, ".pdf", recursive = T, ignore.case = T))
    
    imf_results_new <- imf_results_dt[!(raw.filename %in% imf_results_existing)]
    
    if(nrow(imf_results_new) > 0){
      message("\n", paste0(nrow(imf_results_new), " new Article IV documents found for this selection."))
    } else {

      return(message(paste0("\nNo new Article IV documents found for this selection.")))
    }
  } else {
    
    imf_results_new <- imf_results_dt
    message("Downloading ", paste0("new_only is FALSE.", nrow(imf_results_new), " Article IV document", ifelse(nrow(imf_results_new) == 1, "", "s"), " for this selection."))
  }
  
  if(!dir.exists(download_dir)){
    
    dir.create(download_dir)
  }
  
  t2 <- system.time({
  pb <- txtProgressBar(0, nrow(imf_results_new), style = 3)
  fails <- 0
  for(i in 1:nrow(imf_results_new)){
    
    result_new <- imf_results_new[i]
    
    url <- result_new$clickUri
    iso <- paste0(result_new$raw.imfisocode[[1]], collapse = ",")
    if(iso == "")iso <- "Unknown"
    country <- paste0(result_new$raw.imfcountry[[1]], collapse = ",")
    year <- result_new$raw.imfyear
    filename <- result_new$raw.filename
    
    if(!dir.exists(paste0(download_dir, "/", iso))){
      
      dir.create(paste0(download_dir, "/", iso))
    }
    
    fails <- fails + download.file(url, paste0(download_dir, "/", iso,"/", filename), quiet = T, mode = "wb")
    setTxtProgressBar(pb, i)
  }
  })
  
  message("\n------------------------------\nDownload summary:\n------------------------------\n",
          ifelse(new_only, paste0(length(imf_results_existing), " document" , ifelse(length(imf_results_existing) == 1, "", "s"), " already downloaded.\n"), ""),
          nrow(imf_results_new) - fails, " document", ifelse(nrow(imf_results_new) - fails == 1, "", "s"), " newly downloaded.\n",
          fails, " document downloads failed.\n------------------------------")
  
  if(fails > 0)message("You can re-run this function to try downloading again.")
  if(report_times) message("Downloading took ", round(t2[3], 2), " seconds.")
  
}

