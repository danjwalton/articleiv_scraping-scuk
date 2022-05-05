#This function combines key terms used to search for selected tables
#

term_combiner <- function(terms_group1 = NULL, terms_group2 = NULL, terms_standalone = NULL){
  terms <- c(apply(rbind(expand.grid(terms_group1, terms_group2), expand.grid(terms_group2, terms_group1)), 1, paste0, collapse = ".*"), terms_standalone)
  return(terms)
}

