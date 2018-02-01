
#' @import tidyverse
dropRedundantTags <- function(pos_lists, sparse=0.99){
  dt_pos <- tibble(raw = (unlist(pos_lists))) %>%
    mutate(token = gsub("_[A-Z]+$","",raw)) %>%
    mutate(pos = gsub("^.*_","",raw)) %>%
    group_by(raw) %>%
    summarize(count=n(), token=first(token),pos=first(pos))
  dt_keepers<- dt_pos %>%
    filter(count>length(pos_lists)*(1-sparse)) %>%
    filter(token%in%(token[duplicated(token)]))
  dt_switchers<-dt_pos[!dt_pos$raw%in%dt_keepers$raw,]
  subbed_lists<-lapply(pos_lists, function(x) unlist(plyr::mapvalues(x,
                                                                     dt_switchers$raw,
                                                                     dt_switchers$token,
                                                                     warn_missing=F)))
  return(subbed_lists)
}



