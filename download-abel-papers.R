########################################################
## download Abel papers                               ##
## Friedolin Merhout                                  ##
## Duke University                                    ##
########################################################


# this script downloads all Nazi biograms collected by Theodore Abel and digitized
# by the Hoover Institution in 2017: https://www.hoover.org/news/newly-digitized-nazi-biograms-now-available
# original script courtesy of Christopher Spoerlein: https://chspoerlein.netlify.com/
# provided by Sarah Patton (Hoover Institution): https://www.hoover.org/library-archives/visit

# load required packages
library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)
library(beepr)

# loop through Nazi biograms in Hoover Institute archive

# first, get metadata
for (k in 58227:58810){
  # retrieve metadata
  meta_link <- paste0("https://digitalcollections.hoover.org/objects/",k,"/json")
  data <- fromJSON(meta_link)
  # extract name from json
  name <- data$object$title$value
  # extract number of pages
  pages <- data$object$dimensions$value # get page no entry from json
  if (!is.null(pages)){
  pages <- as.numeric(gsub("\\D", "", pages))} else { # sub out all non-digit characters and return numeric value
    pages <- NA
    if(!exists("pages_missing")){
      pages_missing <- k} else {
      pages_missing <- c(pages_missing, k)
    }
    print(paste0(name, ", record number ", k, ", is missing number of pages."))
  }
  if(!exists("meta_df")){
    meta_df <- data_frame(record_id = k, name = name, pages = pages)} else {
      meta_df[nrow(meta_df)+1,] <- c(k, name, pages)
    }
  print(k)
}

# loop is done
beep(6)

# add page number where missing in json
# "Hermann Herx" and "Hans Schumacher"
# discrepancy between Hoover Institution metainfo and archive
# lists 18 and 28 pages, files contain 19 and 30 images respectively
meta_df$pages[meta_df$record_id%in%pages_missing] <- c(19,30)
meta_df$pages <- as.numeric(meta_df$pages)

## case id 58254 is breaking loop/download
## name Georg Lossau. potential issue page n (48)

# second, get associated image files
for (k in seq_along(meta_df$record_id)){
  # iterate over people using the info on the number of pages
  doc_link <- paste0("https://digitalcollections.hoover.org/objects/", meta_df$record_id[k])
  html <- read_html(doc_link)
  # extract all links
  ref <-  html_nodes(html,"a")
  # link no 9 has the picture id
  # !!! look at this again to figure out exactly what it does
  id <- as.numeric(str_extract(ref[9], "[0-9]{6}"))
  # create file directory
  # use doc_ids to avoid having to deal with duplicate names and punctuations
  # generate path
  path <- paste0("~/Dropbox/Work/Nazis and Contemporaries/Abel papers/",  meta_df$record_id[k])
  # create new folder
  dir.create(path)
  # set wd to new folder
  setwd(path)
  # loop through all files and download them to specified path
  page_n <- meta_df$pages[k]
    for (i in id:(id+page_n-1)) {
      img_link <- paste0("https://digitalcollections.hoover.org/internal/media/dispatcher/", i, "/resize:format=full")
      download.file(img_link, destfile = paste0(meta_df$record_id[k], "_",i,".jpg"), mode="wb")
      print(paste0("Successfully downloaded image ", length(list.files(path)), " of ", meta_df$pages[k], 
                   " for record number ", meta_df$record_id[k], "."))
    }
  print(paste0("Successfully downloaded ALL images for record number ", meta_df$record_id[k], ". Finished iteration ", k, "."))
  }

# loop is done
beep(6)