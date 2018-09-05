
##
## V1: password with random length between 8 and 21 characters  
##      and combination of four types of characters
gen_pass_v1 <- function(x){
  require(magrittr)
  require(stringr)
  set.seed(x) # to allow replicability of passw generation
  # special character vector borrowed from moazzem's answer:
  # https://stackoverflow.com/questions/22219035/function-to-generate-a-random-password
  punc <- "!#$%&’()*+,-./:;<=>?@[]^_`{|}~" %>%
    str_split("", simplify = TRUE) %>%
    as.vector()
  chars <- list(punc,LETTERS,0:9,letters) # list of all potential characters
  pass_chars_l<- lapply(chars,function(x) sample(x,sample(2:3,1))) # get 2-3 from each set
  pass_chars<- unlist(pass_chars_l) # unlist sets of password characters
  passw <- str_c(sample(pass_chars),collapse = "") # jumble characters and combine into password
  return(passw)
}


##
## V2: allows setting the length of the password
##
gen_pass_v2 <- function(len=8,seeder=NULL){
  set.seed(seeder) # to allow replicability of passw generation
  all_combs <- expand.grid(1:(len-3),1:(len-3),1:(len-3),1:(len-3)) # all combinations of 4
  sum_combs <- all_combs[apply(all_combs, 1, function(x) sum(x)==len),] # summing to length len
  punc <- unlist(strsplit("!#$%&()*+-/:;<=>?@[]^_{|}~","")) # special character vector
  chars <- list(punc,LETTERS,0:9,letters) # list of all potential characters
  # retrieve the number of characters from each list element specified in sampled row of sum_combs
  pass_chars_l<- mapply(sample, chars, sum_combs[sample(1:nrow(sum_combs),1),],replace = TRUE) 
  pass_chars<- unlist(pass_chars_l) # unlist sets of password characters
  passw <- paste0(sample(pass_chars),collapse = "") # jumble characters and combine into password
  return(passw)
}


##
## V3: adds option to determine special characters to use
##
gen_pass_v3 <- function(len=8,sp.char="!#$%&()*+-/:;<=>?@[]^_{|}~",seeder=NULL){
  set.seed(seeder) # to allow replicability of passw generation
  all_combs <- expand.grid(1:(len-3),1:(len-3),1:(len-3),1:(len-3)) # all combinations of 4
  sum_combs <- all_combs[apply(all_combs, 1, function(x) sum(x)==len),] # summing to length len
  punc <- unlist(strsplit(sp.char,"")) # special character vector
  chars <- list(punc,LETTERS,0:9,letters) # list of all potential characters
  # retrieve the number of characters from each list element specified in sampled row of sum_combs
  pass_chars_l<- mapply(sample, chars, sum_combs[sample(1:nrow(sum_combs),1),],replace = TRUE) 
  pass_chars<- unlist(pass_chars_l) # unlist sets of password characters
  passw <- paste0(sample(pass_chars),collapse = "") # jumble characters and combine into password
  return(passw)
}


##
## V4: without special characters
##
gen_pass_v4 <- function(len=8,seeder=NULL){
  set.seed(seeder) # to allow replicability of passw generation
  all_combs <- expand.grid(1:(len-3),1:(len-3),1:(len-3)) # all combinations of 4
  sum_combs <- all_combs[apply(all_combs, 1, function(x) sum(x)==len),] # summing to length len
  chars <- list(LETTERS,0:9,letters) # list of all potential characters
  # retrieve the number of characters from each list element specified in sampled row of sum_combs
  pass_chars_l<- mapply(sample, chars, sum_combs[sample(1:nrow(sum_combs),1),],replace = TRUE) 
  pass_chars<- unlist(pass_chars_l) # unlist sets of password characters
  passw <- paste0(sample(pass_chars),collapse = "") # jumble characters and combine into password
  return(passw)
}
