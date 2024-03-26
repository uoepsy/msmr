get_my_data <- function(exam_num){
  tryCatch(
    suppressWarnings(load(url(paste0("https://uoepsy.github.io/msmr/2324/misc/",toupper(exam_num),".RData")))), 
    error=function(e) cat("No Data Found. Are you sure you used the correct exam number?\nExam numbers can be found on your student card, and take the form of the letter B followed by 6 numbers, for example get_my_data(\"B123456\")\nIf you cannot make this work with your exam number, please use get_my_data(\"UNKNOWN\")"))
  cbtmodes <<- cbtmodes
  locus <<- locus
}