# Make first letter of any string capital, and all other letters lower case
cap.first<-function(string){
  
  split<-strsplit(string, " |-")
  
  sapply(split, function(split.entry){
    paste0(paste0(substr(toupper(split.entry), 1, 1),
                  substr(tolower(split.entry), 2, nchar(split.entry))),
           collapse=" ")
  })
}