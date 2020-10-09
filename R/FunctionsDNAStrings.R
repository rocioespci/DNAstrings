## Simple functions to (reverse) complement DNA sequences
dna_complement <- function(x){
  if(length(x)>1){stop("dna_complement: x has length > 1")}
  if(x=="A" | x=="a"){ y <- "T"}
  if(x=="C" | x=="c"){ y <- "G"}
  if(x=="G" | x=="g"){ y <- "C"}
  if(x=="T" | x=="t"){ y <- "A"}

  return(y)
}
dna_complement_vector <- function(x){
  y <- sapply(x, dna_complement, USE.NAMES = F)
  return(y)
}
dna_complement_string <- function(x){
  x <- strsplit(x, split="")[[1]]
  y <- dna_complement_vector(x)
  y <- paste(y, collapse = "")
  return(y)
}
dna_reverse_complement_vector <- function(x){
  y <- rev(dna_complement_vector(x))
  return(y)
}
dna_reverse_complement_string <- function(x){
  x <- strsplit(x, split="")[[1]]
  y <- dna_reverse_complement_vector(x)
  y <- paste(y, collapse = "")
  return(y)
}
