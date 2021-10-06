# This function should take in a vector of DNA sequence strings and return a 
# vector of %GC values (what percentage of the bases are G or C).

#' Title Calculates the QC content of a set of DNA sequences
#'
#' @param seq A character vestor of DNA sequences
#'
#' @return A numeric vector of GC percentages 
#' @export
#'
#' @examples
#' qc_content("GATATCG)
gc_content <- function(seq){
  
  assertthat::assert_that(is.character(seq), msg="seq is not a character vector")
  
  base::toupper(seq) -> seq
  
  if (any(stringr::str_detect(seq,"[^GATC]"))) {
    base::warning("Non GATC characters found in seq")
  }
  
  stringr::str_replace_all(seq,"[^GC]","") -> GC
  return(100*(nchar(GC)/nchar(seq)))
}



# The function should read a fastq file and put it into a tibble with one row per 
# fastq entry where the columns are:
#  1. ID (the sequence ID from the first line, minus the @)
#  2. Bases (the bases from the second line)
#  3. Qualities (the quality string from the 4th line)
#  4. GC (the GC content of the bases)

#' Title
#'
#' @param file A file in fastqR format
#'
#' @return a tibble contaning the read ids, base calls and quality strings 
#' @export
#'
#' @examples
read_fastq <- function(file){
  
  assertthat::assert_that(assertthat::is.readable(file), msg="Not a readable file")
  assertthat::assert_that(assertthat::has_extension(file,"fq"))
  
  base::scan(file, character()) -> raw_data
  raw_data[c(T,F,F,F)] -> id
  raw_data[c(F,T,F,F)] -> seq
  raw_data[c(F,F,F,T)] -> qual
  
  if (!all(base::startsWith(id,"@"))) {
    base::stop("Some ID lines didn't start with @ as specified")
  }
  
  stringr::str_sub(id,start=2) -> id
  
  if (!all(base::nchar(seq)==base::nchar(qual))) {
    base::stop("some sequances were a different length to the qual")
  }
  
  if (any(base::duplicated(id))) {
    base::stop("Some Id's are duplicatated")
  }
  
  tibble::tibble(ID=id, 
                 Bases=seq,  
                 Qualities=qual, 
                 GC=gc_content(seq)) %>% 
    return()
}