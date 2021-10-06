# This function converts a scalar string of quality values into a vector of Phred scores.
# The conversion calculation is: Phred score = ASCII value of letter - offset

decode_qualities <- function(qualities, offset=33) {
  
  assertthat::assert_that(assertthat::is.scalar(qualities))
  assertthat::assert_that(assertthat::is.string(qualities))  
  assertthat::assert_that(assertthat::is.scalar(offset))
  assertthat::assert_that(assertthat::is.number(offset))
  
  if (!(offset==33 | offset==64)) {
    base::stop("Offset can only be 33 or 64")
  }
  
  as.integer(base::charToRaw(qualities)) - offset -> phred_scores
  
  if (any(phred_scores < 1)) {
    base::stop("Negative phred scores produced - check offset")
  }
  
  return(phred_scores)
}