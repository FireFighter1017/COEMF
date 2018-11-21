normVendor <- function(vendor){
# Normalizes vendor name format for comparison
# and corrects any wrong names to facilitate
# vendor identification
#
# Args:
#   vendor: vector of character values reprensenting 
#           vendor names
# Return:
#   vendor: Normalized vendor names single spaced,
#           containing only alphanumeric values.

  # Uppercase
  vendor <- toupper(vendor)

  # Remove anything that is not a letter or a space
  vendor <- gsub("[^a-zA-Z[:space:][:digit:]]", "", vendor)
  
  # Remove carriage returns
  vendor <- gsub("\\n", "", vendor)
  
  # Corrections helping recognition
  # Thomas & Betts is often spelled T&B (TB when normalized)
  vendor[vendor=="TB"] <- "THOMAS & BETTS"
  
  return(vendor)
}
