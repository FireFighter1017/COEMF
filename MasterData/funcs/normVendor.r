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
    # Remove invalid entries
  VENDOR[VENDOR==0] = NA
  VENDOR[VENDOR=="TB"] <- "THOMAS & BETTS"
  VENDOR[VENDOR=="SKF CANADA"] <- "SKF"
  VENDOR[grep("^REGAL BELOIT", VENDOR)] <- "REGAL BELOIT"
  VENDOR[grep("^ABB MOTOR", VENDOR)] <- "ABB"
  VENDOR[VENDOR=="NTN BEARING CORP OF CANADA LTD"] <- "NTN"
  VENDOR[grep("MARTIN SPROCKET", VENDOR)] <- "MARTIN SPROCKET & GEAR"
  VENDOR[grep("^REXNORD", VENDOR)] <- "REXNORD"
  VENDOR[VENDOR=="CHEMLINE PLASTICS LIMITED"] <- "CHEMLINE"
  VENDOR[VENDOR=="JDB BEARINGS OF CANADA LTD"] <- "JDB BEARINGS"
  VENDOR[grep("^OSISENSE", VENDOR)] <- "TELEMECANIQUE"
  VENDOR[grep("^YUEQING BETHEL", VENDOR)] <- "YUEQING BETHEL"
  VENDOR[grep("^ADAPTA BEACON", VENDOR)] <- "EDWARDS"
  
  return(VENDOR)
  
  return(vendor)
}

normParts <- function(PART){
# Normalizes part number format for comparison
#
# Args:
#   PART: vector of character values reprensenting 
#           part numbers
# Return:
#   PART: Normalized part numbers with no space

    # Uppercase
  PART <- toupper(PART)
    # Remove any spaces
  PART <- gsub("[:space:]", "", PART)
    # Remove carriage returns
  PART <- gsub("\\n", "", PART)
  PART[PART==0] = NA
  
  return(PART)
}
