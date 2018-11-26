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
  vendor[vendor==0] = NA
  vendor[vendor=="TB"] <- "THOMAS & BETTS"
  vendor[vendor=="SKF CANADA"] <- "SKF"
  vendor[grep("^REGAL BELOIT", vendor)] <- "REGAL BELOIT"
  vendor[grep("^ABB MOTOR", vendor)] <- "ABB"
  vendor[vendor=="NTN BEARING CORP OF CANADA LTD"] <- "NTN"
  vendor[grep("MARTIN SPROCKET", vendor)] <- "MARTIN SPROCKET & GEAR"
  vendor[grep("^REXNORD", vendor)] <- "REXNORD"
  vendor[vendor=="CHEMLINE PLASTICS LIMITED"] <- "CHEMLINE"
  vendor[vendor=="JDB BEARINGS OF CANADA LTD"] <- "JDB BEARINGS"
  vendor[grep("^OSISENSE", vendor)] <- "TELEMECANIQUE"
  vendor[grep("^YUEQING BETHEL", vendor)] <- "YUEQING BETHEL"
  vendor[grep("^ADAPTA BEACON", vendor)] <- "EDWARDS"
  
  return(vendor)
  
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
