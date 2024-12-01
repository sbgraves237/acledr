#' grep for each element of a character vector patter in referenceTable
#'
#' @param pattern A character vector containing regular expressions to search for matches in refereanceTable[, -1] and return the corresponding element of referenceTable[, 1]
#' @param referenceTable An object to search for pattern in as.data.frame(referenceTable)[-1]
#' @param ignore.case FALSE to search for pattern; TRUE to search for toupper(pattern)
#' @return A character vector containing matches or error messages with names = pattern
#' @examples
#' AfgNethAnt <- grepInTable(c('Afghanistan', 'Netherlands Antilles'))
#' AfgNthAnt <- c(Afghanistan="AFG", "Netherlands Antilles" = 
#'        "Found in row 179 of referenceTable with column 1 = ''") 
#' all.equal(AfgNethAnt, AfgHtnAnt)
grepInTable <- function(pattern, referenceTable = 
                          rworldmap::countrySynonyms[, -1], 
                        ignore.case=TRUE){
  rT <- as.data.frame(referenceTable)
  if(ignore.case){ 
    ux <- toupper(pattern)
    iso3 <- toupper(rT[, 1])
    ctrySyn <- lapply(rT[-1], toupper)
  } else {
    ux <- pattern
    iso3 <- rT[, 1]
    ctrySyn <- rT[-1]
  }
  nRefs <- length(ctrySyn)
  nStr <- length(pattern)
  out <- rep(NA, nStr)
  names(out) <- pattern
  for(iStr in 1:nStr){ 
    for(i in 1:nRefs){
      ix <- which(ux[iStr] == ctrySyn[[i]])
      if(length(ix)>0) break
      ix <- grep(ux[iStr], ctrySyn[[i]])
      if(length(ix)>0) break  
    }
    if(length(ix)<1){
      out[iStr] <- paste('No match found for', 
                         pattern[iStr])
    } else{
      out[iStr] <- iso3[ix]
      if(nchar(out[iStr])<1){ 
        out[iStr] <- paste('Found in row', ix, 
                "of referenceTable with column 1 = ''")
      }
    }
  }
  out
}