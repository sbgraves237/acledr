grepInTable <- function(pattern, referenceTable = 
          countrySynonyms[, -1], ignore.case=TRUE, 
          collapse=', ', ...){
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
      ix <- grep(ux[iStr], ctrySyn[[i]], ...)
      if(length(ix)>0) break  
    }
    if(length(ix)<1){
      out[iStr] <- paste('No match found for', 
                         pattern[iStr])
    } else{
      iso3ix <- unique(iso3[ix])
      out[iStr] <- paste(iso3ix, collapse=collapse)
      if(nchar(out[iStr])<1){ 
        out[iStr] <- paste('Found in row', ix, 
                "of referenceTable with column 1 = ''")
      }
    }
  }
  out
}