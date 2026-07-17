#' format organism label 
#' @export
format_organism_name <- function(label, tag_open = "<i>", tag_close = "</i>", plotmath.safe = F) {
  
    # Apply the helper function to the whole vector
    purrr::map_chr(label, \(name){
        # NA handling
        if (is.na(name) || trimws(name) == "") return(name)
        
        # split name
        tokens <- strsplit(trimws(name), "\\s+")[[1]]

        # temporal names
        if (all(stringr::str_detect(tokens, "[0-9]+"))) {
            if (plotmath.safe) {
                return(sprintf("plain('%s')", name))
            } else {
                return(name)
            }
        }
        
        # genus name
        tokens[1] <- paste0(tag_open, tokens[1], tag_close)
        
        if (length(tokens) >= 2) {
        if (tokens[2] == "sp.") {
            # sp and everything after sp are not italic
        } else if (tokens[2] %in% c("cf.", "aff.")) {
            # check content after cf or aff
            if (length(tokens) >= 3) {
            if (tokens[3] == "sp.") { # same as as
            } else { # epithet
                tokens[3] <- paste0(tag_open, tokens[3], tag_close)
            }
            }
        } else { # epithet
            tokens[2] <- paste0(tag_open, tokens[2], tag_close)
        }
        }
        
        if (plotmath.safe) {
            paste(tokens, collapse = "~")
        } else {
            paste(tokens, collapse = " ")
        }
    })
}