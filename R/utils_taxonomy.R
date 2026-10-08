#' Format Orgnism Name like a Descent Taxonomist
#'
#' @description 
#' Applies appropriate formatting (italics) to organism names. The genus name is always italicized. Specific epithets are 
#' formatted unless they are "sp.". Taxonomic qualifiers like "cf." and "aff." are 
#' left unformatted, while the subsequent species epithets (splitted by whitespace) are formatted. 
#' 
#' Can output standard HTML/Markdown tags or `plotmath`-safe strings for `ggplot2`, or using any defined opening and closing tags. 
#'
#' @param label A character vector of organism/taxa names to format.
#' @param format An optional character string specifying a preset format. Accepts 
#'   unambiguous partial matches of `"html"`, `"markdown"`, or `"plotmath"`. 
#'   If provided, this overrides `tag_open`, `tag_close`, and `plotmath.safe`.
#' @param tag_open A character string for the opening format tag (default: `"<i>"`). 
#'   Ignored if `format` is specified.
#' @param tag_close A character string for the closing format tag (default: `"</i>"`). 
#'   Ignored if `format` is specified.
#' @param plotmath.safe Logical. If `TRUE`, formats the output as a valid R expression 
#'   string for `ggplot2` parsing (e.g., replacing spaces with `~` and wrapping 
#'   non-italicized text in single quotes). Ignored if `format` is specified.
#'
#' @return A character vector of formatted organism names, the same length as `label`.
#' @export
format_organism_name <- function(label, format = NULL, tag_open = "<i>", tag_close = "</i>", plotmath.safe = FALSE) {
  
    # Handle optional formatting presets with unambiguous matching
    if (!is.null(format)) {
        format <- match.arg(tolower(format), c("html", "markdown", "plotmath"))
        
        if (format == "html") {
            tag_open <- "<i>"
            tag_close <- "</i>"
            plotmath.safe <- FALSE
        } else if (format == "markdown") {
            tag_open <- "*"
            tag_close <- "*"
            plotmath.safe <- FALSE
        } else if (format == "plotmath") {
            tag_open <- "italic('"
            tag_close <- "')"
            plotmath.safe <- TRUE
        }
    }
    

    safe_token <- function(x, italicize) {
        if (italicize) {
            paste0(tag_open, x, tag_close)
        } else if (plotmath.safe) { # quote everything irrelevant
            paste0("'", x, "'") 
        } else {
            x
        }
    }
  
    # Apply to the whole vector
    purrr::map_chr(label, \(name) {
        # NA handling
        if (is.na(name) || trimws(name) == "") return(name)
        
        # split name
        tokens <- strsplit(trimws(name), "\\s+")[[1]]
        
        # temporal names
        if (all(stringr::str_detect(tokens, "[0-9]+"))) {
            if (plotmath.safe) {
                return(sprintf("'%s'", name))
            } else {
                return(name)
            }
        }
        
        # Track which tokens should be italicized
        is_italic <- rep(FALSE, length(tokens))
        is_italic[1] <- TRUE # genus name is always italic
        
        if (length(tokens) >= 2) {
            if (tokens[2] == "sp.") {
                # sp and everything after sp are not italic 
            } else if (tokens[2] %in% c("cf.", "aff.")) {
                if (length(tokens) >= 3 && tokens[3] != "sp.") {
                    is_italic[3] <- TRUE # epithet
                }
            } else { # epithet
                is_italic[2] <- TRUE
            }
        }
        
        # Apply formatting to all tokens
        for (i in seq_along(tokens)) {
            tokens[i] <- safe_token(tokens[i], is_italic[i])
        }
        
        # Join tokens appropriately
        if (plotmath.safe) {
            paste(tokens, collapse = "~")
        } else {
            paste(tokens, collapse = " ")
        }
    })
}