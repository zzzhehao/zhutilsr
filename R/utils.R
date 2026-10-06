#' Ask user for input by binary options.
#' @return Logical. 
#' @export
.user_input_yn <- function(prompt) {
    resp <- readline(sprintf("%s >>> [y/n]", prompt))
    if (tolower(trimws(resp)) %in% c("y", "yes")) {return(TRUE)} else {return(FALSE)}
}

#' Ask user for input by selecting given options.
#' @param random Logical. Randomize option order. Default to \code{TRUE}
#' @param options Character vectors. Options. 
#' @export
.user_input_select <- function(prompt, options, random = T, abort_option = T) {
    if (random) options_r <- sample(options) else options_r <- options
    if (abort_option) {
        options_r <- c(options_r, "cancel")
    }
    n <- length(options_r)

    option_text <- paste(1:n, options_r, sep = ". ") %>% paste(collapse = "\n")

    resp <- readline(sprintf("%s\n\n%s\n\n>>> ", prompt, option_text)) %>% as.numeric()

    while (!resp %in% 1:n) {
        resp <- readline(sprintf("Please select a number\n>>> ", prompt, option_text)) %>% as.numeric()
    }

    if (resp == n) {cli::cli_abort("User has aborted the process.")}
    resp.idx <- which(options == options_r[resp]) 

    attr(resp.idx, "text") <- options[resp.idx]
    return(resp.idx)
}

#' Encode binary results into decimal
#' 
#' @param input A numeric vector, a logical vector or a logical matrix of binary bits to be encoded. Columns of the logical matrix represent the objects and rows the bits. 
#' @return Numeric vector. The converted decimal number.
#' @export
.bitencode <- function(input) {
    if (is.logical(input)) {
        storage.mode(input) <- "numeric"
    } else if (!is.numeric(input)) {
        cli::cli_abort("Input must be numeric or logical.")
    }
    if (is.matrix(input)) {
        input <- apply(input, 2, paste, collapse = "")
    } else {
        input <- paste(format(as.numeric(input), scientific = FALSE), collapse = "")
    }

    return(strtoi(input, 2L))
}

#' Decode decimal number into binary.
#' @param decimal A vector of decimals.
#' @param i Numeric. The position in decoded binary string to extract.
#' @export
.bitdecode <- function(decimal, i) {
  raw_bits <- as.integer(intToBits(decimal))
  
  bit_mat <- matrix(raw_bits, nrow = 32)
  
  bit_df <- as.data.frame(t(bit_mat[32:1, , drop = FALSE]))
  full_bin_strings <- do.call(paste0, bit_df)
  
  bin_strings <- sub("^0+(?=[0-9])", "", full_bin_strings, perl = TRUE)
  
  if (!missing(i)) { # extract i-th digit
    res <- substr(bin_strings, i, i)
    res[res == ""] <- NA_character_
    return(res)
  } else {
    return(as.character(bin_strings))
  }
}

#' Abort function after time out
#' @details Source: https://stackoverflow.com/a/53018594
#' @author landau
#' @export
.with_timeout <- function(expr, cpu, elapsed){
    expr <- substitute(expr)
    envir <- parent.frame()
    setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
    eval(expr, envir = envir)
}

#' Check whether the required dependency is installed
#' @param pkg Character vector. Package names.
#' @param install.call A suggested function (or list of functions) for installing the package if missing. Default to \code{NULL}, no installation will be suggested.
#' @return A logical vector of the same length as \code{pkg} indicating if packages are installed.
#' @export
.dependency_check <- function(
    pkg,
    install.call = NULL
) {
    icall_expr <- rlang::enexpr(install.call)
    if (is.null(icall_expr)) {
        icall_list <- replicate(length(pkg), NULL, simplify = FALSE)
    } else if (rlang::is_call(icall_expr, "list")) {
        icall_list <- rlang::call_args(icall_expr)
    } else {
        icall_list <- replicate(length(pkg), icall_expr, simplify = FALSE)
    }
    
    check_single <- function(p, icall) {
        if (requireNamespace(p, quietly = TRUE)) {
            return(TRUE)
        } 
        
        if (is.null(icall)) {
            cli::cli_alert_danger("Required dependency '{p}' is missing.")
            return(FALSE)
        }
        
        if (!interactive()) {
            return(FALSE)
        }
        
        call <- rlang::call2(icall, p)
        call_text <- rlang::expr_text(call)

        
        cli::cli_alert_warning("Required dependency '{p}' is not installed.")
        cli::cli_text("Do you want to install it? (This will execute: {.code {call_text}})")
        resp <- .user_input_yn("")
        
        if (resp) {
            cli::cli_alert_info("Installing '{p}'...")
            rlang::eval_tidy(call)
            
            if (requireNamespace(p, quietly = TRUE)) {
                cli::cli_alert_success("Successfully installed '{p}'.")
                return(TRUE)
            } else {
                cli::cli_alert_danger("Installation of '{p}' failed.")
                return(FALSE)
            }
        } else {
            cli::cli_alert_warning("Skipped installation of '{p}'.")
            return(FALSE)
        }
    }
    
    results <- mapply(
        check_single, 
        p = pkg, 
        icall = icall_list, 
        USE.NAMES = TRUE
    )
    return(results)
}


#' Clear temporary cache file of R
#' @export
.clear_r_cache <- function() {
    unlink(tempdir(), recursive = TRUE)
    dir.create(tempdir())
}

#' Map a vector's values using a lookup table
#'
#' @param input_vector The vector of original values
#' @param lut_df The lookup data frame
#' @param key_col The unquoted column name in `lut_df` to match against.
#' @param value_col The unquoted column name in `lut_df` to get the new values from.
#' @param keep_original Keep original value if no match is found. Default is TRUE.
#'
#' @return A new vector with the mapped values, in the same order as the input.
#' @export
map_values <- function(input_vector, lut_df, key_col, value_col, keep_original = TRUE) {    
    key_col_string <- rlang::as_name(rlang::enquo(key_col))
    value_col_string <- rlang::as_name(rlang::enquo(value_col))

    match_indices <- match(input_vector, lut_df[[key_col_string]])
    mapped_values <- lut_df[[value_col_string]][match_indices]
    
    # fall back
    if (keep_original) {
        na_mask <- is.na(mapped_values)
        mapped_values[na_mask] <- input_vector[na_mask]
    }

    return(mapped_values)
}

#' Find Best Match in A Vector of Strings using Automatic Gap Detection
#'
#' @author Zhehao Hu
#'
#' @param pattern A pattern to look for.
#' @param strings A vector of strings to look for the pattern.
#' @param index Logical. Whether to return index of the best matching item in the string vector (TRUE) or return the best matching item value (FALSE).
#' @param silent Logical. Set to TRUE to disable result printing.
#' @export
find_best_match <- function(pattern, strings, index = F, silent = F) {
    name.dist <- stringdist::stringdistmatrix(strings, pattern) %>% as.numeric()
    name.dist.sorted <- name.dist %>% sort()
    weights <- 1 / log(name.dist.sorted[-1] + 1) # weight gap significance decreasingly while upper value of the gap increases
    maxGapIndex.sorted <- diff(name.dist.sorted) * weights %>% which.max()
    threshold <- mean(name.dist.sorted[c(
        maxGapIndex.sorted,
        maxGapIndex.sorted + 1
    )]) # identify the threshold of the gap
    pattern.match <- strings[name.dist < threshold]

    cat("\nPattern given as:", pattern, "\nMatched", pattern.match, "\n\n")

    if (index) {
        return(which(name.dist < threshold))
    } else {
        return(pattern.match)
    }
}

#' Safely remove flat or nested properties from a list
#' @param lst The frontmatter list 
#' @param excludes A character vector of properties to remove (e.g., c("name", "parent$child"))
.clean_yaml_list <- function(lst, excludes = NULL) {
    if (is.null(excludes) || length(excludes) == 0) return(lst)
    
    # split nested properties into vectors
    paths <- strsplit(excludes, "$", fixed = TRUE)
    
    for (path in paths) {
        tryCatch({
            lst[[path]] <- NULL
        }, error = function(e) {
            warning(paste("Could not remove property:", paste(path, collapse = "$")))
        })
    }
    
    return(lst)
}