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


#' Check if target folder exists and has files
#' @return Numeric code for overwrite permission. 0: user-defined overwrite or target folder does not yet exist. 1: user-specified overwrite rejection. 2: overwrite rejected and no options given for permission request. 
.overwrite_check <- function(
    dir,
    overwrite,
    interactive
) {
    if (overwrite) {return(invisible(0))}

    exist <- dir.exists(dir) & length(list.files(dir) > 0)
    if (!exist) {return(invisible(0))}
    if (!overwrite & exist) {
        if (interactive) {
            cli::cli_alert_warning("Directory `{dir}` already exists and has files.")
            resp <- readline("Overwrite? <y/n>")
            if (!resp %in% c("y", "Y")) {
                cli::cli_abort("Overwrite rejected.")
                return(invisible(1))
            } else {
                cli::cli_alert_info("Overwrite granted.")
                return(invisible(0))
            }
        } else {
            cli::cli_abort("Directory `{dir}` already exists and has files. Use either `overwrite = TRUE` or interactive decision to overwrite.")
            return(invisible(2))
        }
    }
} 

#' Check output directory 
#' @param dir A path to output directory.
#' @param overwrite Overwrite permission. Default to \code{FALSE}
#' @param interactive Interactive console request on user for granting overwrite permission.
#' @export
.dir_check <- function(
    dir,
    overwrite = FALSE,
    interactive = TRUE
) {
    o <- .overwrite_check(dir, overwrite, interactive)

    suppressWarnings({dir.create(dir, recursive = T)})
    if (o == 0) {
        file.remove(list.files(dir, full.names = TRUE, recursive = TRUE))
    }
}

#' Check whether the required dependency is installed
#' @param pkg Package name.
#' @param install.call A suggested call for installing the package if missing. The call must be passed quoted by [quote()] or [rlang::expr()].
#' @export
.dependency_check <- function(
    pkg,
    install.call = NULL
) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        return(TRUE)
    } else {
        if (!is.null(install.call)) {
            if (!interactive()) {
                return(FALSE)
            }
            call_text <- rlang::expr_text(install.call)
            
            cli::cli_alert_warning("Required dependency '{pkg}' is not installed.")
            cli::cli_text("Do you want to install it? (This will execute: {.code {call_text}})")
            resp <- readline(">>> [y/n]: ")
            
            if (tolower(trimws(resp)) %in% c("y", "yes")) {
                cli::cli_alert_info("Installing '{pkg}'...")
                rlang::eval_tidy(install.call)
                if (requireNamespace(pkg, quietly = TRUE)) {
                    cli::cli_alert_success("Successfully installed '{pkg}'.")
                    return(TRUE)
                } else {
                    cli::cli_alert_danger("Installation of '{pkg}' failed.")
                    return(FALSE)
                }
            } else {
                cli::cli_alert_warning("Skipped installation of '{pkg}'.")
                return(FALSE)
            }
        } else {
            cli::cli_alert_danger("Required dependency '{pkg}' is missing. {.say_no()}")
            return(FALSE)
        }
    }
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
