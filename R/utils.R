#' Ask user for input by binary options.
#' @return Logical. 
#' @export
.user_input_yn <- function(prompt) {
    resp <- readline(sprintf(">>> %s [y/n]", prompt))
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

#' Check overwrite policy
#' 
#' @param ... This is a placeholder, it enforces whoever use this function to consciously specify their input correctly as a directory or file. 
#' @param dir Path to directory (can be a vector)
#' @param file Path to file (can be a vector)
#' @param overwrite Logical or vector. Whether to overwrite the target. 
#' @param suffix Logical or vector. Whether to use unique suffix for output. If \code{overwrite} is \code{TRUE}, this value is always treated as \code{FALSE}.
#' @param interactive Logical or vector. Whether to start an interactive instance in console to let user decide overwrite option. If \code{overwrite} is \code{TRUE}, this value is always treated as \code{FALSE}.
#' @return A vector of binary encoded check results. See details in attributes. 
#' @export
.overwrite_check <- function(
    ...,
    dir = NULL,
    file = NULL,
    overwrite = FALSE,
    suffix = FALSE,
    interactive = FALSE
) {
    # input check
    if (!missing(...)) {
        cli::cli_alert_warning("Nothing should be passed to `...`, ignoring ... = {paste(..., sep = ', ')}")
    }
    
    if (!is.null(dir) && !is.null(file)) {
        cli::cli_abort("One check at a time. `dir` or `file`, not both.")
    }
    
    if (is.null(dir) && is.null(file)) {
        cli::cli_abort("I have nothing to check, so { .say_no() }")
    }
    
    # Determine paths and types
    is_dir <- !is.null(dir)
    paths <- if (is_dir) dir else file
    n_paths <- length(paths)
    
    # Recycle arguments to match the length of paths
    overwrite <- rep_len(overwrite, n_paths)
    suffix <- rep_len(suffix, n_paths)
    interactive <- rep_len(interactive, n_paths)
    
    # Core logic function for a single path
    check_single <- function(p, is_d, ow, suf, int) {
        user <- FALSE
        
        # check exist
        if (is_d) {
        exist <- dir.exists(p) && length(list.files(p)) > 0
        } else {
        exist <- file.exists(p)
        }
        
        # right back
        if (ow) {
        suf <- FALSE
        int <- FALSE
        }
        
        if (int && !suf && exist) {
        user_select <- .user_input_select(
            sprintf("Path `%s` already exists. What to do?", p), 
            c("overwrite", "use unique suffix")
        )
        if (user_select == 1) {
            cli::cli_alert_info(sprintf("Overwrite granted by user for: %s", p))
            ow <- TRUE
            user <- TRUE
        } else if (user_select == 2) {
            suf <- TRUE
            user <- TRUE
        } else if (user_select == 3) {
            user <- TRUE
        }
        }
        
        # conflict ===
        # solve with suffix
        suffix_next <- NA_real_
        
        if (exist && suf) {
            if (is_d) {
                dirs <- list.dirs(dirname(p), recursive = FALSE)
                dirs_suffix <- grep(sprintf("%s-[0-9]+$", p), dirs, value = TRUE)
                
                if (length(dirs_suffix) == 0) {
                suffix_next <- 1
                } else {
                suffix_next <- max(as.numeric(stringr::str_extract(dirs_suffix, sprintf("(?<=%s-)[0-9]+$", p)))) + 1
                }
            } else {
                files <- list.files(dirname(p), full.names = TRUE)
                ext <- tools::file_ext(p)
                filenames <- tools::file_path_sans_ext(p)
                files_suffix <- grep(sprintf("%s-[0-9]+\\.%s", basename(filenames), ext), files, value = TRUE) 
                
                if (length(files_suffix) == 0) {
                    suffix_next <- 1
                } else {
                    suffix_next <- max(as.numeric(stringr::str_extract(files_suffix, sprintf("(?<=%s)-[0-9]+(?=\\.%s)", basename(p), ext)))) + 1
                }
            }
        }
        
        # Return a list for this specific element to be aggregated later
        code <- .bitencode(c(ow || suf, ow, user, suf, exist, is_d))
        list(
            code = code,
            bitcode = stringr::str_pad(.bitdecode(code), 5, "left", "0"),
            suffix = suffix_next
        )
    }
    
    # Apply across all inputs
    results <- mapply(
        check_single, 
        p = paths, 
        is_d = is_dir, 
        ow = overwrite, 
        suf = suffix, 
        int = interactive, 
        SIMPLIFY = FALSE
    )
    
    # Aggregate results into a vectorized return object
    codes <- sapply(results, `[[`, "code")
    
    attr(codes, "bitcode") <- sapply(results, `[[`, "bitcode")
    attr(codes, "suffix") <- sapply(results, `[[`, "suffix")
    attr(codes, "bitcode_info") <- data.frame(
        bit = 1:6,
        meaning = c(
        "write permission",
        "overwrite permission",
        "user decision",
        "unique suffix",
        "file already existed",
        "path is folder"
        )
    )
    
    return(codes)
}

.safe_write <- function(
    obj,
    ...,
    dir = NULL,
    file = NULL,
    files = NULL,
    overwrite_args = list(interactive = interactive()),
    csv_args = list(sep = ";", row.names = FALSE)
) {
    # input check
    if (!missing(...)) {cli::cli_alert_warning("Nothing should be passed to `...`, ignoring ... = {paste(..., sep = ', ')}")}

    if (is.null(dir) && is.null(file) && !is.null(files)) { # direct file paths
        paths <- files
    } else if (!is.null(dir) && !is.null(file) && is.null(files)) { # dir + file
        # dir+file combi check
        if(all(length(dir) != 1, length(dir) != length(file))) {
            cli::cli_abort("`dir` and `file` must have the same length, or `dir` must be a vector of 1. ")
        }
        paths <- file.path(dir, file)
    }

    if (length(obj) != length(paths)) {
        cli::cli_abort("`obj` and provided path must have the same length. {length(obj)} objects provided with `length(path)` paths.")
    }

    overwrite_args_update <- modifyList(list(file = paths, overwrite = F, suffix = T, interactive = F), overwrite_args)
    overwrite_perm <- do.call(.overwrite_check, overwrite_args_update)

    if (any(overwrite_perm < .bitencode(100000))) {
        cli::cli_alert_warning("Writing permission rejected for `{paste(path[which(overwrite_perm < .bitencode(100000))], sep = ', ')}`. Skipped. Check your overwrite policy.") 
    } 

    # write profile
    ext <- tools::file_ext(paths)
    filenames <- tools::file_path_sans_ext(paths)

    # update with suffix
    write_granted <- overwrite_perm >= .bitencode(100000)
    use_suffix <- !is.na(attr(overwrite_perm, "suffix"))
    paths[use_suffix] <- paste(paste(filenames[use_suffix], purrr::discard(attr(overwrite_perm, "suffix"), is.na), sep = "-"), ext[use_suffix], sep = ".")

    single_write <- function(obj, path, ext, csv_args = List()) {
        if (ext == "csv") {
            wargs_csv <- modifyList(list(x = obj, file = path, sep = ";", row.names = FALSE), csv_args)
            do.call(write.table, wargs_csv)
        } else if (ext %in% c("rds", "RDS")) {
            saveRDS(obj, path)
        } else {
            write(obj, path)
        }
    }

    can_parallel <- all(.dependency_check(c("mirai", "carrier"), install.packages))
    if (can_parallel) {
        purrr::pwalk(list(obj[write_granted], paths[write_granted], ext[write_granted]), purrr::in_parallel(\(o, p, e) single_write(o, p, e, csv_args), single_write = single_write, csv_args = csv_args))
    } else {
        purrr::pwalk(list(obj[write_granted], paths[write_granted], ext[write_granted]), single_write)
    }
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
