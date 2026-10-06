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

#' Safely write output files with overwrite check
#' 
#' Write object to output paths. There are three ways to specify output paths. Provide base file names for all objects and a output directory for all, provide base file names and output directories for all objects, or provide file paths for all objects. The resulted file paths must be in the same length of the object. 
#' 
#' @details
#' Write function could be specified in \code{write_func}. If set to \code{NULL}, some file extension will be detected and written with corresponding functions. 
#' 
#' @param obj Object to write.
#' @param ... Placeholder. Nothing will be evaluated in this slot. It enforces whoever use this function to contiously specifying their output as dir, file, or files. 
#' @param dir Directory to write the output files
#' @param file Mantadory if \code{dir} is provided. File name of the output.
#' @param files File path to write the object. 
#' @param write_func Function to write object, the first argument must be object to write and the second argument must be the path. Default to \code{NULL}.
#' @param create_dir Logical. Whether to create directories if the target directories do not exist. If set to \code{TRUE}, the directories will be created recursively until the file can be written. Default to \code{TRUE}.
#' @param write_func_args Arguments to passed to \code{write_func}. 
#' @param overwrite_args Arguments passed to [.overwrite_check()]
#' @param csv_args Arguments passed to [write.table()]
#' @export
.safe_write <- function(
    obj,
    ...,
    dir = NULL,
    file = NULL,
    files = NULL,
    write_func = NULL,
    create_dir = TRUE,
    write_func_args = NULL,
    overwrite_args = list(interactive = interactive()),
    csv_args = list(sep = ";", row.names = FALSE)
) {
    # input check
    if (!missing(...)) {cli::cli_alert_warning("Nothing should be passed to `...`, ignoring ... = {paste(..., sep = ', ')}")}

    if (is.null(dir) && is.null(file) && !is.null(files)) { 
        paths <- files
    } else if (!is.null(dir) && !is.null(file) && is.null(files)) { 
        if(all(length(dir) != 1, length(dir) != length(file))) {
            cli::cli_abort("`dir` and `file` must have the same length, or `dir` must be a vector of 1. ")
        }
        paths <- file.path(dir, file)
    } else if (!is.null(dir) && is.null(file)) {
        cli::cli_abort("`dir` is provided but `file` is not.")
    }

    # FIX: When writing to a single path, ALWAYS wrap `obj` in a list so purrr::pwalk 
    # treats it as 1 cohesive item rather than iterating over its internal structure.
    if (length(paths) == 1) {
        obj <- list(obj)
    } else if (length(obj) != length(paths)) {
        cli::cli_abort("`obj` and provided path must have the same length. {length(obj)} objects provided with `{length(paths)}` paths.")
    }

    overwrite_args_update <- modifyList(list(file = paths, overwrite = F, suffix = T, interactive = F), overwrite_args)
    overwrite_perm <- do.call(.overwrite_check, overwrite_args_update)

    if (any(overwrite_perm < .bitencode(100000))) {
        cli::cli_alert_warning("Writing permission rejected for `{paste(paths[which(overwrite_perm < .bitencode(100000))], sep = ', ')}`. Skipped. Check your overwrite policy.") 
    } 

    ext <- tools::file_ext(paths)
    filenames <- tools::file_path_sans_ext(paths)

    write_granted <- overwrite_perm >= .bitencode(100000)
    use_suffix <- !is.na(attr(overwrite_perm, "suffix"))
    paths[use_suffix] <- paste(paste(filenames[use_suffix], purrr::discard(attr(overwrite_perm, "suffix"), is.na), sep = "-"), ext[use_suffix], sep = ".")

    dir.missing <- !dir.exists(dirname(paths)) 
    if (any(dir.missing)) {
        if (!create_dir) {
            cli::cli_alert_info("Directories {dirname(paths)[dir.missing]} do not exist.")
            create_dir_des <- .user_input_yn("Create?")
        } else {
            create_dir_des <- create_dir
        }

        if (!create_dir_des) {
            cli::cli_abort("Directories {dirname(paths)[dir.missing]} do not exist.")
        } else {
            purrr::walk2(dir.missing, paths, ~ {
                if (.x) {
                    target_dir <- dirname(.y)
                    if (!dir.exists(target_dir)) {
                        dir.create(target_dir, recursive = TRUE)
                        cli::cli_alert_info("Directory {target_dir} created.")
                    }
                }
            })
        }
    }

    # Simplify single_write to inherit from enclosed environment
    single_write <- function(obj, path, ext) {
        if (ext == "csv") {
            wargs_csv <- utils::modifyList(list(x = obj, file = path, sep = ";", row.names = FALSE), csv_args)
            do.call(utils::write.table, wargs_csv)
        } else if (ext %in% c("rds", "RDS")) {
            saveRDS(obj, path)
        } else if (!is.null(write_func)) {
            do.call(write_func, c(list(obj, path), write_func_args))
        } else {
            write(obj, path)
        }
    }

    purrr::pwalk(
        list(
            obj[write_granted], 
            paths[write_granted], 
            ext[write_granted]
        ), 
        single_write
    )
}

#' Update a markdown file with new YAML frontmatter
#' @param fm The modified frontmatter list
#' @param file_path Path to the markdown file
#' @param exclude_property Character vector of keys to strip before writing
#' @param mode Update mode. Unambiguous abbreviations are accepted. Default to \code{modify}, which keeps the unmodified properties. Also accept \code{rewrite}, which ignores original properties and rewrite completely with given \code{fm}. 
#' @export
update_md_frontmatter <- function(fm, file_path, exclude_property = NULL, mode = c("modify", "rewrite")) {
    mode <- match.arg(mode)
    
    # 1. Read the original file
    lines <- readLines(file_path, warn = FALSE)
    delimiters <- grep("^---$", lines)
    
    # 2. Extract original frontmatter and markdown body
    if (length(delimiters) >= 2) {
        yaml_lines <- lines[(delimiters[1] + 1):(delimiters[2] - 1)]
        
        body_start <- delimiters[2] + 1
        if (body_start <= length(lines)) {
            body <- lines[body_start:length(lines)]
        } else {
            body <- character(0)
        }
    } else {
        yaml_lines <- character(0)
        body <- lines # Fallback if no header existed
    }
    
    # 3. Handle modes
    if (mode == "modify") {
        # Parse the original YAML (if any)
        if (length(yaml_lines) > 0) {
            original_fm <- yaml::yaml.load(paste(yaml_lines, collapse = "\n"))
            if (is.null(original_fm)) original_fm <- list()
            fm <- utils::modifyList(original_fm, fm)
        }
    }
    
    # 4. Clean and convert to YAML text
    fm_clean <- .clean_yaml_list(fm, exclude_property)
    new_yaml_text <- yaml::as.yaml(fm_clean)
    
    # 5. Construct and write the new file content
    new_content <- c("---", trimws(new_yaml_text), "---", body)
    writeLines(new_content, file_path)
}

#' Remove Empty Folders
#' 
#' Removes empty folders recursively downwards.
#' @param path Path to directory. 
#' 
#' @export
remove_empty_folders <- function(path) {
    dirs <- list.dirs(path, full.names = TRUE, recursive = TRUE)
    dirs <- dirs[dirs != path]
    dirs <- dirs[order(nchar(dirs), decreasing = TRUE)]
    
    deleted_count <- 0
    
    # 4. Iterate and remove if empty
    for (d in dirs) {
        if (dir.exists(d)) {
        # list.files with all.files = TRUE checks for hidden files too
        # no.. = TRUE prevents "." and ".." from being counted as files
        contents <- list.files(d, all.files = TRUE, no.. = TRUE)
        
        if (length(contents) == 0) {
            message("Deleting: ", d)
            # recursive = TRUE is required in unlink() to remove directories
            unlink(d, recursive = TRUE) 
            deleted_count <- deleted_count + 1
        }
        }
    }
    
    message("Done. Deleted ", deleted_count, " empty folders.")
}

#' Check if paths exists and are directories.
#' @param path Character vector. Path(s) to be tested.
#' @param error Logical. Whether throw an error if any path didn't pass the text. Default to \code{FALSE}.
#' @return A logical vector in same length of \code{path}.
.is_dir <- function(path, error = F) {
    res <- dir.exists(path) & file.exists(path)
    if (error & any(!res)) {
        cli::cli_abort("{path[!res]} are not directories, or do not exist.")
    }
    return(res)
}

#' Check if paths exists and are files. 
#' @param path Character vector. Path(s) to be tested.
#' @param error Logical. Whether throw an error if any path didn't pass the text. Default to \code{FALSE}.
#' @return A logical vector in same length of \code{path}.
.is_file <- function(path, error = F) {
    res <- !dir.exists(path) & file.exists(path)
    if (error & any(!res)) {
        cli::cli_abort("{path[!res]} are not files, or do not exist.")
    }
    return(res)
}
