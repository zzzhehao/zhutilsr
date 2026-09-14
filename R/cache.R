#' Get Cache File Path
#' 
#' @description Internal helper to generate a deterministic file path for cached objects.
#' @param name Character string for the object's base name.
#' @param identifier An arbitrary vector or list to hash.
#' @param pkg Package name for the cache directory. Defaults to the current package environment.
#' @return Character string of the absolute file path.
#' @keywords internal
.get_cache_path <- function(name, identifier, pkg = utils::packageName()) {
    if (is.null(pkg)) pkg <- "pkg_cache_fallback" # Fallback for interactive non-package testing
    
    cache_folder <- tools::R_user_dir(pkg, which = "cache")
    if (!dir.exists(cache_folder)) {
        dir.create(cache_folder, recursive = TRUE, showWarnings = FALSE)
    }
    
    hash_id <- digest::digest(identifier, algo = "md5")
    file.path(cache_folder, sprintf("%s_%s.rds", name, hash_id))
}

#' Write Object to Cache
#'
#' @description Serializes an R object to an OS-specific cache directory as an `.rds` file.
#'
#' @param obj The R object to cache.
#' @param name Character string representing the conventional name of the object.
#' @param identifier A vector or list used to generate a unique hash, distinguishing 
#'   different versions of the object under the same `name`.
#'
#' @return The original object `obj`, invisibly.
#' @export
.cache_write <- function(obj, name, identifier) {
    file_path <- .get_cache_path(name, identifier)
    saveRDS(obj, file = file_path)
    return(invisible(obj))
}

#' Read Object from Cache
#'
#' @description Reads a previously cached `.rds` file based on its name and identifier hash.
#'
#' @param name Character string representing the conventional name of the object.
#' @param identifier A vector or list used to generate the unique hash.
#'
#' @return The cached R object.
#' @details Throws an error if the corresponding cache file does not exist.
#' @export
.cache_read <- function(name, identifier) {
    file_path <- .get_cache_path(name, identifier)
    
    if (!file.exists(file_path)) {
        stop(sprintf("Cache not found for object '%s' with the provided identifier.", name), call. = FALSE)
    }
    
    readRDS(file_path)
}

#' Evaluate Expression with Caching
#'
#' @description Attempts to retrieve an object from the cache. If it is not found, 
#'   it evaluates the provided expression, writes the result to the cache, and returns it.
#'
#' @param name Character string representing the conventional name of the object.
#' @param identifier A vector or list used to generate the unique hash.
#' @param expr An expression to evaluate if the cache is missing. 
#'
#' @return The result of `expr`, either retrieved from cache or newly evaluated.
#' @export
.use_cache <- function(name, identifier, expr) {
    file_path <- .get_cache_path(name, identifier)
    
    if (file.exists(file_path)) {
        return(readRDS(file_path))
    }
    
    # Evaluate the expression if cache is missing
    res <- force(expr)
    
    # Write the newly evaluated result to cache
    saveRDS(res, file = file_path)
    return(res)
}

#' Clean Stale Cache Files
#'
#' @description Deletes cache files that have not been modified within the specified period.
#'
#' @param period Numeric value indicating the maximum age of cache files in days. 
#'   Files older than this will be deleted. Defaults to `7`.
#' @param pkg Package name for the cache directory. Defaults to the current package environment.
#'
#' @return Invisibly returns the number of files deleted.
#' @export
.cache_clean <- function(period = 7, pkg = utils::packageName()) {
    if (is.null(pkg)) pkg <- "pkg_cache_fallback"
    
    cache_folder <- tools::R_user_dir(pkg, which = "cache")
    if (!dir.exists(cache_folder)) return(invisible(0))
    
    cache_files <- list.files(cache_folder, pattern = "\\.rds$", full.names = TRUE)
    if (length(cache_files) == 0) return(invisible(0))
    
    # Get file modification times
    file_info <- file.info(cache_files)
    
    # Calculate age in days
    file_age <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))
    
    # Identify files exceeding the period
    stale_files <- cache_files[file_age > period & !is.na(file_age)]
    
    if (length(stale_files) > 0) {
        unlink(stale_files)
    }
    
    return(invisible(length(stale_files)))
}