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
#' 
#' @export
remove_empty_folders <- function(root_path) {
    dirs <- list.dirs(root_path, full.names = TRUE, recursive = TRUE)
    dirs <- dirs[dirs != root_path]
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


#' Convert a single SVG file to PNG(s) with varying size, color, and opacity
#'
#' @param input Path to the input SVG file.
#' @param output_dir Path to the output directory.
#' @param size Numeric vector of target sizes in pixels.
#' @param alpha Numeric vector of opacity values between 0 and 1.
#' @param hex Character/Numeric vector of colors (e.g., "#000000", "red").
#' 
#' @importFrom purrr map_chr walk
#' @export
svg2png <- function(input, output_dir, size = 256, alpha = 1, hex = "#000000") {
    if (!file.exists(input)) stop("Input file not found.")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (any(alpha < 0 | alpha > 1)) stop("Alpha must be a numeric between 0 and 1.")
    
    base_name <- tools::file_path_sans_ext(basename(input))
    
    # Standardize colors to strict #RRGGBB format via col2rgb
    std_hex <- purrr::map_chr(hex, \(x) {
        rgb_col <- grDevices::col2rgb(x)
        sprintf("#%02X%02X%02X", rgb_col[1], rgb_col[2], rgb_col[3])
    })
    
    # Create the parameter combinations
    params <- expand.grid(size = unique(size), alpha = unique(alpha), hex = unique(std_hex), stringsAsFactors = FALSE)
    
    # 1. Walk over unique colors (Parse XML once per color)
    purrr::walk(unique(params$hex), \(p_hex) {
        
        xml_obj <- xml2::read_xml(input)
        
        xpath_stroke <- "//*[@stroke='currentColor' or @stroke='black' or @stroke='#000000']"
        xpath_fill <- "//*[@fill='currentColor' or @fill='black' or @fill='#000000']"
        
        # Apply color modification
        purrr::walk(xml2::xml_find_all(xml_obj, xpath_stroke), ~xml2::xml_set_attr(.x, "stroke", p_hex))
        purrr::walk(xml2::xml_find_all(xml_obj, xpath_fill), ~xml2::xml_set_attr(.x, "fill", p_hex))
        
        raw_svg <- charToRaw(as.character(xml_obj))
        grid_hex <- params[params$hex == p_hex, ]
        
        # 2. Walk over unique sizes (Rasterize once per size)
        purrr::walk(unique(grid_hex$size), \(p_size) {
            
            img_base <- rsvg::rsvg(svg = raw_svg, width = p_size, height = p_size)
            grid_hex_size <- grid_hex[grid_hex$size == p_size, ]
            
            # 3. Walk over alphas (Apply alpha math and save once per alpha)
            purrr::walk(unique(grid_hex_size$alpha), \(p_alpha) {
                
                img <- img_base
                if (p_alpha < 1) {
                    img[,,4] <- as.raw(round(as.numeric(img[,,4]) * p_alpha))
                }
                
                # Create filesystem-safe suffix
                safe_hex <- gsub("[^A-Za-z0-9]", "", p_hex)
                safe_alpha <- gsub("\\.", "p", as.character(p_alpha))
                suffix <- sprintf("_s%s_a%s_c%s", p_size, safe_alpha, safe_hex)
                
                out_file <- file.path(output_dir, paste0(base_name, suffix, ".png"))
                png::writePNG(img, out_file)
            })
        })
    })
    
    return(invisible(TRUE))
}


#' Convert a folder of SVG files to PNGs with varying size, color, and opacity
#'
#' @param inputfolder Path to the input directory containing SVGs.
#' @param outputfolder Path to the output directory.
#' @param size Numeric vector of target sizes in pixels.
#' @param alpha Numeric vector of opacity values between 0 and 1.
#' @param hex Character/Numeric vector of colors (e.g., "#000000", "red").
#' @param recursive Logical. Should sub-directories be processed while retaining folder structure?
#' @param regex Character. Optional regex string to filter the target SVG files.
#' 
#' @importFrom purrr walk keep
#' @export
svg2pngf <- function(inputfolder, outputfolder, size = 256, alpha = 1, hex = "#000000", recursive = FALSE, regex = NULL) {
    if (!dir.exists(inputfolder)) stop("Input folder not found.")
    
    # Normalize path to handle slashes uniformly for directory diffing
    inputfolder <- normalizePath(inputfolder, winslash = "/", mustWork = TRUE)
    
    svg_files <- list.files(inputfolder, pattern = "\\.svg$", recursive = recursive, full.names = TRUE, ignore.case = TRUE)
    
    # Filter using purrr::keep if regex is provided
    if (!is.null(regex)) {
        svg_files <- purrr::keep(svg_files, ~grepl(regex, .x))
    }
    
    if (length(svg_files) == 0) {
        warning("No SVG files found matching the criteria.")
        return(invisible(FALSE))
    }
    
    # Process each file iteratively 
    purrr::walk(svg_files, \(svg) {
        svg_norm <- normalizePath(svg, winslash = "/")
        svg_dir <- dirname(svg_norm)
        
        # Retain internal folder structure if recursive = TRUE
        if (recursive) {
            rel_dir <- sub(paste0("^", inputfolder), "", svg_dir)
            rel_dir <- sub("^/", "", rel_dir) # Remove leading slash
            current_out_dir <- file.path(outputfolder, rel_dir)
        } else {
            current_out_dir <- outputfolder
        }
        
        svg2png(
            input = svg,
            output_dir = current_out_dir,
            size = size,
            alpha = alpha,
            hex = hex
        )
    })
    
    return(invisible(TRUE))
}