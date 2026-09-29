#' Alternating Brightness Adjustment for Color Palettes
#'
#' @description 
#' Takes a vector of hex colors and artificially forces contrast between neighboring 
#' colors by making odd-indexed colors brighter and even-indexed colors darker.
#' 
#' @param hex_colors Character vector of hex color codes.
#' @param factor Numeric adjustment factor (0 to 1, e.g., 0.2 for a 20% shift).
#' @return Character vector of updated hex codes.
#' @export
.adjust_palette_brightness <- function(hex_colors, factor = 0.2) {
    if (is.null(factor) || factor == 0 || length(hex_colors) < 2) {
        return(hex_colors)
    }
    
    # 1. Convert hex to RGB, then to HSV (Hue, Saturation, Value/Brightness)
    rgb_matrix <- grDevices::col2rgb(hex_colors)
    hsv_matrix <- grDevices::rgb2hsv(rgb_matrix)
    
    # 2. Apply alternating brightness to the "v" (value) channel
    for (i in seq_along(hex_colors)) {
        if (i %% 2 != 0) {
            # Odd index: make brighter (cap at 1)
            hsv_matrix["v", i] <- min(1, hsv_matrix["v", i] + factor)
        } else {
            # Even index: make darker (floor at 0)
            hsv_matrix["v", i] <- max(0, hsv_matrix["v", i] - factor)
        }
    }
    
    # 3. Convert back to hex
    grDevices::hsv(h = hsv_matrix["h", ], s = hsv_matrix["s", ], v = hsv_matrix["v", ])
}