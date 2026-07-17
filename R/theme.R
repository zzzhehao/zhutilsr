#' HPLNC Graphic Themes
#' @param ... Further argument to be passed to [ggplot2::theme()]
#' @name themes
NULL

#' @rdname themes
#' @export
theme_monochrome <- function(...) {
    list(
        ggplot2::theme_minimal(),
        ggplot2::theme(
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(
                color = "black",
                fill = "transparent",
                linewidth = 1
            ),
        ),
        ggplot2::theme(...)
    )
}

#' @rdname themes
#' @export
theme_monochrome_open <- function(...) {
    list(
        ggplot2::theme_minimal(),
        ggplot2::theme(
            axis.line.x.bottom = ggplot2::element_line(color = "black", linewidth = 0.5),
            axis.line.y.left = ggplot2::element_line(color = "black", linewidth = 0.5),
            panel.grid.minor.x = ggplot2::element_blank()
        ),
        ggplot2::theme(...)
    )
}

#' @rdname themes
#' @export
theme_pub <- function(...) {
    list(
        ggplot2::theme(
            ...
        ),
        ggplot2::labs(title = "", subtitle = "", caption = "")
    )
}

#' Remove Descriptions on `ggplot` Plots for Publication
#' @export
pub_nodescription <- function(ggobj){
    ggobj <- ggobj +
        ggplot2::theme(
            plot.title = ggplot2::element_blank(),
            plot.subtitle = ggplot2::element_blank(),
            plot.caption = ggplot2::element_blank()
        )
    return(ggobj)
}

#' Generate a color palette from base palette 
#' @param palette Base palette in hex code to interpolate.
#' @param n Number of colors to generate.
#' @return A vector of colors.
#' @export
paletten <- function(palette, n) {
  interpolate_colors <- colorRampPalette(palette)
  new_palette <- interpolate_colors(n)
  return(new_palette)
}