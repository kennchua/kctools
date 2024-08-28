#' Theme for ggplot objects (with my preferred defaults)
#'
#' Similar to hrbrthemes::theme_ipsum()
#' @param data The data frame to tabulate
#' @param tabvar The variables to tabulate
#' @param dropna If TRUE then drops NA values; if FALSE then includes NA values
#' @return A ggplot2 object.
#' @import ggplot2
#' @examples
#' \dontrun{
#' ggplot(mtcars, aes(y = mpg, x = wt, color = factor(am))) +
#'   geom_point() +
#'   geom_smooth() +
#'   labs(x = "Weight", y = "Miles per Gallon", color = "Automatic",
#'        caption = "Source: Author's calculations using mtcars dataset") +
#'   theme_kc() +
#'   theme(legend.position = "bottom")
#'
#' ggplot(mtcars, aes(y = mpg, x = wt)) +
#'   geom_point() +
#'   geom_smooth() +
#'   facet_wrap(vars(am), scales = "free_x") +
#'   labs(x = "Weight", y = "Miles per Gallon",
#'        caption = "Source: Author's calculations using mtcars dataset") +
#'   theme_kc() +
#'   theme(legend.position = "bottom")
#'
#' }


theme_kc <- function(text_size = 14, text_font = 'Arial Narrow') {

  theme(text = element_text(family = text_font),
        title = element_text(family = text_font),

        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#cccccc", fill = NA, linewidth = 0.18),
        panel.grid.major = element_line(color = "#cccccc", linewidth = 0.18),
        panel.grid.minor.x = element_blank(),
        #panel.grid.minor.x = element_line(color = "#cccccc", linewidth = 0.18),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(t = 0.5, r = 0.4, b = 0.1, l = 0.1, unit = "cm"),

        axis.text = element_text(size = text_size, family = text_font),
        axis.title = element_text(size = text_size, family = text_font,
                                  hjust = 0.5),
        axis.ticks = element_blank(),

        plot.caption = element_text(size = text_size - 3,
                                    hjust = 0, face = 'plain'),
        legend.position = "none",
        legend.text = element_text(size = text_size-1, family = text_font),
        legend.title = element_text(size = text_size-1, family = text_font),

        strip.text = element_text(size = text_size, family = text_font,
                                  hjust = 0.5, color = "#818589"),
        strip.background = element_rect(color = "#F8F8F8", fill = "#F8F8F8"))


}

