#' @title a ggplot theme with a mixture of dark and light backgrounds
#'
#' @description
#' A theme based on ggplot theme_minimal. Uses default R colors for plot background
#'
#' @param font_size Base font size
#' @param legend_position the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param color_theme Choose from a combination of light and dark themes ("va_light","va_dark","light","lightcyan","cornsilk","antiquewhite","darkslategray","gray25","turquoise4","steelblue")
#'
#' @family ggplot functions
#' @export
#' @examples
#' for(i in c("va_light","va_dark"
#' ,"light","lightcyan","cornsilk","antiquewhite"
#' ,"darkslategray","gray25","turquoise4","steelblue")){
#' print(
#'  iris %>%
#'     ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=factor(paste("Petal",round(Petal.Length,0))))) +
#'       geom_point() +
#'       labs(x="Sepal Length", y="Sepal Width",
#'         title="Petals and Sepals",ro
#'           subtitle="Plot shows the sizes of petals and sepals",
#'           caption=i,
#'           color = "Legend") +
#'           facet_wrap(~Species,scales = "free") +
#'           theme_darklightmix(color_theme = i) +
#'           scale_color_brewer(type="qual",palette = "Set3")  )
#'           }



theme_darklightmix <- function(font_size = 10
                     ,legend_position = "right"
                     ,color_theme ="light"){

  palette <- .get_palette()

  color_theme_no <- case_when(color_theme == "va_light" ~ 1
                              ,color_theme == "va_dark" ~ 2
                              ,color_theme == "light" ~ 3
                              ,color_theme == "lightcyan" ~ 4
                              ,color_theme == "cornsilk" ~ 5
                              ,color_theme == "antiquewhite" ~ 6
                              ,color_theme == "darkslategray" ~ 7
                              ,color_theme == "gray25" ~ 8
                              ,color_theme == "turquoise4" ~ 9
                              ,color_theme == "steelblue" ~ 10
  )

  plot_background = lapply(palette, `[[`,color_theme_no)[[2]]
  font_color =  lapply(palette, `[[`,color_theme_no)[[3]]
  line_color =  lapply(palette, `[[`,color_theme_no)[[3]]

  new <- theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.7, 0.5, 0.5, 0.5,unit= "cm") #top,right,bottom,left
      ,plot.background = element_rect(fill = plot_background, colour = NA)
      ,plot.title=element_text(colour=font_color, size=font_size * 1.6, face = "bold"
                               ,margin = ggplot2::margin(0,0,0.3,0,unit= "cm"))
      ,plot.subtitle=element_text(colour=font_color, size=font_size * 1.1
                                  ,margin = ggplot2::margin(-0.1,0,0.5,0,unit= "cm"))
      ,plot.caption=element_text(colour=font_color, size=font_size * 0.8, hjust = 1,face = "italic"
                                 ,margin = ggplot2::margin(-0.1,0,0,0,unit= "cm"))

      ,legend.title = element_text(colour=font_color, size=font_size)
      ,legend.title.align = 0.5
      ,legend.text = element_text(colour=font_color, size=font_size
                                  ,margin = ggplot2::margin(0,0,0,-0.1,unit= "cm"))
      ,legend.position = legend_position

      ,axis.title = element_text(colour=font_color, size=font_size)
      ,axis.title.x = element_text(margin = ggplot2::margin(0.4,0,0,0,unit= "cm")) #top,right,bottom,left
      ,axis.title.y = element_text(margin = ggplot2::margin(0,0.4,0,0,unit= "cm")) #top,right,bottom,left
      ,axis.text = element_text(colour=font_color, size=font_size * 0.8)

      #        ,panel.background =  element_rect(fill = scales::alpha(panel_background, 0.2),color = NA)
      ,panel.grid.minor=element_blank()
      ,panel.grid.major =element_line(color=scales::alpha(line_color,0.2), size=0.1)
      ,panel.spacing = grid::unit(0.5,"cm")

      ,strip.text = element_text(colour=font_color, size=font_size
                                 ,margin = ggplot2::margin(0,0,0.4,0,unit= "cm"))  #top,right,bottom,left
      # ,strip.background = element_rect(color ="black", linetype = "dotted")

    )
}

#Theme Color combinations
.get_palette <- function(){
  list(
    color_theme = c("va_light","va_dark"
                   ,"light","lightcyan","cornsilk","antiquewhite"
                   ,"darkslategray","gray25","turquoise4","steelblue")
    ,plot = c("lavenderblush1","mediumpurple4"
              ,"#ffffe8","lightcyan","cornsilk","antiquewhite"
              ,"darkslategray","gray25","turquoise4","steelblue")
    ,font_line = c("black","white"
                   ,"black","black","black","black"
                   ,"white","white","white","white")
  )
}




