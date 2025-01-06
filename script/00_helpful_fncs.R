#helpful functions

# set default plotting theme
get_theme <- function(txt = 12, ...){
  theme_light() + theme(
    axis.text    = element_text(size = txt),
    axis.title   = element_text(size = txt),
    strip.text   = element_text(size = txt),
    legend.text  = element_text(size = txt - 1),
    legend.title = element_text(size = txt - 1),
    title        = element_text(size = txt),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(6, "pt"),
    axis.line.x.top  = element_line(color = "black", linewidth = 3),
    text = element_text(face = "bold", size = txt),
    ...)
}

# get_theme <- function(txt = 12, ...){
#   theme_bw() + 
#   theme(
#       legend.background = element_blank(),
#       #legend.box.background = element_rect(colour = "black"),
#       #legend.spacing.y = unit(4, "mm"), 
#       legend.key=element_rect(fill="white"),
#       #panel.background = element_blank(),
#       axis.line.x = element_line(color="black", linewidth = 1),
#       axis.line.y = element_line(color="black", linewidth = 1),
#       axis.ticks = element_line(colour = "black"), 
#       axis.ticks.length = unit(6, "pt"),
#       axis.line.x.top  = element_line(color="black", size=3),
#       #axis.text.x=element_text(angle=45, hjust=1), 
#       axis.text = element_text(colour = "black",size=txt -2),
#       text = element_text(face="bold",size=txt),
#       legend.text  = element_text(size = txt - 1),
#       legend.title = element_text(size = txt - 1),
#      # panel.border = element_rect(colour = "black", fill=NA),
#       panel.background = element_rect(colour = "black", size=1),
#       ...)
# }
