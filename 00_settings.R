Sys.setenv(LANG = "en")

# 1. Dependencies ---------------------------

# 1.1. Used Libraries:

my_packages <- c("ggplot2",    # graphical library
                 "svglite",    # A graphics device for R that produces scalable vector graphics (*.svg)
                 "gghalves",   # half violin, i.e. pdf plots
                 "patchwork",  # https://patchwork.data-imaginist.com/articles/guides/assembly.html
                 "factoextra", # fviz_dend() - dendrogram plots
                 "dendextend"  # https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html
                 ) 

# "pals"     # more colour palettes
# "ggthemes" # more colour palettes

# 1.2. Install packages ---------------------

my_installed_packages <- my_packages %in% rownames(installed.packages())

if (any(my_installed_packages == FALSE)) install.packages(my_packages[!my_installed_packages])

# 1.3. Load Packages  ----------------------

invisible(lapply(my_packages, library, character.only = TRUE))

remove("my_packages", "my_installed_packages")


# 2. Input Parameters ---------------------

my_y_unit <- "[kW]"

K <- 6 # number of clusters

is_normalised <- FALSE # Normalised distance for shape emphasis! 

hdd_threshold <- 16 # Heating Degree Day threshold in [°C]

my_colour_palette <- RColorBrewer::brewer.pal(K,'RdYlBu') # cold to heat colour palette

# my_colour_palette <- RColorBrewer::brewer.pal(K,'Dark2')

if (K == 2) my_colour_palette <- c(my_colour_palette[1], my_colour_palette[3]) 

# my_colour_palette <- as.vector(kovesi.diverging_bwr_40_95_c42(K))
# my_colour_palette <- as.vector(kovesi.diverging_linear_bjr_30_55_c53(K))

my_colour_ind <- "#ffbf00"
my_colour_urb <- "#608000"
my_colour_sub <- "#9bce00"

df_papers_width_mm <- data.frame(
  
  A2width  = 420,
  A2heigth = 594,
  
  A4width  = 210,
  A4heigth = 297)


# 3. My Plots' theme ---------------------------

my_ggtheme <- theme_minimal() + theme(legend.position = "none",
                                      
                                      plot.title = element_text(size = 7 , hjust = 0.5),
                                      
                                      axis.ticks = element_line(),
                                      
                                      # Y-axis:
                                      axis.title.y = element_text(angle = 0, size = 8, vjust = 0.5, face = "bold"),
                                      axis.text.y  = element_text(size = 7),
                                      
                                      # X-axis:
                                      axis.title.x = element_text(angle = 0, size = 8, vjust = 0.5, face = "bold"),
                                      axis.text.x  = element_text(angle = 0, size = 6)
)

theme_set(my_ggtheme) # Set as default theme


# 4. Load Functions  ---------------------

source("functions_core.R")