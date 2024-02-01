library(raster)
library(rgdal)
library(ggplot2)

world_all <- readOGR('/Users/thiagoferreira53/Desktop/Final_Code_Figures_NCC//ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')
world <- world_all[!world_all$CONTINENT == 'Antarctica',]

worldRobin <- spTransform(world, CRS("+proj=robin"))
#plot(worldRobin)
worldRobin_df <- fortify(worldRobin)

baseline_new <- raster("/Users/thiagoferreira53/Projects/GlobalWB/Data\ and\ figures/Data/Data_figure1.asc")
future_new <- raster("/Users/thiagoferreira53/Projects/GlobalWB/Data\ and\ figures/Data/Data_figure2.asc")

#Merge data from rasters and create a dataframe
create_df <- function(raster){
  spdf <- as(raster, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  df
}

baseline_new_df <- create_df(baseline_new)
colnames(baseline_new_df)[1] <- 'loss'

future_new_df <- create_df(future_new)
colnames(future_new_df)[1] <- 'loss'

#blue
colors <- c("#0066CC", "#FF9933", "#FF6600","#FF3366", "#FF0000", "#CC0000", "#990000")


#create groups for visualization, format world map & plot data
world_plot <- function(df){
  df$groups <- NA
  
  df$groups[df$loss == 0] <- "0"
  df$groups[df$loss > 0 & df$loss <= 10] <- "0.1 - 10"
  df$groups[df$loss > 10 & df$loss <= 20] <- "10.1 - 20"
  df$groups[df$loss > 20 & df$loss <= 30] <- "20.1 - 30"
  df$groups[df$loss > 30 & df$loss <= 40] <- "30.1 - 40"
  df$groups[df$loss > 40 & df$loss <= 50] <- "40.1 - 50"
  df$groups[df$loss > 50] <- "50.1+"
  
  df_Robin_format <- project(cbind(df$x, df$y), proj="+proj=robin") 
  valuesRobin <- as.data.frame(df_Robin_format)
  names(valuesRobin) <- c("long", "lat")
  valuesRobin$groups <- df$groups
  valuesRobin[is.na(valuesRobin)] = 0
  
  ggplot() +
    geom_point(data=valuesRobin, aes(long, lat,color = groups), shape='.')+
    geom_map(data = worldRobin_df, map = worldRobin_df, aes(long, lat, map_id = id), 
             color = "black", fill = NA, size = 0.2) +
    scale_color_manual(values = colors)+
    theme_void()+
    guides(color = guide_legend(override.aes = list(shape=15, size = 5)))+ 
    theme(legend.key=element_blank(),
          legend.title = element_text(size=14),
          legend.text = element_text(size=12),
          text = element_text(family = "Helvetica"))+
    scale_fill_discrete(name = "Yield loss(%)")+ 
    labs(color='Yield loss (%)') 
}

fig_baseline <- world_plot(baseline_new_df)
fig_baseline

fig_future <- world_plot(future_new_df)
fig_future

