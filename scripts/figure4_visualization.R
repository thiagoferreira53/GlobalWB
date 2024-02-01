library(ggplot2)
library(cowplot)
library(dplyr)

melt_GCM_country_noNA <- read.csv("/Users/thiagoferreira53/Projects/GlobalWB/Data\ and\ figures/Data/figure4_raw_future_loss_values.csv")

melt_df_mean <- read.csv(file = "/Users/thiagoferreira53/Projects/GlobalWB/Data\ and\ figures/Data/figure4_mean_production_loss.csv")

melt_df_mean$subareas <- factor(melt_df_mean$subareas, levels=c("Africa", "North America", "South America",
                                                                "Europe", "East Asia", "South Asia", "Oceania"))

errorbar_GCMs <- read.csv(file = "/Users/thiagoferreira53/Projects/GlobalWB/Data\ and\ figures/Data/figure4_error_GCMs.csv")

point_data_GCM <- melt_GCM_country_noNA %>% 
  add_row(
    subareas = c('Africa', 'East Asia', 'Europe', 'North America', 'Oceania', 'South America',
                 'South Asia'),
    country = rep(NA, 7),
    variable = rep(NA, 7),
    value = rep(NA, 7),
    scenario = rep('prod_loss_baseline', 7))



legend <- get_legend(ggplot() +
                       geom_bar(data=melt_df_mean, aes(x=subareas, y=value,fill=variable, group=variable),
                                stat="identity", position = "dodge", color = "black",size=0.3, width = 0.7)+
                       geom_errorbar(data=errorbar_GCMs,
                                     aes(x = subareas, ymax = X90., ymin = X10., width = 0.4, group = variable), 
                                     position=position_dodge(width=0.7), size = 0.5)+
                       theme_bw()+
                       theme(axis.text.x  = element_text(angle = 16, size = 22, vjust=0.5, colour = 'black'),
                             axis.text.y  = element_text(size = 22, colour = 'black'),
                             axis.title.y = element_text(size = 27),
                             axis.title.x = element_blank(),
                             legend.title = element_blank(),
                             legend.text = element_text(size = 27),
                             legend.position = 'top',
                             legend.spacing.x = unit(1.0, 'cm'),
                             legend.key.size = unit(2,"line"),
                             panel.grid.major = element_blank(),
                             plot.background = element_blank(),
                             #panel.grid.minor = element_line(size=0.4)
                             panel.grid.minor = element_blank(),
                             text = element_text(family = "Helvetica"))+
                       scale_y_continuous(breaks = seq(0, 100, by = 10), minor_breaks = seq(0, 100, by = 9.9999), limits = c(0,100))+
                       ylab('Mean production loss (%)')+
                       #xlab('Continent')+
                       scale_fill_manual(values = c("#336699", "#FF9933"),
                                         labels = c("Baseline", "Future scenario-RCP8.5"), na.translate = FALSE)+
                       geom_text(data = melt_df_mean, aes(label = value, x = subareas, y = value + 2, group = variable), 
                                 size = 6, position = position_dodge(width = 1.55)))

bar_error_plot <- ggplot() +
  geom_bar(data=melt_df_mean, aes(x=subareas, y=value,fill=variable, group=variable),
           stat="identity", position = "dodge", color = "black",size=0.3, width = 0.7)+
  geom_errorbar(data=errorbar_GCMs,
                aes(x = errorbar_GCMs$subareas, ymax = X90., ymin = X10., width = 0.4, group = variable), 
                position=position_dodge(width=0.7), size = 0.5)+
  geom_point(data=point_data_GCM,aes(x=subareas, y=value,group=scenario, fill=scenario),
             stat="identity",alpha = 0.5,colour="darkorange3",
             position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.3, seed = 124),
             size = 2)+
  theme_bw()+
  theme(axis.text.x  = element_text(angle = 16, size = 22, vjust=0.5, colour = 'black'),
        axis.text.y  = element_text(size = 22, colour = 'black'),
        axis.title.y = element_text(size = 27),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 27),
        legend.position = "none",
        legend.spacing.x = element_blank(),
        legend.key.size =element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        #panel.grid.minor = element_line(size=0.4)
        panel.grid.minor = element_blank(),
        text = element_text(family = "Helvetica"))+
  scale_y_continuous(breaks = seq(0, 100, by = 10), minor_breaks = seq(0, 100, by = 9.9999), limits = c(0,100))+
  ylab('Mean production loss (%)')+
  #xlab('Continent')+
  scale_fill_manual(values = c("#336699","#FF9933", "#336699", "#FF9933"),
                    labels = c("Baseline", "Future scenario-RCP8.5"), na.translate = FALSE)+
  geom_text(data = melt_df_mean, aes(label = value, x = subareas, y = value + 2, group = variable), 
            size = 6, position = position_dodge(width = 1.55))


bar_error_plot_legend <- plot_grid(legend, #legend_dots, 
                                   bar_error_plot, 
                                   rel_heights = c(0.07,1),nrow = 2)


bar_error_plot_legend


