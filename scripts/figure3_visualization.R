library(ggplot2)
library(data.table)

melt_data <- read.csv("/Users/thiagoferreira53/Projects/GlobalWB/Data\ and\ figures/Data/Data_figure3.csv")

melt_data_noNA <- melt_data[!is.na(melt_data$value),]


melt_data$subareas <- factor(melt_data$subareas, levels=c("Africa", "North America", "South America",
                                                          "Europe", "East Asia", "South Asia", "Oceania"))
################################################################################

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- create_quantile_segment_frame(data, draw_quantiles, split = TRUE, grp = grp)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

create_quantile_segment_frame <- function(data, draw_quantiles, split = FALSE, grp = NULL) {
  dens <- cumsum(data$density) / sum(data$density)
  ecdf <- stats::approxfun(dens, data$y)
  ys <- ecdf(draw_quantiles)
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
  violin.xs <- (stats::approxfun(data$y, data$x))(ys)
  if (grp %% 2 == 0) {
    data.frame(
      x = ggplot2:::interleave(violin.xs, violin.xmaxvs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  } else {
    data.frame(
      x = ggplot2:::interleave(violin.xminvs, violin.xs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  }
}

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "width", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

################################################################################

melt_data_violin <- melt_data
levels(melt_data_violin$variable) <- c('baseline', 'future')

median_values <- aggregate(value ~ subareas + variable, melt_data, median)
median_values$value <- round(median_values$value, 1)

#get mean for each subareas
mean_values <- aggregate(value ~ subareas + variable, melt_data, mean)
mean_values$value <- round(mean_values$value, 1)
mean_values


#Africa
Africa_baseline <- melt_data[melt_data$subareas=='Africa' & melt_data$variable=='percent_loss_baseline',]
qAfrica_baseline <- setDT(Africa_baseline)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qAfrica_baseline$variable <- 'percent_loss_baseline'

Africa_future <- melt_data[melt_data$subareas=='Africa' & melt_data$variable=='percent_loss_future',]
qAfrica_future <- setDT(Africa_future)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qAfrica_future$variable <- 'percent_loss_future'

#North America
NorthAmerica_baseline <- melt_data[melt_data$subareas=='North America' & melt_data$variable=='percent_loss_baseline',]
qNorthAmerica_baseline <- setDT(NorthAmerica_baseline)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qNorthAmerica_baseline$variable <- 'percent_loss_baseline'

NorthAmerica_future <- melt_data[melt_data$subareas=='North America' & melt_data$variable=='percent_loss_future',]
qNorthAmerica_future <- setDT(NorthAmerica_future)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qNorthAmerica_future$variable <- 'percent_loss_future'

#South America
SouthAmerica_baseline <- melt_data[melt_data$subareas=='South America' & melt_data$variable=='percent_loss_baseline',]
qSouthAmerica_baseline <- setDT(SouthAmerica_baseline)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qSouthAmerica_baseline$variable <- 'percent_loss_baseline'

SouthAmerica_future <- melt_data[melt_data$subareas=='South America' & melt_data$variable=='percent_loss_future',]
qSouthAmerica_future <- setDT(SouthAmerica_future)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qSouthAmerica_future$variable <- 'percent_loss_future'

#South Asia
SouthAsia_baseline <- melt_data[melt_data$subareas=='South Asia' & melt_data$variable=='percent_loss_baseline',]
qSouthAsia_baseline <- setDT(SouthAsia_baseline)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qSouthAsia_baseline$variable <- 'percent_loss_baseline'

SouthAsia_future <- melt_data[melt_data$subareas=='South Asia' & melt_data$variable=='percent_loss_future',]
qSouthAsia_future <- setDT(SouthAsia_future)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qSouthAsia_future$variable <- 'percent_loss_future'

#East Asia
EastAsia_baseline <- melt_data[melt_data$subareas=='East Asia' & melt_data$variable=='percent_loss_baseline',]
qEastAsia_baseline <- setDT(EastAsia_baseline)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qEastAsia_baseline$variable <- 'percent_loss_baseline'

EastAsia_future <- melt_data[melt_data$subareas=='East Asia' & melt_data$variable=='percent_loss_future',]
qEastAsia_future <- setDT(EastAsia_future)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qEastAsia_future$variable <- 'percent_loss_future'

#Oceania
Oceania_baseline <- melt_data[melt_data$subareas=='Oceania' & melt_data$variable=='percent_loss_baseline',]
qOceania_baseline <- setDT(Oceania_baseline)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qOceania_baseline$variable <- 'percent_loss_baseline'

Oceania_future <- melt_data[melt_data$subareas=='Oceania' & melt_data$variable=='percent_loss_future',]
qOceania_future <- setDT(Oceania_future)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qOceania_future$variable <- 'percent_loss_future'

#Europe
Europe_baseline <- melt_data[melt_data$subareas=='Europe' & melt_data$variable=='percent_loss_baseline',]
qEurope_baseline <- setDT(Europe_baseline)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qEurope_baseline$variable <- 'percent_loss_baseline'

Europe_future <- melt_data[melt_data$subareas=='Europe' & melt_data$variable=='percent_loss_future',]
qEurope_future <- setDT(Europe_future)[, as.list(quantile(value, probs= c(.1, .9), na.rm=T)), subareas]
qEurope_future$variable <- 'percent_loss_future'

quantiles_continent <- rbind(qAfrica_baseline, qAfrica_future,
                             qNorthAmerica_baseline, qNorthAmerica_future,
                             qSouthAmerica_baseline, qSouthAmerica_future,
                             qSouthAsia_baseline, qSouthAsia_future,
                             qEastAsia_baseline, qEastAsia_future, 
                             qOceania_baseline, qOceania_future,
                             qEurope_baseline, qEurope_future)


area_loss <- ggplot() +
  geom_split_violin(data = melt_data_violin, aes(subareas, value, fill=variable, linetype = NA, width=0.9))+
  #stat_boxplot(data = melt_data, aes(subareas, value, fill=variable), geom ='errorbar', width = 0.4)+
  geom_errorbar(data=quantiles_continent,
                aes(x = quantiles_continent$subareas, ymax = `90%`, ymin = `10%`, width = 0.4, group = variable), 
                position=position_dodge(width=0.4), size = 0.8)+
  geom_boxplot(data = melt_data, aes(subareas, value, fill=variable), outlier.shape = NA,
               key_glyph = "rect", width = 0.4, alpha = 0.8, coef = 0) +
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, by = 10),  limits = c(0,100)
                     #, minor_breaks = seq(0, 100, by = 9.9999)
  )+
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
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        text = element_text(family = "Helvetica"))+
  ylab('Vulnerable areas (%)')+
  #xlab('Continent')+
  scale_fill_manual(values = c("#6699CC","#FFCC66", "#336699", "#FF9933"),
                    breaks = levels(melt_data$variable),
                    labels = c("Baseline", "Future scenario-RCP8.5"), na.translate = FALSE)+
  geom_text(data = mean_values, aes(label = value, x = subareas, y = value + 0.08, group = variable), 
            size = 6, position = position_dodge(width = 1.4))

area_loss

