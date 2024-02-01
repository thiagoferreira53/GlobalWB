rm(list = ls(all.names = TRUE))
###############################################################################################
#load functions to read outputs
source("/Users/thiagoferreira53/Desktop/scripts/DSSAT_R_read_outputs.R")
#source("~/Dropbox/Bangladesh/DSSAT_R_read_outputs.R")
###############################################################################################
library(ggplot2)
library(data.table)
library(cowplot)
library(ie2misc)
library(readxl)
library(lemon)

WB_Simulations <- '/Users/thiagoferreira53/Desktop/CIMMYT/global_WB/ExtendedFig4/'


summ_WB <- paste0(WB_Simulations, "/Summary.OUT")

summary_WB_df <- read_out(summ_WB)

eval_out <- paste0(WB_Simulations,"/Evaluate.OUT")

eval_WB_df <- read_out(eval_out)

merged_df <- merge(summary_WB_df, eval_WB_df, by.x= "RUNNO", by.y = "RUN")

#adjusting adap
merged_df$ADAPS[c(1:4)] <- merged_df$ADAPS[c(5:8)]
merged_df$HDAT[c(1:4)] <- merged_df$HDAT[c(5:8)]


merged_df <- merged_df[,c("TNAM","EXNAME", "WYEAR", "HDAT", "SDAT","PDAT","ADAPS","ADAPM","HWAMM", "HWAMS")]

merged_df$pest<-ifelse(merged_df$TNAM=='Diseased', 'With WB damage', 'No WB damage')


###TESTING




summary_WB_df<- merged_df

yield_table <- data.frame(EXP=summary_WB_df$EXNAME[summary_WB_df$TNAM=="No_Disease"],
                          WYEAR=summary_WB_df$WYEAR[summary_WB_df$TNAM=="No_Disease"],
                          HDAT=summary_WB_df$HDAT[summary_WB_df$TNAM=="No_Disease"], 
                          HWAM_D=summary_WB_df$HWAMM[summary_WB_df$TNAM=="Diseased"], 
                          HWAM_ND=summary_WB_df$HWAMM[summary_WB_df$TNAM=="No_Disease"],
                          perc_loss = (1 - summary_WB_df$HWAMM[summary_WB_df$TNAM=="Diseased"]/
                                         summary_WB_df$HWAMM[summary_WB_df$TNAM=="No_Disease"])*100)

yield_table$date <- as.Date(strftime(as.POSIXct(as.character(yield_table$HDAT), format="%Y%j")))

yield_table$perc_loss_char <- paste0(round(yield_table$perc_loss,1), "%")

melt_yield <- melt(yield_table, id.vars = 'EXP', measure.vars = c('HWAM_D','HWAM_ND'))

melt_yield$variable <- factor(melt_yield$variable, levels = c("HWAM_ND", "HWAM_D"))


obs_data <- read_excel("/Users/thiagoferreira53/Desktop/CIMMYT/global_WB/Revision_2/Plots/data/EWS_alerts_validations.xlsx", sheet=2)

obs_data_Londrina<-obs_data[obs_data$location=="Londrina" & obs_data$year=="2012" |
                              obs_data$location=="Londrina" & obs_data$year=="2014" |
                              obs_data$location=="Londrina" & obs_data$data_sowing=="10-03-2015",]


obs_data_Londrina['EXNAME'] <- NA

obs_data_Londrina$EXNAME[obs_data_Londrina$data_sowing == "07-04-2012"] <- "EBLO1201"
obs_data_Londrina$EXNAME[obs_data_Londrina$data_sowing == "03-03-2014"] <- "EBLO1401"
obs_data_Londrina$EXNAME[obs_data_Londrina$data_sowing == "13-03-2014"] <- "EBLO1402"
obs_data_Londrina$EXNAME[obs_data_Londrina$data_sowing == "10-03-2015"] <- "EBLO1501"


#left outer
sim_with_obs_df <- left_join(summary_WB_df, obs_data_Londrina, by='EXNAME')
sim_with_obs_df$yld_mean

#adding production levels (dat_yld.csv)
sim_with_obs_df$yld_mean[1] <- 1022.31 #2012
sim_with_obs_df$yld_mean[2] <- 1799.735 #2014a
sim_with_obs_df$yld_mean[3] <- 1687.5 #2014b
sim_with_obs_df$yld_mean[4] <- 2034.32  #2015

sim_with_obs_df$inc_mean[1] <- 57.2375 #2012
sim_with_obs_df$inc_mean[2] <- 68.25 #2014a
sim_with_obs_df$inc_mean[3] <- 32.5 #2014b
sim_with_obs_df$inc_mean[4] <- 17.95   #2015


melt_yield$EXP[melt_yield$EXP=="EBLO1201"] <- "2012"
melt_yield$EXP[melt_yield$EXP=="EBLO1401"] <- "2014/1"
melt_yield$EXP[melt_yield$EXP=="EBLO1402"] <- "2014/2"
melt_yield$EXP[melt_yield$EXP=="EBLO1501"] <- "2015"

yield_table$EXP[yield_table$EXP=="EBLO1201"] <- "2012"
yield_table$EXP[yield_table$EXP=="EBLO1401"] <- "2014/1"
yield_table$EXP[yield_table$EXP=="EBLO1402"] <- "2014/2"
yield_table$EXP[yield_table$EXP=="EBLO1501"] <- "2015"

sim_with_obs_df$EXNAME[sim_with_obs_df$EXNAME=="EBLO1201"] <- "2012"
sim_with_obs_df$EXNAME[sim_with_obs_df$EXNAME=="EBLO1401"] <- "2014/1"
sim_with_obs_df$EXNAME[sim_with_obs_df$EXNAME=="EBLO1402"] <- "2014/2"
sim_with_obs_df$EXNAME[sim_with_obs_df$EXNAME=="EBLO1501"] <- "2015"


sim_with_obs_df$EXNAME

observed_data_melt <- melt(sim_with_obs_df,id.vars = c('EXNAME','TNAM'), measure.vars = c('yld_mean','inc_mean'))

#adjust INC value according to ylimmax
ylimmax <- 3500
observed_data_melt$value[observed_data_melt$variable=="inc_mean"] <- 
  observed_data_melt$value[observed_data_melt$variable=="inc_mean"] *(ylimmax/100)

observed_data_melt$variable <- ifelse(observed_data_melt$variable=="inc_mean", "Observed incidence", "Observed yield")

observed_data_melt$variable <- factor(observed_data_melt$variable, levels = c("Observed incidence", "Observed yield"))

my_theme <- theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                  legend.position="top", #c(0.98,0.85),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text=element_text(size=25),
                  legend.title=element_blank(),
                  axis.title =element_text(size=26),
                  axis.text=element_text(size=20),
                  #panel.border = element_blank(), #REMOVE ALL BORDERS
                  legend.background = element_blank(),
                  legend.spacing.x = unit(1.0, 'cm'),
                  legend.key.size = unit(2,"line"),
                  axis.text.x = element_text(vjust=-0.5),
                  axis.line.x.bottom = element_line(lineend="round"),
                  axis.ticks.length=unit(.25, "cm"),
                  axis.line.y.left = element_line(lineend="round"),
                  axis.line.y.right = element_line(lineend="round"),
                  axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm"))
                  #text = element_text(family = "Helvetica", color = "black"),
)


bar<- ggplot()+
  geom_bar(data=melt_yield, aes(x=as.character(EXP), 
                                y=value, fill=variable),stat="identity", position = "dodge"
  )+
  my_theme+
  theme(legend.justification='center')+
  scale_x_discrete(expand=c(0.3,0))+
  xlab("Crop Season") +
  ylab("Yield (kg/ha)")+
  scale_fill_manual(values=c("#336699", "#FF9933"),
                    labels = c("Simulated without disease damage","Simulated with disease damage"))
legend_bar <- cowplot::get_legend(bar)


dots <- ggplot()+
  geom_jitter(data=observed_data_melt,
              aes(x=EXNAME,y=value,color=TNAM,shape=variable),position = position_dodge(width = -0.9),size=9)+
  theme_classic()+
  guides(color = "none")+
  my_theme+
  theme(legend.justification='center')+
  scale_fill_manual(values=c("#336699", "#FF9933"),
                    labels = c("Simulated without disease damage","Simulated with disease damage"))+
  scale_shape_manual(values=c(17:16), labels=c("Observed incidence", "Observed yield"))

#legend_dots <- cowplot::get_legend(dots)


resultsData <- ggplot()+
  geom_bar(data=melt_yield, aes(x=as.character(EXP), 
                                y=value, fill=variable),stat="identity", position = "dodge"
  )+
  geom_jitter(data=observed_data_melt,
              aes(x=EXNAME,y=value,color=TNAM,shape=variable),position = position_dodge(width = -0.9),size=9)+
  theme_classic()+
  guides(color = "none",fill=guide_legend(nrow = 2, byrow = TRUE), shape=guide_legend(nrow = 2, byrow = TRUE))+
  my_theme+
  theme(legend.justification='center',
        axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))+
  xlab("Crop season") +
  ylab("Yield (kg/ha)")+
  scale_fill_manual(values=c("#336699", "#FF9933"),
                    labels = c("Simulated without disease damage","Simulated with disease damage"))+
  scale_shape_manual(values=c(17:16), labels=c("Observed incidence", "Observed yield"))+
  #geom_text(data=yield_table, aes(x=as.character(EXP), 
  #                                y=HWAM_ND, label=perc_loss_char, vjust=-0.25), size=8)+
  scale_y_continuous(breaks = seq(0, ylimmax, by = 500), limits = c(0, ylimmax), 
                     sec.axis = sec_axis(~./(ylimmax/100), name="Wheat Blast Incidence (%)"))+
  scale_color_manual(values = c("No_Disease"="darkblue", "Diseased"="darkorange3"))+
  scale_x_discrete(expand=c(0.3,0))+
  coord_capped_cart(bottom=brackets_horizontal(length = unit(3, "cm")), left = 'both', right = 'both')+
  annotate(geom='text', label='(a)', size =10,x = 0.3,y=3500,fontface = "bold")




my_theme <- theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                  legend.position="top", #c(0.98,0.85),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.text=element_text(size=25),
                  legend.title=element_blank(),
                  axis.title =element_text(size=26),
                  axis.text=element_text(size=20),
                  #panel.border = element_blank(), #REMOVE ALL BORDERS
                  legend.background = element_blank(),
                  legend.spacing.x = unit(1.0, 'cm'),
                  legend.key.size = unit(2,"line"),
                  axis.text.x = element_text(vjust=-0.5),
                  axis.line.x.bottom = element_line(lineend="round"),
                  axis.ticks.length=unit(.25, "cm"),
                  axis.line.y.left = element_line(lineend="round"),
                  axis.line.y.right = element_line(lineend="round"),
                  axis.title.x = element_text(margin = unit(c(4, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm"))
                  #text = element_text(family = "Helvetica", color = "black"),
)


eval_WB_df <- merged_df[merged_df$TNAM=="Diseased",]



eval_WB_df$cultivar <- c("Marfim", "Marfim", "Marfim", "IPR Catuara TM")

eval_WB_df$HWAMM
k1=lm(HWAMM~ HWAMS,data=eval_WB_df);
sum_yield=summary(k1);
sum_yield$r.squared

rmsq_yield <- rmse(observed = eval_WB_df$HWAMM, predicted = eval_WB_df$HWAMS)
IA_yield <- dr(observed = eval_WB_df$HWAMM, predicted = eval_WB_df$HWAMS)

k1=lm(ADAPM~ ADAPS,data=eval_WB_df);
sum_adap=summary(k1);
sum_adap$r.squared

rmsq_Anthesis <- rmse(observed = eval_WB_df$ADAPM, predicted = eval_WB_df$ADAPS)
IA_Anthesis <- dr(observed = as.integer(eval_WB_df$ADAPM), predicted = as.integer(eval_WB_df$ADAPS))


#label <- paste0('\\textbf{$\\textit{\\overset{R^{2}=',round(sum_adap$r.squared,2),'}{RMSD=', round(rmsq_Anthesis,2),'}{RMSD=', round(rmsq_Anthesis,2),'}}}')

yield <- ggplot(eval_WB_df, aes(x=HWAMM, y= HWAMS, color=cultivar))+  
  geom_point(size=9, shape=18) +
  geom_abline(intercept=0, slope=1) +
  my_theme +
  theme(panel.border = element_blank(),
        panel.background = element_blank())+
  labs(x='Observed diseased yield (kg/ha)', y='Simulated diseased yield (kg/ha)')+
  xlim(400,1000)+
  ylim(400,1000)+
  annotate(geom = 'text', 
           x = 500, y = seq(900,980,length=3),
           hjust = 0,
           label = list(paste0("italic(~'\nWillmott’s d = '~",round(IA_yield,2),')'),
                        paste0("italic(~'\nRMSE = '~", round(rmsq_yield,2),"~(kg/ha)",')'),
                        paste0("italic(~R^2~' = '~",round(sum_yield$r.squared,2),')')), 
           fontface = 'italic',
           parse=TRUE,
           size=5)+
  guides(shape = "none")+
  #geom_text(aes(fontface=3), x = 500, y = 950, label=paste0("R^2"," = ",round(sum_adap$r.squared,2),
  #                                                          "\nRMSD = ", round(rmsq_Anthesis,2),
  #                                                          "\nIOA = ",round(IA_Anthesis,2)),
  #          hjust = 0)+
  coord_capped_cart(bottom='both', left = 'both', right = 'both')+
  annotate(geom='text', label='(b)', size =10,x = 420,y=1000,fontface = "bold")+
  scale_color_manual(values=c('#FF0000', "#0066CC"))

yield

anthesis <- ggplot(eval_WB_df, aes(x=ADAPM, y= ADAPS,color=cultivar))+  
  geom_point(size=9, shape=18) +
  geom_abline(intercept=0, slope=1) +
  my_theme +
  theme(panel.border = element_blank(),
        panel.background = element_blank())+
  labs(x='Observed anthesis date (Dap)', y='Simulated anthesis date (Dap)')+
  xlim(50,70)+
  ylim(50,70)+
  annotate(geom = 'text', 
           x = 52.5, y = seq(66.5,69,length=3),
           hjust = 0,
           label = list(paste0("italic(~'\nWillmott’s d = '~",round(IA_Anthesis,2),')'),
                        paste0("italic(~'\nRMSE = '~", round(rmsq_Anthesis,2),"~(days)",')'),
                        paste0("italic(~R^2~' = '~",round(sum_adap$r.squared,2),')')), 
           fontface = 'italic',
           parse=TRUE,
           size=5)+
  annotate(geom='text', label='(c)', size =10,x = 50.5,y=70,fontface = "bold")+
  coord_capped_cart(bottom='both', left = 'both', right = 'both')+
  scale_color_manual(values=c('#FF0000', "#0066CC"))
anthesis

legend_one <- get_legend(anthesis)


one_by_one_plots <- plot_grid(yield+theme(legend.position="none"), anthesis+theme(legend.position="none"))


yield_plot <- plot_grid(legend_bar, #legend_dots, 
                        resultsData + theme(legend.position = "none"), 
                        rel_heights = c(0.1,1),nrow = 3)

all_plots <- plot_grid(yield_plot, one_by_one_plots, nrow = 2,rel_heights = c(0.6,0.4))


write.csv(merged_df, "/Users/thiagoferreira53/Desktop/Final_Code_Figures_NCC/source_data/ExtendedData_figure4.csv", row.names = F)





