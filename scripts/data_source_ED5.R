read_Summary <- function(summaryfile){
  suppressMessages({fOUT = readLines(summaryfile)})
  nottrashLines = grep(pattern = '[^ ]', fOUT)[!(grep(pattern = '[^ ]', fOUT) %in% 
                                                   c(grep(pattern = '^\\$', fOUT), 
                                                     grep(pattern = '^\\*', fOUT),
                                                     grep(pattern = '^\\!', fOUT)))]
  fOUT_clean <<- fOUT[nottrashLines]
  trtHeaders<<-which(grepl(pattern = '^@', fOUT_clean))
  if(length(trtHeaders)<=0) {
    print("Forcing return from read_file... No Headers") 
    return(NULL)
  }
  varN <- lapply(trtHeaders, function(i) {
    make.names(scan(text = gsub(pattern = '^@', 
                                replacement = '',fOUT_clean[i]),
                    what = character()))
  })
  pos <- c(trtHeaders,length(fOUT_clean)+1)
  tmpA = lapply(seq(1,length(pos)-1), function(w) {
    res<-read.table(text = fOUT_clean[seq(from=pos[w], length.out = pos[w+1]-pos[w])],
                    skip = 1, 
                    col.names = varN[[w]],
                    na.strings = c('-99', '-99.0', '-99.'))
    res
  })
  dataSummary <- do.call("rbind",tmpA)
  names(dataSummary) <- gsub("\\.", "", names(dataSummary))
  return(dataSummary)
}




work_dir <- "/Users/thiagoferreira53/Desktop/CIMMYT/global_WB/ExtendedFig5/Weather_Jessore/"


summary_file_jashore <- read_Summary(paste0(work_dir,"Summary.OUT"))

summary_file_jashore <- summary_file_jashore[1:40,]

#summary_file_jashore <- summary_file_jashore[1:40,]

summary_file_jashore$date <- as.Date(strftime(as.POSIXct(as.character(summary_file_jashore$HDAT), format="%Y%j")))
summary_file_jashore$date <- as.integer(substr(strftime(as.POSIXct(as.character(summary_file_jashore$HDAT), format="%Y%j")),1,4))

summary_file_jashore$TMEANA <- (summary_file_jashore$TMINA + summary_file_jashore$TMAXA) / 2



summary_file_jashore$data_plot <- "(c) Jashore weather 1980-2020"


summary_file_jashore_10 <- read_Summary(paste0(work_dir,"Summary-10.OUT"))

summary_file_jashore_10$date <- as.Date(strftime(as.POSIXct(as.character(summary_file_jashore_10$HDAT), format="%Y%j")))
summary_file_jashore_10$date <- as.integer(substr(strftime(as.POSIXct(as.character(summary_file_jashore_10$HDAT), format="%Y%j")),1,4))

summary_file_jashore_10$TMEANA <- (summary_file_jashore_10$TMINA + summary_file_jashore_10$TMAXA) / 2

summary_file_jashore_10$data_plot <- "(a) Jashore yield 2010-2020"





#Mpika

work_dir <- "/Users/thiagoferreira53/Desktop/CIMMYT/global_WB/ExtendedFig5/Weather_Mpika/"

summary_file_mpika <- read_Summary(paste0(work_dir,"Summary.OUT"))

summary_file_mpika <- summary_file_mpika[1:40,]

summary_file_mpika$date <- as.Date(strftime(as.POSIXct(as.character(summary_file_mpika$HDAT), format="%Y%j")))
summary_file_mpika$date <- as.integer(substr(strftime(as.POSIXct(as.character(summary_file_mpika$HDAT), format="%Y%j")),1,4))

summary_file_mpika$TMEANA <- (summary_file_mpika$TMINA + summary_file_mpika$TMAXA) / 2

summary_file_mpika$data_plot <- "(d) Mpika weather 1980-2020"

summary_file_mpika_10 <- read_Summary(paste0(work_dir,"Summary-10.OUT"))

summary_file_mpika_10$date <- as.Date(strftime(as.POSIXct(as.character(summary_file_mpika_10$HDAT), format="%Y%j")))
summary_file_mpika_10$date <- as.integer(substr(strftime(as.POSIXct(as.character(summary_file_mpika_10$HDAT), format="%Y%j")),1,4))

summary_file_mpika_10$TMEANA <- (summary_file_mpika_10$TMINA + summary_file_mpika_10$TMAXA) / 2

summary_file_mpika_10$data_plot <- "(b) Mpika yield 2010-2020"



jashore_df <- rbind(summary_file_jashore, summary_file_jashore_10)

mpika_df <- rbind(summary_file_mpika, summary_file_mpika_10)

final_df <- rbind(jashore_df, mpika_df)

write.csv(final_df, "/Users/thiagoferreira53/Desktop/Final_Code_Figures_NCC/source_data/ExtendedData_figure5.csv")

