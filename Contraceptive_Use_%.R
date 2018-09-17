# !diagnostics off




create_cont_use_et<- function(myfile1,myfile2){
  
  data_set <- read_excel(myfile1)
  const_data <- read_excel(myfile2)
  
  
  data_set$hbmc_larc<-round(data_set$Ever_SA_Pt_On_HBMC_LARC_F/data_set$SA_Ever_Pt_F,2)
  
  #prompts user to input title
  titles <- readline(prompt = "PLEASE ENTER YOUR DESIRED TITLE: ") 
  #prompts user to input sub-title
  subtitles <- readline(prompt = "PLEASE ENTER YOUR DESIRED SUB-TITLE: ") 
  
  
  option_text0 <- c("High School Only","Non High School Only",
                    "Both HS and Non-HS")
  
  #lets user choose an option specified in option_text0
  list_School_Level_Options <- c(select.list(option_text0,         
                                             title ="Choose the stratification for the data based on School Levels ",
                                             multiple=FALSE))
  if (list_School_Level_Options == 'High School Only'){
    data_set <- subset(data_set, data_set$Enroll_Campus_Pop_9to12 !=0)
  } else if(list_School_Level_Options == 'Non High School Only'){
    data_set <- subset(data_set, data_set$Enroll_Campus_Pop_9to12 ==0)
  } else if(list_School_Level_Options == 'Both HS and Non-HS'){
    data_set <- data_set
  } 
  
  data_set$data_src <- data_set$DataPullCharts_Name
  data_src_rf <- unique(data_set$data_src)
  
  
  #lets user choose one data source from a list of unique instances of data_pull_name
  list_data_src <- c(select.list(data_src_rf,
                                 title ="Choose the Data Source ",
                                 multiple=FALSE))
  
  #stores unique site types based on the chosen data source
  uniq_Site_Type <- unique(data_set$SITE_TYPE
                           [which(data_set$data_src %in% list_data_src)])
  
  #lets user choose one site type from a list of unique site types
  list_uniq_Site_Type <- c(select.list(uniq_Site_Type
                                       ,multiple = FALSE, 
                                       title = "Choose the specific Site Type"))
  
  
  
  option_text1 <- c("Specific Sponsor","Specific Sponsor (HS) Highlighted","Specific Sponsor (Non-HS) Highlighted")
  list_Sponsor_Level_Options <- c(select.list(option_text1,
                                              title ="Do you want to choose specific Sponsor for your charts ",
                                              multiple=FALSE))
  
  if( list_Sponsor_Level_Options=="Specific Sponsor"){
    
    #stores specific sponsors for the selected site type
    spec_Sponsors_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type
                                                                               & data_set$data_src %in% list_data_src)]
    #stores unique instances of the specific sponsor name
    uniq_spec_Sponsor <- unique(spec_Sponsors_uniq_Site_Type)
    
    #lets user choose a specific sponsor from a list of unique sponsors names
    list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor
                                       ,multiple = FALSE, title = "Choose the specific Sponsor for the chart"))
    
    
    spec_site_name <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_hbmc_larc <- data_set$hbmc_larc[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    new_data_set_1<-data.frame(spec_site_name,spec_hbmc_larc)
    new_data_set_1<-na.omit(new_data_set_1)
    
    option_text<-c('Identified','De-Identified')
    list_identification<-c(select.list(option_text, title ="Please choose how you want your charts "))
    
    if(list_identification=="Identified"){
      ggplot(new_data_set_1,aes(reorder(spec_site_name,spec_hbmc_larc),spec_hbmc_larc,label=paste(as.integer((spec_hbmc_larc)*100),"%")))+
        geom_col(fill="#c5e0b4",width=.7)+
        labs(y="",x="",
             title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
        theme_classic()+
        geom_text(position=position_stack(vjust=1.05), colour="black") +
        geom_abline(aes(slope=0, intercept=const_data$NYSFunded_NYC_FPClinics,
                        col = "2016 NYS-Funded NYC FP Clinics, 65%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=const_data$NYS_Benchmark,
                        col = "NYS_Benchmark, 70%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=0))+
        labs(colour="") +
        scale_colour_manual(values=c('darkorange',"#604a7b","black"))+
        scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0.0,1, by=.1))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(.7, 'lines'))+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        theme(plot.title = element_text(hjust = 0.5,size=20))
    } else if(list_identification=="De-Identified"){
      ggplot(new_data_set_1,aes(reorder(spec_site_name,spec_hbmc_larc),spec_hbmc_larc,label=paste(as.integer((spec_hbmc_larc)),"%")))+
        geom_col(fill="#c5e0b4",width=.7)+
        labs(y="",x="",
             title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
        theme_classic()+
        geom_text(position=position_stack(vjust=1.05), colour="black") +
        geom_abline(aes(slope=0, intercept=const_data$NYSFunded_NYC_FPClinics,
                        col = "2016 NYS-Funded NYC FP Clinics, 65%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=const_data$NYS_Benchmark,
                        col = "NYS_Benchmark, 70%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=0))+
        labs(colour="") +
        scale_colour_manual(values=c('darkorange',"#604a7b","black"))+
        scale_y_continuous(labels=percent,limits=c(0,1.2),breaks=seq(0.0,1.2, by=.1))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(.7, 'lines'))+
        theme(axis.text.x = element_blank())+
        theme(plot.title = element_text(hjust = 0.5,size=20))
    }
    
  } else if( list_Sponsor_Level_Options == "Specific Sponsor (HS) Highlighted"){
    spec_Sponsors_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type
                                                                               & data_set$Enroll_Campus_Pop_9to12 !=0
                                                                               & data_set$data_src %in% list_data_src)]
    uniq_spec_Sponsor <- unique(spec_Sponsors_uniq_Site_Type)
    
    list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor,
                                       title ="Choose specific Sponsor to highlight for your charts ",
                                       multiple=FALSE))
    
    spec_site_name <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_hbmc_larc <- data_set$hbmc_larc[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_hs_popn <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    
    new_data_set_1 <- data.frame(spec_site_name,spec_hbmc_larc,spec_hs_popn)
    new_data_set_1 <- na.omit(new_data_set_1)
    
    site_name_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    hbmc_larc_all_sponsor <- data_set$hbmc_larc[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    hs_popn_all_sponsor <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    
    new_data_set_2 <- data.frame(site_name_all_sponsor,hbmc_larc_all_sponsor,hs_popn_all_sponsor)
    new_data_set_2 <- na.omit(new_data_set_2)
    
    new_data_1_hs <-subset(new_data_set_1, spec_hs_popn!=0)
    
    new_data_2_hs <-subset(new_data_set_2, hs_popn_all_sponsor!=0)
    
    option_text2 <- c('Identified','De-Identified')
    list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
    
    if(list_identification == "Identified"){
      ggplot()+
        geom_col(data = new_data_2_hs,aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),y = hbmc_larc_all_sponsor),
                 fill = "#c5e0b4" ,
                 width=.7)+
        labs(y="",x="",
             title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
        theme_classic()+
        geom_text(data = new_data_2_hs, aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),
                                           y = hbmc_larc_all_sponsor,label=paste(as.integer((hbmc_larc_all_sponsor)*100),"%")),
                  position=position_stack(vjust=1.02), colour="black", size = 1.6) +
        geom_abline(aes(slope=0, intercept=const_data$NYSFunded_NYC_FPClinics,
                        col = "2016 NYS-Funded NYC FP Clinics, 65%    "),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=const_data$NYS_Benchmark,
                        col = "NYS_Benchmark, 70%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=0))+
        
        geom_col(data = new_data_1_hs,aes(x = reorder(spec_site_name,spec_hbmc_larc), fill = "",
                                   y = spec_hbmc_larc),width=.7)+
        labs(colour="") +
        scale_colour_manual(values=c('darkorange',"#604a7b","black"))+
        scale_fill_manual(values = c("#60aa31"),guide="none")+
        scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0.0,1, by=.1))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(.7, 'lines'))+
        theme(axis.text.x = element_text(angle = 70, hjust = 1))+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    } else if (list_identification == "De-Identified"){
      ggplot()+
        geom_col(data = new_data_2_hs,aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),y = hbmc_larc_all_sponsor),
                 fill = "#c5e0b4" ,
                 width=.7)+
        labs(y="",x="",
             title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
        theme_classic()+
        geom_text(data = new_data_2_hs, aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),
                                            y = hbmc_larc_all_sponsor,label=paste(as.integer((hbmc_larc_all_sponsor)*100),"%")),
                  position=position_stack(vjust=1.02), colour="black", size = 1.6) +
        geom_abline(aes(slope=0, intercept=const_data$NYSFunded_NYC_FPClinics,
                        col = "2016 NYS-Funded NYC FP Clinics, 65%    "),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=const_data$NYS_Benchmark,
                        col = "NYS_Benchmark, 70%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=0))+
        
        geom_col(data = new_data_1_hs,aes(x = reorder(spec_site_name,spec_hbmc_larc), fill = "",
                                          y = spec_hbmc_larc),width=.7)+
        labs(colour="") +
        scale_colour_manual(values=c('darkorange',"#604a7b","black"))+
        scale_fill_manual(values = c("#60aa31"),guide="none")+
        scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0.0,1, by=.1))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(.7, 'lines'))+
        theme(axis.text.x = element_blank())+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    }
    
    
    
  } else if( list_Sponsor_Level_Options == "Specific Sponsor (Non-HS) Highlighted"){
    spec_Sponsors_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type
                                                                               & data_set$Enroll_Campus_Pop_9to12 == 0
                                                                               & data_set$data_src %in% list_data_src)]
    uniq_spec_Sponsor <- unique(spec_Sponsors_uniq_Site_Type)
    
    list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor,
                                       title ="Choose specific Sponsor to highlight for your charts ",
                                       multiple=FALSE))
    
    spec_site_name <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_hbmc_larc <- data_set$hbmc_larc[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_hs_popn <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    
    new_data_set_1 <- data.frame(spec_site_name,spec_hbmc_larc,spec_hs_popn)
    new_data_set_1 <- na.omit(new_data_set_1)
    
    site_name_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    hbmc_larc_all_sponsor <- data_set$hbmc_larc[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    hs_popn_all_sponsor <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    
    new_data_set_2 <- data.frame(site_name_all_sponsor,hbmc_larc_all_sponsor,hs_popn_all_sponsor)
    new_data_set_2 <- na.omit(new_data_set_2)
    
    new_data_1_hs <-subset(new_data_set_1, spec_hs_popn==0)
    
    new_data_2_hs <-subset(new_data_set_2, hs_popn_all_sponsor==0)
    
    option_text2 <- c('Identified','De-Identified')
    list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
    
    if(list_identification == "Identified"){
      ggplot()+
        geom_col(data = new_data_2_hs,aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),y = hbmc_larc_all_sponsor),
                 fill = "#c5e0b4" ,
                 width=.7)+
        labs(y="",x="",
             title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
        theme_classic()+
        geom_text(data = new_data_2_hs, aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),
                                            y = hbmc_larc_all_sponsor,label=paste(as.integer((hbmc_larc_all_sponsor)*100),"%")),
                  position=position_stack(vjust=1.02), colour="black", size = 1.6) +
        geom_abline(aes(slope=0, intercept=const_data$NYSFunded_NYC_FPClinics,
                        col = "2016 NYS-Funded NYC FP Clinics, 65%    "),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=const_data$NYS_Benchmark,
                        col = "NYS_Benchmark, 70%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=0))+
        
        geom_col(data = new_data_1_hs,aes(x = reorder(spec_site_name,spec_hbmc_larc), fill = "",
                                          y = spec_hbmc_larc),width=.7)+
        labs(colour="") +
        scale_colour_manual(values=c('darkorange',"#604a7b","black"))+
        scale_fill_manual(values = c("#60aa31"),guide="none")+
        scale_y_continuous(labels=percent,limits=c(0,1.1),breaks=seq(0.0,1.1, by=.1))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(.7, 'lines'))+
        theme(axis.text.x = element_text(angle = 70, hjust = 1))+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    } else if (list_identification == "De-Identified"){
      ggplot()+
        geom_col(data = new_data_2_hs,aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),y = hbmc_larc_all_sponsor),
                 fill = "#c5e0b4" ,
                 width=.7)+
        labs(y="",x="",
             title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
        theme_classic()+
        geom_text(data = new_data_2_hs, aes(x = reorder(site_name_all_sponsor,hbmc_larc_all_sponsor),
                                            y = hbmc_larc_all_sponsor,label=paste(as.integer((hbmc_larc_all_sponsor)*100),"%")),
                  position=position_stack(vjust=1.02), colour="black", size = 1.6) +
        geom_abline(aes(slope=0, intercept=const_data$NYSFunded_NYC_FPClinics,
                        col = "2016 NYS-Funded NYC FP Clinics, 65%    "),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=const_data$NYS_Benchmark,
                        col = "NYS_Benchmark, 70%"),size=1,alpha=.7)+
        geom_abline(aes(slope=0, intercept=0))+
        
        geom_col(data = new_data_1_hs,aes(x = reorder(spec_site_name,spec_hbmc_larc), fill = "",
                                          y = spec_hbmc_larc),width=.7)+
        labs(colour="") +
        scale_colour_manual(values=c('darkorange',"#604a7b","black"))+
        scale_fill_manual(values = c("#60aa31"),guide="none")+
        scale_y_continuous(labels=percent,limits=c(0,1.1),breaks=seq(0.0,1.1, by=.1))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(.7, 'lines'))+
        theme(axis.text.x = element_blank())+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    }
  }
  
}
