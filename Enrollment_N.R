

# !diagnostics off

create_en_by_N_etn<-function(myfile1){
  
  #imports the excel(QA or RH) file
  data_set <- read_excel(myfile1)
  
   
  data_set$n_enrolled <- data_set$Enroll_Campus_Popn - data_set$Enroll_Clinic_Enroll
  
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
    #lets user choose an option specified in option_text1
    list_Sponsor_Level_Options <- c(select.list(option_text1,
                      title ="Do you want to choose specific Sponsor for your charts ",
                      multiple=FALSE))
    
    if(list_Sponsor_Level_Options == "Specific Sponsor"){
      
      #stores specific sponsors for the selected site type
      spec_Sponsor_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type 
                                                                                & data_set$data_src %in% list_data_src)]
      
      #stores unique instances of the specific sponsor name
      uniq_spec_Sponsor <- unique(spec_Sponsor_uniq_Site_Type)
      
      #lets user choose a specific sponsor from a list of unique sponsors names
      list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor
                        ,multiple = TRUE, title = "Choose the sponsor/sponsors for the chart"))
      
      #stores specific variables(like, site_name) based on the selected unique sponsor
      spec_site_name <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_en_cam_popn <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_clinic_en_pcnt <- data_set$Enroll_Clinic_Enroll_Pcnt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_clinic_en <- data_set$Enroll_Clinic_Enroll[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_n_enrolled <- data_set$n_enrolled[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      #creates a new data set with the specific variables
      new_data_set <- data.frame(spec_site_name,spec_en_cam_popn,spec_clinic_en_pcnt,spec_clinic_en,spec_n_enrolled )
      
      #creates a data set from the new data set in the long format. Selects variables from the data set which will be considered as values
      new_data_1 <- melt(as.data.frame(new_data_set), id.vars=c('spec_site_name','spec_en_cam_popn','spec_clinic_en_pcnt'),
                measure.vars=c('spec_n_enrolled','spec_clinic_en'))
      
      #creates a data set from the new data set in the long format. Selects variables from the data set which will be considered as values
      new_data_2 <- melt(as.data.frame(new_data_set), id.vars=c('spec_site_name','spec_en_cam_popn','spec_clinic_en_pcnt'),
                measure.vars=c('spec_n_enrolled','spec_clinic_en','spec_en_cam_popn'))
      #creates a unique data label for the bars consisting two different variable values
      new_data_2$dt_lbl <- paste(new_data_2$spec_en_cam_popn,";\n",new_data_2$spec_clinic_en_pcnt,"%")
      
      #makes sure the data label is not repeated for each variable and is only displayed once
      new_sub_data <- subset(new_data_2, variable == "spec_en_cam_popn")
      
      option_text2<-c('Identified','De-Identified')
      #lets user choose an option specified in option_text2 
      list_identification<-c(select.list(option_text2, title ="Please choose how you want your charts "))
      
      if(list_identification == "Identified"){
        ggplot()+
          #creates the first bar based on new_data_1, sets the order, x and y axis values, and the fill
          geom_col(data = new_data_1, aes(x=reorder(spec_site_name,value,FUN = sum),
                                          value, fill=variable),width=.6) + 
          #inserts blank for the x and y axis titles and displays the actual title for the chart
          labs(fill="",y="",x="",
               title=paste("\nNYC ", list_uniq_Sponsor ,": ",titles,"\n",subtitles))+
          #sets the theme of the chart to classic (no gridlines or background color)
          theme_classic()+
          #displays the stacked data labels for the bars
          annotate("text", x=new_sub_data$spec_site_name, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.4,size=3)+
          
          #displays the value data labels for the bars
          geom_text(data=new_data_1, aes(x=reorder(spec_site_name,value,FUN = sum),
                                         value,fill=variable, label=value),
                    position=position_stack(vjust=.5),size=3)+
          #sets hte colors for the bars; sets the breaks for the x-axis; sets label-title for the fill-legends
          scale_fill_manual(values=c("#f8cbad","#bdd7ee"),
                            labels=c("Enrolled   ",
                                     "Not Enrolled   "),
                            breaks=c("spec_clinic_en","spec_n_enrolled"))+
          #sets the scale for the chart
          scale_y_continuous(limits=c(0,3200),breaks=seq(0,3200, by = 500))+
          #sets the position, direction and size for the legends
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          #sets the position and angle of the x-axis labels
          theme(axis.text.x = element_text(angle = 60, hjust = 1))+
          #sets the position, size, and color for the chart-title
          theme(plot.title = element_text(hjust = 0.5,size=20))
      } else if(list_identification == "De-Identified"){
        ggplot()+
          geom_col(data = new_data_1, aes(x=reorder(spec_site_name,value,FUN = sum),
                                          value, fill=variable),width=.6) + 
          labs(fill="",y="",x="",
               title=paste("\nNYC ", list_uniq_Sponsor ,": ",titles,"\n",subtitles))+
          theme_classic()+
          annotate("text", x=new_sub_data$spec_site_name, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.4,size=3)+
          
          geom_text(data=new_data_1, aes(x=reorder(spec_site_name,value,FUN = sum),
                                         value,fill=variable, label=value),
                    position=position_stack(vjust=.5),size=3)+
          scale_fill_manual(values=c("#f8cbad","#bdd7ee"),
                            labels=c("Enrolled   ",
                                     "Not Enrolled   "),
                            breaks=c("spec_clinic_en","spec_n_enrolled"))+
          scale_y_continuous(limits=c(0,3200),breaks=seq(0,3200, by = 500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))
      }
    } else if (list_Sponsor_Level_Options == "Specific Sponsor (HS) Highlighted"){
      
      spec_Sponsors_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type & data_set$Enroll_Campus_Pop_9to12 !=0
                                                                                 & data_set$data_src %in% list_data_src)]
      uniq_spec_Sponsor <- unique(spec_Sponsors_uniq_Site_Type)
      
      list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor,
                        title ="Choose specific Sponsor to highlight for your charts ",
                        multiple=FALSE))
      
      spec_site_name <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_en_cam_popn <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_clinic_en_pcnt <- data_set$Enroll_Clinic_Enroll_Pcnt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_clinic_en <- data_set$Enroll_Clinic_Enroll[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_n_en <- data_set$n_enrolled[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_hs_popn <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      new_data_set_1 <- data.frame(spec_site_name,spec_en_cam_popn,spec_clinic_en_pcnt,spec_clinic_en,spec_n_en,spec_hs_popn)
      
      new_data_1 <- melt(as.data.frame(new_data_set_1), id.vars=c('spec_site_name',
                                                                'spec_en_cam_popn','spec_clinic_en_pcnt','spec_hs_popn'),
                measure.vars=c('spec_n_en','spec_clinic_en'))
      new_data_1 <- na.omit(new_data_1)
      
      
      sitename_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      en_cam_popn_all_sponsor <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      clinic_en_pcnt_all_sponsor <- data_set$Enroll_Clinic_Enroll_Pcnt[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      clinic_en_all_sponsor <- data_set$Enroll_Clinic_Enroll[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      n_en_all_sponsor <- data_set$n_enrolled[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      hs_popn_all_sponsor <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      new_dat_set_2 <- data.frame(sitename_all_sponsor,en_cam_popn_all_sponsor,clinic_en_pcnt_all_sponsor,
                       clinic_en_all_sponsor,n_en_all_sponsor,hs_popn_all_sponsor)
      new_data_2 <- melt(as.data.frame(new_dat_set_2), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor',
                                                                 'clinic_en_pcnt_all_sponsor','hs_popn_all_sponsor'),
                measure.vars=c('n_en_all_sponsor','clinic_en_all_sponsor'))
      new_data_2 <- na.omit(new_data_2)
      
      new_data_3 <- melt(as.data.frame(new_dat_set_2), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor',
                                             'clinic_en_pcnt_all_sponsor','hs_popn_all_sponsor'),
                measure.vars=c('n_en_all_sponsor','clinic_en_all_sponsor','en_cam_popn_all_sponsor'))
      new_data_3 <- na.omit(new_data_3)
      
      
      new_data_1_hs <-subset(new_data_1, spec_hs_popn!=0)
      #nsd1<-subset(nod1, value!=0)
      
      new_data_2_hs <- subset(new_data_2, hs_popn_all_sponsor!=0)
      #nsd2<-subset(nod2, value!=0)
      
      new_data_3_hs <- subset(new_data_3, hs_popn_all_sponsor!=0)
      #nsd3 <- subset(nod3, value!=0)
      
      new_data_3_hs$dt_lbl<- paste(new_data_3_hs$en_cam_popn_all_sponsor,";\n",new_data_3_hs$clinic_en_pcnt_all_sponsor,"%")
      new_sub_data <- subset(new_data_3_hs, variable == "en_cam_popn_all_sponsor")
      
      option_text2 <- c('Identified','De-Identified')
      list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
      
      if(list_identification == "Identified"){
        
        ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value,FUN = sum), value,
                                         fill=variable,label=value))+
          geom_col(width = .8) + 
          labs(fill="",y="",x="",
               title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value,
                   label= new_sub_data$dt_lbl, vjust=-.4,size=1.4)+
          
          geom_text(position=position_stack(vjust=.5), size=1.4)+
          scale_fill_manual(values=c("#d9eaf9", "#f9dbc7","#5781a5","#f29c63"),
                            labels=c("Enrolled   ",
                                     "Not Enrolled  "),
                            breaks=c("spec_clinic_en","spec_n_en"))+
          #scale_y_continuous(breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          geom_col(data=new_data_1_hs, aes(reorder(spec_site_name,value,FUN = sum),y=value
                                  ,fill=variable),width=.8)+
          geom_text(aes(x=reorder(spec_site_name,value,FUN = sum),y=value,label=value),
                    position=position_stack(vjust=.5), data=new_data_1_hs, size=1.4,color='white')+
          scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
      } else if(list_identification == "De-Identified"){
        ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value,FUN = sum), value,
                                         fill=variable,label=value))+
          geom_col(width = .8) + 
          labs(fill="",y="",x="",
               title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value,
                   label= new_sub_data$dt_lbl, vjust=-.4,size=1.4)+
          
          geom_text(position=position_stack(vjust=.5), size=1.4)+
          scale_fill_manual(values=c("#d9eaf9", "#f9dbc7","#5781a5","#f29c63"),
                            labels=c("Enrolled   ",
                                     "Not Enrolled  "),
                            breaks=c("spec_clinic_en","spec_n_en"))+
          #scale_y_continuous(breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          geom_col(data=new_data_1_hs, aes(reorder(spec_site_name,value,FUN = sum),y=value
                                           ,fill=variable),width=.8)+
          geom_text(aes(x=reorder(spec_site_name,value,FUN = sum),y=value,label=value),
                    position=position_stack(vjust=.5), data=new_data_1_hs, size=1.4,color='white')+
          scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
      }
    } else if (list_Sponsor_Level_Options =="Specific Sponsor (Non-HS) Highlighted"){
      
      spec_Sponsors_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type & data_set$Enroll_Campus_Pop_9to12 == 0
                                                                                 & data_set$data_src %in% list_data_src)]
      uniq_spec_Sponsor <- unique(spec_Sponsors_uniq_Site_Type)
      
      list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor,
                                         title ="Choose specific Sponsor to highlight for your charts ",
                                         multiple=FALSE))
      
      spec_site_name <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_en_cam_popn <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_clinic_en_pcnt <- data_set$Enroll_Clinic_Enroll_Pcnt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_clinic_en <- data_set$Enroll_Clinic_Enroll[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_n_en <- data_set$n_enrolled[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_hs_popn <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      new_data_set_1 <- data.frame(spec_site_name,spec_en_cam_popn,spec_clinic_en_pcnt,spec_clinic_en,spec_n_en,spec_hs_popn)
      
      new_data_1 <- melt(as.data.frame(new_data_set_1), id.vars=c('spec_site_name',
                                                                  'spec_en_cam_popn','spec_clinic_en_pcnt','spec_hs_popn'),
                         measure.vars=c('spec_n_en','spec_clinic_en'))
      new_data_1 <- na.omit(new_data_1)
      
      
      sitename_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      en_cam_popn_all_sponsor <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      clinic_en_pcnt_all_sponsor <- data_set$Enroll_Clinic_Enroll_Pcnt[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      clinic_en_all_sponsor <- data_set$Enroll_Clinic_Enroll[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      n_en_all_sponsor <- data_set$n_enrolled[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      hs_popn_all_sponsor <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      new_dat_set_2 <- data.frame(sitename_all_sponsor,en_cam_popn_all_sponsor,clinic_en_pcnt_all_sponsor,
                                  clinic_en_all_sponsor,n_en_all_sponsor,hs_popn_all_sponsor)
      new_data_2 <- melt(as.data.frame(new_dat_set_2), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor',
                                                                 'clinic_en_pcnt_all_sponsor','hs_popn_all_sponsor'),
                         measure.vars=c('n_en_all_sponsor','clinic_en_all_sponsor'))
      new_data_2 <- na.omit(new_data_2)
      
      new_data_3 <- melt(as.data.frame(new_dat_set_2), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor',
                                                                 'clinic_en_pcnt_all_sponsor','hs_popn_all_sponsor'),
                         measure.vars=c('n_en_all_sponsor','clinic_en_all_sponsor','en_cam_popn_all_sponsor'))
      new_data_3 <- na.omit(new_data_3)
      
      
      new_data_1_hs <-subset(new_data_1, spec_hs_popn == 0)
      #nsd1<-subset(nod1, value!=0)
      
      new_data_2_hs <- subset(new_data_2, hs_popn_all_sponsor == 0)
      #nsd2<-subset(nod2, value!=0)
      
      new_data_3_hs <- subset(new_data_3, hs_popn_all_sponsor == 0)
      #nsd3 <- subset(nod3, value!=0)
      
      new_data_3_hs$dt_lbl<- paste(new_data_3_hs$en_cam_popn_all_sponsor,";\n",new_data_3_hs$clinic_en_pcnt_all_sponsor,"%")
      new_sub_data <- subset(new_data_3_hs, variable == "en_cam_popn_all_sponsor")
      
      option_text2 <- c('Identified','De-Identified')
      list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
      
      if(list_identification == "Identified"){
        
        ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value,FUN = sum), value,
                                         fill=variable,label=value))+
          geom_col(width = .8) + 
          labs(fill="",y="",x="",
               title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value,
                   label= new_sub_data$dt_lbl, vjust=-.4,size=1.4)+
          
          geom_text(position=position_stack(vjust=.5), size=1.4)+
          scale_fill_manual(values=c("#d9eaf9", "#f9dbc7","#5781a5","#f29c63"),
                            labels=c("Enrolled   ",
                                     "Not Enrolled  "),
                            breaks=c("spec_clinic_en","spec_n_en"))+
          #scale_y_continuous(limits=c(0,1500),breaks=seq(0,1500, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          geom_col(data=new_data_1_hs, aes(reorder(spec_site_name,value,FUN = sum),y=value
                                           ,fill=variable),width=.8)+
          geom_text(aes(x=reorder(spec_site_name,value,FUN = sum),y=value,label=value),
                    position=position_stack(vjust=.5), data=new_data_1_hs, size=1.4,color='white')+
          scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
      } else if(list_identification == "De-Identified"){
        ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value,FUN = sum), value,
                                         fill=variable,label=value))+
          geom_col(width = .8) + 
          labs(fill="",y="",x="",
               title=paste("\nNYC ", list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value,
                   label= new_sub_data$dt_lbl, vjust=-.4,size=1.4)+
          
          geom_text(position=position_stack(vjust=.5), size=1.4)+
          scale_fill_manual(values=c("#d9eaf9", "#f9dbc7","#5781a5","#f29c63"),
                            labels=c("Enrolled   ",
                                     "Not Enrolled  "),
                            breaks=c("spec_clinic_en","spec_n_en"))+
          #scale_y_continuous(limits=c(0,1500),breaks=seq(0,1500, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          geom_col(data=new_data_1_hs, aes(reorder(spec_site_name,value,FUN = sum),y=value
                                           ,fill=variable),width=.8)+
          geom_text(aes(x=reorder(spec_site_name,value,FUN = sum),y=value,label=value),
                    position=position_stack(vjust=.5), data=new_data_1_hs, size=1.4,color='white')+
          scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
      }
      
    }
    
    

  
  
  
}






