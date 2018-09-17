
# !diagnostics off

create_rc_cam_et<-function(myfile1){
  
  data_set <- read_excel(myfile1)
  
  
  data_set$x_cam_pop<-data_set$Enroll_Campus_Popn-data_set$Utiliz_All_Med_Pt
  data_set$x_cam_pct <-round((data_set$Utiliz_All_Med_Pt/data_set$Enroll_Campus_Popn)*100)
  
  titles <- readline(prompt = "PLEASE ENTER YOUR DESIRED TITLE: ")
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
  
  
    if(list_Sponsor_Level_Options=="Specific Sponsor"){
      
      spec_Sponsor_uniq_Site_Type<-data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type
                                                                              & data_set$data_src %in% list_data_src)]
      uniq_spec_Sponsor <- unique(spec_Sponsor_uniq_Site_Type)
      
      list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor
                        ,multiple = TRUE, title = "Choose the sponsor/sponsors for the chart"))
      
      spec_sitename <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_en_camp_popn <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_ut_md_pt <- data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_x_cam_popn<-data_set$x_cam_pop[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_x_cam_pct<-data_set$x_cam_pct[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      #spec_hs_cam_popn<-data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      new_data_set_1 <- data.frame(spec_sitename,spec_en_camp_popn,spec_ut_md_pt,spec_x_cam_popn,spec_x_cam_pct)
      
      new_data_1<-melt(as.data.frame(new_data_set_1), id.vars=c('spec_sitename','spec_en_camp_popn'),
                measure.vars=c('spec_x_cam_popn','spec_ut_md_pt'))
      new_data_2<-melt(as.data.frame(new_data_set_1), id.vars=c('spec_sitename','spec_en_camp_popn'),
                measure.vars=c('spec_x_cam_popn','spec_ut_md_pt','spec_en_camp_popn'))
      
      new_data_3<-melt(as.data.frame(new_data_set_1), id.vars=c('spec_sitename','spec_en_camp_popn','spec_x_cam_pct'),
                measure.vars=c('spec_ut_md_pt'))
      
      #new_data_1<-subset(new_data_1, spec_hs_cam_popn!=0)
      
      #new_data_2<-subset(new_data_2, spec_hs_cam_popn!=0)
      
      #new_data_3<-subset(new_data_3, spec_hs_cam_popn!=0)
      
      k<- as.numeric(length(unique(new_data_set_1$spec_sitename)))
      
      if (k>12){
        new_data_3$dt_lbl2<- paste(new_data_3$value,";","\n",new_data_3$spec_x_cam_pct,"%")
      } else if (k<12){
        new_data_3$dt_lbl2<- paste(new_data_3$value,";",new_data_3$spec_x_cam_pct,"%")
      }else if (k==12){
        new_data_3$dt_lbl2<- paste(new_data_3$value,";",new_data_3$spec_x_cam_pct,"%")
      }
      
      new_data_2$dt_lbl<- new_data_2$spec_en_camp_popn
      
      
      new_sub_data <- subset(new_data_2, variable == "spec_en_camp_popn")
      
      option_text2<-c('Identified','De-Identified')
      list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))  
      
      
      if (list_identification=="Identified"){
        
        ggplot()+
          geom_col(data = new_data_1, aes(x=reorder(spec_sitename,value), value, fill=variable),width=.5) + 
          labs(fill="",y="",x="",  title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
          )+ theme_classic()+ 
          annotate("text", x=new_sub_data$spec_sitename, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=3)+
          
          geom_text(position=position_stack(vjust=.68),size=0,color="#d9d9d9",alpha=0)+
          scale_fill_manual(values=c("#d9d9d9","#1f4e79"),
                            labels=c("Medical Patients Seen(N;%)   ","  Campus Population")
                            , breaks=c("spec_ut_md_pt","spec_x_cam_popn"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(1, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          
          geom_col(data=new_data_3,aes(x=reorder(spec_sitename,value),y=value,fill=variable),width=.5)+
          annotate("text", x=new_data_3$spec_sitename, y=new_data_3$value, label= new_data_3$dt_lbl2
                   ,vjust=1.5,size=2,color="white")+
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))
        
        
      } else if(list_identification=="De-Identified"){
        
        ggplot()+
          geom_col(data = new_data_1, aes(x=reorder(spec_sitename,value), value, fill=variable),width=.5) + 
          labs(fill="",y="",x="",  title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
          )+ theme_classic()+ 
          annotate("text", x=new_sub_data$spec_sitename, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=3)+
          
          geom_text(position=position_stack(vjust=.68),size=3,color="#d9d9d9")+
          scale_fill_manual(values=c("#d9d9d9","#1f4e79"),
                            labels=c("Medical Patients Seen(N;%)   ","  Campus Population")
                            , breaks=c("spec_ut_md_pt","spec_x_cam_popn"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(1, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          
          geom_col(data=new_data_3,aes(x=reorder(spec_sitename,value),y=value,fill=variable),width=.5)+
          annotate("text", x=new_data_3$spec_sitename, y=new_data_3$value, label= new_data_3$dt_lbl2
                   ,vjust=1.5,size=2,color="white")+
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))
      } 
      
      
    }else if (list_Sponsor_Level_Options=="Specific Sponsor (HS) Highlighted"){
      
      
      spec_Sponsor_uniq_Site_Type<-data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type
                                                                              & data_set$Enroll_Campus_Pop_9to12 !=0
                                                                              & data_set$data_src %in% list_data_src)]
      
      uniq_spec_Sponsor<-unique(spec_Sponsor_uniq_Site_Type)
      list_uniq_Sponsor<-c(select.list(uniq_spec_Sponsor,
                        title ="Choose specific Sponsor to highlight for your charts ",
                        multiple=FALSE))
      
      
      
      sitename_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      en_cam_popn_all_sponsor <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      ut_md_pt_all_sponsor <- data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      x_cam_pop_all_sponsor<-data_set$x_cam_pop[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      hs_cam_pop_all_sponsor<-data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      x_cam_pct_all_sponsor<-data_set$x_cam_pct[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      new_data_set_1<-data.frame(sitename_all_sponsor,en_cam_popn_all_sponsor,ut_md_pt_all_sponsor,x_cam_pop_all_sponsor,hs_cam_pop_all_sponsor,x_cam_pct_all_sponsor)
      
      new_data_1<-melt(as.data.frame(new_data_set_1), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor','hs_cam_pop_all_sponsor'),
                measure.vars=c('x_cam_pop_all_sponsor','ut_md_pt_all_sponsor'))
      new_data_1<-na.omit(new_data_1)
      
      new_data_2<-melt(as.data.frame(new_data_set_1), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor','hs_cam_pop_all_sponsor'),
                measure.vars=c('x_cam_pop_all_sponsor','ut_md_pt_all_sponsor','en_cam_popn_all_sponsor'))
      new_data_2<-na.omit(new_data_2)
      
      
      new_data_3<-melt(as.data.frame(new_data_set_1), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor','hs_cam_pop_all_sponsor','x_cam_pct_all_sponsor'),
                measure.vars=c('ut_md_pt_all_sponsor'))
      new_data_3<-na.omit(new_data_3)
      
      
      spec_sitename<-data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_en_camp_popn<-data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_ut_md_pt<-data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_x_cam_popn<-data_set$x_cam_pop[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_hs_cam_popn<-data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_x_cam_pct<-data_set$x_cam_pct[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      d3<-data.frame(spec_sitename,spec_en_camp_popn,spec_ut_md_pt,spec_x_cam_popn,spec_hs_cam_popn,spec_x_cam_pct)
      
      new_data_4<-melt(as.data.frame(d3), id.vars=c('spec_sitename','spec_en_camp_popn','spec_hs_cam_popn'),
                measure.vars=c('spec_x_cam_popn','spec_ut_md_pt'))
      new_data_5<-melt(as.data.frame(d3), id.vars=c('spec_sitename','spec_en_camp_popn','spec_hs_cam_popn'),
                measure.vars=c('spec_x_cam_popn','spec_ut_md_pt','spec_en_camp_popn'))
      
      new_data_6<-melt(as.data.frame(d3), id.vars=c('spec_sitename','spec_en_camp_popn','spec_hs_cam_popn','spec_x_cam_pct'),
                measure.vars=c('spec_ut_md_pt'))
      
      
      new_data_hs_1<-subset(new_data_1, hs_cam_pop_all_sponsor!=0)
      
      new_data_hs_2<-subset(new_data_2, hs_cam_pop_all_sponsor!=0)
      
      new_data_hs_3<-subset(new_data_3, hs_cam_pop_all_sponsor!=0)
      
      new_data_hs_4<-subset(new_data_4, spec_hs_cam_popn!=0)
      
      new_data_hs_5<-subset(new_data_5, spec_hs_cam_popn!=0)
      
      new_data_hs_6<-subset(new_data_6, spec_hs_cam_popn!=0)
      
      new_data_hs_6$dt_lbl3<- paste(new_data_hs_6$value,";","\n",new_data_hs_6$spec_x_cam_pct,"%")
      new_data_hs_3$dt_lbl2<- paste(new_data_hs_3$value,";","\n",new_data_hs_3$x_cam_pct_all_sponsor,"%")
      new_data_hs_2$dt_lbl<- new_data_hs_2$en_cam_popn_all_sponsor
      
      new_sub_data <- subset(new_data_hs_2, variable == "en_cam_popn_all_sponsor")
      
      option_text2 <- c('Identified','De-Identified')
      list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))  
      
      if (list_identification=="Identified"){
        
        ggplot()+
          geom_col(data = new_data_hs_1, aes(x=reorder(sitename_all_sponsor,value), value, fill=variable),width=.8) + 
          labs(fill="",y="",x="",  title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
          )+ theme_classic()+ 
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=1.5)+
          
          geom_text(position=position_stack(vjust=.68),size=1.5,color="#d9d9d9")+
          scale_fill_manual(values=c("#1f4e79","#918e8e","#b6d9f9","#d9d9d9"),
                            labels=c("Medical Patients Seen(N;%)   ","  Campus Population")
                            , breaks=c("spec_ut_md_pt","spec_x_cam_popn"))+
          
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(1, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          
          geom_col(data=new_data_hs_3,aes(x=reorder(sitename_all_sponsor,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_3$sitename_all_sponsor, y=new_data_hs_3$value, label= new_data_hs_3$dt_lbl2
                   ,vjust=1.5,size=1,color="black")+
          
          geom_col(data = new_data_hs_4, aes(x=reorder(spec_sitename,value), value, fill=variable),width=.8) +
          
          geom_col(data=new_data_hs_6,aes(x=reorder(spec_sitename,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_6$spec_sitename, y=new_data_hs_6$value, label=new_data_hs_6$dt_lbl3
                   ,vjust=1.5,size=1,color="white")+
          
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_4$spec_sitename)
        
      } else if(list_identification=="De-Identified"){
        
        ggplot()+
          geom_col(data = new_data_hs_1, aes(x=reorder(sitename_all_sponsor,value), value, fill=variable),width=.8) + 
          labs(fill="",y="",x="",  title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
          )+ theme_classic()+ 
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=1.5)+
          
          geom_text(position=position_stack(vjust=.68),size=1.5,color="#d9d9d9")+
          scale_fill_manual(values=c("#1f4e79","#918e8e","#b6d9f9","#d9d9d9"),
                            labels=c("Medical Patients Seen(N;%)   ","  Campus Population")
                            , breaks=c("spec_ut_md_pt","spec_x_cam_popn"))+
          
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(1, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          
          geom_col(data=new_data_hs_3,aes(x=reorder(sitename_all_sponsor,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_3$sitename_all_sponsor, y=new_data_hs_3$value, label= new_data_hs_3$dt_lbl2
                   ,vjust=1.5,size=1,color="black")+
          
          geom_col(data = new_data_hs_4, aes(x=reorder(spec_sitename,value), value, fill=variable),width=.8) +
          
          geom_col(data=new_data_hs_6,aes(x=reorder(spec_sitename,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_6$spec_sitename, y=new_data_hs_6$value, label= new_data_hs_6$dt_lbl3
                   ,vjust=1.5,size=1,color="white")+
          
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_4$spec_sitename)
      } 
      
    } else if (list_Sponsor_Level_Options == "Specific Sponsor (Non-HS) Highlighted"){
      
      spec_Sponsor_uniq_Site_Type<-data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type
                                                                              & data_set$Enroll_Campus_Pop_9to12 ==0
                                                                              & data_set$data_src %in% list_data_src)]
      
      uniq_spec_Sponsor<-unique(spec_Sponsor_uniq_Site_Type)
      list_uniq_Sponsor<-c(select.list(uniq_spec_Sponsor,
                                       title ="Choose specific Sponsor to highlight for your charts ",
                                       multiple=FALSE))
      
      
      
      sitename_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      en_cam_popn_all_sponsor <- data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      ut_md_pt_all_sponsor <- data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      x_cam_pop_all_sponsor<-data_set$x_cam_pop[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      hs_cam_pop_all_sponsor<-data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      x_cam_pct_all_sponsor<-data_set$x_cam_pct[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      new_data_set_1<-data.frame(sitename_all_sponsor,en_cam_popn_all_sponsor,ut_md_pt_all_sponsor,x_cam_pop_all_sponsor,hs_cam_pop_all_sponsor,x_cam_pct_all_sponsor)
      
      new_data_1<-melt(as.data.frame(new_data_set_1), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor','hs_cam_pop_all_sponsor'),
                       measure.vars=c('x_cam_pop_all_sponsor','ut_md_pt_all_sponsor'))
      new_data_1<-na.omit(new_data_1)
      
      new_data_2<-melt(as.data.frame(new_data_set_1), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor','hs_cam_pop_all_sponsor'),
                       measure.vars=c('x_cam_pop_all_sponsor','ut_md_pt_all_sponsor','en_cam_popn_all_sponsor'))
      new_data_2<-na.omit(new_data_2)
      
      
      new_data_3<-melt(as.data.frame(new_data_set_1), id.vars=c('sitename_all_sponsor','en_cam_popn_all_sponsor','hs_cam_pop_all_sponsor','x_cam_pct_all_sponsor'),
                       measure.vars=c('ut_md_pt_all_sponsor'))
      new_data_3<-na.omit(new_data_3)
      
      
      spec_sitename<-data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_en_camp_popn<-data_set$Enroll_Campus_Popn[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_ut_md_pt<-data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_x_cam_popn<-data_set$x_cam_pop[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_hs_cam_popn<-data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_x_cam_pct<-data_set$x_cam_pct[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      d3<-data.frame(spec_sitename,spec_en_camp_popn,spec_ut_md_pt,spec_x_cam_popn,spec_hs_cam_popn,spec_x_cam_pct)
      
      new_data_4<-melt(as.data.frame(d3), id.vars=c('spec_sitename','spec_en_camp_popn','spec_hs_cam_popn'),
                       measure.vars=c('spec_x_cam_popn','spec_ut_md_pt'))
      new_data_5<-melt(as.data.frame(d3), id.vars=c('spec_sitename','spec_en_camp_popn','spec_hs_cam_popn'),
                       measure.vars=c('spec_x_cam_popn','spec_ut_md_pt','spec_en_camp_popn'))
      
      new_data_6<-melt(as.data.frame(d3), id.vars=c('spec_sitename','spec_en_camp_popn','spec_hs_cam_popn','spec_x_cam_pct'),
                       measure.vars=c('spec_ut_md_pt'))
      
      
      new_data_hs_1<-subset(new_data_1, hs_cam_pop_all_sponsor==0)
      
      new_data_hs_2<-subset(new_data_2, hs_cam_pop_all_sponsor==0)
      
      new_data_hs_3<-subset(new_data_3, hs_cam_pop_all_sponsor==0)
      
      new_data_hs_4<-subset(new_data_4, spec_hs_cam_popn==0)
      
      new_data_hs_5<-subset(new_data_5, spec_hs_cam_popn==0)
      
      new_data_hs_6<-subset(new_data_6, spec_hs_cam_popn==0)
      
      new_data_hs_6$dt_lbl3<- paste(new_data_hs_6$value,";","\n",new_data_hs_6$spec_x_cam_pct,"%")
      new_data_hs_3$dt_lbl2<- paste(new_data_hs_3$value,";","\n",new_data_hs_3$x_cam_pct_all_sponsor,"%")
      new_data_hs_2$dt_lbl<- new_data_hs_2$en_cam_popn_all_sponsor
      
      new_sub_data <- subset(new_data_hs_2, variable == "en_cam_popn_all_sponsor")
      
      option_text2 <- c('Identified','De-Identified')
      list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))  
      
      if (list_identification=="Identified"){
        
        ggplot()+
          geom_col(data = new_data_hs_1, aes(x=reorder(sitename_all_sponsor,value), value, fill=variable),width=.8) + 
          labs(fill="",y="",x="",  title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
          )+ theme_classic()+ 
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=1.5)+
          
          geom_text(position=position_stack(vjust=.68),size=1.5,color="#d9d9d9")+
          scale_fill_manual(values=c("#1f4e79","#918e8e","#b6d9f9","#d9d9d9"),
                            labels=c("Medical Patients Seen(N;%)   ","  Campus Population")
                            , breaks=c("spec_ut_md_pt","spec_x_cam_popn"))+
          
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(1, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          
          geom_col(data=new_data_hs_3,aes(x=reorder(sitename_all_sponsor,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_3$sitename_all_sponsor, y=new_data_hs_3$value, label= new_data_hs_3$dt_lbl2
                   ,vjust=1.5,size=1,color="black")+
          
          geom_col(data = new_data_hs_4, aes(x=reorder(spec_sitename,value), value, fill=variable),width=.8) +
          
          geom_col(data=new_data_hs_6,aes(x=reorder(spec_sitename,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_6$spec_sitename, y=new_data_hs_6$value, label= new_data_hs_6$dt_lbl3
                   ,vjust=1.5,size=1,color="white")+
          
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_4$spec_sitename)
        
        
      } else if(list_identification=="De-Identified"){
        
        ggplot()+
          geom_col(data = new_data_hs_1, aes(x=reorder(sitename_all_sponsor,value), value, fill=variable),width=.8) + 
          labs(fill="",y="",x="",  title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
          )+ theme_classic()+ 
          annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=1.5)+
          
          geom_text(position=position_stack(vjust=.68),size=1.5,color="#d9d9d9")+
          scale_fill_manual(values=c("#1f4e79","#918e8e","#b6d9f9","#d9d9d9"),
                            labels=c("Medical Patients Seen(N;%)   ","  Campus Population")
                            , breaks=c("spec_ut_md_pt","spec_x_cam_popn"))+
          
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3000, by=500))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(1, 'lines'))+
          geom_abline(aes(slope=0, intercept=0))+
          scale_colour_manual(values=c("black"))+
          
          geom_col(data=new_data_hs_3,aes(x=reorder(sitename_all_sponsor,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_3$sitename_all_sponsor, y=new_data_hs_3$value, label= new_data_hs_3$dt_lbl2
                   ,vjust=1.5,size=1,color="black")+
          
          geom_col(data = new_data_hs_4, aes(x=reorder(spec_sitename,value), value, fill=variable),width=.8) +
          
          geom_col(data=new_data_hs_6,aes(x=reorder(spec_sitename,value),y=value,fill=variable),width=.8)+
          annotate("text", x=new_data_hs_6$spec_sitename, y=new_data_hs_6$value, label= new_data_hs_6$dt_lbl3
                   ,vjust=1.5,size=1,color="white")+
          
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_4$spec_sitename)
      } 
      
      
    }
    
  
}