
# !diagnostics off


create_capacity_et <- function(myfile1){
  data_set<-read_excel(myfile1)

  
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
  list_Sponsor_Level_Options <- c(select.list(option_text1,
                                              title ="Do you want to choose specific Sponsor for your charts ",
                                              multiple=FALSE))
  
    if(  list_Sponsor_Level_Options =="Specific Sponsor"){
      spec_Sponsor_uniq_Site_Type<-data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type
                                                                              & data_set$data_src %in% list_data_src)]
      uniq_spec_Sponsor<-unique(spec_Sponsor_uniq_Site_Type)
      list_uniq_Sponsor<-c(select.list(uniq_spec_Sponsor
                        ,multiple = TRUE, title = "Choose the sponsor/sponsors for the chart"))
      
      spec_sitename<-data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_pt_capacity<-data_set$Site_Patient_Capacity_Year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_visit_capacity<-data_set$Site_Visit_Capacity_year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_ut_md_pt<-data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_dat_md_vst<-data_set$DatEnt_Med_Vst[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      new_data_set_1 <-data.frame(spec_sitename,spec_pt_capacity,spec_visit_capacity,spec_ut_md_pt,spec_dat_md_vst)
      new_data_1<-melt(as.data.frame(new_data_set_1),
                id.vars=c('spec_sitename','spec_pt_capacity','spec_visit_capacity'),
                measure.vars=c('spec_ut_md_pt','spec_dat_md_vst'))
      
      option_text2<-c('Identified','De-Identified')
      list_identification<-c(select.list(option_text2, title ="Please choose how you want your charts "))
      
      if (list_identification=="Identified"){
        ggplot(data=new_data_1,aes(reorder(spec_sitename,value),value,label=value,fill=variable))+
          geom_col(position=position_dodge())+
          labs(fill="", y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+
          geom_text(position=position_dodge(width=1),aes(vjust=-.8), colour="black",size =2.8) +
          geom_abline(aes(slope=0, intercept=new_data_1$spec_pt_capacity,col = "Patients Capacity   "),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=new_data_1$spec_visit_capacity,col = "Visits Capacity"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0))+
          labs(colour="") +
          scale_colour_manual(values=c("#2f5597",'#548235',"black"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3500, by=500))+
          scale_fill_manual(values=c("#9dc3e6","#c5e0b4"),labels=c("Medical Patients   ","Medical Visits"))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))
        
      } else if(list_identification == "De-Identified"){
        
        ggplot(data=new_data_1,aes(reorder(spec_sitename,value),value,label=value,fill=variable))+
          geom_col(position=position_dodge())+
          labs(fill="", y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+
          geom_text(position=position_dodge(width=1),aes(vjust=-.8), colour="black",size = 2.8) +
          geom_abline(aes(slope=0, intercept=new_data_1$spec_pt_capacity,col = "Patients Capacity"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=new_data_1$spec_visit_capacity,col = "Visits Capacity"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0))+
          labs(colour="") +
          scale_colour_manual(values=c("#2f5597",'#548235',"black"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3500, by=500))+
          scale_fill_manual(values=c("#9dc3e6","#c5e0b4"),labels=c("Medical Patients","Medical Visits"))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))
      }
      
    } else if(  list_Sponsor_Level_Options == "Specific Sponsor (HS) Highlighted"){
      
      spec_Sponsor_uniq_Site_Type<-data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type 
                                                                              & data_set$Enroll_Campus_Pop_9to12 !=0
                                                                              & data_set$data_src %in% list_data_src)]
      uniq_spec_Sponsor<-unique(spec_Sponsor_uniq_Site_Type)
      list_uniq_Sponsor<-c(select.list(uniq_spec_Sponsor,
                        title ="Choose specific Sponsor to highlight for your charts ",
                        multiple=FALSE))
      
      sitename_all_sponsor<-data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      pt_capacity_all_sponsor<-data_set$Site_Patient_Capacity_Year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      vst_capacity_all_sponsor<-data_set$Site_Visit_Capacity_year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      ut_md_pt_all_sponsor<-data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      dat_md_vst_all_sponsor<-data_set$DatEnt_Med_Vst[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      hs_cam_popn_all_sponsor<-data_set$Enroll_Campus_Pop_9to12 [which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      new_data_set_1<- data.frame(sitename_all_sponsor,pt_capacity_all_sponsor,vst_capacity_all_sponsor,ut_md_pt_all_sponsor,dat_md_vst_all_sponsor,hs_cam_popn_all_sponsor)
      
      
      
      spec_sitename<-data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_pt_capacity<-data_set$Site_Patient_Capacity_Year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_visit_capacity<-data_set$Site_Visit_Capacity_year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_ut_md_pt<-data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_dat_md_vst<-data_set$DatEnt_Med_Vst[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_hs_cam_popn<-data_set$Enroll_Campus_Pop_9to12 [which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      new_data_set_2<- data.frame(spec_sitename,spec_pt_capacity,spec_visit_capacity,spec_ut_md_pt,spec_dat_md_vst,spec_hs_cam_popn)
      
      
      new_data_1<-melt(as.data.frame(new_data_set_1),
                id.vars=c('sitename_all_sponsor','pt_capacity_all_sponsor','vst_capacity_all_sponsor','hs_cam_popn_all_sponsor'),
                measure.vars=c('ut_md_pt_all_sponsor','dat_md_vst_all_sponsor'))
      new_data_1<- na.omit(new_data_1)
      
      
      new_data_2 <- melt(as.data.frame(new_data_set_2),
                  id.vars=c('spec_sitename','spec_pt_capacity','spec_visit_capacity','spec_hs_cam_popn'),
                  measure.vars=c('spec_ut_md_pt','spec_dat_md_vst'))
      
      new_sub_data_hs_1<-subset(new_data_1, hs_cam_popn_all_sponsor!=0)
      new_data_hs_1<- subset(new_sub_data_hs_1,value!=0)
      new_sub_data_hs_2<- subset(new_data_2, spec_hs_cam_popn!=0)
      new_data_hs_2<- subset(new_sub_data_hs_2,value!=0)
      
      
      option_text<-c('Identified','De-Identified')
      list_identification<-c(select.list(option_text, title ="Please choose how you want your charts "))
      
      if (list_identification=="Identified"){
        ggplot(data=new_data_hs_1,aes(reorder(sitename_all_sponsor,value),value,label=value,fill=variable))+
          geom_col(position=position_dodge(), width =.7)+
          labs(fill="", y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+ 
          geom_text(position=position_dodge(width=1),aes(vjust=-.8), colour="black", size=1.1) +
          geom_abline(aes(slope=0, intercept=new_data_hs_1$pt_capacity_all_sponsor,col = "Patients Capacity"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=new_data_hs_1$vst_capacity_all_sponsor,col = "Visits Capacity"),size=1,alpha=.7)+
          
          geom_col(data=new_data_hs_2, aes(reorder(spec_sitename,value),value,
                                  fill=variable),position=position_dodge(), width =.7)+
          
          geom_abline(aes(slope=0, intercept=0))+
          labs(colour="") +
          scale_colour_manual(values=c("#2f5597",'#548235',"black"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3500, by=500))+
          scale_fill_manual(values=c("#c5e0b4","#47911a","#619af4","#b6ccef"),
                            labels=c("Medical Patients","Medical Visits"),
                            breaks=c('spec_ut_md_pt','spec_dat_md_vst'))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_2$spec_sitename)
        
        
      }else if (list_identification == "De-Identified"){
        ggplot(data=new_data_hs_1,aes(reorder(sitename_all_sponsor,value, FUN = mean),value,label=value,fill=variable))+
          geom_col(position=position_dodge(), width =.7)+
          labs(fill="", y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+ 
          geom_text(position=position_dodge(width=.7),aes(vjust=-.8), colour="black", size=1.5) +
          geom_abline(aes(slope=0, intercept=new_data_hs_1$pt_capacity_all_sponsor,col = "Patients Capacity"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=new_data_hs_1$vst_capacity_all_sponsor,col = "Visits Capacity"),size=1,alpha=.7)+
          
          geom_col(data=new_data_hs_2, aes(reorder(spec_sitename,value, FUN = mean),value,
                                  fill=variable),position=position_dodge(), width =.7)+
          
          geom_abline(aes(slope=0, intercept=0))+
          labs(colour="") +
          scale_colour_manual(values=c("#2f5597",'#548235',"black"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3500, by=500))+
          scale_fill_manual(values=c("#c5e0b4","#47911a","#619af4","#b6ccef"),
                            labels=c("Medical Patients","Medical Visits"),
                            breaks=c('spec_ut_md_pt','spec_dat_md_vst'))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_2$spec_sitename)
      }
      
      
    } else if(  list_Sponsor_Level_Options == "Specific Sponsor (Non-HS) Highlighted"){
      spec_Sponsor_uniq_Site_Type<-data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type 
                                                                              & data_set$Enroll_Campus_Pop_9to12  ==0
                                                                              & data_set$data_src %in% list_data_src)]
      uniq_spec_Sponsor<-unique(spec_Sponsor_uniq_Site_Type)
      list_uniq_Sponsor<-c(select.list(uniq_spec_Sponsor,
                                       title ="Choose specific Sponsor to highlight for your charts ",
                                       multiple=FALSE))
      
      sitename_all_sponsor<-data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      pt_capacity_all_sponsor<-data_set$Site_Patient_Capacity_Year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      vst_capacity_all_sponsor<-data_set$Site_Visit_Capacity_year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      ut_md_pt_all_sponsor<-data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      dat_md_vst_all_sponsor<-data_set$DatEnt_Med_Vst[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      hs_cam_popn_all_sponsor<-data_set$Enroll_Campus_Pop_9to12 [which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      new_data_set_1<- data.frame(sitename_all_sponsor,pt_capacity_all_sponsor,vst_capacity_all_sponsor,ut_md_pt_all_sponsor,dat_md_vst_all_sponsor,hs_cam_popn_all_sponsor)
      
      
      
      spec_sitename<-data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_pt_capacity<-data_set$Site_Patient_Capacity_Year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_visit_capacity<-data_set$Site_Visit_Capacity_year_HRSA[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_ut_md_pt<-data_set$Utiliz_All_Med_Pt[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_dat_md_vst<-data_set$DatEnt_Med_Vst[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      spec_hs_cam_popn<-data_set$Enroll_Campus_Pop_9to12 [which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      new_data_set_2<- data.frame(spec_sitename,spec_pt_capacity,spec_visit_capacity,spec_ut_md_pt,spec_dat_md_vst,spec_hs_cam_popn)
      
      
      new_data_1<-melt(as.data.frame(new_data_set_1),
                       id.vars=c('sitename_all_sponsor','pt_capacity_all_sponsor','vst_capacity_all_sponsor','hs_cam_popn_all_sponsor'),
                       measure.vars=c('ut_md_pt_all_sponsor','dat_md_vst_all_sponsor'))
      new_data_1<- na.omit(new_data_1)
      
      
      new_data_2 <- melt(as.data.frame(new_data_set_2),
                         id.vars=c('spec_sitename','spec_pt_capacity','spec_visit_capacity','spec_hs_cam_popn'),
                         measure.vars=c('spec_ut_md_pt','spec_dat_md_vst'))
      
      new_sub_data_hs_1<-subset(new_data_1, hs_cam_popn_all_sponsor==0)
      new_data_hs_1<- subset(new_sub_data_hs_1,value!=0)
      new_sub_data_hs_2<- subset(new_data_2, spec_hs_cam_popn==0)
      new_data_hs_2<- subset(new_sub_data_hs_2,value!=0)
      
      
      option_text<-c('Identified','De-Identified')
      list_identification<-c(select.list(option_text, title ="Please choose how you want your charts "))
      
      if (list_identification=="Identified"){
        ggplot(data=new_data_hs_1,aes(reorder(sitename_all_sponsor,value),value,label=value,fill=variable))+
          geom_col(position=position_dodge(), width =.7)+
          labs(fill="", y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+ 
          geom_text(position=position_dodge(width=.7),aes(vjust=-.8), colour="black", size=1.1) +
          geom_abline(aes(slope=0, intercept=new_data_hs_1$pt_capacity_all_sponsor,col = "Patients Capacity"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=new_data_hs_1$vst_capacity_all_sponsor,col = "Visits Capacity"),size=1,alpha=.7)+
          
          geom_col(data=new_data_hs_2, aes(reorder(spec_sitename,value),value,
                                           fill=variable),position=position_dodge(), width =.7)+
          
          geom_abline(aes(slope=0, intercept=0))+
          labs(colour="") +
          scale_colour_manual(values=c("#2f5597",'#548235',"black"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3500, by=500))+
          scale_fill_manual(values=c("#c5e0b4","#47911a","#619af4","#b6ccef"),
                            labels=c("Medical Patients","Medical Visits"),
                            breaks=c('spec_ut_md_pt','spec_dat_md_vst'))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_2$spec_sitename)
        
        
      }else if (list_identification == "De-Identified"){
        ggplot(data=new_data_hs_1,aes(reorder(sitename_all_sponsor,value, FUN = mean),value,label=value,fill=variable))+
          geom_col(position=position_dodge(), width =.7)+
          labs(fill="", y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          theme_classic()+ 
          geom_text(position=position_dodge(width=.7),aes(vjust=-.8), colour="black", size=1.5) +
          geom_abline(aes(slope=0, intercept=new_data_hs_1$pt_capacity_all_sponsor,col = "Patients Capacity"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=new_data_hs_1$vst_capacity_all_sponsor,col = "Visits Capacity"),size=1,alpha=.7)+
          
          geom_col(data=new_data_hs_2, aes(reorder(spec_sitename,value, FUN = mean),value,
                                           fill=variable),position=position_dodge(), width =.7)+
          
          geom_abline(aes(slope=0, intercept=0))+
          labs(colour="") +
          scale_colour_manual(values=c("#2f5597",'#548235',"black"))+
          #scale_y_continuous(limits=c(0,3500),breaks=seq(0,3500, by=500))+
          scale_fill_manual(values=c("#c5e0b4","#47911a","#619af4","#b6ccef"),
                            labels=c("Medical Patients","Medical Visits"),
                            breaks=c('spec_ut_md_pt','spec_dat_md_vst'))+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          theme(axis.text.x = element_blank())+
          theme(plot.title = element_text(hjust = 0.5,size=20))+
          scale_x_discrete(breaks = new_data_hs_2$spec_sitename)
      }
      
    }
  
}
