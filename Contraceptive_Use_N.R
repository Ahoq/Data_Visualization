# !diagnostics off




create_ct_use_n<-function(myfile){ 
  
  
  
  data_set<-read_excel(myfile)
  data_set$ever_not_using <- (data_set$SA_Ever_Pt_F)-(data_set$Ever_SA_Pt_On_LARC_F + data_set$Ever_SA_Pt_On_ModEff)
  data_set$using_pct <- round(((data_set$Ever_SA_Pt_On_LARC_F + data_set$Ever_SA_Pt_On_ModEff)/(data_set$SA_Ever_Pt_F))*100)
  
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
    spec_Sponsor_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type &
                                                                                data_set$data_src %in% list_data_src)]
    
    #stores unique instances of the specific sponsor name
    uniq_spec_Sponsor <- unique(spec_Sponsor_uniq_Site_Type)
    
    #lets user choose a specific sponsor from a list of unique sponsors names
    list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor
                                       ,multiple = TRUE, title = "Choose the sponsor/sponsors for the chart"))
    
    spec_site_name <-  data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_ever_not_using <- data_set$ever_not_using[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_Ever_SA_Pt_On_ModEff <- data_set$Ever_SA_Pt_On_ModEff[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_Ever_SA_Pt_On_LARC_F <- data_set$Ever_SA_Pt_On_LARC_F[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_SA_Ever_Pt_F <- data_set$SA_Ever_Pt_F[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    
    new_data_set <- data.frame(spec_site_name, spec_ever_not_using, spec_Ever_SA_Pt_On_ModEff,
                               spec_Ever_SA_Pt_On_LARC_F, spec_SA_Ever_Pt_F)
    
    new_data_1<-melt(as.data.frame(new_data_set), id.vars=c('spec_site_name'),
            measure.vars=c('spec_ever_not_using','spec_Ever_SA_Pt_On_ModEff','spec_Ever_SA_Pt_On_LARC_F'))
    new_data_2<-melt(as.data.frame(new_data_set), id.vars=c('spec_site_name','spec_SA_Ever_Pt_F'),
            measure.vars=c('spec_ever_not_using','spec_Ever_SA_Pt_On_ModEff','spec_Ever_SA_Pt_On_LARC_F','spec_SA_Ever_Pt_F'))
  
    new_data_2$dt_lbl<- new_data_2$spec_SA_Ever_Pt_F
  
    new_sub_data <- subset(new_data_2, variable == "spec_SA_Ever_Pt_F")
    option_text2 <- c('Identified','De-Identified')
    list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
    
    if(list_identification=='Identified'){
      ggplot(data = new_data_1, aes(x=reorder(spec_site_name,value), value, fill=variable,label=value))+
        geom_col(width=.5) + 
        labs(fill="",y="",x="",
             title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
             )+ theme_classic()+
        annotate("text", x=new_sub_data$spec_site_name, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=3)+
        
        geom_text(position=position_stack(vjust=.5),size=3,color="black")+
        scale_fill_manual(values=c("#ccc1d4","#d7e4bd","#e6b9b8"),
                          labels=c("Most Effective (IUD or Implant)   ",
                                   "Moderately Effective (OCP,Shot,Ring,Patch)   ","SA Not Using Most or Moderately Effective")
                          ,breaks=c("spec_Ever_SA_Pt_On_LARC_F","spec_Ever_SA_Pt_On_ModEff","spec_ever_not_using"))+
        scale_y_continuous(limits=c(0,350),breaks=seq(0,350, by=50))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(1, 'lines'))+
        geom_abline(aes(slope=0, intercept=0))+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_colour_manual(values=c("black"))
      
    } else if(list_identification=='De-Identified'){
      ggplot(data = new_data_1, aes(x=reorder(SiteName_OutreachCharts_Current,value), value, fill=variable,label=value))+ geom_col(width=.5) + 
        labs(fill="",y="",x="",
             title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
             )+ theme_classic()+ annotate("text", x=new_sub_data$SiteName_OutreachCharts_Current, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.8,size=3)+
        
        geom_text(position=position_stack(vjust=.5),size=3,color="black")+
        scale_fill_manual(values=c("#ccc1d4","#d7e4bd","#e6b9b8"),
                          labels=c("Most Effective (IUD or Implant)   ",
                                   "Moderately Effective (OCP,Shot,Ring,Patch)   ","SA Not Using Most or Moderately Effective")
                          ,breaks=c("Ever_SA_Pt_On_LARC_F","Ever_SA_Pt_On_ModEff","ever_not_using"))+
        scale_y_continuous(limits=c(0,350),breaks=seq(0,350, by=50))+
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(1, 'lines'))+
        geom_abline(aes(slope=0, intercept=0))+
        scale_colour_manual(values=c("black"))+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        theme(axis.text.x = element_blank())
    } 
  }else if(list_Sponsor_Level_Options == "Specific Sponsor (HS) Highlighted"){
    #stores specific sponsors for the selected site type
    spec_Sponsor_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type &
                                                                                data_set$data_src %in% list_data_src & data_set$Enroll_Campus_Pop_9to12 !=0)]
                                                                        
    
    #stores unique instances of the specific sponsor name
    uniq_spec_Sponsor <- unique(spec_Sponsor_uniq_Site_Type)
    
    #lets user choose a specific sponsor from a list of unique sponsors names
    list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor
                                       ,multiple = TRUE, title = "Choose the sponsor/sponsors for the chart"))
    
    spec_site_name <-  data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_ever_not_using <- data_set$ever_not_using[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_Ever_SA_Pt_On_ModEff <- data_set$Ever_SA_Pt_On_ModEff[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_Ever_SA_Pt_On_LARC_F <- data_set$Ever_SA_Pt_On_LARC_F[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_SA_Ever_Pt_F <- data_set$SA_Ever_Pt_F[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_hs_popn <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    
    new_data_set_1 <- data.frame(spec_site_name, spec_ever_not_using, spec_Ever_SA_Pt_On_ModEff,
                               spec_Ever_SA_Pt_On_LARC_F, spec_SA_Ever_Pt_F,spec_hs_popn)
    
    new_data_1<-melt(as.data.frame(new_data_set_1), id.vars=c('spec_site_name','spec_hs_popn'),
                     measure.vars=c('spec_ever_not_using','spec_Ever_SA_Pt_On_ModEff','spec_Ever_SA_Pt_On_LARC_F'))
    
 
    
    
    sitename_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which 
                                                                           (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    ever_not_using_all_sponsor <- data_set$ever_not_using[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    Ever_SA_Pt_On_ModEff_all_sponsor <- data_set$Ever_SA_Pt_On_ModEff[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    Ever_SA_Pt_On_LARC_F_all_sponsor <- data_set$Ever_SA_Pt_On_LARC_F[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    SA_Ever_Pt_F_all_sponsor <- data_set$SA_Ever_Pt_F[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    hs_popn_all_sponsor <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    using_pct_all_sponsor <- data_set$using_pct[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
    new_data_set_2 <- data.frame(sitename_all_sponsor, ever_not_using_all_sponsor,Ever_SA_Pt_On_ModEff_all_sponsor,
                               Ever_SA_Pt_On_LARC_F_all_sponsor, SA_Ever_Pt_F_all_sponsor, hs_popn_all_sponsor,using_pct_all_sponsor)
    new_data_2 <- melt(as.data.frame(new_data_set_2), id.vars=c('sitename_all_sponsor','hs_popn_all_sponsor'),
                     measure.vars=c('ever_not_using_all_sponsor','Ever_SA_Pt_On_ModEff_all_sponsor','Ever_SA_Pt_On_LARC_F_all_sponsor'))
    
    new_data_3 <- melt(as.data.frame(new_data_set_2), id.vars=c('sitename_all_sponsor','hs_popn_all_sponsor','SA_Ever_Pt_F_all_sponsor','using_pct_all_sponsor'),
                     measure.vars=c('ever_not_using_all_sponsor','Ever_SA_Pt_On_ModEff_all_sponsor','Ever_SA_Pt_On_LARC_F_all_sponsor','SA_Ever_Pt_F_all_sponsor'))
    new_data_2 <- na.omit(new_data_2)
    new_data_3 <- na.omit(new_data_3)
    
    new_data_1_hs <-subset(new_data_1, spec_hs_popn!=0)
    #nsd1<-subset(nod1, value!=0)
    
    new_data_2_hs <- subset(new_data_2, hs_popn_all_sponsor!=0)
    #nsd2<-subset(nod2, value!=0)
    
    new_data_3_hs <- subset(new_data_3, hs_popn_all_sponsor!=0)
    
    
    new_data_3_hs$dt_lbl<- paste(new_data_3_hs$SA_Ever_Pt_F_all_sponsor,";\n",new_data_3_hs$using_pct_all_sponsor,"%")
    new_sub_data <- subset(new_data_3_hs, variable == "SA_Ever_Pt_F_all_sponsor")
    
    
    option_text2 <- c('Identified','De-Identified')
    list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
    
    if(list_identification == "Identified"){
      ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value), value,
                                       fill=variable, label=value))+
        geom_col(width=.5) + 
        labs(fill="",y="",x="",
             title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
             )+ theme_classic()+
        annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.5,size=1.5)+
        
        geom_text(position=position_stack(vjust=.5),color="black", size = 1.5)+
        
        geom_col(data = new_data_1_hs, aes(x=reorder(spec_site_name,value), value,
                                           fill=variable),width=.5)+
        geom_text(data=new_data_1_hs, aes(x=reorder(spec_site_name,value),y= value, label = value),
                  position=position_stack(vjust=.5),color="white", size = 1.5)+
        scale_fill_manual(values=c("#ccc1d4","#d7e4bd","#e6b9b8","#633287","#5e7530","#d65553"),
                          labels=c("Most Effective (IUD or Implant)   ",
                                   "Moderately Effective (OCP,Shot,Ring,Patch)   ","SA Not Using Most or Moderately Effective")
                          ,breaks=c("spec_Ever_SA_Pt_On_LARC_F","spec_Ever_SA_Pt_On_ModEff","spec_ever_not_using"))+
        #scale_y_continuous(limits=c(0,350),breaks=seq(0,350, by=50))+
        
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(1, 'lines'))+
        geom_abline(aes(slope=0, intercept=0))+
        scale_colour_manual(values=c("black"))+
        
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    } else if(list_identification == "De-Identified"){
      ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value), value,
                                       fill=variable, label=value))+
        geom_col(width=.5) + 
        labs(fill="",y="",x="",
             title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
        )+ theme_classic()+
        annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.5,size=1.5)+
        
        geom_text(position=position_stack(vjust=.5),color="black", size = 1.5)+
        
        geom_col(data = new_data_1_hs, aes(x=reorder(spec_site_name,value), value,
                                           fill=variable),width=.5)+
        geom_text(data=new_data_1_hs, aes(x=reorder(spec_site_name,value),y= value, label = value),
                  position=position_stack(vjust=.5),color="white", size = 1.5)+
        scale_fill_manual(values=c("#ccc1d4","#d7e4bd","#e6b9b8","#633287","#5e7530","#d65553"),
                          labels=c("Most Effective (IUD or Implant)   ",
                                   "Moderately Effective (OCP,Shot,Ring,Patch)   ","SA Not Using Most or Moderately Effective")
                          ,breaks=c("spec_Ever_SA_Pt_On_LARC_F","spec_Ever_SA_Pt_On_ModEff","spec_ever_not_using"))+
        #scale_y_continuous(limits=c(0,350),breaks=seq(0,350, by=50))+
        
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(1, 'lines'))+
        geom_abline(aes(slope=0, intercept=0))+
        scale_colour_manual(values=c("black"))+
        
        theme(axis.text.x = element_blank())+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    }
  }else if(list_Sponsor_Level_Options == "Specific Sponsor (Non-HS) Highlighted"){
    #stores specific sponsors for the selected site type
    spec_Sponsor_uniq_Site_Type <- data_set$Sponsor_OutreachCharts_Name[which(data_set$SITE_TYPE %in% list_uniq_Site_Type &
                                                                                data_set$data_src %in% list_data_src & data_set$Enroll_Campus_Pop_9to12 == 0)]
    
    
    #stores unique instances of the specific sponsor name
    uniq_spec_Sponsor <- unique(spec_Sponsor_uniq_Site_Type)
    
    #lets user choose a specific sponsor from a list of unique sponsors names
    list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor
                                       ,multiple = TRUE, title = "Choose the sponsor/sponsors for the chart"))
    
    spec_site_name <-  data_set$SiteName_OutreachCharts_Current[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_ever_not_using <- data_set$ever_not_using[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_Ever_SA_Pt_On_ModEff <- data_set$Ever_SA_Pt_On_ModEff[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_Ever_SA_Pt_On_LARC_F <- data_set$Ever_SA_Pt_On_LARC_F[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_SA_Ever_Pt_F <- data_set$SA_Ever_Pt_F[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    spec_hs_popn <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
    
    new_data_set_1 <- data.frame(spec_site_name, spec_ever_not_using, spec_Ever_SA_Pt_On_ModEff,
                                 spec_Ever_SA_Pt_On_LARC_F, spec_SA_Ever_Pt_F,spec_hs_popn)
    
    new_data_1<-melt(as.data.frame(new_data_set_1), id.vars=c('spec_site_name','spec_hs_popn'),
                     measure.vars=c('spec_ever_not_using','spec_Ever_SA_Pt_On_ModEff','spec_Ever_SA_Pt_On_LARC_F'))
    
    
    
    
    sitename_all_sponsor <- data_set$SiteName_OutreachCharts_Current[which 
                                                                           (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    ever_not_using_all_sponsor <- data_set$ever_not_using[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    Ever_SA_Pt_On_ModEff_all_sponsor <- data_set$Ever_SA_Pt_On_ModEff[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    Ever_SA_Pt_On_LARC_F_all_sponsor <- data_set$Ever_SA_Pt_On_LARC_F[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    SA_Ever_Pt_F_all_sponsor <- data_set$SA_Ever_Pt_F[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    hs_popn_all_sponsor <- data_set$Enroll_Campus_Pop_9to12[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    using_pct_all_sponsor <- data_set$using_pct[which (data_set$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
    
    new_data_set_2 <- data.frame(sitename_all_sponsor, ever_not_using_all_sponsor,Ever_SA_Pt_On_ModEff_all_sponsor,
                                 Ever_SA_Pt_On_LARC_F_all_sponsor, SA_Ever_Pt_F_all_sponsor, hs_popn_all_sponsor,using_pct_all_sponsor)
    new_data_2 <- melt(as.data.frame(new_data_set_2), id.vars=c('sitename_all_sponsor','hs_popn_all_sponsor'),
                       measure.vars=c('ever_not_using_all_sponsor','Ever_SA_Pt_On_ModEff_all_sponsor','Ever_SA_Pt_On_LARC_F_all_sponsor'))
    
    new_data_3 <- melt(as.data.frame(new_data_set_2), id.vars=c('sitename_all_sponsor','hs_popn_all_sponsor','SA_Ever_Pt_F_all_sponsor','using_pct_all_sponsor'),
                       measure.vars=c('ever_not_using_all_sponsor','Ever_SA_Pt_On_ModEff_all_sponsor','Ever_SA_Pt_On_LARC_F_all_sponsor','SA_Ever_Pt_F_all_sponsor'))
    new_data_2 <- na.omit(new_data_2)
    new_data_3 <- na.omit(new_data_3)
    
    new_data_1_hs <-subset(new_data_1, spec_hs_popn == 0)
    #nsd1<-subset(nod1, value!=0)
    
    new_data_2_hs <- subset(new_data_2, hs_popn_all_sponsor == 0)
    #nsd2<-subset(nod2, value!=0)
    
    new_data_3_hs <- subset(new_data_3, hs_popn_all_sponsor == 0)
    
    
    new_data_3_hs$dt_lbl<- paste(new_data_3_hs$SA_Ever_Pt_F_all_sponsor,";\n",new_data_3_hs$using_pct_all_sponsor,"%")
    new_sub_data <- subset(new_data_3_hs, variable == "SA_Ever_Pt_F_all_sponsor")
    
    
    option_text2 <- c('Identified','De-Identified')
    list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
    
    if(list_identification == "Identified"){
      ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value), value,
                                       fill=variable, label=value))+
        geom_col(width=.5) + 
        labs(fill="",y="",x="",
             title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
        )+ theme_classic()+
        annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.5,size=1.5)+
        
        geom_text(position=position_stack(vjust=.5),color="black", size = 1.5)+
        
        geom_col(data = new_data_1_hs, aes(x=reorder(spec_site_name,value), value,
                                           fill=variable),width=.5)+
        geom_text(data=new_data_1_hs, aes(x=reorder(spec_site_name,value),y= value, label = value),
                  position=position_stack(vjust=.5),color="white", size = 1.5)+
        scale_fill_manual(values=c("#ccc1d4","#d7e4bd","#e6b9b8","#633287","#5e7530","#d65553"),
                          labels=c("Most Effective (IUD or Implant)   ",
                                   "Moderately Effective (OCP,Shot,Ring,Patch)   ","SA Not Using Most or Moderately Effective")
                          ,breaks=c("spec_Ever_SA_Pt_On_LARC_F","spec_Ever_SA_Pt_On_ModEff","spec_ever_not_using"))+
        #scale_y_continuous(limits=c(0,350),breaks=seq(0,350, by=50))+
        
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(1, 'lines'))+
        geom_abline(aes(slope=0, intercept=0))+
        scale_colour_manual(values=c("black"))+
        
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    } else if(list_identification == "De-Identified"){
      ggplot(data = new_data_2_hs, aes(x=reorder(sitename_all_sponsor,value), value,
                                       fill=variable, label=value))+
        geom_col(width=.5) + 
        labs(fill="",y="",x="",
             title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles)
        )+ theme_classic()+
        annotate("text", x=new_sub_data$sitename_all_sponsor, y=new_sub_data$value, label= new_sub_data$dt_lbl,vjust=-.5,size=1.5)+
        
        geom_text(position=position_stack(vjust=.5),color="black", size = 1.5)+
        
        geom_col(data = new_data_1_hs, aes(x=reorder(spec_site_name,value), value,
                                           fill=variable),width=.5)+
        geom_text(data=new_data_1_hs, aes(x=reorder(spec_site_name,value),y= value, label = value),
                  position=position_stack(vjust=.5),color="white", size = 1.5)+
        scale_fill_manual(values=c("#ccc1d4","#d7e4bd","#e6b9b8","#633287","#5e7530","#d65553"),
                          labels=c("Most Effective (IUD or Implant)   ",
                                   "Moderately Effective (OCP,Shot,Ring,Patch)   ","SA Not Using Most or Moderately Effective")
                          ,breaks=c("spec_Ever_SA_Pt_On_LARC_F","spec_Ever_SA_Pt_On_ModEff","spec_ever_not_using"))+
        #scale_y_continuous(limits=c(0,350),breaks=seq(0,350, by=50))+
        
        theme(legend.direction = 'horizontal', 
              legend.position = 'top',
              legend.key = element_rect(size = .2),
              legend.key.size = unit(1, 'lines'))+
        geom_abline(aes(slope=0, intercept=0))+
        scale_colour_manual(values=c("black"))+
        
        theme(axis.text.x = element_blank())+
        theme(plot.title = element_text(hjust = 0.5,size=20))+
        scale_x_discrete(breaks = new_data_1_hs$spec_site_name)
    }
  }
}