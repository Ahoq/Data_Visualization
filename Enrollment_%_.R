
# !diagnostics off
create_Enrollment_Charts_et<-function(myfile1,myfile2){
  
  #imports the excel(QA or RH) file
  data <- read_excel(myfile1)
  
  #imports the constant file
  const_data <- read_excel(myfile2) 
  
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
    data <- subset(data, data$Enroll_Campus_Pop_9to12 !=0)
  } else if(list_School_Level_Options == 'Non High School Only'){
    data <- subset(data, data$Enroll_Campus_Pop_9to12 ==0)
  } else if(list_School_Level_Options == 'Both HS and Non-HS'){
    data <- data
  } 
  
  data$data_src <- data$DataPullCharts_Name
  
  data_src_rf <- unique(data$data_src)
  
  
  #lets user choose one data source from a list of unique instances of data_pull_name
  list_data_src <- c(select.list(data_src_rf,
                                 title ="Choose the Data Source ",
                                 multiple=FALSE))
  
  #stores unique site types based on the chosen data source
  uniq_Site_Type <- unique(data$SITE_TYPE
                           [which(data$data_src %in% list_data_src)])
  
  #lets user choose one site type from a list of unique site types
  list_uniq_Site_Type <- c(select.list(uniq_Site_Type
                                       ,multiple = FALSE, 
                                       title = "Choose the specific Site Type"))
  
  option_text1 <- c("Specific Sponsor","Specific Sponsor (HS) Highlighted",
                    "Specific Sponsor (Non-HS) Highlighted")
    
  #lets user choose an option specified in option_text1
  list_Sponsor_Level_Options <- c(select.list(option_text1,         
                      title ="Choose the stratification for the chart based on Sponsor Levels ",
                      multiple=FALSE))
    
    if(list_Sponsor_Level_Options == 'Specific Sponsor'){
      
      #stores specific sponsors for the selected site type
      spec_Sponsor_uniq_Site_Type <- data$Sponsor_OutreachCharts_Name[which(data$SITE_TYPE %in% list_uniq_Site_Type 
                                                                                & data$data_src %in% list_data_src)]
      #stores unique instances of the specific sponsor name
      uniq_spec_Sponsor <- unique(spec_Sponsor_uniq_Site_Type)
      
      #lets user choose a specific sponsor from a list of unique sponsors names
      list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor
                        ,multiple = FALSE, title = "Choose the specific Sponsor for the chart"))
      
      #stores specific site name based on the selected unique sponsor
      spec_site_name <- data$SiteName_OutreachCharts_Current[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      #stores clinic enrollment percentage based on the selected sponsor
      spec_clinic_en_pcnt <- data$Enroll_Clinic_Enroll_Pcnt[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      #creates a new data frame with spec_site_name and spec_clinic_en_pcnt as the fields
      new_data <-data.frame(spec_site_name, spec_clinic_en_pcnt)
      
      option_text2<-c('Identified','De-Identified')
      #lets user choose an option specified in option_text2 
      list_identification <- c(select.list(option_text2, title ="Please choose how you want your charts "))
      
      if(list_identification == "Identified"){
        
        #plots using ggplot for an Identified chart
         chart <- ggplot(new_data, aes(reorder(spec_site_name,spec_clinic_en_pcnt/100,FUN = sum),
                                  spec_clinic_en_pcnt/100,label=paste(spec_clinic_en_pcnt,"%")))+
          #sets the color and width of the bar
          geom_col(fill="#b6ccef",width=.7)+
          #inserts blank for the x and y axis titles and displays the actual title for the chart
          labs(y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          #sets the theme of the chart to classic (no gridlines or background color)
          theme_classic()+
          #puts the data label for the bars
          geom_text(position=position_stack(vjust=1.03), colour="black") +
          #the line, paralled to the x axis, which correspons to the enrollment benchmark data
          geom_abline(aes(slope=0, intercept = const_data$Placeholder_Enrollment_Chart,
                          col = "Enrollment Benchmark"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0,col = "%Enrollment    "))+
          labs(colour="") +
          #sets the color for the lines
          scale_colour_manual(values=c("#b6ccef",'#f9ea0c'))+
          #sets the scale for the chart
          scale_y_continuous(labels=percent,limits=c(0,1.2),breaks=seq(0.0,1.2, by=.1))+
          #sets the position, direction and size for the legends
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          #sets the position and angle of the x-axis labels
          theme(axis.text.x = element_text(angle = 60, hjust = 1))+
          #sets the position, size, and color for the chart
          theme(plot.title = element_text(hjust = 0.5,size=25,color = "#203960"))
        
      } else if(list_identification == "De-Identified"){
        #plots using ggplot for a De-Identified chart
        chart <- ggplot(new_data, aes(reorder(spec_site_name,spec_clinic_en_pcnt/100,FUN = sum),
                                      spec_clinic_en_pcnt/100,label=paste(spec_clinic_en_pcnt,"%")))+
          #sets the color and width of the bar
          geom_col(fill="#b6ccef",width=.7)+
          #inserts blank for the x and y axis titles and displays the actual title for the chart
          labs(y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          #sets the theme of the chart to classic (no gridlines or background color)
          theme_classic()+
          #puts the data label for the bars
          geom_text(position=position_stack(vjust=1.03), colour="black") +
          #the line, paralled to the x axis, which correspons to the enrollment benchmark data
          geom_abline(aes(slope=0, intercept = const_data$Placeholder_Enrollment_Chart,
                          col = "Enrollment Benchmark"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0,col = "%Enrollment    "))+
          labs(colour="") +
          #sets the color for the lines
          scale_colour_manual(values=c("#b6ccef",'#f9ea0c'))+
          #sets the scale for the chart
          scale_y_continuous(labels=percent,limits=c(0,1.2),breaks=seq(0.0,1.2, by=.1))+
          #sets the position, direction and size for the legends
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.7, 'lines'))+
          #removes the text of the x-axis labels and de-identifies the chart
          theme(axis.text.x = element_blank())+
          #sets the position, size, and color for the chart
          theme(plot.title = element_text(hjust = 0.5,size=25,color = "#203960"))
        
      } 
      
    } else if (list_Sponsor_Level_Options == "Specific Sponsor (HS) Highlighted"){
     
      #stores specific sponsors for the selected site type (high school only)
      spec_Sponsors_uniq_Site_Type <- data$Sponsor_OutreachCharts_Name[which(data$SITE_TYPE %in% list_uniq_Site_Type & data$Enroll_Campus_Pop_9to12 !=0
                                                                                 & data$data_src %in% list_data_src)]
      #stores unique instances of the specific sponsor names
      uniq_spec_Sponsor <- unique(spec_Sponsors_uniq_Site_Type)
      #lets user choose a specific sponsor from a list of unique sponsors names
      list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor,
                        title ="Choose specific Sponsor to highlight for your charts ",
                        multiple=FALSE))
      #stores sitename of all the specific sponsors
      sitename_spec_Sponsor <- data$SiteName_OutreachCharts_Current[which (data$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      #stores clinic enrollment pct of all the specific sponsors
      clinic_en_pcnt_spec_Sponsor <- data$Enroll_Clinic_Enroll_Pcnt[which (data$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      #stores high school campus population of all the specific sponsors
      high_school_spec_Sponsor <- data$Enroll_Campus_Pop_9to12[which (data$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      #stores sitename of the selected sponsors
      spec_site_name <- data$SiteName_OutreachCharts_Current[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      #stores clinic enrollment pct of the selected sponsors
      spec_clinic_en_pcnt <- data$Enroll_Clinic_Enroll_Pcnt[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      #stores high school campus population of the selected sponsors
      spec_high_school <- data$Enroll_Campus_Pop_9to12[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      #creates a data frame with sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor,high_school_spec_Sponsor as the fields
      newdata_1 <- data.frame(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor,high_school_spec_Sponsor)
      
      #removes all the NA values from the dataframe
      newdata_1 <- na.omit(newdata_1)
      
      #creates a data frame with spec_site_name,spec_clinic_en_pcnt,spec_high_school as the fields
      new_data_2 <- data.frame(spec_site_name,spec_clinic_en_pcnt,spec_high_school)
      
      #makes a subset of newdata_1 where only the field that have high school data are stored
      newdata_1_hs <-subset(newdata_1, high_school_spec_Sponsor!=0)
      #nsd<-subset(nod, b1n!=0)
      
      #makes a subset of newdata_2 where only the field that have high school data are stored
      newdata_2_hs <- subset(new_data_2, spec_high_school!=0)
      #nsd1<-subset(nod1, b2!=0)
      
      option_text2 <- c("Identified","De-Identified")
      #lets user choose an option specified in option_text2
      list_identification <- c(select.list(option_text2, title="Please Choose How You Want Your Charts: "))
      
      if (list_identification == "Identified"){
        
        chart <- ggplot()+
          #plots the bars of all the high school sites of all the sponsors
          geom_col(data=newdata_1_hs,aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                y=clinic_en_pcnt_spec_Sponsor/100),fill="#b6ccef",width=.7)+
          #sets x and y-axis labels to blank and also displays the chart title
          labs(fill="",y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          #sets the theme of the chart to classic (no gridlines or background color)
          theme_classic()+
          #displays the data label for the bars
          geom_text(data=newdata_1_hs, aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                           y=clinic_en_pcnt_spec_Sponsor/100,label=paste(clinic_en_pcnt_spec_Sponsor,"%")),
                    position=position_stack(vjust=1.02), colour="black",size=1.6) +
          #the line, paralled to the x axis, which correspons to the enrollment benchmark data
          geom_abline(aes(slope=0, intercept = const_data$Placeholder_Enrollment_Chart,
                          col = "Enrollment Benchmark"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0,col = "%Enrollment    "))+
          labs(colour="") +
          #sets the color for the lines
          scale_colour_manual(values=c("#597eba",'#f9ea0c'))+
          #sets the scale for the chart
          scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0.0,1, by=.1))+
          #sets the position, direction and size for the legends
          theme(legend.position="top")+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.5, 'lines'))+
          #sets the position and angle of the x-axis labels
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          
          #plots the bars of all the high school sites of only the selected the sponsor
          geom_col(data=newdata_2_hs, aes(reorder(spec_site_name, spec_clinic_en_pcnt/100,FUN = sum),
                                  y=spec_clinic_en_pcnt/100,fill=""),width=.7)+
          
          #displays the x-axis tick labels of only the sites of the selected sponsor
          scale_x_discrete(breaks = reorder(newdata_2_hs$spec_site_name, newdata_2_hs$spec_clinic_en_pcnt/100,FUN = sum))+
          #sets the color of the bars of the sites of the selected sponsor
          scale_fill_manual(values = c("#597eba"),guide="none")+
          #sets the position, size, and color for the chart-title
          theme(plot.title = element_text(hjust = 0.5,size=25,color = "#203960"))
        
        
      } else if(list_identification == "De-Identified"){
        
        chart <- ggplot()+
          #plots the bars of all the high school sites of all the sponsors
          geom_col(data=newdata_1_hs,aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                         y=clinic_en_pcnt_spec_Sponsor/100),fill="#b6ccef",width=.7)+
          #sets x and y-axis labels to blank and also displays the chart title
          labs(fill="",y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          #sets the theme of the chart to classic (no gridlines or background color)
          theme_classic()+
          #displays the data label for the bars
          geom_text(data=newdata_1_hs, aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                           y=clinic_en_pcnt_spec_Sponsor/100,label=paste(clinic_en_pcnt_spec_Sponsor,"%")),
                    position=position_stack(vjust=1.02), colour="black",size=1.6) +
          #the line, paralled to the x axis, which corresponds to the enrollment benchmark data
          geom_abline(aes(slope=0, intercept = const_data$Placeholder_Enrollment_Chart,
                          col = "Enrollment Benchmark"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0,col = "%Enrollment    "))+
          labs(colour="") +
          #sets the color for the lines
          scale_colour_manual(values=c("#597eba",'#f9ea0c'))+
          #sets the scale for the chart
          scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0.0,1, by=.1))+
          #sets the position, direction and size for the legends
          theme(legend.position="top")+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.5, 'lines'))+
          #removes the text of the x-axis labels and de-identifies the chart
          theme(axis.text.x = element_blank())+
          
          #plots the bars of all the high school sites of only the selected the sponsor
          geom_col(data=newdata_2_hs, aes(reorder(spec_site_name, spec_clinic_en_pcnt/100,FUN = sum),
                                          y=spec_clinic_en_pcnt/100,fill=""),width=.7)+
          
          #displays the x-axis tick labels of only the sites of the selected sponsor
          scale_x_discrete(breaks = reorder(newdata_2_hs$spec_site_name, newdata_2_hs$spec_clinic_en_pcnt/100,FUN = sum))+
          #sets the color of the bars of the sites of the selected sponsor
          scale_fill_manual(values = c("#597eba"),guide="none")+
          #sets the position, size, and color for the chart
          theme(plot.title = element_text(hjust = 0.5,size=25,color = "#203960"))
      }
      
      
    } else if ( list_Sponsor_Level_Options == "Specific Sponsor (Non-HS) Highlighted"){
      
      #stores specific sponsors for the selected site type (non-high school only)
      spec_Sponsors_uniq_Site_Type <- data$Sponsor_OutreachCharts_Name[which(data$SITE_TYPE %in% list_uniq_Site_Type & data$Enroll_Campus_Pop_9to12 ==0
                                                                                 & data$data_src %in% list_data_src)]
      #stores unique instances of the specific sponsor names
      uniq_spec_Sponsor <- unique(spec_Sponsors_uniq_Site_Type)
      #lets user choose a specific sponsor from a list of unique sponsors names
      list_uniq_Sponsor <- c(select.list(uniq_spec_Sponsor,
                                         title ="Choose specific Sponsor to highlight for your charts ",
                                         multiple=FALSE))
      #stores sitename of all the specific sponsors
      sitename_spec_Sponsor <- data$SiteName_OutreachCharts_Current[which (data$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      #stores clinic enrollment pct of all the specific sponsors
      clinic_en_pcnt_spec_Sponsor <- data$Enroll_Clinic_Enroll_Pcnt[which (data$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      #stores high school campus population of all the specific sponsors
      high_school_spec_Sponsor <- data$Enroll_Campus_Pop_9to12[which (data$Sponsor_OutreachCharts_Name %in% uniq_spec_Sponsor)]
      
      #stores sitename of the selected sponsors
      spec_site_name <- data$SiteName_OutreachCharts_Current[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      #stores clinic enrollment pct of the selected sponsors
      spec_clinic_en_pcnt <- data$Enroll_Clinic_Enroll_Pcnt[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      #stores high school campus population of the selected sponsors
      spec_high_school <- data$Enroll_Campus_Pop_9to12[which (data$Sponsor_OutreachCharts_Name %in% list_uniq_Sponsor)]
      
      #creates a data frame with sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor,high_school_spec_Sponsor as the fields
      newdata_1 <- data.frame(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor,high_school_spec_Sponsor)
      
      #removes all the NA values from the dataframe
      newdata_1 <- na.omit(newdata_1)
      
      #creates a data frame with spec_site_name,spec_clinic_en_pcnt,spec_high_school as the fields
      new_data_2 <- data.frame(spec_site_name,spec_clinic_en_pcnt,spec_high_school)
      
      #makes a subset of newdata_1 where only the field that have non-high school data are stored
      newdata_1_hs <-subset(newdata_1, high_school_spec_Sponsor==0)
      #nsd<-subset(nod, b1n!=0)
      
      #makes a subset of newdata_2 where only the field that have non-high school data are stored
      newdata_2_hs <- subset(new_data_2, spec_high_school==0)
      #nsd1<-subset(nod1, b2!=0)
      
      option_text2 <- c("Identified","De-Identified")
      #lets user choose an option specified in option_text2
      list_identification <- c(select.list(option_text2, title="Please Choose How You Want Your Charts: "))
      
      if (list_identification == "Identified"){
        
        chart <- ggplot()+
          #plots the bars of all the high school sites of all the sponsors
          geom_col(data=newdata_1_hs,aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                         y=clinic_en_pcnt_spec_Sponsor/100),fill="#b6ccef",width=.7)+
          #sets x and y-axis labels to blank and also displays the chart title
          labs(fill="",y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          #sets the theme of the chart to classic (no gridlines or background color)
          theme_classic()+
          #displays the data label for the bars
          geom_text(data=newdata_1_hs, aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                           y=clinic_en_pcnt_spec_Sponsor/100,label=paste(clinic_en_pcnt_spec_Sponsor,"%")),
                    position=position_stack(vjust=1.02), colour="black",size=1.6) +
          #the line, paralled to the x axis, which correspons to the enrollment benchmark data
          geom_abline(aes(slope=0, intercept = const_data$Placeholder_Enrollment_Chart,
                          col = "Enrollment Benchmark"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0,col = "%Enrollment    "))+
          labs(colour="") +
          #sets the color for the lines
          scale_colour_manual(values=c("#597eba",'#f9ea0c'))+
          #sets the scale for the chart
          scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0.0,1, by=.1))+
          #sets the position, direction and size for the legends
          theme(legend.position="top")+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.5, 'lines'))+
          #sets the position and angle of the x-axis labels
          theme(axis.text.x = element_text(angle = 70, hjust = 1))+
          
          #plots the bars of all the high school sites of only the selected the sponsor
          geom_col(data=newdata_2_hs, aes(reorder(spec_site_name, spec_clinic_en_pcnt/100,FUN = sum),
                                          y=spec_clinic_en_pcnt/100,fill=""),width=.7)+
          
          #displays the x-axis tick labels of only the sites of the selected sponsor
          scale_x_discrete(breaks = reorder(newdata_2_hs$spec_site_name, newdata_2_hs$spec_clinic_en_pcnt/100,FUN = sum))+
          #sets the color of the bars of the sites of the selected sponsor
          scale_fill_manual(values = c("#597eba"),guide="none")+
          #sets the position, size, and color for the chart
          theme(plot.title = element_text(hjust = 0.5,size=25,color = "#203960"))
        
        
      } else if(list_identification == "De-Identified"){
        
        chart <- ggplot()+
          #plots the bars of all the high school sites of all the sponsors
          geom_col(data=newdata_1_hs,aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                         y=clinic_en_pcnt_spec_Sponsor/100),fill="#b6ccef",width=.7)+
          #sets x and y-axis labels to blank and also displays the chart title
          labs(fill="",y="",x="", title=paste("\nNYC ",list_uniq_Sponsor,": ",titles,"\n",subtitles))+
          #sets the theme of the chart to classic (no gridlines or background color)
          theme_classic()+
          #displays the data label for the bars
          geom_text(data=newdata_1_hs, aes(x=reorder(sitename_spec_Sponsor,clinic_en_pcnt_spec_Sponsor/100,FUN = sum),
                                           y=clinic_en_pcnt_spec_Sponsor/100,label=paste(clinic_en_pcnt_spec_Sponsor,"%")),
                    position=position_stack(vjust=1.02), colour="black",size=1.6) +
          #the line, paralled to the x axis, which correspons to the enrollment benchmark data
          geom_abline(aes(slope=0, intercept = const_data$Placeholder_Enrollment_Chart,
                          col = "Enrollment Benchmark"),size=1,alpha=.7)+
          geom_abline(aes(slope=0, intercept=0,col = "%Enrollment    "))+
          labs(colour="") +
          #sets the color for the lines
          scale_colour_manual(values=c("#597eba",'#f9ea0c'))+
          #sets the scale for the chart
          scale_y_continuous(labels=percent,limits=c(0,1),breaks=seq(0.0,1, by=.1))+
          #sets the position, direction and size for the legends
          theme(legend.position="top")+
          theme(legend.direction = 'horizontal', 
                legend.position = 'top',
                legend.key = element_rect(size = .2),
                legend.key.size = unit(.5, 'lines'))+
          #removes the text of the x-axis labels and de-identifies the chart
          theme(axis.text.x = element_blank())+
          
          #plots the bars of all the high school sites of only the selected the sponsor
          geom_col(data=newdata_2_hs, aes(reorder(spec_site_name, spec_clinic_en_pcnt/100,FUN = sum),
                                          y=spec_clinic_en_pcnt/100,fill=""),width=.7)+
          
          #displays the x-axis tick labels of only the sites of the selected sponsor
          scale_x_discrete(breaks = reorder(newdata_2_hs$spec_site_name, newdata_2_hs$spec_clinic_en_pcnt/100,FUN = sum))+
          #sets the color of the bars of the sites of the selected sponsor
          scale_fill_manual(values = c("#597eba"),guide="none")+
          #sets the position, size, and color for the chart
          theme(plot.title = element_text(hjust = 0.5,size=25,color = "#203960"))
      }
      
      
    }
    
  print(chart)
}

