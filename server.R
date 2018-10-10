shinyServer(function(input, output) {
  output$outputPlot <- renderPlot({
    year<- input$current_year 
    country<- input$country
    education<- input$education
    gender<- input$gender
    index<- input$index 
    info_extra <- input$info_extra
    
    if(index==1){
      index_name ="Completion Rate"
    }else{
      index_name ="Net Enrollment Rate"
    }
    
    if(gender==1){ # female
      if(index==1){ # completion rate
        if(education==1){ # primary
          data_to_use<- female_PrimCompletionRate
          title<-"Primary completion rate for females"
        }else{ # then we know its secondary
          data_to_use<- female_SecCompletionRate
          title<-"Secondary completion rate for females"
        }
      }else{ # enrollment rate
        if(education==1){ # primary
          data_to_use<- female_PrimNetEnrollment
          title<-"Primary net enrollment rate for females"
        }else{ # then we know its secondary
          data_to_use<- female_SecNetEnrollment
          title<-"Secondary net enrollment rate for females"
        }
      }
    }else if(gender==2){ # male
      if(index==1){ # completion rate
        if(education==1){ # primary
          data_to_use<- male_PrimCompletionRate
          title<-"Primary completion rate for males"
        }else{ # then we know its secondary
          data_to_use<- male_SecCompletionRate
          title<-"Secondary completion rate for males"
        }
      }else{ # enrollment rate
        if(education==1){ # primary
          data_to_use<- male_PrimNetEnrollment
          title<-"Primary net enrollment rate for males"
        }else{ # then we know its secondary
          data_to_use<- male_SecNetEnrollment
          title<-"Secondary net enrollment rate for males"
        }
      }
    }else if(gender==3){ # combined
      if(index==1){ # completion rate
        if(education==1){ # primary
          data_to_use<- both_genders_PrimCompletionRate
          title<-"Primary completion rate for both genders"
        }else{ # then we know its secondary
          data_to_use<- both_genders_SecCompletionRate
          title<-"Secondary completion rate for both genders"
        }
      }else{ # enrollment rate
        if(education==1){ # primary
          data_to_use<- both_genders_PrimNetEnrollment
          title<-"Primary net enrollment rate for both genders"
        }else{ # then we know its secondary
          data_to_use<- both_genders_SecNetEnrollment
          title<-"Secondary net enrollment rate for both genders"
        }
      }
    }
    
    
    # here we will be working with all of the countries at once
    if(country=="All"){
      
      # nothing to do here...
      title<- paste(title, "for All Countries")
      # filter by year now...
      data_to_use<- filter(data_to_use, data_to_use$Year == year)
      gdp_literacy <- filter(gdp_literacy, gdp_literacy$Year==year)
      
    }else{ # otherwise! we will be working with an individual country
      data_to_use<- filter(data_long, data_long$region== country)
      title<- paste(title, "for")
      title<- paste(title, country)
    }

    
    if (info_extra==3) # here we will not be using any other info.. so we will show the map for net enrollment, and the bar graph for completion rate 
    {
      # proceed to normal graphing situation -- either line or bar...
      if(country=="All" & index==1){ # completion rate + all countries -- use the map
        
        world_data <- left_join(world, data_to_use, by = "region")
        world_data$Year <- as.numeric(as.character(world_data$Year))
        world_data$Value <- as.numeric(as.character(world_data$Value))
        
        final_plot<-ggplot()+
          geom_polygon(data=world_data, aes(x=long, y=lat,fill=Value, group=group), color="grey50")+ 
          scale_fill_gradient(name=index_name, high="red", low="yellow")+ 
          theme_void() +geom_path(data = world, aes(x = long, y = lat, group = group))+
          coord_map(xlim=c(-95,-31),ylim=c(-66,16))
        
        x_label= "Country"
        y_label= index_name
        
      }else if(country=="All" & index==2){ # net enrollment rate + all countries -- use a bar graph
        
        final_plot <- ggplot(data_to_use, aes(x=data_to_use$region, y=data_to_use$Value, fill=data_to_use$region))+
          geom_bar(stat="identity", position="dodge")+
          scale_fill_manual(values = c("darkgreen", "blue2", "yellow1", "red", "navyblue" ), name = "Country")
        x_label= "Country"
        y_label= index_name
        
      }else if(country!="All" & index==1){ # completion rate + one country -- use a line graph
        data_to_use$Year <- (as.factor(data_to_use$Year))
        final_plot <- ggplot(data_to_use, aes(x=data_to_use$Year, y=data_to_use$Value, fill=Year)) + 
          geom_bar(stat="identity", position="dodge")+
          scale_fill_manual(values = c("navyblue","darkblue", "blue2", "blue", "skyblue","lightblue"), name = "Year")
        
        x_label= "Year"
        y_label= index_name
        
      }else{ # net enrollment rate + one country
        data_to_use$Year <- (as.factor(data_to_use$Year))
        final_plot <- ggplot(data_to_use, aes(x=data_to_use$Year, y=data_to_use$Value, fill=Year)) + 
          geom_bar(stat="identity", position="dodge")+
          scale_fill_manual(values = c("navyblue","darkblue", "blue2", "blue", "skyblue","lightblue"), name = "Year")
        x_label= "Year"
        y_label= index_name
      }
    }else if(info_extra==2){ # here we will show the line graph that shows GDP versus index
      # proceed to graph with the graph showing GDP per Capita
      title<- paste("Relationship Between GDP Per Capita and ", title)
      if(country=="All"){ # show dots...
        data_to_use <- left_join(data_to_use, gdp_literacy, by=c("region","Year"))
        final_plot<- ggplot(data_to_use)+
          geom_point(aes(x=data_to_use$GDPPerCapita, y=data_to_use$Value, color=data_to_use$region, size=data_to_use$Value)) + 
          theme_minimal()+
          scale_size(name=index_name)+ 
          scale_color_manual(values = c("darkgreen", "blue2", "yellow1", "red", "navyblue" ), name = "Country")
        x_label="GDP per Capita"
        y_label=index_name
      }else{
        data_to_use <- inner_join(data_to_use, gdp_literacy, by=c("region","Year"))
        data_to_use$Year <- (as.factor(data_to_use$Year))
        final_plot<- ggplot(data_to_use)+
          geom_point(aes(x=data_to_use$Year, y=data_to_use$GDPPerCapita, size=data_to_use$Value, color=data_to_use$Year)) + 
          scale_size(name=index_name)+ 
          scale_color_manual(values = c("navyblue","darkblue", "blue2", "blue", "skyblue","lightblue"), name = "Year")
        x_label="GDP per Capita"
        y_label= "Year"
      }
    }else{ # here we will show the other graph that shows literacy rate versus index
      # proceed to graph with the literacy rate 
      title<- paste("Relationship Between Literacy Rate and ", title)
      
      if(country=="All"){
        data_to_use <- left_join(data_to_use, gdp_literacy, by=c("region","Year"))
        final_plot<- ggplot(data_to_use)+
          geom_point(aes(x=data_to_use$Literacy, y=data_to_use$Value, color=data_to_use$region, size=data_to_use$Value)) + 
          theme_minimal()+
          scale_size(name=index_name)+ 
          scale_color_manual(values = c("darkgreen", "blue2", "yellow1", "red", "navyblue" ), name = "Country")
        y_label=index_name
        x_label="Literacy Rate"
          
      }else{
        data_to_use <- inner_join(data_to_use, gdp_literacy, by=c("region","Year"))
        data_to_use$Year <- (as.factor(data_to_use$Year))
        final_plot<- ggplot(data_to_use)+
          geom_point(aes(x=data_to_use$Year, y=data_to_use$Literacy, size=data_to_use$Value, color=data_to_use$Year)) + 
          scale_size(name=index_name)+ 
          scale_color_manual(values = c("navyblue","darkblue", "blue2", "blue", "skyblue","lightblue"), name = "Year")
        y_label="Literacy Rate"
        x_label="Year"
      }
    }
    
    # determine what labels and add them...
    # title_lab= "Net Enrolment Rate in Primary School For Both Genders by Year " # figure out how to add year value here...
    final_plot <-final_plot+ ggtitle(title) + xlab(x_label)+ ylab(y_label)
    final_plot
  }, height = 700, width = 800 )
})
