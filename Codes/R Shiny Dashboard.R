# This is the code for the R Shiny dashboard for the ECC Capacity Planning tool


###################################################################### Loading the required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(stringr)
library(dplyr)
library(readxl)
library(data.table)
library(DescTools)
library(scales)
library(lubridate)
library(zoo)
library(splusTimeDate)
library(readtext)


###################################################################### Variable Decleration

# Setting up the directory where HR Employee data is available

directory = "Z:/Final/Capacity Planning Tool"; #---------- please change the directory
setwd(directory)



###################################################################### Dashboard Code


#************************************************** Dashboard UI

# In this section, the code is to build the layout of the dashboard
# It has 3 sections - Dashboard Header, Dashboard Sidebar, Dashboard Body

# Dashboard Header: Setting up the title of the dashboard
# Dashboard Sidebar: Setting up the sidebar. This is used if you want to add tabs to the dashboard
# Dashboard Body: Setting up the layout on the dashboard page for each tab (if any)

ui <- 
  dashboardPage(
      
      # Providing a header to the dashboard
      dashboardHeader(title = "ECC Capacity Planning Tool", titleWidth = 350),
      
      # Creating a sidebar with 2 tabs - Overview and Forecast dashboard
      dashboardSidebar( width = 350,sidebarMenu(
                                                menuItem("Overview", tabName = "overview", icon = icon("file-alt")),
                                                menuItem("Resource Forecast", tabName = "forecast", icon = icon("chart-line"))
                                               )
                      ),
      
      # providing the content/layout of each tab created above
      dashboardBody
        (tabItems
            (   # Layout for the overview tab
                tabItem(tabName = "overview",
                        h2("Capacity Planning Tool"), # header of the tab
                        
                        # creating the Box with overview of the tool
                        fluidRow(
                                box(
                                    title = "Overview", status = 'primary',solidHeader = TRUE, htmlOutput("txt1"), width = 12
                                    )
                                ),  
                        
                        # creating the Box for the metrics included in the tool
                        fluidRow(
                                box(
                                    title = "Metrics", status = 'primary', solidHeader = TRUE, htmlOutput("txt2"), width = 12
                                    )
                                ),
                        
                        # creating the Box for the Guidelines of the tool
                        fluidRow(
                          box(
                            title = "Guidelines", status = 'primary', solidHeader = TRUE, htmlOutput("txt3"), width = 12
                          )
                        ),
                        
                        # creating the Box for the calculations involved in the tool
                        fluidRow(
                          box(
                            title = "Calculations", status = 'primary', solidHeader = TRUE, htmlOutput("txt4"), width = 12
                          )
                        ),
                        
                        # creating the Box for the POC
                        fluidRow(
                          box(
                            title = "Point of Contact", status = 'primary', solidHeader = TRUE, htmlOutput("txt5"), width = 12
                          )
                        )
                      ),
              
                # Layout for the Forecasting tab
                tabItem(tabName = "forecast",
                        
                        # creating a box which holds the call volume forecast graph and the estimated staffing table
                        box(title = "Call Volume Forecast & Capacity Planning Estimate", status = 'primary', solidHeader = TRUE,plotOutput("plot1", height = 250),
                            tabBox(title = "Capacity Planning Estimation", selected = "Weekly", width = '3000px', height = '40px',
                                  tabPanel(title = "Weekly",  column(width = 12, tableOutput('table1')))
                                # tabPanel(title = "Monthly", column(width = 12, tableOutput('table2')))
                              )
                            ),
                        
                        # creating the control panel with all the filters which ECC supervisors can toggle between
                        box(
                              title = "Controls", status = 'warning', solidHeader = TRUE,
                              
                              # Creating a date range input to select the forecasting window the stakeholders want to see
                              # This calendar is restricted from the 7 days prior to today's date till 6 months from today
                              dateRangeInput('date',label = 'Forecasting Period',
                                       start = Sys.Date(), end = Sys.Date() + 42,
                                       min = cut(Sys.Date() - 7, "month"), max= as.Date(as.yearqtr(AddMonths(Sys.Date(), 6), "%b%Y"))),
                              
                              # Creating a dropdown to select the business line the stakeholders want to see
                              # The list is hardcoded as mentioned in choices section below
                              selectInput(inputId = "business",label = "Business Line",
                                    choices = c("Individual Life", "Individual Annuity", "Group", 
                                                "FIG", "Retirement", "Claims", "Operator"),
                                    selected = "Retirement",multiple = FALSE),
                              
                              # Creating a slider to toggle the AHT by the stakeholders
                              # The range is fixed from 0 to 20 mins with a break of 5 mins 
                              # Default value is set as 5 mins as per assumption
                              sliderInput("aht", "Average Handling Time(in Mins):", 0, 15,value = 5, 0.25),
                              
                              # Creating a slider to toggle the Aux Rate by the stakeholders
                              # The range is fixed from 0 to 40% with a break of 5%
                              # Default value is set as 15% as per assumption
                              sliderInput("aux", "Not Available Rate(%):", 0, 40,value = 15, 1),
                              
                              # Creating a slider to toggle the idle Rate by the stakeholders
                              # The range is fixed from 0 to 40% with a break of 5%
                              # Default value is set as 15% as per assumption
                              sliderInput("idle", "Idle Rate(%):", 0, 40,value = 15, 1),
                              
                              # Creating a slider to toggle the shrinkage % by the stakeholders
                              # The range is fixed from 0 to 40% with a break of 5%
                              # Default value is set as 20% as per assumption
                              sliderInput("sh", "Labor Shrinkage(%):", 0, 40,value = 20, 1),
                              
                              # This is text output which shows the translation of shrinkage % into absolute workforce number
                              htmlOutput("text1")
                          )
                       
                        )
            )
        )
  )
   

#************************************************** Dashboard data and plots


server <- function(input, output) {
  
  set.seed(122)
  ########################################### Forecasting Tab
  
  #******************************************************** Forecasting Plot ***********************************************************#
  output$plot1 <- renderPlot({
    
    
    # This code is to plot the forecasting values of all the business lines for selected timeframe
    
    #  The code will be split into 2 phases (if else statement) - one for Retirement and other for the rest of the business lines
    
    # fetching the business line selected by the user
    business_line = input$business
    
    # Getting the dates as per inputs
    st = as.Date(input$date[1], '%Y-%m-%d') - 7
    en = as.Date(input$date[2], '%Y-%m-%d') + 7
    
    # fetching the required data according to the business line selection
    if (business_line == 'Retirement')
    {
      
      # Reading the forecast file to fetch the daily forecast of call volume and renaming the column names
      
      ############################################ This needs to be automated by fetching it form table
      
      forecast <- read.csv('Volume Forecast.csv', stringsAsFactors = FALSE)
      colnames(forecast) <- c('Date', 'Total_Daily_Calls')
      
      # Subsetting the data based on the dates selected in the control panel
      result <- forecast %>% filter(as.Date(forecast$Date, '%m/%d/%Y') > st & as.Date(forecast$Date, '%m/%d/%Y') < en)
      
      
      # Plotting the call volume forecast for the selected period
      ggplot(data = result, aes(x = as.Date(Date, '%m/%d/%Y'), y = Total_Daily_Calls, group = 1)) +
        geom_point(colour = "#0099cc", size = 2.5) + geom_line(colour = "#0099cc", size = 1) + 
        labs (x= "Date", y = "Call Volume") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                    legend.title=element_text(size=12, face="bold"),
                                                    legend.text=element_text(size=10, face="bold"),
                                                    axis.title=element_text(size=10,face="bold"),
                                                    axis.text.x = element_text(angle = 65, hjust = 1, size=10,face="bold"),
                                                    axis.text.y = element_text(size=10,face="bold"),
                                                    legend.key.size = unit(2,"line")) + 
        scale_x_date(date_labels = "%b %d", date_breaks = "1 week", limits = c(st, en)) + 
        scale_y_continuous(limits = c(min(result$Total_Daily_Calls) - 200, max(result$Total_Daily_Calls) + 200))
            
    }
    else
    {
       
      # extracting filenames from the directory with all staffing files available
      files <- file.info(list.files(directory))
      no_files <- length(rownames(files))
      
      file_names <- data.frame(rownames(files))
      
      colnames(file_names) <- "file_name"
      
      # extracting the date from all the filenames
      file_names$month <- substr(file_names$file_name, 18, 20)
      file_names$date <- substr(file_names$file_name, 22, 23)
      file_names$year <- substr(file_names$file_name, 25, 26)
      file_names$final_date <- as.Date(paste(file_names$year, file_names$month, file_names$date), '%y%m%d')
      
      # Sorting the data frame in descending order to extract the latest filename according to the date present in the filename
      filename = file_names %>% arrange(final_date) %>% top_n(1, final_date) %>% pull(file_name)
      
      # Getting the sheet name based on the business line selected in the tool
      sheet_name <- ifelse(business_line == "Individual Life", "Ind Life",
                           ifelse(business_line == "Individual Annuity", "Ind Annuity",
                                  ifelse(business_line == "Group Annuity", "Group",
                                         ifelse(business_line == "FIG", "FIG",
                                               ifelse(business_line == "Claims", "Claims", "Operator")))))
      
      # reading the sheet of the business line selected
      excel <- read_excel(paste(directory,'/', filename, sep = ""), sheet = sheet_name)
      colnames(excel)[1] <- "Description"
      
      # Extracting the start week
      to_date <- excel %>% filter(grepl('As of:', Description, fixed = FALSE))
      value <- to_date[[1]][1]
      start_week <- as.Date(str_sub(value, -10, -1), "%m/%d/%Y")
      
      # Extracting the row where the projected volume is recorded for the particular business lines
      column_name <- colnames(excel[,1])
      projected_volume_row <- excel %>% filter(grepl('Projected Volume', Description, fixed = FALSE))
      
      count = min(which(is.na(projected_volume_row))) - 1
      end_week = start_week + ((count-2)*7)
      
      
      dates = seq(start_week, end_week, by = "week")
      
      forecast <- as.data.frame(dates)
      forecast$forecast <- as.numeric(t(projected_volume_row)[2:count])
      
      colnames(forecast) <- c("Date", "Total_Daily_Calls")
      
      # Converting the monthly forecast into weekly by diving by 4 or 5 depending on number of weeks in the week
      forecast <- forecast %>%
                          mutate(period_month = month(Date)) %>%
                          group_by(period_month) %>%
                          mutate(number_weeks = n(),
                                 actual_forecast = Total_Daily_Calls/number_weeks) %>%
                          mutate(number_weeks = ifelse(number_weeks < 4, 4, number_weeks))
                        
      
      # Subsetting the data based on the dates selected in the control panel
      result <- forecast %>% filter(Date > st & Date < en)
      
      
      # Plotting the call volume forecast for the selected period
      ggplot(data = result, aes(x = as.Date(Date, '%m/%d/%Y'), y = actual_forecast, group = 1)) +
        geom_point(colour = "#0099cc", size = 2.5) + geom_line(colour = "#0099cc", size = 1) +
        labs (x= "Date", y = "Call Volume") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                    legend.title=element_text(size=12, face="bold"),
                                                    legend.text=element_text(size=10, face="bold"),
                                                    axis.title=element_text(size=10,face="bold"),
                                                    axis.text.x = element_text(angle = 65, hjust = 1, size=10,face="bold"),
                                                    axis.text.y = element_text(size=10,face="bold"),
                                                    legend.key.size = unit(2,"line")) + 
        scale_x_date(date_labels = "%b %d", date_breaks = "1 week", limits = c(st, en)) + 
        scale_y_continuous(limits = c(min(result$actual_forecast) - 200, max(result$actual_forecast) + 200))
      
      
    }
    
    
  })
  
  #******************************************************** Staff Estimation Weekly ***********************************************************#
  output$table1 <- renderTable({  
    
    ####################### Extracting the available Staff from ECC Staffing file ###############################
    
    # extracting filenames from the directory with all staffing files available
    files <- file.info(list.files(directory))
    no_files <- length(rownames(files))
    
    file_names <- data.frame(rownames(files))
    
    colnames(file_names) <- "file_name"
    
    # extracting the date from all the filenames
    file_names$month <- substr(file_names$file_name, 18, 20)
    file_names$date <- substr(file_names$file_name, 22, 23)
    file_names$year <- substr(file_names$file_name, 25, 26)
    file_names$final_date <- as.Date(paste(file_names$year, file_names$month, file_names$date), '%y%m%d')
    
    # Sorting the data frame in descending order to extract the latest filename according to the date present in the filename
    filename = file_names %>% arrange(final_date) %>% top_n(1, final_date) %>% pull(file_name)
    
    # reading the first sheet of the latest staffing file (Resource Plan)
    avb_staff <- read_excel(paste(directory,'/', filename, sep = ""), sheet = 1)
    
    # Extracting the row where all the available staff is recorded for all business lines
    avb_staff_row <- avb_staff %>% filter(grepl('Total available to answer the phone', `ENTERPRISE CONTACT CENTER RESOURCE PLAN`, fixed = FALSE))
    
    # Assigning each value to each variable
    avb_ind_life <- avb_staff_row[2]
    avb_ind_ann <- avb_staff_row[3]
    avb_ind_grp <- avb_staff_row[4]
    avb_ind_fig <- avb_staff_row[5]
    avb_ind_ret <- avb_staff_row[6]
    avb_ind_claims <- avb_staff_row[7]
    avb_ind_operator <- avb_staff_row[8]
    
    
    ####################### Calculating the estimated required staff ###############################
    
    # This code is to plot the forecasting values of all the business lines for selected timeframe
    
    #  The code will be split into 2 phases (if else statement) - one for Retirement and other for the rest of the business lines
    
    # fetching the business line selected by the user
    business_line = input$business
    
    # Getting the dates as per inputs
    st = as.Date(input$date[1], '%Y-%m-%d') - 7
    en = as.Date(input$date[2], '%Y-%m-%d') + 7
    
    # Extractin the AHT, Auxilary Rate, Idle Rate, Shrinkage from the controls tab based on the user's selections
    AHT_Seconds = input$aht
    AUX_rate = input$aux
    idle_rate = input$idle
    shrinkage_rate = input$sh
    
    # fetching the required data according to the business line selection
    if (business_line == 'Retirement')
    {
      
      # Reading the forecast file to fetch the daily forecast of call volume and renaming the column names
      
      ############################################ This needs to be automated by fetching it form table
      
      forecast <- read.csv('Volume Forecast.csv', stringsAsFactors = FALSE)
      colnames(forecast) <- c('start_week', 'total_daily_calls')
      forecast$start_week <- as.Date(forecast$start_week, "%m/%d/%Y")
      
      forecast$end_week = forecast$start_week + 6
      
      # Calculating the required people as per the calculation in the existing capacity planning excel (dummy formula)
      forecast$people = forecast$total_daily_calls*AHT_Seconds*60/3600
      forecast$people = ceiling(forecast$people/40)
      
      # This will ensure that even if the user selects the mid of the week, the tool will show from the beginning of that week
      new_df <- forecast[which(forecast$start_week > st & forecast$end_week < en), ]
      
      new_df$total_daily_calls <- NULL
      new_df$Business_Line = 'Retirement'
      
      new_df$available[new_df$Business_Line == 'Retirement'] = as.integer(avb_ind_ret)
      
      # Calculating the difference between the required staff and available staff
      new_df$Difference <- new_df$available - new_df$people
      
      # Creating the final data frame with only the required columns
      final <- new_df %>% select(start_week, end_week, people, available, Difference)
      
      final$start_week <- as.character(final$start_week)
      final$end_week <- as.character(final$end_week)
      final$people <- as.integer(final$people)
      final$Difference <- as.integer(final$Difference)
      
      # Changing the column names
      colnames(final) <- c('Start of the Week', 'End of the Week', 'Estimated Required Staff', 'Available Staff', 'Difference')
      
      final
      
      
    }
    else
    {
      
      # extracting filenames from the directory with all staffing files available
      files <- file.info(list.files(directory))
      no_files <- length(rownames(files))
      
      file_names <- data.frame(rownames(files))
      
      colnames(file_names) <- "file_name"
      
      # extracting the date from all the filenames
      file_names$month <- substr(file_names$file_name, 18, 20)
      file_names$date <- substr(file_names$file_name, 22, 23)
      file_names$year <- substr(file_names$file_name, 25, 26)
      file_names$final_date <- as.Date(paste(file_names$year, file_names$month, file_names$date), '%y%m%d')
      
      # Sorting the data frame in descending order to extract the latest filename according to the date present in the filename
      filename = file_names %>% arrange(final_date) %>% top_n(1, final_date) %>% pull(file_name)
      
      # Getting the sheet name based on the business line selected in the tool
      sheet_name <- ifelse(business_line == "Individual Life", "Ind Life",
                           ifelse(business_line == "Individual Annuity", "Ind Annuity",
                                  ifelse(business_line == "Group Annuity", "Group",
                                         ifelse(business_line == "FIG", "FIG",
                                                ifelse(business_line == "Claims", "Claims", "Operator")))))
      
      # reading the sheet of the business line selected
      excel <- read_excel(paste(directory,'/', filename, sep = ""), sheet = sheet_name)
      colnames(excel)[1] <- "Description"
      
      # Extracting the start week
      to_date <- excel %>% filter(grepl('As of:', Description, fixed = FALSE))
      value <- to_date[[1]][1]
      start_week <- as.Date(str_sub(value, -10, -1), "%m/%d/%Y")
      
      # Extracting the row where the projected volume is recorded for the particular business lines
      column_name <- colnames(excel[,1])
      projected_volume_row <- excel %>% filter(grepl('Projected Volume', Description, fixed = FALSE))
      
      #Identifying till which week the projected volume is available
      count = min(which(is.na(projected_volume_row))) - 1
      end_week = start_week + ((count-2)*7)
      
      # Creating a sequence of dates based on availablity of projected volume weeks
      dates = seq(start_week, end_week, by = "week")
      
      # Creating a dataframe with these dates for further usage
      forecast <- as.data.frame(dates)
      forecast$forecast <- as.numeric(t(projected_volume_row)[2:count])
      
      colnames(forecast) <- c("Date", "Total_Daily_Calls")
      
      # Since the available projected volume is at month level, calculating the number of weeks in each month to convert the projected volume
      # from monthly to weekly
      forecast <- forecast %>%
        mutate(period_month = month(Date)) %>%
        group_by(period_month) %>%
        mutate(number_weeks = n(),
               actual_forecast = Total_Daily_Calls/number_weeks) %>%
        mutate(number_weeks = ifelse(number_weeks < 4, 4, number_weeks)) %>%
        select('Date', 'Total_Daily_Calls', 'actual_forecast') %>% 
        ungroup()
      
      forecast$period_month <- NULL
      
      # Calulating the end date of the week
      forecast$end_week = forecast$Date + 6
      
      # Calculating the required people as per the calculation in the existing capacity planning excel
      forecast$people = forecast$actual_forecast*AHT_Seconds*60/3600
      forecast$people = ceiling(forecast$people/40)
      
      
      available_staff <- ifelse(business_line == "Individual Life", avb_ind_life,
                           ifelse(business_line == "Individual Annuity", avb_ind_ann,
                                  ifelse(business_line == "Group Annuity", avb_ind_grp,
                                         ifelse(business_line == "FIG", avb_ind_fig,
                                                ifelse(business_line == "Claims", avb_ind_claims, avb_ind_operator)))))


      forecast$available = as.integer(available_staff)

      
      # Calculating the difference between the required staff and available staff
      forecast$Difference <- forecast$available - forecast$people

      # Subsetting the data based on the dates selected in the control panel
      new_df <- forecast %>% filter(Date > st & Date < en) 
      
      # Removing the columns which are not needed
      new_df$Total_Daily_Calls <- NULL
      new_df$actual_forecast <- NULL
    

      # renaming the columns
      colnames(new_df) <- c("start_week", "end_week", "people", "available", "Difference")


      # Creating the final data frame with only the required columns
      final <- new_df

      final$start_week <- as.character(final$start_week)
      final$end_week <- as.character(final$end_week)
      final$people <- as.integer(final$people)
      final$available <- as.integer(final$available)
      final$Difference <- as.integer(final$Difference)

      # Changing the column names
      colnames(final) <- c('Start of the Week', 'End of the Week', 'Estimated Required Staff', 'Available Staff','Difference')

      final
    }
    
  # Incorporating the Training data into the available staff
    
    # Reading the training sheet from the ECC staffing file
    excel <- read_excel(paste(directory,'/', filename, sep = ""), sheet = "Training", col_types = c("text", "text", "text", "date", "text", "text", "text", "text"))
    training <- excel[-(1:4), 1:4]
    colnames(training) <- c("Name", "Primary_Line", "Cross_training_flag", "Productive_date")

    # Creating a flag based on the business line selected as per the values present in the file
    primary_line <- ifelse(business_line == "Individual Life", 'IL',
                           ifelse(business_line == "Individual Annuity", 'IA',
                                  ifelse(business_line == "Group Annuity", 'G',
                                         ifelse(business_line == "FIG", 'F',
                                                ifelse(business_line == "Claims", 'C',
                                                       ifelse(business_line == "Retirement", 'R', 'O'))))))
    
    # Subsetting the training dataset for the selected business line and rows which are populated with productive date
    avaialable <- training[(is.na(training$Productive_date) == FALSE & training$Primary_Line == primary_line),]
    avaialable$Productive_date <- as.Date(avaialable$Productive_date)
    
    final$`Start of the Week` <- as.Date(final$`Start of the Week`)
    
    # For each value in the productive date in the sheet, it identifies the records from the final dataset which needs to be incremented
    for (value in avaialable$Productive_date){
      df <- subset(final, final$`Start of the Week` >= value)
      staff = df$`Available Staff` + 1
      final$`Available Staff`[final$`Start of the Week` >= df$`Start of the Week`[1]] = staff
    }
    
    # Making formatting changes to display
    final$`Available Staff` = as.integer(final$`Available Staff`)
    final$`Start of the Week` = as.Date(final$`Start of the Week`, '%Y-%m-%d')
    
    final$`Start of the Week` <- as.character(final$`Start of the Week`)
    final$`End of the Week` <- as.character(final$`End of the Week`)
    final$`Estimated Required Staff` <- as.integer(final$`Estimated Required Staff`)
    final$`Available Staff` <- as.integer(final$`Available Staff`)
    final$Difference <- as.integer(final$Difference)
    
    # Re-Calculating the difference between the required staff and available staff
    final$Difference <- final$`Available Staff` - final$`Estimated Required Staff`
    
    #final <- final %>% select('Start of the Week', 'End of the Week', 'Estimated Required Staff', 'Available Staff','Difference')
    
    final
    
  })
  
  
  #******************************************************** Assumption Estimation ***********************************************************#
  
  output$text1 <- renderText({
    
    
    files <- file.info(list.files(directory))
    no_files <- length(rownames(files))
    file_names <- data.frame(rownames(files))
    
    colnames(file_names) <- "file_name"
    
    # extracting the date from all the filenames
    
    file_names$month <- substr(file_names$file_name, 18, 20)
    file_names$date <- substr(file_names$file_name, 22, 23)
    file_names$year <- substr(file_names$file_name, 25, 26)
    file_names$final_date <- as.Date(paste(file_names$year, file_names$month, file_names$date), '%y%m%d')
    
    # Sorting the data frame in descending order to extract the latest filename according to the date present in the filename
    
    filename = file_names %>% arrange(final_date) %>% top_n(1, final_date) %>% pull(file_name)
    
    # reading the first sheet of the latest staffing file
    
    avb_staff <- read_excel(paste(directory,'/', filename, sep = ""), sheet = 1)
    
    # Extracting the only row where all the available staff is recorded for all business lines
    avb_staff_row <- avb_staff %>% filter(grepl('Total available to answer the phone', `ENTERPRISE CONTACT CENTER RESOURCE PLAN`, fixed = FALSE))
    
    # Assigning each value to each variable
    avb_ind_life <- avb_staff_row[2]
    avb_ind_ann <- avb_staff_row[3]
    avb_ind_grp <- avb_staff_row[4]
    avb_ind_fig <- avb_staff_row[5]
    avb_ind_ret <- avb_staff_row[6]
    avb_ind_claims <- avb_staff_row[7]
    avb_ind_operator <- avb_staff_row[8]
    
    available <- ifelse(input$business == 'Individual Life', avb_ind_life, 
                   ifelse(input$business == 'Individual Annuity', avb_ind_ann,
                    ifelse(input$business == 'Group Annuity', avb_ind_grp,
                     ifelse(input$business == 'FIG', avb_ind_fig,
                      ifelse(input$business == 'Retirement', avb_ind_ret,
                       ifelse(input$business == 'Claims', avb_ind_claims,avb_ind_operator))))))
    
    AHT_Seconds = input$aht
    AUX_rate = input$aux
    idle_rate = input$idle
    shrinkage_rate = input$sh

    paste("<B>The above assumption estimates: </B>",
                                    "<li>The selected shrinkage % corresponds to", "<B>", ceiling((shrinkage_rate*as.numeric(available)/100)), "</B>", "customer support associates</li>")
    
    })
  
    ############## Now populating the overview page
    output$txt1 <- renderText({
      
    
    files <- file.info(list.files(directory))
    doc.text <- readtext(paste(directory,'/', "Overview.docx", sep = ""))$text
    
    doc.parts <- strsplit(doc.text, "\n")[[1]]
    
    text <- doc.parts[2]
    
    text  
    })
    
    output$txt2 <- renderText({
      
      
      files <- file.info(list.files(directory))
      doc.text <- readtext(paste(directory,'/', "Overview.docx", sep = ""))$text
      
      doc.parts <- strsplit(doc.text, "\n")[[1]]
      
      text1 <- doc.parts[4]
      text2 <- doc.parts[5]
      text3 <- doc.parts[6]
      text4 <- doc.parts[7]
      
      h1 <- paste(strsplit(text1, ":")[[1]][[1]], ":")
      h2 <- paste(strsplit(text2, ":")[[1]][[1]], ":")
      h3 <- paste(strsplit(text3, ":")[[1]][[1]], ":")
      h4 <- paste(strsplit(text4, ":")[[1]][[1]], ":")
      
      t1 <- strsplit(text1, ":")[[1]][[2]]
      t2 <- strsplit(text2, ":")[[1]][[2]]
      t3 <- strsplit(text3, ":")[[1]][[2]]
      t4 <- strsplit(text4, ":")[[1]][[2]]
      
      
      
      
      paste("<li><B>", h1, "</B>", t1, "</li>","<br/>",
            "<li><B>", h2, "</B>", t2, "</li>","<br/>",
            "<li><B>", h3, "</B>", t3, "</li>","<br/>",
            "<li><B>", h4, "</B>", t4, "</li>")
    })
    
    
    output$txt3 <- renderText({
      
      
      files <- file.info(list.files(directory))
      doc.text <- readtext(paste(directory,'/', "Overview.docx", sep = ""))$text
      
      doc.parts <- strsplit(doc.text, "\n")[[1]]
      
      text1 <- doc.parts[9]
      text2 <- doc.parts[10]
      text3 <- doc.parts[11]
      
      h1 <- paste(strsplit(text1, ":")[[1]][[1]], ":")
      h2 <- paste(strsplit(text2, ":")[[1]][[1]], ":")
      h3 <- paste(strsplit(text3, ":")[[1]][[1]], ":")
      
      t1 <- strsplit(text1, ":")[[1]][[2]]
      t2 <- strsplit(text2, ":")[[1]][[2]]
      t3 <- strsplit(text3, ":")[[1]][[2]]
      
      paste("<li><B>", h1, "</B>", t1, "</li>",
            "<li><B>", h2, "</B>", t2, "</li>",
            "<li><B>", h3, "</B>", t3, "</li>")
    })
    
    
    output$txt4 <- renderText({
      
      
      files <- file.info(list.files(directory))
      doc.text <- readtext(paste(directory,'/', "Overview.docx", sep = ""))$text
      
      doc.parts <- strsplit(doc.text, "\n")[[1]]
      
      text1 <- doc.parts[13]
      text2 <- doc.parts[14]
      text3 <- doc.parts[15]
      
      h1 <- paste(strsplit(text1, ":")[[1]][[1]], ":")
      h2 <- paste(strsplit(text2, ":")[[1]][[1]], ":")
      h3 <- paste(strsplit(text3, ":")[[1]][[1]], ":")
      
      t1 <- strsplit(text1, ":")[[1]][[2]]
      t2 <- strsplit(text2, ":")[[1]][[2]]
      t3 <- strsplit(text3, ":")[[1]][[2]]
      
      paste("<B>", h1, "</B>", t1, "<br>",
            "<B>", h2, "</B>", t2, "<br>",
            "<B>", h3, "</B>", t3)
    })
    
    output$txt5 <- renderText({
      
      
      files <- file.info(list.files(directory))
      doc.text <- readtext(paste(directory,'/', "Overview.docx", sep = ""))$text
      
      doc.parts <- strsplit(doc.text, "\n")[[1]]
      
      text1 <- doc.parts[17]
      text2 <- doc.parts[18]
      
      h1 <- paste(strsplit(text1, ":")[[1]][[1]], ":")
      h2 <- paste(strsplit(text2, ":")[[1]][[1]], ":")
      
      t1 <- strsplit(text1, ":")[[1]][[2]]
      t2 <- strsplit(text2, ":")[[1]][[2]]
      
      paste("<li><B>", h1, "</B>", "<em>", t1, "</em>", "</li>",
            "<li><B>", h2, "</B>", "<em>", t2, "</em>", "</li>")
    })
  
}

shinyApp(ui = ui, server = server)

