#2/1/2020

#LOAD IN PACKAGES
{
  install.packages("dplyr")
  install.packages("plyr")
  install.packages("ggplot2")
  install.packages("magrittr")
  install.packages("reshape")
  install.packages("lubridate")
  install.packages("forecast")
  
  require(dplyr)
  require(plyr)
  require(ggplot2)
  require(magrittr)
  require(reshape)
  require(lubridate)
  require(forecast)
}

#LOAD AND CREATE DATA FRAMES
{
  data_contacts = read.csv"contacts.csv", stringsAsFactors = FALSE)
  data_listings = read.csv("listings.csv", stringsAsFactors = FALSE)
  data_users = read.csv("users.csv", stringsAsFactors = FALSE)
  glimpse(data_contacts)
  glimpse(data_listings)
  glimpse(data_users)
}

#DATA WRANGLING
{
  #CONTACTS
  colnames(data_contacts)
  working_data_contacts = data_contacts %>% mutate(
    id_guest_anon = as.factor(id_guest_anon),
    id_host_anon = as.factor(id_host_anon),
    id_listing_anon = as.factor(id_listing_anon),
    ts_interaction_first = as.POSIXct(ts_interaction_first, format = "%Y-%m-%d %H:%M:%S"),
    ts_reply_at_first = as.POSIXct(ts_reply_at_first, format = "%Y-%m-%d %H:%M:%S"),
    ts_accepted_at_first = as.POSIXct(ts_accepted_at_first, format = "%Y-%m-%d %H:%M:%S"),
    ts_booking_at = as.POSIXct(ts_booking_at, format = "%Y-%m-%d %H:%M:%S"),
    ds_checkin_first = as.Date(ds_checkin_first),
    ds_checkout_first = as.Date(ds_checkout_first),
    contact_channel_first = as.factor(contact_channel_first),
    guest_user_stage_first = as.factor(guest_user_stage_first),
    
    #new columns
    length_days_visited = as.numeric(ds_checkout_first - ds_checkin_first),
    booked_flag = ifelse(is.na(ts_booking_at), FALSE, TRUE),
    minutes_to_reply = round(as.numeric(ts_reply_at_first - ts_interaction_first)/60,2),
    minutes_to_accept = round(as.numeric(ts_accepted_at_first - ts_interaction_first)/60,2),
    minutes_to_book = round(as.numeric(ts_booking_at - ts_interaction_first)/60,2),
    ts_interaction_first_day = weekdays(ts_interaction_first),
    ts_reply_at_first_day = weekdays(ts_reply_at_first),
    ts_accepted_at_first_day = weekdays(ts_accepted_at_first),
    ts_booking_at_day = weekdays(ts_booking_at)
  )
  
  #LISTINGS
  colnames(data_listings)
  working_data_listings = data_listings %>% mutate(
    id_listing_anon = as.factor(id_listing_anon),
    room_type = as.factor(room_type),
    listing_neighborhood = as.factor(listing_neighborhood)
  )
  
  #USERS
  colnames(data_users)
  working_data_users = data_users %>% mutate(
    id_user_anon = as.factor(id_user_anon),
    country = as.factor(country)
  )
  
  glimpse(working_data_contacts)
  glimpse(working_data_listings)
  glimpse(working_data_users)
  
  #inner join data_contacts and data_listings
  working_data_contacts = merge(working_data_contacts, data_listings, by = "id_listing_anon")
  
  #inner join to create user data on guest and host columns
  working_data_guest = merge(working_data_contacts, working_data_users, by.x = "id_guest_anon", by.y = "id_user_anon")
  working_data_host = merge(working_data_contacts, working_data_users, by.x = "id_host_anon", by.y = "id_user_anon")
}

#INITIAL DESCRIPTIVE DATA EXPLORATION & VISUALIZATION 
{
  #CONTACTS _______________________________________________
  
  #id of the guest making inquiry
  summary(working_data_contacts$id_guest_anon)
  
  #id of the host of the listing to which the inquiry is made
  summary(working_data_contacts$id_host_anon)
  
  #id of the listing to which the inquiry is made
  summary(working_data_contacts$id_listing_anon)
  
  #UTC timestamp of the moment the inquiry is made
  qplot(working_data_contacts$ts_interaction_first) 
  sum(is.na(working_data_contacts$ts_interaction_first)) #number of NA points
  
  #UTC timestamp of the moment the host replies to the inquiry, if so
  qplot(working_data_contacts$ts_reply_at_first)
  sum(is.na(working_data_contacts$ts_reply_at_first)) #number of NA points
  
  #UTC timestamp of the moment the host accepts the inquiry, if so
  qplot(working_data_contacts$ts_accepted_at_first)
  sum(is.na(working_data_contacts$ts_accepted_at_first)) #number of NA points
  
  #UTC timestamp of the moment the booking is made, if so
  qplot(working_data_contacts$ts_booking_at)
  sum(is.na(working_data_contacts$ts_booking_at)) #number of NA points
  
  #Date stamp of the check-in date of the inquiry
  qplot(working_data_contacts$ds_checkin_first)
  
  #Date stamp of the check-out date of the inquiry
  qplot(working_data_contacts$ds_checkout_first)
  
  #The number of guests the inquiry is for
  qplot(working_data_contacts$m_guests)
  
  #Number of characters in the first message sent by guest, if message was sent
  qplot(working_data_contacts$m_first_message_length_in_characters)
  
  #The contact channel through which the inquiry was made {contact_me, book_it, instant_book}
  qplot(working_data_contacts$contact_channel_first,
        xlab = "Contact Channel", ylab = "Frequency", main = "Contact Channel Breakdown")
  summary(working_data_contacts$contact_channel_first)
  
  #Indicates whether user has made booking before sending inquiry ("past booker") If the 
  #user has not booked before, then the user is a new user
  qplot(working_data_contacts$guest_user_stage_first)
  summary(working_data_contacts$guest_user_stage_first)
  
  #Length of visits in days
  qplot(working_data_contacts$length_days_visited)
  summary(working_data_contacts$length_days_visited)
  
  #Length from first interaction to: minutes to reply, minutes to accept, and minutes to book
  {
    ##:::::::::::::::::::::::::::::::::
    ## FIGURE 6
    ##:::::::::::::::::::::::::::::::::
    qplot(working_data_contacts$minutes_to_reply, 
          xlim = c(0,300),
          col = working_data_contacts$booked_flag, geom = 'freqpoly') + 
      labs(col = 'Booking Status', y = 'Number of Successful Bookings', x = 'Minutes to Reply', title = 'Host Reply Time')
    summary(working_data_contacts$minutes_to_reply)
    summary(working_data_contacts$minutes_to_reply[which(working_data_contacts$booked_flag == TRUE)])
    summary(working_data_contacts$minutes_to_reply[which(working_data_contacts$booked_flag == FALSE)])
    
    ##:::::::::::::::::::::::::::::::::
    ## FIGURE 7
    ##:::::::::::::::::::::::::::::::::
    qplot(working_data_contacts$minutes_to_accept, 
          xlim = c(0,300),
          col = working_data_contacts$booked_flag, geom = 'freqpoly') + 
      labs(col = 'Booked Flag', y = 'Number of Successful Bookings', x = 'Minutes to Accept', title = 'Host Acceptance Time') 
    summary(working_data_contacts$minutes_to_accept)
    summary(working_data_contacts$minutes_to_accept[which(working_data_contacts$booked_flag == TRUE)])
    summary(working_data_contacts$minutes_to_accept[which(working_data_contacts$booked_flag == FALSE)])
    
    qplot(working_data_contacts$minutes_to_book)
    summary(working_data_contacts$minutes_to_book)
  }
  
  #LISTINGS _______________________________________________
  
  #Anonymized id of the listing
  summary(working_data_listings$id_listing_anon)
  
  #Whether room is entire home, private room, or shared room
  qplot(working_data_listings$room_type)
  
  #Neighborhood of listing
  summary(working_data_listings$listing_neighborhood)
  
  #Total number of reviews of listing, at time data was pulled
  {
    ##:::::::::::::::::::::::::::::::::
    ## FIGURE 10
    ##:::::::::::::::::::::::::::::::::
    qplot(working_data_contacts$total_reviews, 
          xlim = c(0, 300),
          col = working_data_contacts$booked_flag, geom = 'freqpoly') + 
      labs(col = 'Booked Flag', y = 'Number of Successful Bookings', x = 'Number of Reviews', title = 'Number of Reviews on Listing')
    summary(working_data_contacts$total_reviews)
    summary(working_data_contacts$total_reviews[which(working_data_contacts$booked_flag == TRUE)])
    summary(working_data_contacts$total_reviews[which(working_data_contacts$booked_flag == FALSE)])
  }
  #USERS _______________________________________________
  
  #Anonymized id of user
  summary(working_data_users$id_user_anon)
  
  #Number of words in about me section of user's profile, at time of contact
  {
    qplot(working_data_users$words_in_user_profile)
    
    #GUEST USERS
    {
      ##:::::::::::::::::::::::::::::::::
      ## FIGURE 9
      ##:::::::::::::::::::::::::::::::::
      qplot(working_data_guest$words_in_user_profile, 
            xlim = c(0,500),
            col = working_data_guest$booked_flag, geom = 'freqpoly') + 
        labs(col = 'Booked Flag', y = 'Frequency', x = 'Number of Words in Profile (Guest)', title = 'Number of Words in Profile (Guest)')
      summary(working_data_guest$words_in_user_profile)
      summary(working_data_guest$words_in_user_profile[which(working_data_guest$booked_flag == TRUE)])
      summary(working_data_guest$words_in_user_profile[which(working_data_guest$booked_flag == FALSE)])
    }
    
    #HOST USERS
    {
      ##:::::::::::::::::::::::::::::::::
      ## FIGURE 8
      ##:::::::::::::::::::::::::::::::::
      qplot(working_data_host$words_in_user_profile, 
            xlim = c(0,500),
            col = working_data_host$booked_flag, geom = 'freqpoly') + 
        labs(col = 'Booked Flag', y = 'Number of Successful Bookings', x = 'Number of Words in Profile (Host)', title = 'Number of Words in Profile (Host)')
      summary(working_data_host$words_in_user_profile)
      summary(working_data_host$words_in_user_profile[which(working_data_host$booked_flag == TRUE)])
      summary(working_data_host$words_in_user_profile[which(working_data_host$booked_flag == FALSE)])
    }
  }
  
  #Origin country of user
  summary(working_data_users$country)
}


#FURTHER DATA EXPLORATION (DATA FOR SLIDE 4)
{
  channel_book = working_data_contacts[which(working_data_contacts$contact_channel_first == "book_it"),]
  channel_contact = working_data_contacts[which(working_data_contacts$contact_channel_first == "contact_me"),]
  channel_instant = working_data_contacts[which(working_data_contacts$contact_channel_first == "instant_book"),]
  
  #LOOKING INTO BOOK_IT SUBSET
  nrow(channel_book[which(channel_book$booked_flag == TRUE),])
  nrow(channel_book[which(channel_book$booked_flag == FALSE),])
  
  #LOOKING INTO CONTACT_ME SUBSET
  nrow(channel_contact[which(channel_contact$booked_flag == TRUE),])
  nrow(channel_contact[which(channel_contact$booked_flag == FALSE),])
  
  #LOOKING INTO INSTANT_BOOK SUBSET
  nrow(channel_instant[which(channel_instant$booked_flag == TRUE),])
  nrow(channel_instant[which(channel_instant$booked_flag == FALSE),])
}

##:::::::::::::::::::::::::::::::::
## FIGURE 1
##:::::::::::::::::::::::::::::::::
{
  figure1_data_raw = working_data_contacts[, c("ts_interaction_first", "booked_flag")]
  figure1_data = figure1_data_raw %>% 
    mutate(
      date = format(as.Date(ts_interaction_first,  tz = 'UTC'), "%Y-%m"),
      ts_interaction_first = NULL,
      freq = 1
    ) %>% 
    filter(!date == '2016-07')#when we convert UTC time using as.Date, a few samples are grouped into July
  
  figure1_data = cast(figure1_data, date ~ booked_flag)
  figure1_data = melt(figure1_data, id.vars = date)
  glimpse(figure1_data)
  
  ggplot(data = figure1_data, aes(x=date, y=value, col = booked_flag)) +
    ylim(c(0,6000))+
    geom_line(aes(group = booked_flag) ) + geom_point() + 
    labs(title = 'Total Interactions: Attempted vs. Successful Bookings', 
         x = 'Date (Monthly Aggregated)', y = 'Number of First Interactions',
         color = 'Booking Status') +
    scale_color_discrete(labels = c('Interaction\n occurred\n but no booking','Booking success'))
}

##:::::::::::::::::::::::::::::::::
## FIGURE 4
##:::::::::::::::::::::::::::::::::
{
  figure4_data_raw = working_data_contacts[,c("ts_interaction_first", "booked_flag", "contact_channel_first")]
  figure4_data = figure4_data_raw %>%
    mutate(
      date = format(as.Date(ts_interaction_first,  tz = 'UTC'), "%Y-%m"),
      ts_interaction_first = NULL,
      freq = 1
    ) %>% 
    filter(!date == '2016-07', #when we convert UTC time using as.Date, a few samples are grouped into July
    booked_flag == TRUE)
  glimpse(figure4_data)
  
  figure4_data = cast(figure4_data, date ~ contact_channel_first)
  figure4_data = melt(figure4_data, id.vars = date)
  glimpse(figure4_data)
  
  ggplot(data = figure4_data, aes(x=date, y=value, col = contact_channel_first)) +
    ylim(0,1500)+
    geom_line(aes(group = contact_channel_first)) + geom_point() + 
    labs(title = 'Number of Successful Bookings Across Channels', x = 'Date (Monthly Aggregated)', y = 'Number of Successful Bookings') +
    labs(color = 'Contact Channel') 
}

##:::::::::::::::::::::::::::::::::
## FIGURE 5
##:::::::::::::::::::::::::::::::::
{
  figure5_data_raw = working_data_contacts[,c("ts_interaction_first", "booked_flag", "contact_channel_first")]
  figure5_data = figure5_data_raw %>%
    mutate(
      date = format(as.Date(ts_interaction_first,  tz = 'UTC'), "%Y-%m"),
      ts_interaction_first = NULL,
      freq = 1
    ) %>% 
    filter(!date == '2016-07')#when we convert UTC time using as.Date, a few samples are grouped into July
  glimpse(figure5_data)
  
  figure5_data = cast(figure5_data, date ~ contact_channel_first)
  figure5_data = melt(figure5_data, id.vars = date)
  glimpse(figure5_data)
  
  ggplot(data = figure5_data, aes(x=date, y=value, col = contact_channel_first)) +
    ylim(0,4200)+
    geom_line(aes(group = contact_channel_first)) + geom_point() + 
    labs(title = 'Number of Total Interactions Across Channels', x = 'Date (Monthly Aggregated)', y = 'Number of Successful Bookings') +
    labs(color = 'Contact Channel')
}

##:::::::::::::::::::::::::::::::::
## FIGURE 3
##:::::::::::::::::::::::::::::::::
{
  #grab top 10 neighborhoods
  summary_neighborhoods = as.data.frame(summary(working_data_listings$listing_neighborhood))
  summary_neighborhoods = cbind(row.names(summary_neighborhoods),summary_neighborhoods)
  colnames(summary_neighborhoods) = c('Neighborhoods', 'Frequency')
  topneighborhoods = summary_neighborhoods[order(-summary_neighborhoods$Frequency),]
  top10neighborhoods = summary_neighborhoods[order(-summary_neighborhoods$Frequency),][1:11,]
  top10neighborhoods = top10neighborhoods[!top10neighborhoods$Neighborhoods == "-unknown-",]
  names_top10neighborhoods = as.vector(top10neighborhoods$Neighborhoods)
  
  figure3_data_raw = working_data_contacts[,c("ts_interaction_first", 
                                            "listing_neighborhood", "booked_flag")]
  figure3_data = figure3_data_raw[which(figure3_data_raw$booked_flag == TRUE),]
  figure3_data = figure3_data[which(figure3_data$listing_neighborhood %in% names_top10neighborhoods),]
  figure3_data = figure3_data[figure3_data$ts_interaction_first < as.Date("2016-06-30"),]
  figure3_data = figure3_data %>% mutate(
    date = format(as.Date(ts_interaction_first), "%Y-%m"),
    ts_interaction_first = NULL,
    booked_flag = NULL,
    freq = 1
  )
  
  figure3_data = cast(figure3_data, date ~ listing_neighborhood)
  figure3_data = melt(figure3_data, id.vars = date)
  glimpse(figure3_data)
  
  {
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Copacabana")] = "1 :Copacabana"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Ipanema")] = "2 :Ipanema"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Barra da Tijuca")] = "3 :Barra da Tijuca"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Leblon")] = "4 :Leblon"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Botafogo")] = "5 :Botafogo"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Santa Teresa")] = "6 :Santa Teresa"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Flamengo")] = "7 :Flamengo"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Leme")] = "8 :Leme"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Lapa")] = "9 :Lapa"
    figure3_data$listing_neighborhood[which(figure3_data$listing_neighborhood == "Recreio dos Bandeirantes")] = "10 :Recreio\n dos Bandeirantes"
  }
  
  #order our legend
  figure3_data$listing_neighborhood = factor(figure3_data$listing_neighborhood, levels = c("1 :Copacabana",
                                                             "2 :Ipanema",
                                                             "3 :Barra da Tijuca",
                                                             "4 :Leblon",
                                                             "5 :Botafogo",
                                                             "6 :Santa Teresa",
                                                             "7 :Flamengo",
                                                             "8 :Leme",
                                                             "9 :Lapa",
                                                             "10 :Recreio\n dos Bandeirantes"
  ))
  
  ggplot(data = figure3_data, aes(x=date, y=value, col = listing_neighborhood)) +
    geom_line(aes(group = listing_neighborhood)) +
    # geom_area(aes(group = listing_neighborhood)) + 
    labs(title = 'Top 10 Neighborhoods of Listings', x = 'Date (Monthly Aggregated)', y = 'Number of Successful Bookings') +
    labs(col = 'Neighborhoods')
}

##:::::::::::::::::::::::::::::::::
## FIGURE 2
##:::::::::::::::::::::::::::::::::
{
  #grab top 10 countries
  summary_countries = as.data.frame(summary(working_data_users$country))
  summary_countries = cbind(row.names(summary_countries),summary_countries)
  colnames(summary_countries) = c('Country', 'Frequency')
  top_countries = summary_countries[order(-summary_countries$Frequency),]
  top10countries = summary_countries[order(-summary_countries$Frequency),][1:10,]
  names_top10countries = as.vector(top10countries$Country) 

  #guest top countries 
  figure2_data_raw = working_data_guest[,c("ts_interaction_first", 
                                            "country", "booked_flag")]
  figure2_data = figure2_data_raw[which(figure2_data_raw$booked_flag == TRUE),]
  figure2_data = figure2_data[which(figure2_data$country %in% names_top10countries),]
  figure2_data = figure2_data[figure2_data$ts_interaction_first < as.Date("2016-06-30"),]
  figure2_data = figure2_data %>% mutate(
    date = format(as.Date(ts_interaction_first), "%Y-%m"),
    # date = as.Date(ts_interaction_first),
    ts_interaction_first = NULL,
    booked_flag = NULL,
    freq = 1
  )
  
  figure2_data = cast(figure2_data, date ~ country)
  figure2_data = melt(figure2_data, id.vars = date)
  figure2_data$country = as.character(figure2_data$country)
  glimpse(figure2_data)
  
  {
    figure2_data$country[which(figure2_data$country == "BR")] = "1 :Brazil"
    figure2_data$country[which(figure2_data$country == "US")] = "2 :United States"
    figure2_data$country[which(figure2_data$country == "AR")] = "3 :Argentina"
    figure2_data$country[which(figure2_data$country == "FR")] = "4 :France"
    figure2_data$country[which(figure2_data$country == "GB")] = "5 :United Kingdom"
    figure2_data$country[which(figure2_data$country == "DE")] = "6 :Germany"
    figure2_data$country[which(figure2_data$country == "CL")] = "7 :Chile"
    figure2_data$country[which(figure2_data$country == "AU")] = "8 :Australia"
    figure2_data$country[which(figure2_data$country == "CA")] = "9 :Canada"
    figure2_data$country[which(figure2_data$country == "NL")] = "10 :Netherlands"
  }
  
  #order our legend
  figure2_data$country = factor(figure2_data$country, levels = c("1 :Brazil",
                              "2 :United States",
                              "3 :Argentina",
                              "4 :France",
                              "5 :United Kingdom",
                              "6 :Germany",
                              "7 :Chile",
                              "8 :Australia",
                              "9 :Canada",
                              "10 :Netherlands"
                                ))
  
  ggplot(data = figure2_data, aes(x=date, y=value, col = country)) +
    geom_line(aes(group = country)) +
    labs(title = 'Top 10 Countries of Guests', x = 'Date (Monthly Aggregated)', y = 'Number of Successful Bookings') +
    labs(col = 'Countries') 
}


#MODEL BUILDING (SLIDE 7)
{
  model_data = figure1_data
  
  #MODEL TO PREDICT OBJECTIVE ONE
  {
    model_data_objective1 = aggregate(value ~ date, data=model_data, FUN=sum)
    qplot(data = model_data_objective1, date, group =1, value, geom='line')
    
    #LINEAR REGRESSION (has the lower RMSE)
    y_temp = model_data_objective1$value
    x_temp = c(1:length(model_data_objective1$date))
    m1_linear = lm(y_temp ~ x_temp)
    summary(m1_linear)
    m1_linear_rmse = sqrt(mean(m1_linear$residuals^2))
    
    #linear regression's prediction on slide 7 for objective 1
    predict(m1_linear, data.frame(x_temp = c(length(x_temp) + 1)))
    
    #ARIMA MODEL (TIME SERIES)
    m1_arima = auto.arima(model_data_objective1$value)
    summary(m1_arima)
    predict(m1_arima)
  }
  
  #MODEL TO PREDICT OBJECTIVE TWO
  {
    model_data_objective2 = model_data %>% filter(booked_flag == TRUE)
    qplot(data = model_data_objective2, date, group =1, value, geom='line')
    
    #LINEAR REGRESSION (has the lower RMSE)
    y_temp = model_data_objective2$value
    x_temp = c(1:length(model_data_objective2$date))
    m2_linear = lm(y_temp ~ x_temp)
    summary(m2_linear)
    m2_linear_rmse = sqrt(mean(m2_linear$residuals^2))
    
    #linear regression's prediction on slide 7 for objective 2
    predict(m2_linear, data.frame(x_temp = c(length(x_temp) + 1)))
    
    #ARIMA MODEL (TIME SERIES)
    m2_arima = auto.arima(model_data_objective2$value)
    summary(m2_arima)
    predict(m2_arima)
  }
}  
 
