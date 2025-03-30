#browseURL("https://raffaelevacca.github.io/egocentric-r-book/structure.html#apply-purrr")

# uploading needed libraries
library(httr2)
library(jsonlite)
library(tidyverse)

#setwd
setwd("C:/Users/hornu/OneDrive/Master Social Scientific Data Analysis/Courses/SIMZ-2025 - Master Thesis Course/20250206 Spring Thesis Work/Data Collection/data")

#browseURL("https://vkhost.github.io/") #to get you access token
#browseURL("https://dev.vk.com/en/api/api-requests") #to view API documentation


# https://api.vk.com/method/<имя-API-метода>?<параметры>
#<имя-API-метода> section name and API operations for a call, for example users.get or likes.add
#<параметры> parameters that are passed to the method in the query string, such as ...?v=5.199&p1=v1 . These are called query parameters.

#https://api.vk.com/method/status.get?<PARAMS>
# PARAMS ( optional ) — the input parameters of the corresponding method API, the sequence of pairs name=value separated by an ampersand. The parameter list is listed on the method description page. The following query parameters must be specified for each request:
#•access_token ( Mandatory ) — access key . Non-binding If it is indicated title .
#•v ( Mandatory ) is the API version USED. The use of this parameter affects the format of the responses of various methods. Now. current version of API — 5.199.
# Example: user_ids=743784474&fields=bdate&access_token=533bacf01e11f55b536a565b57531ac114461ae8736d6506a3&v=5.199 HTTP/1.1


# VK access token #####
access_token <- "vk1.a.7RCGumxfITyEglGdOQwSnngXdJGoFhuQKG1uPZoDHVTYu6ntXdiCk6YpG72IXV1voG0KxiscUCZPM1BgAt7gNnx1nPWeQ6esegeiCs7oCxGD-3so9hYOO40WJxeSq33BxQnNLHhGolky4qeo4Evcpzg5_aaWZDuXZjZmhrRP1sx6ufAvdI4ZUacFhELTZLRhiewyEi4MI-J-NqAOcA6j0g"


# Functions ####



wall_get <- function(user_id, access_token, count){
  
  
  resp <- request("https://api.vk.com/method/wall.get") |>
    req_url_query(
      access_token = access_token,
      domain = user_id,
      filter = "owner",
      count = count,
      v = "5.199",
      lang = "en"
    ) |>  
    req_perform() |>  
    resp_body_json()
  
  attachments  <- resp$response$items |> 
    map( \(x) x$attachments) |> 
    map(unlist) |> 
    map_if(is.null, as.character) |> 
    map(t) |>  
    map(as.data.frame) |> 
    map(~add_column(.,temp = NA, .name_repair = 'minimal'))  |>  # this row is needed for the posts that do not have attachments
    bind_rows() |> 
    select(any_of(c("link.url", "link.title", "link.caption")))
  
  re_posts <- resp$response$items |> 
    map( \(x) x$copy_history) |> 
    map(unlist) |> 
    map_if(is.null, as.character) |> 
    map(t) |> 
    map( as.data.frame) |>  
    map(~add_column(.,temp = NA, .name_repair = 'minimal'))  |>  # this row is needed for the posts that do not have attachments
    bind_rows() |> 
    select(any_of(c(attachment_text = "text"))) |> 
    bind_cols(attachments)
  
  
  wall_post <- resp$response$items |> 
    map_dfr(~ tibble(
      post_text = .x$text %||% "",  # Extract text, replace NULL with empty string
      post_date = as.POSIXct(.x$date, origin = "1970-01-01", tz = "UTC") # Convert UNIX timestamp to date
    )) |>  
    bind_cols(re_posts) |>  
    mutate(user_id = user_id)
  
  return(wall_post)
  
}


users_get <- function(user_ids, access_token, fields = NULL){
  
  # Check if fields is provided; if not, use a default value
  if (is.null(fields)) {
    fields <- "id, about, activities, bdate, city, country, domain, education, first_name, last_name, home_town, interests, is_favorite, is_friend, last_seen, personal, quotes, screen_name, sex, timezone, deactivated, is_closed, can_access_closed"
  }  
  

  resp <- request("https://api.vk.com/method/users.get") |>
    req_url_query(
      access_token = access_token,
      user_ids = user_ids,
      fields = fields,
      name_case = "nom",
      v = "5.199",
      lang = "en"
    ) |>  
    req_perform() |>  
    resp_body_json()
  
  
  #iterate through the nested list structure
  languages <- resp$response %>% 
    map(function(x) {
      # Flatten nested elements
      x$personal$langs
    }) %>% 
    map(unlist) %>%
    map_if(is.null, as.character) %>%
    map(t) %>% 
    map(as.data.frame) %>%
    map(~add_column(.,temp = NA, .name_repair = 'minimal'))  %>% # this row is needed for the posts that do not have attachments
    bind_rows() |> 
    rename_with(~str_replace(.x, "^V", "Language")) |> 
    select(-any_of("temp"))
  
  
  ##iterate through the main list structure
  df <- resp$response %>% 
    map(unlist) %>%
    map_if(is.null, as.character) %>%
    map(t) %>% 
    map(as.data.frame) %>%
    map(~add_column(.,temp = NA, .name_repair = 'minimal'))  %>% # this row is needed for the posts that do not have attachments
    bind_rows() %>%
    select(-contains('personal.langs'), -contains('universities'), -contains('university'), -contains('faculty'), -contains('graduation')) %>%
    bind_cols(languages) |>  #bind all the columns together
    select(-any_of("temp"))
  
  
  return(df)
  
}


friends_get <- function(user_id, access_token, n_user, alters_df = FALSE, degree_1.5 = FALSE){

  all_friends <- list()
  offset <- 0
  count <- 1000  # Maximum VK allows per request
  repeat {
    resp <- request("https://api.vk.com/method/friends.get") |>
      req_url_query(
        access_token = access_token,
        user_id = user_id,
        fields = "id, deactivated, is_closed, can_access_closed",
        count = count,
        offset = offset,
        v = "5.199",
        lang = "en"
      ) |>  
      req_perform() |>  
      resp_body_json()
    
    friends <- resp$response$items

    # Stop if there are no more friends
    if (length(friends) == 0) break
    
    all_friends <- append(all_friends, friends)
    
    # Increase offset for the next batch
    offset <- offset + count
  }
  
  friends_df <- all_friends |> 
    map(\(x) { 
      if (!"deactivated" %in% colnames(x) & x$can_access_closed == TRUE & x$is_closed == FALSE) {  # Remove User who's profile is private and are banned and cannot see the user profile 
        return(x$id)
      }
    }) |> 
    compact() |> # Remove NULL entries if any are returned when conditions are not met
    tibble(id = `compact(...)`) |> 
    select(-`compact(...)`) 
  

# if statement if 1.5 degree only
  if( degree_1.5 == TRUE){

    friends_df <- friends_df |> 
      filter(id %in% alters_df$to)

  } 

  valid_groups <- friends_df |> 
    mutate(group_ids = ceiling(seq_along(id)/100)) |>  # the user.get method only accepts max 100 ids at once
    filter(group_ids <= n_user)  #How many users? n_users times 100
  
  chunks <- split(valid_groups, valid_groups$group_ids) # Perform the split
  
  
  all_users <- list()  # Initialize an empty list to store results
  
  # Get the total number of chunks
  total_chunks <- length(chunks)
  
  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    
    # Combine IDs into a comma-separated string
    ids <- chunk$id |> 
      unlist() |> 
      paste(collapse = ",")
    
    # Get user data for the chunk
    save_users <- users_get(access_token = access_token, user_ids = ids)
    
    # Append the response to the all_users list
    all_users <- c(all_users, list(save_users))  # Efficiently append the new data
    
    # Pause to avoid hitting rate limits (optional)
    Sys.sleep(0.5)
    
    # Print chunk number and total chunks
    cat(paste('Finished processing chunk', i, 'of', total_chunks, 'with', length(chunk$id), 'users.\n'))
  }
  
  friends_df <- all_users|> 
    bind_rows()
  
  return(friends_df)
  
}



alters_get <- function(df, method, access_token = access_token, n_user, degree_1.5 = FALSE) { 
  
  
  alters_method <- tibble()
  total_alters <- length(df$"to")
  
  for(i in seq_along(df$"to")){
    
    alter <- df$"to"[i]
    
    alters <- do.call(method, args = list(user_id = alter, access_token = access_token, n_user = n_user, alters_df = df,  degree_1.5 = degree_1.5))
    
    if ("id" %in% colnames(alters)) {   # if the user has friends add the alter id to the alter_alter df
      alters <- alters |> 
        rename(to = id) |> 
        mutate(from = df$to[i]) |> 
        select(from,to, everything()) 
    }
        
    alters_method <- bind_rows(alters_method, alters)
    
    # Pause to avoid hitting rate limits
    Sys.sleep(0.5)
    
    
    cat(paste('Finished retrieving alter', i, 'of', total_alters, 'with method',deparse(substitute(method)),': '))
    
  }
  
  return(alters_method)
  
}


ego_network_get <- function(user_id, access_token = access_token, method = friends_get, n_user, degree_1.5, wall_count, save_as_RDS){

  
ego <- users_get(user_ids = user_id, access_token = access_token)

ego_friends <- friends_get(user_id = ego$id, access_token = access_token, n_user = n_user) |>  #n_user is times 100, so n_user = 50 retrieves 5000 user friends
  rename(to = id) |> 
  mutate(from = ego$id) |> 
  select(from, everything())  
  


 alters_friends <-  alters_get(df = ego_friends, method = friends_get, access_token = access_token, n_user = 50, degree_1.5 = degree_1.5)
 
 

 edge_list <- ego_friends |>   #create edge list 
   bind_rows(alters_friends) |> 
   select(from, to) |> 
   filter(!is.na(from) & !is.na(to) & from != to) |>  
   distinct()
 
 
 attr_list <- ego |>   
   rename(to = id) |> 
   bind_rows(ego_friends)  |> 
   bind_rows(alters_friends) |> 
   select(-from) |> 
   distinct(to, .keep_all = TRUE) |>  # Keeps only the first occurrence of each unique 'to' 
   mutate(
     time_data_collection = Sys.time(),  # Save date of data collection
     is_ego = ifelse(to == ego$id[1], 1, 0),  
     last_seen.time = as.POSIXct(as.numeric(last_seen.time), origin = "1970-01-01", tz = "UTC") # Convert properly
   )     
 
 
 #get wall for every unique user
 wall <- tibble()
 for( i in seq_along(attr_list$to)){
   
   
  
   user <- attr_list$to[i] 
   total_user <- length(attr_list$to)
   
   user_wall <- wall_get(user_id = user, access_token = access_token, count = wall_count)
   
   wall <- bind_rows(wall, user_wall)
   
   # Pause to avoid hitting rate limits
   Sys.sleep(0.3)
   
   cat(paste('Finished retrieving wall posts for alter', i, 'of', total_user, '\n'))
 }
 
 
  network_list <- list("edge_list" = edge_list, "attr_list" = attr_list, "wall_posts" = wall)
  
  
 if(save_as_RDS == T) {
   
   file_name <- paste0("ego_network_id", ego$id[1], ".rds")
   saveRDS(network_list, file = file_name)
   
 } else{
   
   return(network_list)
   
 }

 
 
}
 
 
# get ego network data ####
## "583048964" ####
#850793471
### function for full ego network

ego_network_1.5_id850793471 <- ego_network_get(user_id = "850793471", access_token = access_token, n_user = 50, degree_1.5 = TRUE, wall_count = 50, save_as_RDS = F) #n_user is times 100, so n_user = 50 retrieves 5000 user friends
 

wall_posts <- ego_network_1.5_id850793471$wall_posts
 
write.csv(ego_network_1.5_id850793471, "ego_friends_id850793471_friendsmax_5000.csv", row.names = FALSE)

 

 
## id179158619
 
ego_network_1.5_id179158619 <- ego_network_get(user_id = "179158619", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends


write.csv(ego_network_1.5_id179158619, "ego_friends_id179158619_friendsmax_5000.csv", row.names = FALSE)


## id95009612
ego_network_1.5_id95009612 <- ego_network_get(user_id = "95009612", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends


write.csv(ego_network_1.5_id95009612, "ego_friends_id95009612_friendsmax_5000.csv", row.names = FALSE)

id95009612
 
 
## kinesiologie_potsdam

ego_network_1.5_idkinesiologie_potsdam <- ego_network_get(user_id = "kinesiologie_potsdam", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends


write.csv(ego_network_1.5_id95009612, "ego_friends_idkinesiologie_potsdam_friendsmax_5000.csv", row.names = FALSE)
 
 length(unique(ego_network_1.5_id95009612$to))
 
 
 
## katrinreichwald
 
 
 ego_network_1.5_idkatrinreichwald <- ego_network_get(user_id = "katrinreichwald", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends
 
 
 write.csv(ego_network_1.5_idkatrinreichwald, "ego_friends_idkatrinreichwald_friendsmax_5000.csv", row.names = FALSE)
 
 
 
 
 
 ## s.serebrennikov
 ego_network_1.5_ids.serebrennikov <- ego_network_get(user_id = "s.serebrennikov", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends
 
 
 write.csv(ego_network_1.5_ids.serebrennikov, "ego_friends_ids.serebrennikov_friendsmax_5000.csv", row.names = FALSE)
 
 
 
 
 ## 330121965 
 
 ego_network_1.5_id330121965 <- ego_network_get(user_id = "330121965", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends
 
 
 write.csv(ego_network_1.5_id330121965, "ego_friends_id330121965_friendsmax_5000.csv", row.names = FALSE)
 
 
 
## kurthugentobler
 
 
 ego_network_1.5_idkurthugentobler <- ego_network_get(user_id = "kurthugentobler", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends
 
 
 write.csv(ego_network_1.5_idkurthugentobler, "ego_friends_idkurthugentobler_friendsmax_5000.csv", row.names = FALSE)
 
 ## lianek
 
 
 
 ego_network_1.5_idlianek <- ego_network_get(user_id = "lianek", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends
 
 
 write.csv(ego_network_1.5_idlianek, "ego_friends_idlianek_friendsmax_5000.csv", row.names = FALSE)
 
 
 
 
 ego_network_1.5_id521995446 <- ego_network_get(user_id = "521995446", access_token = access_token, n_user = 50, degree_1.5 = TRUE) #n_user is times 100, so n_user = 50 retrieves 5000 user friends
 
 
 ###

 
   
   
unique(alters_friends$alter_alter_id)
 
 
   test <- wall_get(user_id = "707333906", access_token = access_token)
   
   
   
   
   
   wall_get_alters <- function(df, access_token, count = 20) {
     
     wall <- tibble()
     unique_alters <- unique(df$to)
     total_alters <- length(unique_alters)
     
     for (i in seq_along(to)) {
       
       alter <- unique_alters[i]
       
       # Fetch wall posts
       posts <- wall_get(domain = alter, access_token = access_token, count = count)
       
       if (nrow(posts) > 0) {
         # Append alter_alter_id to each row
         posts <- posts |> mutate(alter_alter_id = alter)
       } else {
         # If no posts, add an empty row with alter_alter_id
         posts <- tibble(alter_alter_id = alter)
       }
       
       # Combine results
       wall_posts <- bind_rows(wall, posts)
       
       # Pause to avoid hitting rate limits
       Sys.sleep(0.3)
       
       cat(paste('Finished retrieving wall posts for alter', i, 'of', total_alters, '\n'))
     }
     
     return(wall_posts)
   }
   
   
 test_wide <- test %>%
   group_by(to) %>% 
   mutate(post_num = row_number()) %>%  # Create a post index per user
   pivot_wider(names_from = post_num, values_from = -c(to, post_num), names_prefix = "post_") %>%
   ungroup()
   
   
# to do's:
   #data collection wall get
   #get time 
   #add ego to the full datafame
   #save the ego 
   
   
   
   
   
   
   
   
   #set wd
   setwd("C:/Users/hornu/OneDrive/Master Social Scientific Data Analysis/Courses/SIMZ-2025 - Master Thesis Course/20250206 Spring Thesis Work")
   
   #load data
   ego_network <- read_csv("./Data Collection/data/ego_friends_id179158619_friendsmax_5000.csv")
   
   
   
   edge_list <- ego_network |> 
     select(from, to) |> 
     filter(!is.na(from) & !is.na(to) & from != to) |>  
     distinct()
   
   attr_list <- ego_network |> 
     distinct(to, .keep_all = TRUE)  # Keeps only the first occurrence of each unique 'to'
   
   
   
   
   
   
   
   
   
   
   
   
 resp <- request("https://api.vk.com/method/wall.get") |>
   req_url_query(
     access_token = access_token,
     domain = "850793471", 
     filter = "owner",
     count = "20",
     v = "5.199",
     lang = "en"
   ) |>  
   req_perform() |>  
   resp_body_json()
 
 
 
 
 attachments  <- resp$response$items |> 
   map( \(x) x$attachments) |> 
   map(unlist) |> 
   map_if(is.null, as.character) |> 
   map(t) |>  
   map(as.data.frame) |> 
   map(~add_column(.,temp = NA, .name_repair = 'minimal'))  |>  # this row is needed for the posts that do not have attachments
   bind_rows() |> 
   select(link.url, link.title, link.caption)
 
 wall_post <- resp$response$items |> 
   map( \(x) x$text) |> 
   map_if(is.null, as.character) |>  # Replace NULL values with an empty string
   map_chr(identity) |>  
   as.data.frame() |>  
   rename(post_text = 1) |> 
   bind_cols(attachments)
 
 
 
 
 
 
 
 
 #|> 
   map(unlist()) |> 
   map_if(is.null, as.character) |> 
   map(t) |>  
   map(as.data.frame) |> 
   map(~add_column(.,temp = NA, .name_repair = 'minimal'))  |>  # this row is needed for the posts that do not have attachments
   bind_rows()
   
 
 
 
 
 
 
 
 wall_df = data.frame(resp$posts$id, resp$posts$from_id, resp$posts$owner_id, resp$posts$date, resp$posts$text,
                     resp$posts$likes$count, resp$posts$comments$count, resp$posts$reposts$count)
 
 colnames(wal_df)[1:8] = c('id', 'from_id', 'owner_id', 'date', 'text', 'likes.count', 'comments.count', 'reposts.count')
 
 # unlisting attachments 
 
 if ('attachments' %in% colnames(wall$posts) & attachments = T){
   
   attachments = wall$posts$attachments %>% 
     map(unlist) %>%
     map_if(is.null, as.character) %>%
     map(t) %>% 
     map(as.data.frame) %>%
     map(~add_column(.,temp = NA, .name_repair = 'minimal'))  %>% # this row is needed for the posts that do not have attachments
     bind_rows() %>%
     select(matches('^type|link.url|link.title')) 
   
   colnames(attachments) = str_c('attachments.', colnames(attachments))
   wal_df = cbind(wal_df, attachments)
 }
 
 # unlisting reposts
 
 if ('copy_history' %in% colnames(wall$posts) & reposts = T){
   
   copy_history = wall$posts$copy_history %>% 
     map(unlist) %>%
     map_if(is.null, as.character) %>%
     map(t) %>% 
     map(as.data.frame) %>%
     map(~add_column(.,temp = NA, name_repair = 'minimal')) %>%
     bind_rows() %>%
     select(matches('^id|^owner_id|^from_id|^date|^post_type|^text|attachments.link.url|attachments.link.title')) 
   
   colnames(copy_history) = str_c('copy_history.', colnames(copy_history))
   wal_df = cbind(wal_df, copy_history)
 }
 
 
 
 alters_df <-  alters_friends |> 
   filter(country.title %in% c("Germany","Russia") #| city.title %in% c(German_cities, Russian_cities)
   )
 
 edges <- alters_df |> 
   select(from = alter_id, to = alter_alter_id) |>
   distinct()
 
 
 
 net <- graph_from_data_frame(edges, directed = FALSE)
 V(net)$country <- alters_df[match(V(net)$name,alters_df$alter_alter_id),]$country.title
 
 ggraph(net, layout = "fr") + 
   geom_edge_link() +  # Draw edges
   geom_node_point(aes(color = country), size = 1) +  # Color nodes by country
   scale_color_manual(values = c("Germany" = "blue", "Russia" = "red")) + # Customize colors for Germany and Russia
   theme_graph(base_family = 'Helvetica')  # Set graph theme and font
 
 