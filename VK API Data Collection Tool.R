

# Load Libraries ####
library(httr2)
library(jsonlite)
library(tidyverse)

# Load Custom Functions ####

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
  languages <- resp$response |>
    map(function(x) {
      # Flatten nested elements
      x$personal$langs
    }) |>
    map(unlist) |>
    map_if(is.null, as.character) |>
    map(t) |> 
    map(as.data.frame) |>
    map(~add_column(.,temp = NA, .name_repair = 'minimal'))  |> # this row is needed for the posts that do not have attachments
    bind_rows() |> 
    rename_with(~str_replace(.x, "^V", "Language")) |> 
    select(-any_of("temp"))
  
  
  ##iterate through the main list structure
  df <- resp$response |> 
    map(unlist) |>
    map_if(is.null, as.character) |>
    map(t) |> 
    map(as.data.frame) |>
    map(~add_column(.,temp = NA, .name_repair = 'minimal'))  |> # this row is needed for the posts that do not have attachments
    bind_rows() |>
    select(-contains('personal.langs'), -contains('universities'), -contains('university'), -contains('faculty'), -contains('graduation')) |>
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
    
    
    cat(paste('Finished retrieving alter', i, 'of', total_alters, 'with method',deparse(substitute(method)),'| '))
    
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
 
 
 # get wall for every unique user
 wall <- tibble()
 for( i in seq_along(attr_list$to)){
   
   
  
   user <- attr_list$to[i] 
   total_user <- length(attr_list$to)
   
   user_wall <- wall_get(user_id = user, access_token = access_token, count = wall_count)
   
   wall <- bind_rows(wall, user_wall)
   
   # Pause to avoid hitting rate limits
   Sys.sleep(0.3)
   
   cat(paste('Finished retrieving wall posts for user', i, 'of', total_user, '\n'))
 }
 
 
  network_list <- list("edge_list" = edge_list, "attr_list" = attr_list, "wall_posts" = wall)
  
  
 if(save_as_RDS == T) {
   
   file_name <- paste0("ego_network_id", ego$id[1], ".rds")
   saveRDS(network_list, file = file_name)
   
   cat(paste('Saved', file_name, 'in current wd'))
   
 } else{
   
   return(network_list)

 }

 
 
}
 
 
# Get Ego-Network ####

## Set Arguments ####

#setwd
setwd("") #location where the file is being saved

## VK access token 
#browseURL("https://vkhost.github.io/") #to get you access token
#browseURL("https://dev.vk.com/en/api/api-requests") #to view API documentation

access_token <- ""

user_id <- ""  # bilingual user id

n_user <- 50   # this argument decides how many friends are being scraped and is multiplied by 100 (e.g. 50 * 100 = max 5000 friends)

wall_count <- 100 # how many user posts are being scraped (note: the API method excepts max 100 posts per request)



## Execute Function ####

ego_network_get(user_id = user_id, access_token = access_token, n_user = n_user, degree_1.5 = TRUE, wall_count = wall_count, save_as_RDS = TRUE) 
 
# create a list a loop through it

user_list <- list("")


users_ego_network <- lapply(user_list_6, function(user) {
  ego_network_get(user_id = user, access_token = access_token, 
                  n_user = n_user, degree_1.5 = TRUE, 
                  wall_count = wall_count, save_as_RDS = FALSE)
})

saveRDS(users_ego_network, file = "")




