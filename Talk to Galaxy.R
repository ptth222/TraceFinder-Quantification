## These functions handle the HTTP requests to Galaxy for submitting a file to be NA corrected and downloading the corrected version.



################################
## Get the username and password for Galaxy
################################

get_galaxy_login <- function(){
  ## Create a pop up window to get a username and password for Galaxy.
  tt <- tktoplevel()
  tkfocus(tt)
  tkwm.title(tt, "Galaxy Log In")
  tkgrid(ttklabel(tt, text = "Please enter your username and password for Galaxy." ), columnspan = 2)
  tkgrid(ttklabel(tt, text = "This is the same as the username and password for the CESB wiki." ), columnspan = 2)
  tkgrid(ttklabel(tt,text="    " ))
  
  username_label <- tklabel(tt, text="Username: ")
  username_entry_variable <- tclVar("")
  username_entry <- tkentry(tt, background = "white", textvariable = username_entry_variable, width = 20)
  
  tkgrid(username_label, username_entry)
  tkgrid.configure(username_label, sticky = "e")
  tkgrid.configure(username_entry, sticky = "w")
  
  password_label <- tklabel(tt, text="Password: ")
  password_entry_variable <- tclVar("")
  password_entry <- tkentry(tt, background = "white", textvariable = password_entry_variable, width = 20, show = "*")
  
  
  tkgrid(password_label, password_entry)
  tkgrid.configure(password_label, sticky = "e")
  tkgrid.configure(password_entry, sticky = "w")
  tkgrid(ttklabel(tt,text="    " ))
  
  ## Add a variable to capture whether the done button was pressed or the x.
  test <- tclVar("")
  tclvalue(test) <- 0
  
  ## Add button to close the window.
  login_check <- function(){
    
    if(tclvalue(username_entry_variable) == ""){
      tkconfigure(error_label, text = "Error. No username entered.", foreground = "red")
    }
    else if(tclvalue(password_entry_variable) == ""){
      tkconfigure(error_label, text = "Error. No password entered.", foreground = "red")
    }
    else{
      tclvalue(test) <- 1
    }
  }
  tkgrid(tkbutton(tt, text='Submit', command = login_check), columnspan = 2)
  error_label <- tklabel(tt)
  tkgrid(error_label, columnspan = 2)
  tkbind(tt, "<Return>", login_check)
  tkbind(tt, "<Destroy>", function() tclvalue(test)<-2)
  tkwait.variable(test)
  testval <- as.integer(tclvalue(test))
  tkdestroy(tt)
  
  if(testval == 2) {stop()}
  
  ## Turn tcl variables into regular ones.
  username <- tclvalue(username_entry_variable)
  password <- tclvalue(password_entry_variable)
  
  return(list(username=username, password=password))
}



######################################
## Log in to Galaxy
######################################

galaxy_login <- function(){
  
  repeat{
    
    temp_return <- get_galaxy_login()
    
    username <- temp_return$username
    password <- temp_return$password
  
    ## Send a GET request to Galaxy to log in and get a cookie.
    try_result <- try(r <- GET("https://galaxy.cesb.uky.edu/", authenticate(username, password, type = "basic"), add_headers("Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8", "Accept-Encoding" = "gzip, deflate, sdch, br", "Accept-Language" = "en-US,en;q=0.8", "Connection" = "keep-alive", "Upgrade-Insecure-Requests" = "1", "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")))
    
    ## Check for curl error after sending a request. Curl should only ever really error if there is no 
    ## internet connection.
    if(class(try_result) == "try-error"){
      ## Create a variable to store the button pressed by the user. Stored as a character string of
      ## which button was pressed.
      response <- tkmessageBox(title = "Curl Error", 
                               message = "Curl error when logging in to Galaxy. The server may be down. Check your internet connection and try again.", 
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## Check to make sure the http response is 200 OK. If not try to give the user a decent error message.
    if(r$status_code != "200"){
      if(r$status_code == "401"){
        response <- tkmessageBox(title = "Login Error", 
                                 message = "Error when logging in to Galaxy. Check your username and password and try again.", 
                                 icon = "error", type = "retrycancel")
        
        if(tclvalue(response) == "cancel"){
          stop()
        } else{
          next()
        }
        
      } else{
        response <- tkmessageBox(title = "Login Error", 
                                 message = paste("Error when logging in to Galaxy. HTTP error ", r$status_code, ".", sep = ""),
                                 icon = "error", type = "retrycancel")
        
        if(tclvalue(response) == "cancel"){
          stop()
        } else{
          next()
        }
        
      }
    }
    
    ## If the request goes through successfully, break the loop and proceed with execution of the program.
    if(r$status_code == "200"){break()}
  }
  
    return(list(r=r, username=username, password=password))
}


#############################
## Request history from Galaxy. If this request is not made then Galaxy won't accept new files.
############################

galaxy_history <- function(username, password){

  ## Keep trying to send each request until the user cancels or it is successful.
  repeat{
    ## If you don't send this GET request Galaxy won't let you POST the file.
    try_result<- try(r <- GET("https://galaxy.cesb.uky.edu/history/current_history_json", authenticate(username, password, type = "basic"), add_headers("Accept" = "*/*", "Accept-Encoding" = "gzip, deflate, sdch, br", "Accept-Language" = "en-US,en;q=0.8", "Connection" = "keep-alive", "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36", "Referer" = "https://galaxy.cesb.uky.edu/", "X-Requested-With" = "XMLHttpRequest")))
    
    ## Check for curl error after sending a request. Curl should only ever really error if there is no 
    ## internet connection.
    if(class(try_result) == "try-error"){
      ## Create a variable to store the button pressed by the user. Stored as a character string of
      ## which button was pressed.
      response <- tkmessageBox(title = "Curl Error", 
                               message = "Curl error requesting history from Galaxy. The server may be down. Check your internet connection and try again.", 
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## Check to make sure the http response is 200 OK. If not try to give the user a decent error message.
    if(r$status_code != "200"){
      response <- tkmessageBox(title = "HTTP Error", 
                               message = paste("Error requesting history from Galaxy. HTTP error ", r$status_code, ".", sep = ""),
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## If the request goes through successfully, break the loop and proceed with execution of the program.
    if(r$status_code == "200"){break()}
  }
  
  return(r)
}


#############################
## Upload the file created previously to Galaxy.
#############################


galaxy_upload <- function(username, password, body){

  repeat{
    ## Send the POST request to upload the file to Galaxy.
    try_result <- try(r <- POST("https://galaxy.cesb.uky.edu/api/tools", body = body, authenticate(username, password, type = "basic"), content_type("multipart/form-data"), add_headers("Cache-Control" = "no-cache", "Connection" = "keep-alive", "Origin" = "https://galaxy.cesb.uky.edu", "Referer" = "https://galaxy.cesb.uky.edu/", "X-Requested-With" = "XMLHttpRequest", "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36", "Expect" = "", "Accept" = "application/json", "Accept-Encoding" = "gzip, deflate, br", "Accept-Language" = "en-US,en;q=0.8")))
    
    ## Check for curl error after sending a request. Curl should only ever really error if there is no 
    ## internet connection.
    if(class(try_result) == "try-error"){
      ## Create a variable to store the button pressed by the user. Stored as a character string of
      ## which button was pressed.
      response <- tkmessageBox(title = "Curl Error", 
                               message = "Curl error uploading file to Galaxy. The server may be down. Check your internet connection and try again.", 
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## Check to make sure the http response is 200 OK. If not try to give the user a decent error message.
    if(r$status_code != "200"){
      response <- tkmessageBox(title = "HTTP Error", 
                               message = paste("Error uploading file to Galaxy. HTTP error ", r$status_code, ".", sep = ""),
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## If the request goes through successfully, break the loop and proceed with execution of the program.
    if(r$status_code == "200"){break()}
  }

  return(r)
}


####################################
## Once the file is uploaded we have to keep asking Galaxy if it recieved it, and when it is ready to
## be used in tools.
####################################


galaxy_upload_status <- function(username, password, url, history_id, state){ 
  
  ## Send GET requests to check on the state of the uploaded file until it is ok.
  while(state != "ok") 
  {
    try_result <- try(r <- GET(url = url, authenticate(username, password, type = "basic"), add_headers("Accept" = "application/json, text/javascript, */*; q=0.01", "Accept-Encoding" = "gzip, deflate, sdch, br", "Accept-Language" = "en-US,en;q=0.8", "Connection" = "keep-alive", "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36", "Referer" = "https://galaxy.cesb.uky.edu/", "X-Requested-With" = "XMLHttpRequest")))
    ## Check for curl error after sending a request. Curl should only ever really error if there is no 
    ## internet connection.
    if(class(try_result) == "try-error"){
      ## Create a variable to store the button pressed by the user. Stored as a character string of
      ## which button was pressed.
      response <- tkmessageBox(title = "Curl Error", 
                               message = "Curl error when checking the status of file upload to Galaxy. The server may be down. Check your internet connection and try again.", 
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## Check to make sure the http response is 200 OK. If not try to give the user a decent error message.
    if(r$status_code != "200"){
      response <- tkmessageBox(title = "HTTP Error", 
                               message = paste("Error when checking the status of file upload to Galaxy. HTTP error ", r$status_code, ".", sep = ""),
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    content <- content(r)
    state <- content[[1]]$state
  }

  return(r)
}



##################################
## Configure the NA correction tool to run on the uploaded file.
##################################

galaxy_tool_configure <- function(username, password, Labelling){
  
  ## Build the JSON that will tell Galaxy how to configure the NA correction tool.
  if(Labelling == "C13"){
    NA_corrector_JSON <- sprintf('{"tool_id":"nacorrector","tool_version":"1.0.0","inputs":{"input":{"batch":false,"values":[{"src":"hda","hid":1,"id":"%s","name":"%s"}]},"formula_column":"2","intensity_column":"5","group_columns":["1","4"],"labeling|labels":"13C","labeling|column_13C":"3","labeling|column_2H":"-1","labeling|column_15N":"-1"}}', id, filename)
  } else{
    NA_corrector_JSON <- sprintf('{"tool_id":"nacorrector","tool_version":"1.0.0","inputs":{"input":{"batch":false,"values":[{"src":"hda","hid":1,"id":"%s","name":"%s"}]},"formula_column":"2","intensity_column":"6","group_columns":["1","5"],"labeling|labels":"13C,15N","labeling|column_13C":"3","labeling|column_2H":"-1","labeling|column_15N":"4"}}', id, filename)
  }
  
  repeat{
    ## Tell Galaxy to run NA correction on the file just uploaded.
    try_result <- try(r <- POST("https://galaxy.cesb.uky.edu/api/tools", body = NA_corrector_JSON, authenticate(username, password, type = "basic"),  add_headers("Content-Type" = "application/json", "Connection" = "keep-alive", "Origin" = "https://galaxy.cesb.uky.edu", "Referer" = "https://galaxy.cesb.uky.edu/tool_runner?tool_id=nacorrector", "X-Requested-With" = "XMLHttpRequest", "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36", "Expect" = "", "Accept" = "application/json, text/javascript, */*; q=0.01", "Accept-Encoding" = "gzip, deflate, br", "Accept-Language" = "en-US,en;q=0.8")))
    
    ## Check for curl error after sending a request. Curl should only ever really error if there is no 
    ## internet connection.
    if(class(try_result) == "try-error"){
      ## Create a variable to store the button pressed by the user. Stored as a character string of
      ## which button was pressed.
      response <- tkmessageBox(title = "Curl Error", 
                               message = "Curl error submitting to the NA correction tool in Galaxy. The server may be down. Check your internet connection and try again.", 
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## Check to make sure the http response is 200 OK. If not try to give the user a decent error message.
    if(r$status_code != "200"){
      response <- tkmessageBox(title = "HTTP Error", 
                               message = paste("Error submitting to the NA correction tool in Galaxy. HTTP error ", r$status_code, ".", sep = ""),
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## If the request goes through successfully, break the loop and proceed with execution of the program.
    if(r$status_code == "200"){break()}
  }
    
  return(r)
}


#################################
## Keep asking Galaxy if the file is ready for download until it is ready.
#################################

galaxy_NA_corrected_status <- function(username, password, url, state){
  
  ## Check on the status of the file being created until it is ok or errored.
  while(state != "ok" && state != "error") 
  {
    try_result <- try(r <- GET(url = url, authenticate(username, password, type = "basic"), add_headers("Accept" = "application/json, text/javascript, */*; q=0.01", "Accept-Encoding" = "gzip, deflate, sdch, br", "Accept-Language" = "en-US,en;q=0.8", "Connection" = "keep-alive", "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36", "Referer" = "https://galaxy.cesb.uky.edu/", "X-Requested-With" = "XMLHttpRequest")))
    
    ## Check for curl error after sending a request. Curl should only ever really error if there is no 
    ## internet connection.
    if(class(try_result) == "try-error"){
      ## Create a variable to store the button pressed by the user. Stored as a character string of
      ## which button was pressed.
      response <- tkmessageBox(title = "Curl Error", 
                               message = "Curl error checking status of NA correction. The server may be down. Check your internet connection and try again.", 
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## Check to make sure the http response is 200 OK. If not try to give the user a decent error message.
    if(r$status_code != "200"){
      response <- tkmessageBox(title = "HTTP Error", 
                               message = paste("Error checking status of NA correction. HTTP error ", r$status_code, ".", sep = ""),
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    content <- content(r)
    state <- content[[2]]$state
  }
    
    return(r)
}



## TODO It may be worth while to make another request for the errors when doing NA correction.
## Leaving this out for now, but may be good later.



#################################
## Once the NA corrected file is ready, download it from Galaxy.
#################################

galaxy_download <- function(username, password, url){
  
  repeat{
    ## Download the file.
    try_result <- try(r <- GET(url = url, authenticate(username, password, type = "basic"), add_headers("Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8", "Accept-Encoding" = "gzip, deflate, sdch, br", "Accept-Language" = "en-US,en;q=0.8", "Connection" = "keep-alive", "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36", "Referer" = "https://galaxy.cesb.uky.edu/", "Upgrade-Insecure-Requests" = "1")))
    
    ## Check for curl error after sending a request. Curl should only ever really error if there is no 
    ## internet connection.
    if(class(try_result) == "try-error"){
      ## Create a variable to store the button pressed by the user. Stored as a character string of
      ## which button was pressed.
      response <- tkmessageBox(title = "Curl Error", 
                               message = "Curl error downloading file from Galaxy. The server may be down. Check your internet connection and try again.", 
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## Check to make sure the http response is 200 OK. If not try to give the user a decent error message.
    if(r$status_code != "200"){
      response <- tkmessageBox(title = "HTTP Error", 
                               message = paste("Error downloading file from Galaxy. HTTP error ", r$status_code, ".", sep = ""),
                               icon = "error", type = "retrycancel")
      
      if(tclvalue(response) == "cancel"){
        stop()
      } else{
        next()
      }
    }
    
    ## If the request goes through successfully, break the loop and proceed with execution of the program.
    if(r$status_code == "200"){break()}
  }
    
    return(r)
  
}