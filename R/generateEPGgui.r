################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 07.05.2013: name change function importGM() -> import()

#' @title Generate EPG GUI
#'
#' @description
#' \code{generateEPGgui} is a GUI simplifying the process of generating an EPG.
#'
#' @details
#' A graphical user interface (GUI) simplifying the use of generating an 
#' electropherogram from typing data. Support different kits (to be improved)
#' by drop-down meny. Manually enter a profile, or select a data frame from 
#' workspace, or import data from a text file. View and edit the data.
#' Generate EPG. Possible to save the EPG as a png image file.
#' @export
#' @examples
#' \dontrun{
#' # Open the graphical user interface.
#' generateEPGgui()
#' }

generateEPGgui <- function(){
  
  require(strvalidator)
  require(gWidgets)
  options("guiToolkit"="RGtk2")
  
  # Global variables.
  typingData <- NULL
  dnaProfile <- NULL
  debug <- FALSE
  epg <- NULL
  
  
  w <- gwindow(title="EPG generator",
               visible = FALSE,
               name=title)
  
  g <- ggroup(horizontal = FALSE, 
              spacing = 5,
              use.scrollwindow = FALSE,
              container = w)
  
  # TOOLS #####################################################################


  # STR TYPING KIT ------------------------------------------------------------
  
  frame_0 <- gframe(text = "STR typing kit",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = g,
                            expand=TRUE) 
  
  grid_0 <- glayout(container = frame_0)
  
  grid_0[1,1] <- glabel("", container=grid_0) # Adds some space.
  
  grid_0[2,1] <- profile_lbl <- glabel(text="The following kit will be used:",
                                               container=grid_0,
                                               anchor=c(-1 ,0))
  
  grid_0[2,2] <- profile_kit_drop <- gdroplist(items=getKit(), 
                                                       selected = 3,
                                                       editable = FALSE,
                                                       container = grid_0) 
  
  
  addHandlerChanged(profile_kit_drop, handler = function(h, ...) {
    val <- svalue (h$obj)
    
    typingData <<- data.frame(Marker=getKit(val)$locus,
                             Allele.1="NA",
                             Allele.2="NA",
                             Height.1="NA",
                             Height.2="NA",
                             stringsAsFactors=FALSE)
    
    profile_tbl[,] <- typingData
    
  } )
  
  
  
  grid_0[3,1] <- glabel("", container=grid_0) # Adds some space.
  
  
  
  glabel("", container=g) # Adds some space.

  
  # DNA PROFILE --------------------------------------------------------------
  
  frame_1 <- gframe(text = "Create profile",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = g,
                            expand=TRUE) 
  
  
  grid_1 <- glayout(container = frame_1)
  
  grid_1[1,1] <- glabel("", container=grid_1) # Adds some space.
  
  profile_options <- c("Manually enter a profile",
                       "Select a data frame from workspace",
                       "Import a profile from file")
  
  grid_1[2,1:2] <- profile_opt <- gradio(items=profile_options,
                                                 selected = 1,
                                                 horizontal = FALSE,
                                                 container = grid_1)
  
  addHandlerChanged(profile_opt, handler = function(h, ...) {
    val <- svalue (h$obj, index=TRUE)
    
    # Disable all.
    enabled(profile_ws_btn) <- FALSE      
    enabled(profile_ws_drp) <- FALSE      
    enabled(profile_file_browser) <- FALSE
    enabled(profile_import_btn) <- FALSE      
    
    # Enable allowed buttons.    
    if (val == 2 ){
      enabled(profile_ws_drp) <- TRUE      
      enabled(profile_ws_btn) <- TRUE
      wsObj <- ls(.GlobalEnv)
      wsObj <- .filterDf(wsObj)
      profile_ws_drp[] <- wsObj
      
    } else if (val == 3 ){
      enabled(profile_file_browser) <- TRUE      
      enabled(profile_import_btn) <- TRUE
      
    }
  } )
  
  grid_1[3,1] <- profile_ws_lbl <- glabel(text="Select from workspace:",
                                                  container=grid_1,
                                                  anchor=c(-1 ,0))
  
  wsObj <- ls(.GlobalEnv)
  
  # Input: a list of object names in the global environment.
  # Returns: a list of objec names of type data.frame.
  .filterDf <- function (x) {
    
    classes <- character(length(x))
    
    for(i in seq(along=x)){
      obj <- get(x[i])
      classes[i] <- class(obj)
      
    }
    
    dfs <- x[classes=="data.frame"]
    
    return(dfs)
  }
  
  wsObj <- .filterDf(wsObj)
  grid_1[3,2] <- profile_ws_drp <- gdroplist(items=wsObj,
                                                     selected = 1,
                                                     editable = FALSE,
                                                     container = grid_1) 
  enabled(profile_ws_drp) <- FALSE        
  
  addHandlerChanged(profile_ws_drp, handler = function(h, ...) {
    val_obj <- svalue(profile_ws_drp)
    
    if (!is.na(val_obj) && !is.null(val_obj)){
      
      dnaProfile <<- get(val_obj)
      
      dnaProfile <<- trim(data=dnaProfile, samples=NULL,
                         columns="Marker|Allele|Height", ignoreCase=TRUE,
                         invertS=FALSE, invertC=FALSE, rmNaCol=TRUE,
                         rmEmptyCol=TRUE, missing=NA)
      
      profile_tbl[,] <- dnaProfile
    } 
  } )
  
  grid_1[3,3] <- profile_ws_btn <- gbutton(text = "Select",
                                                   border=TRUE, container = grid_1)
  enabled(profile_ws_btn) <- FALSE      
  
  addHandlerChanged(profile_ws_btn, handler = function(h, ...) {
    val_obj <- svalue(profile_ws_drp)
    
    if (!is.na(val_obj) && !is.null(val_obj)){
      
      dnaProfile <<- get(val_obj)
      
      dnaProfile <<- trim(data=dnaProfile, samples=NULL,
                         columns="Marker|Allele|Height", ignoreCase=TRUE,
                         invertS=FALSE, invertC=FALSE, rmNaCol=TRUE,
                         rmEmptyCol=TRUE, missing=NA)
      
      profile_tbl[,] <- dnaProfile
    } 
  } )
  
  grid_1[4,1] <- profile_file_lbl <- glabel(text="Import from file:",
                                                    container=grid_1,
                                                    anchor=c(-1 ,0))
  
  profileText <- "Select for import..."
  grid_1[4,2] <- profile_file_browser <- gfilebrowse(text=profileText,
                                                             quote=FALSE,
                                                             type="open",
                                                             container=grid_1)
  enabled(profile_file_browser) <- FALSE      
  
  # Import button is here because handler is only triggered when hitting <Enter>
  grid_1[4,3] <- profile_import_btn <- gbutton(text = "Import",
                                                       border=TRUE, container = grid_1)
  
  enabled(profile_import_btn) <- FALSE      
  
  addHandlerChanged(profile_file_browser, handler = function(h, ...) {
    val <- svalue(profile_file_browser)
    
    if (file.exists(val)){
      
      dnaProfile <<- import(resultFiles=val)
      
      dnaProfile <<- trim(data=dnaProfile, samples=NULL,
                         columns="Marker|Allele|Height", ignoreCase=TRUE,
                         invertS=FALSE, invertC=FALSE, rmNaCol=TRUE,
                         rmEmptyCol=TRUE, missing=NA)
      
      profile_tbl[,] <- dnaProfile
    } else {
      
      gmessage(message="File not found!",
               title="Error",
               icon = "error")      
      
    } 
  } )
  
  # Import button is here because handler is only triggered when hitting <Enter>
  addHandlerChanged(profile_import_btn, handler = function(h, ...) {
    val <- svalue(profile_file_browser)
    
    if (file.exists(val)){
      
      dnaProfile <<- import(resultFiles=val)
      
      dnaProfile <<- trim(data=dnaProfile, samples=NULL,
                         columns="Marker|Allele", ignoreCase=TRUE,
                         invertS=FALSE, invertC=FALSE, rmNaCol=TRUE,
                         rmEmptyCol=TRUE, missing=NA)
      
      profile_tbl[,] <- dnaProfile
    } else {
      
      gmessage(message="File not found!",
               title="Error",
               icon = "error")      
      
    }
  } )
  
  
  # EDIT PROFILE ----------------------------------------------------------------
  
  frame_2 <- gframe(text = "View and edit profile",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = g,
                            expand=TRUE) 
  
  
  grid_2 <- glayout(container = frame_2)
  
  
  typingData <- data.frame(Marker=getKit(svalue(profile_kit_drop))$locus,
                           Allele.1="NA",
                           Allele.2="NA",
                           Height.1="NA",
                           Height.2="NA",
                           stringsAsFactors=FALSE)
  
  grid_2[1,1] <- profile_tbl <- gdf(items=typingData,
                                            container = grid_2)
  
  
  size(profile_tbl) <- c(600, 300)
  
  
  
  # EPG -----------------------------------------------------------------------
  
  frame_3_txt_width <- 8 
  
  frame_3 <- gframe(text = "Elechtropherogram",
                        markup = FALSE,
                        pos = 0,
                        horizontal=TRUE,
                        container = g,
                        expand=TRUE) 
  
  
  grid_3 <- glayout(container = frame_3)
  
  grid_3[1,1] <- glabel("", container=grid_3) # Adds some space.
  
  
  grid_3[2,1] <- epg_title_lbl <- glabel(text="Plot title:",
                                                 container=grid_3,
                                                 anchor=c(-1 ,0))
  
  grid_3[2,2] <- epg_title_txt <- gedit(text="",
                                                width=20,
                                                initial.msg="",
                                                container=grid_3)
  
  grid_3[2,3:4] <- epg_btn <- gbutton(text = "Generate EPG",
                                                border=TRUE,
                                                container = grid_3) 
  
  grid_3[3,1:2] <- epg_name_lbl <- glabel(text="Image name:",
                                                  container=grid_3,
                                                  anchor=c(-1 ,0))
  grid_3[3,3] <- epg_w_lbl <- glabel(text="Width:",
                                             container=grid_3,
                                             anchor=c(-1 ,0))
  grid_3[3,4] <- epg_h_lbl <- glabel(text="Height:",
                                             container=grid_3,
                                             anchor=c(-1 ,0))
  grid_3[3,5] <- epg_res_lbl <- glabel(text="Resolution:",
                                               container=grid_3,
                                               anchor=c(-1 ,0))
  
  grid_3[4,1:2] <- epg_name_txt <- gedit(text="",
                                                 width=15,
                                                 initial.msg=".png is added automatically",
                                                 container=grid_3)
  
  grid_3[4,3] <- epg_w_txt <- gedit(text="3000",
                                            width=4,
                                            initial.msg="",
                                            container=grid_3)
  
  grid_3[4,4] <- epg_h_txt <- gedit(text="2000",
                                            width=4,
                                            initial.msg="",
                                            container=grid_3)
  
  grid_3[4,5] <- epg_res_txt <- gedit(text="250",
                                              width=4,
                                              initial.msg="",
                                              container=grid_3)
  
  grid_3[5,1:5] <- epg_paht_lbl <- glabel(text="File path:",
                                                  container=grid_3,
                                                  anchor=c(-1 ,0))
  epgDefText <- "Select folder..."
  grid_3[6,1:5] <- epg_path_brw <- gfilebrowse(text=epgDefText,
                                                       quote=FALSE,
                                                       type="selectdir",
                                                       container=grid_3)
  
  grid_3[7,1:5] <- epg_save_btn <- gbutton(text = "Save EPG",
                                                   border=TRUE,
                                                   container = grid_3) 
  
  
  grid_3[8,1] <- glabel("", container=grid_3) # Adds some space.
  
  
  addHandlerChanged(epg_btn, handler = function(h, ...) {
    
    # Load all settings.
    val_title <- svalue(epg_title_txt)
    val_kit <- svalue(profile_kit_drop)
    typingData <<- profile_tbl[,]

    # Generate EPG.      
    epg <<- generateEPG(data=typingData, 
                           kit=val_kit, 
                           plotTitle=val_title, 
                           debugInfo=debug)
    
  } )
  
  addHandlerChanged(epg_save_btn, handler = function(h, ...) {
    
    # Disable all buttons until simulation is finished.
    enabled(epg_save_btn) <- FALSE
    
    
    # Load all settings.
    val_name <- svalue(epg_name_txt)
    val_w <- as.numeric(svalue(epg_w_txt))
    val_h <- as.numeric(svalue(epg_h_txt))
    val_res <- as.numeric(svalue(epg_res_txt))
    val_path <- svalue(epg_path_brw)
    val_OK <- TRUE
    
    if(debug){
      print("val_name:")
      print(val_name)
      print("val_w:")
      print(val_w)
      print("val_h:")
      print(val_h)
      print("val_res:")
      print(val_res)
      print("val_path:")
      print(val_path)
    }
    
    if(val_name == ""){
      val_OK <- FALSE
    }
    
    if(val_path == epgDefText || val_path == ""){
      val_OK <- FALSE
    }
    
    if(val_w <20 || is.na(val_w)){
      val_OK <- FALSE
    }
    
    if(val_h <20 || is.na(val_h)){
      val_OK <- FALSE
    }
    
    if(val_res <20 || is.na(val_res)){
      val_OK <- FALSE
    }
    
    if(debug){
      print("val_OK:")
      print(val_OK)
      print("file:")
      print(paste(val_path, val_name, ".png", sep=""))
    }
    
    if(!is.null(epg) && val_OK){
      
      separator <- .Platform$file.sep
      
      # Save EPG.      
      png(filename=paste(val_path, separator, val_name, ".png", sep=""),  
          width=val_w, height=val_h, res=val_res)
      plot(epg)
      dev.off()
      
    }
    
    if(!val_OK){
      
      gmessage(message="File name, path, width (>20), height (>20), and resolution (>20) must be provided.",
               title="Error",
               icon = "error")      
      
    }
    # Enable buttons.
    enabled(epg_save_btn) <- TRUE
    
  } )
  
  # END GUI ###################################################################
  
  
  # Show GUI
  visible(w) <- TRUE
  
  return(NULL)  
}
