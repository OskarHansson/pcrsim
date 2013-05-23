################################################################################
# TODO LIST
# TODO: Check if file exist and ask for overwrite when saving...
# TODO: Add "advanced" setting "pcr efficiency" for "Amplification"
# TODO: Fix gui, add more controls and tidy up code...
# TODO: Option to use interlocus balance or not.

# NOTE:
# \u00B5 is the unicode for µ 

################################################################################
# CHANGE LOG
# 07.05.2013: name change function importGM() -> import()
# <07.05.2013: Added handlers for degradation.

#' @title GUI for PCR simulator
#'
#' @description
#' \code{pcrsim} is a GUI for simulation of the entire DNA process.
#'
#' @details
#' This graphical user interface make it very easy to simulate the forensic
#' DNA process. Detailes are entered in text boxes organised into tabs for
#' the respective subprocess. Simulation is performed and the result can
#' be viewed within the GUI or plotted as an electropherogram (EPG), 
#' saved to a text file, or as an R object in the global environment.
#' The EPG can be saved as a png-image.
#' @export
#' @examples
#' \dontrun{
#' # Open the graphical user interface.
#' pcrsim()
#' }

pcrsim <- function(){

  # Load dependencies.
  require(strvalidator)
  require(gWidgets)
  require(RGtk2)  ## Is this needed? Seems to be on a fresh install (without tlc/tk)
  options(guiToolkit="RGtk2")
  
  # Global variables.
  debug=FALSE
  simProfile <- NULL # data.frame.
  simData <- data.frame(Marker=NA, Allele=NA, Height=NA)
  simEPG <- NULL # ggplot2 object.
  separator <- .Platform$file.sep # Platform dependent path separator.

  # Main window.
  w <- gwindow(title="PCRsim",
               visible = FALSE,
               name=title)
  

  g <- ggroup(horizontal = TRUE, 
              spacing = 5,
              use.scrollwindow = FALSE,
              container = w)
  
  nb <- gnotebook(closebuttons = FALSE,
                  dontCloseThese = NULL,
                  container = g)
  
  
  # Define groups.
  start_gf <- ggroup(horizontal = FALSE,
                       spacing=10,
                       use.scrollwindow=FALSE,
                       container = nb,
                       label="Welcome",
                       expand=FALSE)

  profile_gf <- ggroup(horizontal = FALSE,
                      spacing=10,
                      use.scrollwindow=FALSE,
                      container = nb,
                      label="Profile",
                      expand=FALSE)
  
  sample_gf <- ggroup(horizontal = FALSE,
                   spacing=5,
                   use.scrollwindow=FALSE,
                   container = nb,
                   label="Sample",
                   expand=FALSE)
  
  ex_gf <- ggroup(horizontal = FALSE,
                   spacing=5,
                   use.scrollwindow=FALSE,
                   container = nb,
                   label="Extraction",
                   expand=FALSE)
  
  amp_gf <- ggroup(horizontal = FALSE,
                  spacing=5,
                  use.scrollwindow=FALSE,
                  container = nb,
                  label="Amplification",
                  expand=FALSE)
  
  ce_gf <- ggroup(horizontal = FALSE,
                   spacing=5,
                   use.scrollwindow=FALSE,
                   container = nb,
                   label="Analysis",
                   expand=FALSE)
  
  sim_gf <- ggroup(horizontal = FALSE,
                   spacing=5,
                   use.scrollwindow=FALSE,
                   container = nb,
                   label="Simulation",
                   expand=FALSE)
  
  # START #####################################################################
  
  glabel("", container=start_gf) # Adds some space.
  
  # STR TYPING KIT ------------------------------------------------------------
  
  start_frame_1 <- gframe(text = "PCR sim",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = start_gf,
                            expand=TRUE) 
  
  about_txt <- paste("PCR sim is a package for simulation of the forensic ",
                     "DNA process. This graphical user interface make it very ",
                     "easy to perform simulations. Parameters are entered in ",
                     "text boxes organised into tabs for the respective ",
                     "subprocess. Simulation is performed and the result can ",
                     "be viewed as a table within the GUI or plotted as an ",
                     "electropherogram (EPG), saved to a text file. The EPG ",
                     "can be saved as an image.\n\n",
                     "The simulator has to be calibrated to the quantification ",
                     "method used and for each capillary electrophoresis ",
                     "instrument in order to make realistic simulations.\n\n",
                     "Keep in mind that this is an early version still under ",
                     "development.\n\n",
                     "Please report bugs to:\n",
                     "https://github.com/OskarHansson/pcrsim/issues\n\n",
                     "The source is hosted at GitHub:\n",
                     "https://github.com/OskarHansson/pcrsim", sep="")
  
  gtext(text=about_txt, width = NULL, height = 300, font.attr = NULL, 
               wrap = TRUE, expand=TRUE, container = start_frame_1) 

  start_frame_2 <- gframe(text = "License",
                         markup = FALSE,
                         pos = 0,
                         horizontal=TRUE,
                         container = start_gf,
                         expand=TRUE) 
  
  license_txt <- paste("Copyright (C) 2013 Oskar Hansson\n\n",
        "This program is free software; you can redistribute it and/or ",
        "modify it under the terms of the GNU General Public License ",
        "as published by the Free Software Foundation; either version 2 ",
        "of the License, or (at your option) any later version.\n\n",
        "This program is distributed in the hope that it will be useful, ",
        "but WITHOUT ANY WARRANTY; without even the implied warranty of ",
        "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the ",
        "GNU General Public License for more details.\n\n",
        "You should have received a copy of the GNU General Public License ",
        "along with this program; if not, write to the Free Software ",
        "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, ",
        "MA  02110-1301, USA.", sep="")
  
  gtext(text=license_txt, width = NULL, height = 300, font.attr = NULL, 
        wrap = TRUE, expand=TRUE, container = start_frame_2) 
  
                     
  # PROFILE PARAMETERS ########################################################
  
  glabel("", container=profile_gf) # Adds some space.
  
  # STR TYPING KIT ------------------------------------------------------------
  
  profile_frame_1 <- gframe(text = "STR typing kit",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = profile_gf,
                            expand=FALSE) 
  
  profile_grid_1 <- glayout(container = profile_frame_1)
  
  profile_grid_1[1,1] <- glabel("", container=profile_grid_1) # Adds some space.
  
  profile_grid_1[2,1] <- profile_lbl <- glabel(text="The following kit will be used:",
                        container=profile_grid_1,
                        anchor=c(-1 ,0))
  
  profile_grid_1[2,2] <- profile_kit_drop <- gdroplist(items=getKit(), 
                                                         selected = 3,
                                                         editable = FALSE,
                                                         container = profile_grid_1) 

  
  addHandlerChanged(profile_kit_drop, handler = function(h, ...) {
    val <- svalue (h$obj)

    dnaProfile <- data.frame(Marker=getKit(val)$locus,
                             Allele.1="NA",
                             Allele.2="NA",
                             stringsAsFactors=FALSE)
    
    profile_tbl[,] <- dnaProfile

  } )
  
  
  
  profile_grid_1[3,1] <- glabel("", container=profile_grid_1) # Adds some space.
  
  # DNA PROFILE --------------------------------------------------------------
  
  profile_frame_2 <- gframe(text = "Create profile",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = profile_gf,
                            expand=FALSE) 
  
  
  profile_grid_2 <- glayout(container = profile_frame_2)
  
  profile_grid_2[1,1] <- glabel("", container=profile_grid_2) # Adds some space.
  
  profile_options <- c("Manually enter a profile",
                       "Select a data frame from workspace",
                       "Import a profile from file", 
                       "[Not implemented] Generate a random profile", 
                       "[Not implemented] Generate a random profile for each simulation")
  
  profile_grid_2[2,1:2] <- profile_opt <- gradio(items=profile_options,
                                     selected = 1,
                                     horizontal = FALSE,
                                     container = profile_grid_2)

  addHandlerChanged(profile_opt, handler = function(h, ...) {
    val <- svalue (h$obj, index=TRUE)
    
    # Disable all.
    enabled(profile_ws_btn) <- FALSE      
    enabled(profile_ws_drp) <- FALSE      
    enabled(profile_file_browser) <- FALSE      
    enabled(profile_pop_drp) <- FALSE      
    enabled(profile_import_btn) <- FALSE      
    enabled(profile_generate_btn) <- FALSE      
    
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
    
    # TODO: Implement
    #} else if ( val == 4 || val == 5) {
    #  enabled(profile_pop_drp) <- TRUE      
    #  enabled(profile_generate_btn) <- TRUE

    }
  } )
  
  profile_grid_2[3,1] <- profile_ws_lbl <- glabel(text="Select from workspace:",
                                                    container=profile_grid_2,
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
  profile_grid_2[3,2] <- profile_ws_drp <- gdroplist(items=wsObj,
                                                      selected = 1,
                                                      editable = FALSE,
                                                      container = profile_grid_2) 
  enabled(profile_ws_drp) <- FALSE        
  
  addHandlerChanged(profile_ws_drp, handler = function(h, ...) {
    val_obj <- svalue(profile_ws_drp)
    
    if (!is.na(val_obj) && !is.null(val_obj)){
      
      dnaProfile <- get(val_obj)
      
      dnaProfile <- trim(data=dnaProfile, samples=NULL,
                         columns="Marker|Allele", ignoreCase=TRUE,
                         invertS=FALSE, invertC=FALSE, rmNaCol=TRUE,
                         rmEmptyCol=TRUE, missing=NA)
      
      profile_tbl[,] <- dnaProfile
    } 
  } )

  profile_grid_2[3,3] <- profile_ws_btn <- gbutton(text = "Select",
                                                         border=TRUE, container = profile_grid_2)
  enabled(profile_ws_btn) <- FALSE      
  
  addHandlerChanged(profile_ws_btn, handler = function(h, ...) {
    val_obj <- svalue(profile_ws_drp)

    if (!is.na(val_obj) && !is.null(val_obj)){
    #if (!length(val_obj) > 0){
      
      dnaProfile <- get(val_obj)
      
      dnaProfile <- trim(data=dnaProfile, samples=NULL,
                         columns="Marker|Allele", ignoreCase=TRUE,
                         invertS=FALSE, invertC=FALSE, rmNaCol=TRUE,
                         rmEmptyCol=TRUE, missing=NA)
      
      profile_tbl[,] <- dnaProfile
    } 
  } )
  
  profile_grid_2[4,1] <- profile_file_lbl <- glabel(text="Import from file:",
                                                   container=profile_grid_2,
                                                   anchor=c(-1 ,0))
  
  # TODO: functions for storing and importing freq databases.
  profileText <- "Select for import..."
  profile_grid_2[4,2] <- profile_file_browser <- gfilebrowse(text=profileText,
                                                             quote=FALSE,
                                                             type="open",
                                                             container=profile_grid_2)
  enabled(profile_file_browser) <- FALSE      

  # Import button is here because handler is only triggered when hitting <Enter>
  profile_grid_2[4,3] <- profile_import_btn <- gbutton(text = "Import",
                      border=TRUE, container = profile_grid_2)

  enabled(profile_import_btn) <- FALSE      

  addHandlerChanged(profile_file_browser, handler = function(h, ...) {
    val <- svalue(profile_file_browser)
    
    if (file.exists(val)){
      
      dnaProfile <- import(resultFiles=val)
      
      dnaProfile <- trim(data=dnaProfile, samples=NULL,
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

  # Import button is here because handler is only triggered when hitting <Enter>
  addHandlerChanged(profile_import_btn, handler = function(h, ...) {
    val <- svalue(profile_file_browser)

    if (file.exists(val)){
        
     dnaProfile <- import(resultFiles=val)

      dnaProfile <- trim(data=dnaProfile, samples=NULL,
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
  
  
  profile_grid_2[5,1] <- profile_pop_lbl <- glabel(text="Population database:",
                                             container=profile_grid_2,
                                             anchor=c(-1 ,0))
  
  # TODO: functions for storing and importing freq databases.
  profile_grid_2[5,2] <- profile_pop_drp <- gdroplist(items=c("Norway","Sweden","..."),
                                                     selected = 1,
                                                     editable = FALSE,
                                                     container = profile_grid_2) 
  enabled(profile_pop_drp) <- FALSE        

  profile_grid_2[5,3] <- profile_generate_btn <- gbutton(text = "Generate",
                      border=TRUE, container = profile_grid_2)

  enabled(profile_generate_btn) <- FALSE      


  profile_grid_2[6,1] <- glabel("", container=profile_grid_2) # Adds some space.
  
  # EDIT PROFILE ----------------------------------------------------------------
  
  profile_frame_3 <- gframe(text = "View and edit profile",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = profile_gf,
                            expand=FALSE) 
  
  
  profile_grid_3 <- glayout(container = profile_frame_3)
  
  
  profile_grid_3[1,1] <- glabel("", container=profile_grid_3) # Adds some space.
  
  
  simProfile <- data.frame(Marker=getKit(svalue(profile_kit_drop))$locus,
                               Allele.1="NA",
                               Allele.2="NA",
                               stringsAsFactors=FALSE)

  profile_grid_3[2,1] <- profile_tbl <- gdf(items=simProfile,
                                            container = profile_grid_3)
  

  size(profile_tbl) <- c(600, 300)
  
  
  # SAMPLE PARAMETERS ###########################################################
  
  glabel("", container=sample_gf) # Adds some space.
  
  # SAMPLE NAME ---------------------------------------------------------------
  
  sample_frame_3_txt_width <- 20
  
  sample_frame_3 <- gframe(text = "Sample name",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = sample_gf,
                            expand=FALSE) 
  
  
  sample_grid_3 <- glayout(container = sample_frame_3)
  
  sample_grid_3[1,1] <- glabel("", container=sample_grid_3) # Adds some space.
  
  
  sample_grid_3[2,1] <- sample_name_lbl <- glabel(text="Sample name:",
                                                    container=sample_grid_3,
                                                    anchor=c(-1 ,0))
  sample_grid_3[2,2] <- sample_name_txt <- gedit(text="",
                                                   width=sample_frame_3_txt_width,
                                                   container=sample_grid_3)

  sample_grid_3[3,1] <- glabel("", container=sample_grid_3) # Adds some space.
  
  # AMOUNT OF DNA ---------------------------------------------------------------
  
  sample_frame_1_txt_width <- 6
  
  sample_frame_1 <- gframe(text = "Amount of DNA",
                            markup = FALSE,
                            pos = 0,
                            horizontal=TRUE,
                            container = sample_gf,
                            expand=FALSE) 
  
  
  sample_grid_1 <- glayout(container = sample_frame_1)
  
  sample_grid_1[1,1] <- glabel("", container=sample_grid_1) # Adds some space.
  
  
  sample_grid_1[2,1] <- sample_conc_lbl <- glabel(text="DNA concentration (ng/\u00B5L):",
                                                    container=sample_grid_1,
                                                    anchor=c(-1 ,0))
  sample_grid_1[2,2] <- sample_conc_txt <- gedit(text="",
                                                   width=sample_frame_1_txt_width,
                                                   container=sample_grid_1)
  
  addHandlerKeystroke(sample_conc_txt, handler = function(h, ...) {
    val <- svalue (h$obj)
    if (val != "" ){
      enabled(sample_ncells_txt) <- FALSE
      enabled(sample_ncells_sd_txt) <- FALSE
      
      enabled(sample_degI_txt) <- FALSE
      enabled(sample_degI_sd_txt) <- FALSE
      
      enabled(sample_degS_txt) <- FALSE
      enabled(sample_degS_sd_txt) <- FALSE
      
    } else{
      enabled(sample_ncells_txt) <- TRUE
      enabled(sample_ncells_sd_txt) <- TRUE
      
      enabled(sample_degI_txt) <- TRUE
      enabled(sample_degI_sd_txt) <- TRUE
      
      enabled(sample_degS_txt) <- TRUE
      enabled(sample_degS_sd_txt) <- TRUE
      
    }
  } )
  
  
  sample_grid_1[2,3] <- sample_conc_sd_lbl <- glabel(text="standard deviation:",
                                                       container=sample_grid_1,
                                                       anchor=c(-1 ,0))
  sample_grid_1[2,4] <- sample_conc_sd_txt <- gedit(text="0",
                                                      width=sample_frame_1_txt_width,
                                                      container=sample_grid_1)
  
  
  sample_grid_1[3,1] <- sample_celldna_lbl <- glabel(text="DNA per diploid cell (pg):",
                                                       container=sample_grid_1,
                                                       anchor=c(-1 ,0))
  sample_grid_1[3,2] <- sample_celldna_txt <- gedit(text="6",
                                                      width=sample_frame_1_txt_width,
                                                      container=sample_grid_1)
  
  
  sample_grid_1[4,1] <- sample_ncells_lbl <- glabel(text="Number of cells:",
                                                    container=sample_grid_1,
                                                    anchor=c(-1 ,0))
  sample_grid_1[4,2] <- sample_ncells_txt <- gedit(text="",
                                                   width=sample_frame_1_txt_width,
                                                   container=sample_grid_1)
  
  addHandlerKeystroke(sample_ncells_txt, handler = function(h, ...) {
    val <- svalue (h$obj)
    if (val != "" ){
      enabled(sample_conc_txt) <- FALSE
      enabled(sample_conc_sd_txt) <- FALSE
      
      enabled(sample_degI_txt) <- FALSE
      enabled(sample_degI_sd_txt) <- FALSE
      
      enabled(sample_degS_txt) <- FALSE
      enabled(sample_degS_sd_txt) <- FALSE
      
    } else{
      enabled(sample_conc_txt) <- TRUE
      enabled(sample_conc_sd_txt) <- TRUE
    
      enabled(sample_degI_txt) <- TRUE
      enabled(sample_degI_sd_txt) <- TRUE
      
      enabled(sample_degS_txt) <- TRUE
      enabled(sample_degS_sd_txt) <- TRUE
      
    }
  } )
  
  sample_grid_1[4,3] <- sample_ncells_sd_lbl <- glabel(text="standard deviation:",
                                                     container=sample_grid_1,
                                                     anchor=c(-1 ,0))
  sample_grid_1[4,4] <- sample_ncells_sd_txt <- gedit(text="0",
                                                    width=sample_frame_1_txt_width,
                                                    container=sample_grid_1)
  
  sample_grid_1[5,1] <- glabel("", container=sample_grid_1) # Adds some space.
  
  # DEGRADATION -----------------------------------------------------------------
  
  sample_frame_2_txt_width <- 6
  
  sample_frame_2 <- gframe(text = "Degradation",
                           markup = FALSE,
                           pos = 0,
                           horizontal=TRUE,
                           container = sample_gf,
                           expand=FALSE) 
  
  
  sample_grid_2 <- glayout(container = sample_frame_2)
  
  sample_grid_2[1,1] <- glabel("", container=sample_grid_2) # Adds some space.
  
  
  sample_grid_2[2,1] <- sample_degS_lbl <- glabel(text="Degradation slope (log(ng/\u00B5L)/bp):",
                                     container=sample_grid_2,
                                     anchor=c(-1 ,0))
  sample_grid_2[2,2] <- sample_degS_txt <- gedit(text="",
                                    width=sample_frame_2_txt_width,
                                    container=sample_grid_2)
  
  sample_grid_2[2,3] <- sample_degS_sd_lbl <- glabel(text="standard deviation:",
                                                container=sample_grid_2,
                                                anchor=c(-1 ,0))
  sample_grid_2[2,4] <- sample_degS_sd_txt <- gedit(text="0",
                                               width=sample_frame_2_txt_width,
                                               container=sample_grid_2)
  
  addHandlerKeystroke(sample_degS_txt, handler = function(h, ...) {
    val <- svalue (h$obj)
    if (val != "" ){
      enabled(sample_conc_txt) <- FALSE
      enabled(sample_conc_sd_txt) <- FALSE
      
      enabled(sample_ncells_txt) <- FALSE
      enabled(sample_ncells_sd_txt) <- FALSE
      
      val_degI <- as.numeric(svalue(sample_degI_txt))
      val_degS <- as.numeric(svalue(sample_degS_txt))
      
      if(!is.na(val_degI) && !is.na(val_degS)){
        if (val_degI != "" && val_degS!= ""){
          
          conc <- .degToConc(x=100, i=val_degI, s=val_degS)
          
          svalue(sample_degEx1_txt) <- round(conc,4)
          
          conc <- .degToConc(x=400, i=val_degI, s=val_degS)
          
          svalue(sample_degEx2_txt) <- round(conc,4)
          
        }
      }
      
    } else{
      enabled(sample_conc_txt) <- TRUE
      enabled(sample_conc_sd_txt) <- TRUE
      
      enabled(sample_ncells_txt) <- TRUE
      enabled(sample_ncells_sd_txt) <- TRUE
      
    }
  } )
  
  sample_grid_2[3,1] <- sample_degI_lbl <- glabel(text="Degradation intercept (log(ng/\u00B5L)):",
                                                container=sample_grid_2,
                                                anchor=c(-1 ,0))
  sample_grid_2[3,2] <- sample_degI_txt <- gedit(text="",
                                               width=sample_frame_2_txt_width,
                                               container=sample_grid_2)
  
  sample_grid_2[3,3] <- sample_degI_sd_lbl <- glabel(text="standard deviation:",
                                                   container=sample_grid_2,
                                                   anchor=c(-1 ,0))
  sample_grid_2[3,4] <- sample_degI_sd_txt <- gedit(text="0",
                                                  width=sample_frame_2_txt_width,
                                                  container=sample_grid_2)
  
  addHandlerKeystroke(sample_degI_txt, handler = function(h, ...) {
    val <- svalue (h$obj)
    if (val != "" ){
      enabled(sample_conc_txt) <- FALSE
      enabled(sample_conc_sd_txt) <- FALSE
      
      enabled(sample_ncells_txt) <- FALSE
      enabled(sample_ncells_sd_txt) <- FALSE
      
      val_degI <- as.numeric(svalue(sample_degI_txt))
      val_degS <- as.numeric(svalue(sample_degS_txt))
      
      if(!is.na(val_degI) && !is.na(val_degS)){
        if (val_degI != "" && val_degS!= ""){
          
          conc <- .degToConc(x=100, i=val_degI, s=val_degS)
          
          svalue(sample_degEx1_txt) <- round(conc,4)

          conc <- .degToConc(x=400, i=val_degI, s=val_degS)
          
          svalue(sample_degEx2_txt) <- round(conc,4)
          
        }
      }
    } else{
      enabled(sample_conc_txt) <- TRUE
      enabled(sample_conc_sd_txt) <- TRUE
      
      enabled(sample_ncells_txt) <- TRUE
      enabled(sample_ncells_sd_txt) <- TRUE
      
    }
  } )
  
  
  
  .degToConc <- function(x, i, s){
    
    res <- 10^(s * x + i)
    
    return(res)
  }  
  
  sample_grid_2[4,1] <- sample_degEx1_lbl <- glabel(text="Concentration @ 100bp (ng/\u00B5L):",
                                                  container=sample_grid_2,
                                                  anchor=c(-1 ,0))
  sample_grid_2[4,2] <- sample_degEx1_txt <- gedit(text="",
                                                 width=sample_frame_2_txt_width*1.5,
                                                 container=sample_grid_2)
  
  enabled(sample_degEx1_txt) <- FALSE


  sample_grid_2[4,3] <- sample_degEx2_lbl <- glabel(text="Concentration @ 400bp (ng/\u00B5L):",
                                                   container=sample_grid_2,
                                                   anchor=c(-1 ,0))
  sample_grid_2[4,4] <- sample_degEx2_txt <- gedit(text="",
                                                  width=sample_frame_2_txt_width*1.5,
                                                  container=sample_grid_2)
  
  enabled(sample_degEx2_txt) <- FALSE

  sample_grid_2[5,1] <- glabel("", container=sample_grid_2) # Adds some space.
  
  # EXTRACTION PARAMETERS #######################################################
  
  glabel("", container=ex_gf) # Adds some space.
  
  # EFFICIENCY ------------------------------------------------------------------
  
  ex_frame_1_txt_width <- 6 
  
  ex_frame_1 <- gframe(text = "Extraction efficiency",
                           markup = FALSE,
                           pos = 0,
                           horizontal=TRUE,
                           container = ex_gf,
                           expand=FALSE) 
  
  
  ex_grid_1 <- glayout(container = ex_frame_1)
  
  ex_grid_1[1,1] <- glabel("", container=ex_grid_1) # Adds some space.
  
  
  ex_grid_1[2,1] <- ex_eff_lbl <- glabel(text="Extraction efficiency:",
                                       container=ex_grid_1,
                                       anchor=c(-1 ,0))
  ex_grid_1[2,2] <- ex_eff_txt <- gedit(text="",
                                      width=ex_frame_1_txt_width,
                                      container=ex_grid_1)
  
  ex_grid_1[2,3] <- ex_eff_sd_lbl <- glabel(text="standard deviation:",
                                          container=ex_grid_1,
                                          anchor=c(-1 ,0))
  ex_grid_1[2,4] <- ex_eff_sd_txt <- gedit(text="0",
                                         width=ex_frame_1_txt_width,
                                         container=ex_grid_1)
  
  ex_grid_1[3,1] <- glabel("", container=ex_grid_1) # Adds some space.
  
  # VOLUME ----------------------------------------------------------------------
  
  ex_frame_2_txt_width <- 6 
  
  ex_frame_2 <- gframe(text = "Extraction volume",
                       markup = FALSE,
                       pos = 0,
                       horizontal=TRUE,
                       container = ex_gf,
                       expand=FALSE) 
  
  
  ex_grid_2 <- glayout(container = ex_frame_2)
  
  ex_grid_2[1,1] <- glabel("", container=ex_grid_2) # Adds some space.
  
  
  ex_grid_2[2,1] <- ex_vol_lbl <- glabel(text="Extraction volume (\u00B5L):",
                                        container=ex_grid_2,
                                        anchor=c(-1 ,0))
  ex_grid_2[2,2] <- ex_vol_txt <- gedit(text="",
                                        width=ex_frame_2_txt_width,
                                        container=ex_grid_2)
  
  ex_grid_2[2,3] <- ex_vol_sd_lbl <- glabel(text="standard deviation:",
                                       container=ex_grid_2,
                                       anchor=c(-1 ,0))
  ex_grid_2[2,4] <- ex_vol_sd_txt <- gedit(text="0",
                                      width=ex_frame_2_txt_width,
                                      container=ex_grid_2)
  
  ex_grid_2[3,1] <- glabel("", container=ex_grid_2) # Adds some space.
  
  
  # AMPLIFICATION PARAMETERS ####################################################
  
  glabel("", container=amp_gf) # Adds some space.
  
  # ALIQUOTE --------------------------------------------------------------------
  
  amp_frame_1_txt_width <- 6 
  
  amp_frame_1 <- gframe(text = "Aliquote",
                       markup = FALSE,
                       pos = 0,
                       horizontal=TRUE,
                       container = amp_gf,
                       expand=FALSE) 
  
  
  amp_grid_1 <- glayout(container = amp_frame_1)
  
  amp_grid_1[1,1] <- glabel("", container=amp_grid_1) # Adds some space.
  
  
  
  amp_grid_1[2,1] <- amp_vol_lbl <- glabel(text="Aliquote for PCR (\u00B5L):",
                                       container=amp_grid_1,
                                       anchor=c(-1 ,0))
  amp_grid_1[2,2] <- amp_vol_txt <- gedit(text="",
                                      width=amp_frame_1_txt_width,
                                      container=amp_grid_1)
  
  amp_grid_1[2,3] <- amp_vol_sd_lbl <- glabel(text="standard deviation:",
                                          container=amp_grid_1,
                                          anchor=c(-1 ,0))
  amp_grid_1[2,4] <- amp_vol_sd_txt <- gedit(text="0",
                                         width=amp_frame_1_txt_width,
                                         container=amp_grid_1)
  
  amp_grid_1[3,1] <- glabel("", container=amp_grid_1) # Adds some space.
  
  # PCR -------------------------------------------------------------------------
  
  amp_frame_2_txt_width <- 6 
  
  amp_frame_2 <- gframe(text = "PCR",
                        markup = FALSE,
                        pos = 0,
                        horizontal=TRUE,
                        container = amp_gf,
                        expand=FALSE) 
  
  
  amp_grid_2 <- glayout(container = amp_frame_2)
  
  amp_grid_2[1,1] <- glabel("", container=amp_grid_2) # Adds some space.
  
  
  amp_grid_2[2,1] <- cyc_lbl <- glabel("Number of PCR cycles:", container=amp_grid_2, anchor=c(-1 ,0))
  amp_grid_2[2,2] <- cyc_sb <- gspinbutton(from=1, to = 50, by =1, value=30, container = amp_grid_2)
  
  amp_grid_2[2,3] <- amp_tvol_lbl <- glabel(text="Total amplification volume:",
                                              container=amp_grid_2,
                                              anchor=c(-1 ,0))
  amp_grid_2[2,4] <- amp_tvol_txt <- gedit(text="",
                                             width=amp_frame_2_txt_width,
                                             container=amp_grid_2)
  
  amp_grid_2[3,1] <- glabel("", container=amp_grid_2) # Adds some space.
  
  # ANALYSIS PARAMETERS #########################################################
  
  glabel("", container=ce_gf) # Adds some space.
  
  # ALIQUOTE --------------------------------------------------------------------
  
  ce_frame_1_txt_width <- 8 
  
  ce_frame_1 <- gframe(text = "Capillary elechrophoresis",
                        markup = FALSE,
                        pos = 0,
                        horizontal=TRUE,
                        container = ce_gf,
                        expand=FALSE) 
  
  
  ce_grid_1 <- glayout(container = ce_frame_1)
  
  ce_grid_1[1,1] <- glabel("", container=ce_grid_1) # Adds some space.
  
  
  
  ce_grid_1[2,1] <- ce_detect_lbl <- glabel(text="Detection threshold for generating a signal (molecules/\u00B5L):",
                                           container=ce_grid_1,
                                           anchor=c(-1 ,0))
  ce_grid_1[2,2] <- ce_detect_txt <- gedit(text="4e+05",
                                          width=ce_frame_1_txt_width,
                                          initial.msg="",
                                          container=ce_grid_1)
  
  ce_grid_1[3,1] <- glabel("", container=ce_grid_1) # Adds some space.
  
  # VISUALIZATION ---------------------------------------------------------------
  
  ce_frame_2_txt_width <- 8 
  
  ce_frame_2 <- gframe(text = "Visualization parameters",
                       markup = FALSE,
                       pos = 0,
                       horizontal=TRUE,
                       container = ce_gf,
                       expand=FALSE) 
  
  
  ce_grid_2 <- glayout(container = ce_frame_2)
  
  ce_grid_2[1,1] <- glabel("", container=ce_grid_2) # Adds some space.
  
  
  ce_grid_2[2,1] <- ce_kh_lbl <- glabel(text="Number of molecules -> peak height scaling constant:",
                                          container=ce_grid_2,
                                          anchor=c(-1 ,0))
  ce_grid_2[2,2] <- ce_kh_txt <- gedit(text="55",
                                         width=ce_frame_2_txt_width,
                                         initial.msg="",
                                         container=ce_grid_2)
  
  
  ce_grid_2[3,1] <- glabel("", container=ce_grid_2) # Adds some space.
  
  
  # SIMULATION PARAMETERS #####################################################
  
  glabel("", container=sim_gf) # Adds some space.
  
  # SIMULATION ----------------------------------------------------------------
  
  sim_frame_1_txt_width <- 8 
  
  sim_frame_1 <- gframe(text = "Simulation",
                       markup = FALSE,
                       pos = 0,
                       horizontal=TRUE,
                       container = sim_gf,
                       expand=FALSE) 
  
  
  sim_grid_1 <- glayout(container = sim_frame_1)
  
  sim_grid_1[1,1] <- glabel("", container=sim_grid_1) # Adds some space.
  
  
  sim_grid_1[2,1] <- sim_sim_lbl <- glabel(text="Number of simulations:",
                                     container=sim_grid_1,
                                     anchor=c(-1 ,0))
  sim_grid_1[2,2] <- sim_sim_txt <- gedit(text="1",
                                    width=10,
                                    initial.msg="",
                                    container=sim_grid_1)


  sim_grid_1[2,3] <-   sim_sim_btn <- gbutton(text = "Simulate",
                         border=TRUE,
                         container = sim_grid_1) 
  
  sim_grid_1[2,4] <-   sim_sim_chk <- gcheckbox(text="Auto update EPG", 
                                                checked=FALSE, 
                                                use.togglebutton=FALSE,
                                                container = sim_grid_1) 
  
  
  sim_grid_1[3,1] <- glabel("", container=sim_grid_1) # Adds some space.


  addHandlerChanged(sim_sim_btn, handler = function(h, ...) {

    val_ok <- TRUE
    
    # Disable button until simulation is finished.
    enabled(sim_sim_btn) <- FALSE

    # Load all settings.
    val_sample <- svalue(sample_name_txt)
    val_kit <- svalue(profile_kit_drop)
    val_alleles <- profile_tbl[,]
    val_title <- svalue(sim_epg_title_txt)
    val_conc <- as.numeric(svalue(sample_conc_txt))
    val_conc_sd <- as.numeric(svalue(sample_conc_sd_txt))
    val_ncells <- as.numeric(svalue(sample_ncells_txt))
    val_ncells_sd <- as.numeric(svalue(sample_ncells_sd_txt))
    # Convert from pico grams to nano grams.
    val_celldna <- as.numeric(svalue(sample_celldna_txt)) / 1000
    val_slope <- as.numeric(svalue(sample_degS_txt))
    val_slope_sd <- as.numeric(svalue(sample_degS_sd_txt))
    val_intercept <- as.numeric(svalue(sample_degI_txt))
    val_intercept_sd <- as.numeric(svalue(sample_degI_sd_txt))
    val_exprob <- as.numeric(svalue(ex_eff_txt))
    val_exprob_sd <- as.numeric(svalue(ex_eff_sd_txt))
    val_exvol <- as.numeric(svalue(ex_vol_txt))
    val_exvol_sd <- as.numeric(svalue(ex_vol_sd_txt))
    val_amp_alq <- as.numeric(svalue(amp_vol_txt))
    val_amp_alq_sd <- as.numeric(svalue(amp_vol_sd_txt))
    val_amp_pcr <- as.numeric(svalue(cyc_sb))
    val_amp_tvol <- as.numeric(svalue(amp_tvol_txt))
    # Pass the total number of molecules needed to trigger a signal.
    val_ce_detect <- as.numeric(svalue(ce_detect_txt)) * val_amp_tvol
    val_ce_kh <- as.numeric(svalue(ce_kh_txt))
    val_sim <- as.numeric(svalue(sim_sim_txt))
    
    if(!is.numeric(val_ce_detect)){
      
      val_ok <- FALSE
      
      gmessage(message="Total amplification volume and detection threshold must be provided.",
               title="Error",
               icon = "error")      
      
    }

    if(val_ok){
    
      if(debug){
        print(val_kit)
        print(val_alleles)
        print(val_title)
        print(val_conc)
        print(val_ncells)
        print(val_celldna)
        print(val_slope)
        print(val_intercept)
        print(val_exprob)
        print(val_exvol)
        print(val_amp_alq)
        print(val_amp_pcr)
        print(val_amp_tvol)
        print(val_ce_detect)
        print(val_ce_kh)
        print(val_sim)
      }
  
      # Simulate.
      simData <<- simulateProfile(alleles=val_alleles,
                                  sample.name=val_sample,
                                  ncells=val_ncells,
                                  ncells.sd=val_ncells_sd,
                                  conc=val_conc,
                                  conc.sd=val_conc_sd,
                                  intercept=val_intercept,
                                  intercept.sd=val_intercept_sd,
                                  slope=val_slope,
                                  slope.sd=val_slope_sd,
                                  exprob=val_exprob,
                                  exprob.sd=val_exprob_sd,
                                  volume=val_exvol,
                                  volume.sd=val_exvol_sd,
                                  aliq=val_amp_alq,
                                  aliq.sd=val_amp_alq_sd,
                                  simulations=val_sim,
                                  kit=val_kit,
                                  celldna=val_celldna,
                                  cyc=val_amp_pcr,
                                  tDetect=val_ce_detect,
                                  KH=val_ce_kh)
  
      
      # Update result table.
      sim_res_tbl[,] <- simData
  
      if(svalue(sim_sim_chk)){
        # Generate EPG.      
        simEPG <<- generateEPG(data=simData, 
                              kit=val_kit, 
                              plotTitle=val_title, 
                              debugInfo=FALSE)
        
      }
      
    }
      
    # Enable buttons.
    enabled(sim_sim_btn) <- TRUE

  } )

  
  # RESULT TABLE --------------------------------------------------------------
  
  sim_frame_2 <- gframe(text = "Result",
                        markup = FALSE,
                        pos = 0,
                        horizontal=TRUE,
                        container = sim_gf,
                        expand=FALSE) 
  
  sim_main_grid_2 <- glayout(container = sim_frame_2)
  
  sim_grid_2 <- glayout(container = sim_main_grid_2)
  sim_main_grid_2[1,1] <- sim_grid_2
  
  sim_grid_2[1,1] <- glabel("", container=sim_grid_2) # Adds some space.
  
  sim_grid_2[2,1] <- sim_res_tbl <- gtable(items=simData, container=sim_grid_2)
  
  size(sim_res_tbl) <- c(200, 300)
  
  sim_grid_2[3,1] <- glabel("", container=sim_grid_2) # Adds some space.

  # SAVE RESULT TABLE ---------------------------------------------------------
  
  sim_frame_21_txt_width <- 8 
  
  sim_grid_21 <- glayout(container = sim_main_grid_2)
  sim_main_grid_2[1,2] <- sim_grid_21

  sim_grid_21[1,1] <- glabel("", container=sim_grid_21) # Adds some space.
  
  sim_grid_21[2,1] <- sim_res_name_lbl <- glabel(text="File name:",
                                                 container=sim_grid_21,
                                                 anchor=c(-1 ,0))
  sim_grid_21[3,1] <- sim_res_name_txt <- gedit(text="",
                                                width=sim_frame_21_txt_width,
                                                container=sim_grid_21)
  
  sim_grid_21[2,2] <- sim_res_ext_lbl <- glabel(text="File extension:",
                                                 container=sim_grid_21,
                                                 anchor=c(-1 ,0))
  sim_grid_21[3,2] <- sim_res_ext_txt <- gedit(text=".txt",
                                                width=4,
                                                container=sim_grid_21)
  
  sim_grid_21[2,3] <- sim_res_del_lbl <- glabel(text="Delimeter:",
                                                container=sim_grid_21,
                                                anchor=c(-1 ,0))
  sim_grid_21[3,3] <- sim_res_del_drp <- gdroplist(items=c("TAB","SPACE","COMMA"), 
                                                   selected = 1,
                                                   editable = FALSE,
                                                   container = sim_grid_21) 
  
  sim_grid_21[4,1:2] <- sim_res_paht_lbl <- glabel(text="File path:",
                                                   container=sim_grid_21,
                                                   anchor=c(-1 ,0))
  simResDefText <- "Select folder..."
  sim_grid_21[5,1:2] <- sim_res_save_brw <- gfilebrowse(text=simResDefText,
                                                   quote=FALSE,
                                                   type="selectdir",
                                                   container=sim_grid_21)
  
  sim_grid_21[6,1:3] <- sim_res_save_btn <- gbutton(text="Save result to ASCII text file",
                                         border=TRUE,
                                         container=sim_grid_21) 
  
  addHandlerChanged(sim_res_save_btn, handler = function(h, ...) {

    # Get values.
    path_name <- svalue(sim_res_save_brw)
    file_name <- svalue(sim_res_name_txt)
    ext_name <- svalue(sim_res_ext_txt)
    del_index <- svalue(sim_res_del_drp, index=TRUE)

    if(debug){
      print(file_name)
    }

    if(file_name != "" && path_name != simResDefText){

      # Assign a delimeter character.
      if(del_index == 1){
        delimeter <- "\t"   
      } else if(del_index == 2){
        delimeter <- " "
      } else if(del_index == 3){
        delimeter <- ","
      } 

      if(substr(path_name, nchar(path_name), nchar(path_name)+1) != separator){
        path_name <- paste(path_name, separator, sep="")
      }
      
      # Construct complete file name.
      complete_file_name <- paste(path_name, file_name, ext_name, sep="")
      
      write.table(x=simData, file = complete_file_name,
                  append = FALSE, quote = FALSE, sep = delimeter,
                  dec = ".", row.names = FALSE,
                  col.names = TRUE)
      
    } else {
      
      gmessage(message="File name and path must be provided.",
               title="Error",
               icon = "error")      
    }    
  } )

  sim_grid_21[7,1:3] <- sim_res_save_r_btn <- gbutton(text = "Save as .RData file",
                                                      border=TRUE,
                                                      container = sim_grid_21) 
  
  addHandlerChanged(sim_res_save_r_btn, handler = function(h, ...) {
    
    # Get values.
    path_name <- svalue(sim_res_save_brw)
    file_name <- svalue(sim_res_name_txt)
    
    if(debug){
      print(file_name)
      print(separator)
    }
    
    if(file_name != "" && path_name != simResDefText){

      if(substr(path_name, nchar(path_name), nchar(path_name)+1) != separator){
        path_name <- paste(path_name, separator, sep="")
      }
      
      # Construct complete file name.
      complete_file_name <- paste(path_name, file_name, ".RData", sep="")

      save(simData, file = complete_file_name)
      
    } else {
      
      gmessage(message="File name and path must be provided.",
               title="Error",
               icon = "error")      
    }    
  } )
  
  sim_grid_21[8,1] <- glabel("", container=sim_grid_21) # Adds some space.
  
  sim_grid_21[9,1] <- sim_res_exp_lbl <- glabel(text="Object name:",
                                                 container=sim_grid_21,
                                                 anchor=c(-1 ,0))
  sim_grid_21[10,1] <- sim_res_exp_txt <- gedit(text="",
                                                width=sim_frame_21_txt_width,
                                                container=sim_grid_21)
  
  sim_grid_21[10,2:3] <- sim_res_exp_btn <- gbutton(text = "Export result to R workspace",
                                               border=TRUE,
                                               container = sim_grid_21) 

  enabled(sim_res_exp_txt) <- FALSE
  
  addHandlerChanged(sim_res_exp_btn, handler = function(h, ...) {

    gmessage(message="This function has been disabled until a workaround is found.\n
The 'assign' function is used for saving to the global environment, wich is against CRAN policy.\n
Please use save as ASCII or RData.",
             title="Function disabled",
             icon = "info")      
    
    #object_name <- svalue(sim_res_exp_txt)
    #
    #if (object_name != "" ){
    #  
    #  # Save simulation result to an object in the global environment.
    #  assign(x=substitute(object_name), value=simData, envir = .GlobalEnv)
    #  
    #} else{
    #
    #  gmessage(message="An object name must be provided.",
    #           title="Error",
    #           icon = "error")      
    #}
    
  } )
  
  # EPG -----------------------------------------------------------------------
  
  sim_frame_3_txt_width <- 8 
  
  sim_frame_3 <- gframe(text = "Elechtropherogram",
                        markup = FALSE,
                        pos = 0,
                        horizontal=TRUE,
                        container = sim_gf,
                        expand=FALSE) 
  
  
  sim_grid_3 <- glayout(container = sim_frame_3)
  
  sim_grid_3[1,1] <- glabel("", container=sim_grid_3) # Adds some space.
  
  
  sim_grid_3[2,1] <- sim_epg_title_lbl <- glabel(text="Plot title:",
                                                container=sim_grid_3,
                                                anchor=c(-1 ,0))
  
  sim_grid_3[2,2] <- sim_epg_title_txt <- gedit(text="",
                                               width=20,
                                               initial.msg="",
                                               container=sim_grid_3)
  
  sim_grid_3[2,3:4] <-   sim_epg_btn <- gbutton(text = "Generate EPG",
                                              border=TRUE,
                                              container = sim_grid_3) 

  sim_grid_3[3,1:2] <- sim_epg_name_lbl <- glabel(text="File name:",
                                           container=sim_grid_3,
                                           anchor=c(-1 ,0))
  sim_grid_3[3,3] <- sim_epg_w_lbl <- glabel(text="Width:",
                                           container=sim_grid_3,
                                           anchor=c(-1 ,0))
  sim_grid_3[3,4] <- sim_epg_h_lbl <- glabel(text="Height:",
                                           container=sim_grid_3,
                                           anchor=c(-1 ,0))
  sim_grid_3[3,5] <- sim_epg_res_lbl <- glabel(text="Resolution:",
                                           container=sim_grid_3,
                                           anchor=c(-1 ,0))

  sim_grid_3[4,1:2] <- sim_epg_name_txt <- gedit(text="",
                                          width=15,
                                          initial.msg=".png is added automatically",
                                          container=sim_grid_3)
  
  sim_grid_3[4,3] <- sim_epg_w_txt <- gedit(text="3000",
                                          width=4,
                                          initial.msg="",
                                          container=sim_grid_3)

  sim_grid_3[4,4] <- sim_epg_h_txt <- gedit(text="2000",
                                          width=4,
                                          initial.msg="",
                                          container=sim_grid_3)

  sim_grid_3[4,5] <- sim_epg_res_txt <- gedit(text="250",
                                          width=4,
                                          initial.msg="",
                                          container=sim_grid_3)
  
  sim_grid_3[5,1:5] <- sim_epg_paht_lbl <- glabel(text="File path:",
                                                   container=sim_grid_3,
                                                   anchor=c(-1 ,0))
  simEpgDefText <- "Select folder..."
  sim_grid_3[6,1:5] <- sim_epg_path_brw <- gfilebrowse(text=simEpgDefText,
                                                        quote=FALSE,
                                                        type="selectdir",
                                                        container=sim_grid_3)

  sim_grid_3[7,1:5] <- sim_epg_save_btn <- gbutton(text = "Save EPG",
                                              border=TRUE,
                                              container = sim_grid_3) 
  
  
  sim_grid_3[8,1] <- glabel("", container=sim_grid_3) # Adds some space.
  
  
  addHandlerChanged(sim_epg_btn, handler = function(h, ...) {
    
    # Load all settings.
    val_title <- svalue(sim_epg_title_txt)
    val_kit <- svalue(profile_kit_drop)

    # Generate EPG.      
    simEPG <<- generateEPG(data=simData, 
                          kit=val_kit, 
                          plotTitle=val_title, 
                          debugInfo=FALSE)
    
  } )
  
  addHandlerChanged(sim_epg_save_btn, handler = function(h, ...) {
    
    # Disable all buttons until save is finished.
    enabled(sim_epg_save_btn) <- FALSE

    
    # Load all settings.
    val_name <- svalue(sim_epg_name_txt)
    val_w <- as.numeric(svalue(sim_epg_w_txt))
    val_h <- as.numeric(svalue(sim_epg_h_txt))
    val_res <- as.numeric(svalue(sim_epg_res_txt))
    val_path <- svalue(sim_epg_path_brw)
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
    
    if(val_path == simEpgDefText || val_path == ""){
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
      print(paste(val_path, separator, val_name, ".png", sep=""))
    }

    if(!is.null(simEPG) && val_OK){

      # Save EPG.      
      png(filename=paste(val_path, separator, val_name, ".png", sep=""),
          width=val_w, height=val_h, res=val_res)
      plot(simEPG)
      dev.off()
      
    }

    if(!val_OK){
      
      gmessage(message="File name, path, width (>20), height (>20), and resolution (>20) must be provided.",
               title="Error",
               icon = "error")      
      
    }
    # Enable buttons.
    enabled(sim_epg_save_btn) <- TRUE
    
  } )
  
  
  # END GUI ###################################################################
  
  
  # Show GUI, with tab one.
  svalue(nb) <- 1
  visible(w) <- TRUE

  return(NULL)
  
}
