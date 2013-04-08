# Hack to avoid NOTES in R CMD check
# Hadley does not seem to like it: 
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("Height", "Id","Xmin","Xmax","Text")) ## Neded in function generateEPG
}
