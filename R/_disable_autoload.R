# Tell Shiny not to auto-source files in R/.
# We source explicitly from app.R in a controlled order
# (sheets.R must come before cache.R, which must come before
# lineup_creator.R, etc.). Auto-load uses alphabetical order
# which would break those dependencies.
#
# See https://shiny.posit.co/r/reference/shiny/latest/loadsupport.html