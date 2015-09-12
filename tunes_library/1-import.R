library('XML')

# Need to read the library.xml file from the desktop
library.location <- "~/Desktop"
library.filename <- "Library.xml"
library.file <- paste(library.location, library.filename, sep = "/")

temp <- xmlParse(file = library.file)

xml_data <- xmlToList(temp)
