
library(tidyverse)
library(magick)
library(tesseract)

#

list_of_images <- fs::dir_ls("C:/Users/Jake/Documents/R Code/Cycling/profiles/")

#

fra <- tesseract("fra")
deu <- tesseract("deu")
eng <- tesseract("eng")

#

controls <- tibble(
  
  crop_height = 1, # where do we cut-off the image going from top to bottom
  rotation = 90 # how much to rotate (is text sidewise?)
  
)

#

img <- image_read(list_of_images[[3]])

print(img)

img_ht <- image_info(img)$height
img_wd <- image_info(img)$width

result_ocr <- image_crop(img, paste0(img_wd, "x", (controls$crop_height * img_ht))) %>%
  
  image_convert(type = "Grayscale") %>%
  
  image_modulate(brightness = 100) %>%
  
  image_enhance() %>%
  
  image_rotate(controls$rotation) %>%
  
  ocr(fra, HOCR = FALSE)

# 

text <- result_ocr
text <- stringr::str_split(text, "\n", simplify = TRUE)
text <- stringr::str_remove_all(text, "[0-9]")
text <- stringr::str_remove_all(text, "[:punct:]")
text <- stringr::str_remove_all(text, "\xA9")
text <- stringr::str_remove_all(text, "\u2122")
text <- stringr::str_remove_all(text, "\xae")
text <- stringr::str_remove_all(text, "\x24")
text <- trimws(text)
text <- stringr::str_remove_all(text, " *\\b[[:alpha:]]{1}\\b *")

cat(text)