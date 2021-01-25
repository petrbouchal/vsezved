library(tidyverse)
# pak::pkg_install("dmi3kno/bunny")
library(bunny)
library(magick)
library(fontawesome)
library(svglite)

# https://pkgdown.r-lib.org/reference/build_favicon.html
# https://pkgdown.r-lib.org/reference/build_home.html
# https://www.ddrive.no/post/making-hex-and-twittercard-with-bunny-and-magick/

hex_border <- image_canvas_hexborder(border_color = "#000000", border_size = 2)
hex_canvas <- image_canvas_hex(border_color = "#000000", border_size = 5, fill_color = "white")

orbis <- magick::image_read("prep/orbis.png")

img_hex <- hex_canvas %>%
  bunny::image_compose(orbis, gravity = "north", offset = "+0+290", ) %>%
  # bunny::image_compose(icon, gravity = "north", offset = "+0+650") %>%
  image_annotate("všezvěd", size = 300, gravity = "south", location = "+0+300",
                 font = "Regula Old Face", color = "#c30011", style = "italic") %>%
  # bunny::image_compose(hex_border, gravity = "center", operator = "Over") %>%
  image_annotate("petrbouchal.xyz/vsezved",
                 size = 70, gravity = "south", location = "+400+310",
                 degrees = 330,
                 font = "sans", color = "grey")
img_hex


img_hex %>%
  image_convert("png") %>%
  image_write("prep/logo.png")
img_hex %>%
  image_scale("300x300")

img_hex %>%
  image_scale("1200x1200") %>%
  image_write(here::here("prep", "logo_hex_large.png"), density = 600)

img_hex %>%
  image_scale("200x200") %>%
  image_write(here::here("logo.png"), density = 600)

img_hex_for_pkgdown <- img_hex %>%
  image_scale("480x556") %>%
  image_write(here::here("prep/logo.png"), density = 600, quality = 100)

img_hex_gh <- img_hex %>%
  image_scale("400x400")

gh_logo <- bunny::github %>%
  image_scale("40x40") %>%
  image_colorize(70, "grey")

gh <- image_canvas_ghcard("black") %>%
  image_compose(img_hex_gh, gravity = "East", offset = "+80+0") %>%
  image_annotate("Czech school data in R", gravity = "West", location = "+100-30",
                 color = "white", size = 40, font = "IBM Plex Sans") %>%
  image_compose(gh_logo, gravity = "West", offset = "+110+45") %>%
  image_annotate("petrbouchal/vsezved", gravity = "West", location = "+160+45",
                 size = 35, font="IBM Plex Sans", color = "grey") %>%
  image_annotate("petrbouchal.xyz/vsezved", gravity = "West", location = "+105+105",
                 size = 35, font="IBM Plex Sans", color = "grey") %>%
  image_border_ghcard("grey")

gh %>% image_scale("500")

gh %>%
  image_write(here::here("prep", "vsezved_ghcard.png"))
