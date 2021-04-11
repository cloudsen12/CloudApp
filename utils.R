#' Dilate high-quality results using a 3x3 kernel
#' @noRd
dilate_raster <- function(label_raster) {
  # Labels according to cloudsen12
  # 0 -> clear
  # 1 -> thick cloud
  # 2 -> thin cloud
  # 3 -> shadow

  # Re-order raster value
  clear <- (label_raster == 0) * 0
  thick_cloud <- (label_raster == 1) * 3
  thin_cloud <- (label_raster == 2) * 1
  cloud_shadow <- (label_raster == 3) * 2

  order_new_r <- clear + thick_cloud + thin_cloud + cloud_shadow
  order_new_r_values <- as.matrix(order_new_r)

  # 3x3 kernel
  (k <- matrix(1,nrow=3, ncol=3))

  # dilate raster R
  rdilate <- dilate(order_new_r_values, k)
  order_new_r[] <- rdilate


  # Re-order raster value
  clear <- (order_new_r == 0) * 0
  thick_cloud <- (order_new_r == 3) * 1
  thin_cloud <- (order_new_r == 1) * 2
  cloud_shadow <- (order_new_r == 2) * 3

  # final_raster
  final_r <- clear + thick_cloud + thin_cloud + cloud_shadow
  final_r
}

#' Generate a preview in a specific image
#' @noRd
generate_spplot <-function(rgb, label_raster, output = "comparison.svg") {
  # Convert RGB to use with spplot
  lout <- rgb2spLayout(rgb)

  # raster label remove clear
  label_raster[label_raster == 0] = NA

  # all is NA?
  rat <- nrow(levels(as.factor(label_raster))[[1]])

  # create viz folder
  dir.create(path = "viz", showWarnings = FALSE)

  if (rat > 0) {
    # label spplot
    ## FFFFFF <- Clear (0)
    ## FFFF00 <- Thick cloud (1)
    ## 00FF00 <- Thin cloud (2)
    ## FF0000 <- Shadow (3)
    spplot1 <- spplot(
      obj = label_raster,
      sp.layout = list(
        lout
      ),
      col.regions = c("#FFFF00", "#00FF00", "#FF0000"),
      at = c(0,1,2,3,4),
      par.settings = list(axis.line = list(col = "transparent")),
      colorkey=FALSE
    )
  }

  fake_r <- label_raster
  fake_r[fake_r >= 0] = NA
  fake_r[1,1] = 0

  # RGB spplot
  spplot2 <- spplot(
    obj = fake_r,
    sp.layout = lout,
    cex = 0,
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey=FALSE,
    alpha.regions = 0
  )
  if (rat > 0) {
    svg(output, width = 7*2, height = 7*2)
    grid.arrange(spplot2, spplot1, nrow = 1)
  } else {
    svg(output, width = 7*2, height = 7*2)
    grid.arrange(spplot2, spplot2, nrow=1)
  }
  on.exit(dev.off())
}

#' Hex color (manual.png) to raster
number_to_hex <- function(stack_color, raster_ref) {
  raster_ref_f <- raster_ref
  stk_r <- stack(stack_color)

  #FFFFFF <- Clear (0)
  #FFFF00 <- Thick cloud (1)
  #00FF00 <- Thin cloud (2)
  #FF0000 <- Shadow (3)
  color_values <- rgb(stk_r[],maxColorValue = 255)
  color_values[color_values == "#FFFFFF"] <- 0
  color_values[color_values == "#FFFF00"] <- 1
  color_values[color_values == "#00FF00"] <- 2
  color_values[color_values == "#FF0000"] <- 3
  color_values <- as.numeric(color_values)

  raster_ref_f[] <- color_values
  raster_ref_f
}

#' Generate previews (in the 5 images)
#' @noRd
upload_results <- function(download_csaybar_user = FALSE) {

  if (download_csaybar_user) {
    dir_p  <- paste0(tempdir(), "/cd26ed5dc626f11802a652e81d02762e_s1078735@stud.sbg.ac.at")
    #download privileges
    download.file(
      url = "https://drive.google.com/uc?id=1dg02Ue6rkFa7xn7TU57bWu_gQxuxpGdY&export=download",
      destfile = dir_p
    )

    drive_auth("s1078735@stud.sbg.ac.at", token = dir_p)
  }

  # List files (Get SENTINEL_2 ID)
  point_name <- gsub("\\.gpkg$", "",list.files(pattern = "\\.gpkg$"))

  # Points Google Drive ID
  files_points_general <- googledrive::drive_ls(
    path = as_id("1BeVp0i-dGSuBqCQgdGZVDj4qzX1ms7L6"),
    q = sprintf("name contains '%s'", point_name)
  )

  # files Google Drive ID
  files_points <- googledrive::drive_ls(
    path = as_id(files_points_general$id)
  )
  folder_id <- files_points_general$id

  # List files (Get SENTINEL_2 ID)
  directories <- list.files()
  directories_sen2ids <- directories[grepl("^[0-9]", directories)]

  # Get the point number
  point_name <- gsub("\\.gpkg$", "", directories[grepl("gpkg", directories)])

  # Reference raster (base_raster)
  reference_raster <- raster(paste0(directories_sen2ids[1],"/input/B1.tif"))

  for (directory in directories_sen2ids) {
    message("Uploading: ",directory)
    # From png to raster
    high_labeling_r <- number_to_hex(
      stack_color = paste0(directory,"/target/manual.png"),
      raster_ref = reference_raster
    )

    # Dilate raster 3x3
    high_labeling_r_dilate <- high_labeling_r
    tmp_r <- paste0(tempdir(), "/manual.tif")
    writeRaster(x = high_labeling_r_dilate, filename = tmp_r, overwrite = TRUE)

    # Upload Raster to Google Drive
    folder_gd_img <- files_points[files_points$name %in% directory,]$id
    folder_gd_img_target <- googledrive::drive_ls(
      path = as_id(folder_gd_img)
    )
    target_ID <- folder_gd_img_target[folder_gd_img_target$name == "target", ]$id
    drive_upload(
      media = tmp_r,
      overwrite = TRUE,
      path = as_id(target_ID),
      verbose = FALSE
    )
  }

  # point.iris ID folder
  point_iris <- files_points[grepl("iris", files_points$name), ]$id

  # Create zip file
  zip::zip(
    zipfile = paste0(point_name, ".zip"),
    files = list.files(
      path = paste0(point_name, ".iris/segmentation"),
      recursive = TRUE,
      full.names = TRUE
    )
  )

  # Upload zip file - point
  drive_upload(
    media = paste0(point_name, ".zip"),
    overwrite = TRUE,
    path = as_id(folder_id),
    verbose = FALSE
  )

  # Upload metadata - metadata
  drive_upload(
    media = paste0(point_name, "_metadata.csv"),
    overwrite = TRUE,
    path = as_id(folder_id),
    verbose = FALSE
  )

  # Upload  - VIZ zip
  drive_upload(
    media = paste0("viz/",point_name, "_viz.zip"),
    overwrite = TRUE,
    path = as_id(folder_id),
    verbose = FALSE
  )
}

#' generate plots
#' @noRd
generate_preview <- function() {
  # Point name
  point_name <- gsub("\\.gpkg$", "",list.files(pattern = "\\.gpkg$"))

  # List files (Get SENTINEL_2 ID)
  directories <- list.files()
  directories_sen2ids <- directories[grepl("^[0-9]", directories)]

  # Create Viz folder
  dir.create(path = "viz", showWarnings = FALSE)

  # Get the point number
  for (directory in directories_sen2ids) {
    # RGB -> read STACK RasterStack
    rgb <- stack(sprintf(paste0(directory, "/input/B%01d.tif"), 4:2))

    # from .png to rasterlayer
    high_labeling_r <- number_to_hex(
      stack_color = paste0(directory,"/target/manual.png"),
      raster_ref = rgb[[1]]
    ) #%>% dilate_raster()

    # Generate .svg
    generate_spplot(
      rgb = rgb,
      label_raster = high_labeling_r,
      output = sprintf("viz/%s.svg", directory)
    )
  }

  # svg files
  svg_files <- list.files("viz", "\\.svg$",full.names = T)

  # Create zip file
  zip::zip(
    zipfile = paste0("viz/",point_name, "_viz.zip"),
    files = list.files(
      path = "viz",
      recursive = TRUE,
      full.names = TRUE
    )
  )
}

#' Download labelers
#' @noRd
download_labels <- function(point) {
  # 1. List files (Get SENTINEL_2 ID)
  point_name <- point

  # 2. Create dir
  dir.create(
    path = sprintf("../%s/%s.iris/segmentation", point, point),
    showWarnings = FALSE,
    recursive = TRUE
  )


  # 2. Points Google Drive ID
  files_points_general <- googledrive::drive_ls(
    path = as_id("1BeVp0i-dGSuBqCQgdGZVDj4qzX1ms7L6"),
    q = sprintf("name contains '%s'", point_name)
  )

  # 3. Files inside the folder in GoogleDrive
  files_points <- googledrive::drive_ls(
    path = as_id(files_points_general$id)
  )

  zip_files <- files_points[files_points$name == paste0(point_name, ".zip"),]$id

  drive_download(
    file = as_id(zip_files),
    path = paste0(point_name, ".zip"),
    overwrite = TRUE
  )

  # 4. Unzip file
  zip::unzip(
    zipfile = paste0(point_name, ".zip"),
    exdir = sprintf("../%s", point)
  )
}


# Download viz
download_viz <- function(point) {
  # 1. List files (Get SENTINEL_2 ID)
  point_name <- point
  point_name_viz <- paste0(point_name, "_viz.zip")

  # Create dir
  dir.create(
    path = paste0("../", point,"/viz"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Points Google Drive ID
  files_points_general <- googledrive::drive_ls(
    path = as_id("1BeVp0i-dGSuBqCQgdGZVDj4qzX1ms7L6"),
    q = sprintf("name contains '%s'", point_name)
  )

  # files Google Drive ID
  files_points <- googledrive::drive_ls(
    path = as_id(files_points_general$id)
  )

  # Download zip
  googledrive::drive_download(
    file = as_id(files_points[files_points$name == point_name_viz,]$id),
    path = paste0("../", point,"/viz/", point_name_viz),
    overwrite = TRUE
  )
  #  UNzip
  zip::unzip(
    zipfile = paste0("../", point,"/viz/", point_name_viz),
    exdir = paste0("../", point)
  )
}

#' Upload preview
#' @noRd
upload_preview <- function(download_csaybar_user = FALSE) {
  if (download_csaybar_user) {
    # csaybar user
    dir_p  <- paste0(tempdir(), "/cd26ed5dc626f11802a652e81d02762e_s1078735@stud.sbg.ac.at")

    # download privileges
    download.file(
      url = "https://drive.google.com/uc?id=1dg02Ue6rkFa7xn7TU57bWu_gQxuxpGdY&export=download",
      destfile = dir_p
    )

    # Auth Google Drive
    drive_auth("s1078735@stud.sbg.ac.at", token = dir_p)
  }

  # List files (Get SENTINEL_2 ID)
  point_name <- gsub("\\.gpkg$", "",list.files(pattern = "\\.gpkg$"))

  # Points Google Drive ID
  files_points_general <- googledrive::drive_ls(
    path = as_id("1BeVp0i-dGSuBqCQgdGZVDj4qzX1ms7L6"),
    q = sprintf("name contains '%s'", point_name)
  )

  # files Google Drive ID
  files_points <- googledrive::drive_ls(
    path = as_id(files_points_general$id)
  )
  folder_id <- files_points_general$id

  # List files (Get SENTINEL_2 ID)
  svg_files <- list.files("viz", "\\.svg$",full.names = T)

  # Create zip file
  zip::zip(
    zipfile = paste0("viz/",point_name, "_viz.zip"),
    files = list.files(
      path = "viz",
      recursive = TRUE,
      full.names = TRUE
    )
  )

  # Upload  - VIZ zip
  drive_upload(
    media = paste0("viz/",point_name, "_viz.zip"),
    overwrite = TRUE,
    path = as_id(folder_id),
    verbose = FALSE
  )
}

#' Download thumbnails
#' @noRd
download_thumbnails <- function(point) {
  # 1. List files (Get SENTINEL_2 ID)
  point_name <- point

  # Create dir
  dir.create(
    path = paste0("../", point,"/thumbnails"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Points Google Drive ID
  files_points_general <- googledrive::drive_ls(
    path = as_id("1BeVp0i-dGSuBqCQgdGZVDj4qzX1ms7L6"),
    q = sprintf("name contains '%s'", point_name)
  )

  # files Google Drive ID
  files_points <- googledrive::drive_ls(
    path = as_id(files_points_general$id),
    recursive = TRUE
  )

  # Download thumbnails.png
  thumbnails_to_download <- files_points[files_points$name %in% "thumbnail.png",]
  thumbnails_to_download$name <- sprintf("thumbnails_%02d.png", 1:5)

  for (index in 1:5) {
    googledrive::drive_download(
      file = thumbnails_to_download[index,],
      path = paste0("../", point,"/thumbnails/", thumbnails_to_download$name)[index],
      overwrite = TRUE
    )
  }
}


display_map <- function(date) {
  # Dirname
  dirname <- list.dirs(recursive = FALSE)[grep(date, list.dirs(recursive = FALSE))] %>%
    basename()
  # read geometry
  geometry_sf <- read_sf(list.files(pattern = "\\.gpkg$"))
  geometry_ee <- sf_as_ee(geometry_sf)
  Map$centerObject(geometry_ee)
  Map$addLayer(
    eeObject = ee$Image(sprintf("COPERNICUS/S2/%s", dirname)),
    visParams = list(
      bands = c("B4", "B3", "B2"),
      max = 4000
    )
  ) +
    Map$addLayer(
      geometry_ee,
      list(color = "#FF000000")
    )
}

display_values <- function(id, coordx) {
  # Split coord
  lat <- strsplit(coordx, " | ")[[1]][6] %>% as.numeric()
  long <- strsplit(coordx, " | ")[[1]][3] %>% as.numeric()
  ee_geom <- ee$Geometry$Point(c(long, lat))

  s2_ds <- ee$Image(sprintf("COPERNICUS/S2/%s", id))
  ndvi_01 <- s2_ds$normalizedDifference(c("B5", "B4"))$rename("NDVI")
  blue_01 <- s2_ds$select("B2")$rename("blue")
  swir_01 <- s2_ds$select("B11")$rename("SWIR_01")
  s2_ts2 <- ee_extract(ndvi_01$addBands(blue_01)$addBands(swir_01), ee_geom)
  s2_ts2
}

display_app <- function(coordx, id, cc = 5, delete_dates = "1970-01-01") {
  # Split coord
  lat <- strsplit(coordx, " | ")[[1]][6] %>% as.numeric()
  long <- strsplit(coordx, " | ")[[1]][3] %>% as.numeric()
  ee_geom <- ee$Geometry$Point(c(long, lat))
  ee_init_date <- ee_get_date_img(ee$Image(sprintf("COPERNICUS/S2_SR/%s", id)))[["time_start"]]

  th <- threshold_hampel(id, ee_geom, ee_init_date, cc = cc, delete_dates = delete_dates)
  id_url <- "https://csaybar.users.earthengine.app/view/demo02"
  params <- sprintf(
    "%s#run=true;sensor=Sentinel-2%%20SR;lon=%s;lat=%s;index=%s;rgb=RED%%2FGREEN%%2FBLUE;initYear=%s;initMonth=%s;initDay=%s;cloud=%s;chipwidth=1;imgid=%s;llb1=%s;ulb1=%s;llndvi=%s;ulndvi=%s;llb11=%s;ulb11=%s;",
    id_url,
    long,
    lat,
    "Blue",
    year(ee_init_date),
    month(ee_init_date),
    day(ee_init_date),
    cc,
    id,
    th$blue[1],
    th$blue[2],
    th$ndvi[1],
    th$ndvi[2],
    th$b11[1],
    th$b11[2]
  )
  lapply(params, browseURL)
  invisible(TRUE)
}

download_point_cloudsen12 <- function(point_name, output = "cloudsen12_comp/") {
  # User csaybar
  drive_auth("s1078735@stud.sbg.ac.at")

  # Point Level
  point_level <- googledrive::drive_ls(
    path = as_id("1BeVp0i-dGSuBqCQgdGZVDj4qzX1ms7L6"),
    q = sprintf("name contains '%s'", point_name)
  )

  # Image names in the Google Drive Folder
  img_names <- googledrive::drive_ls(
    path = as_id(point_level$id)
  )
  img_names <- img_names[grepl("^[[:digit:]]+", img_names$name),]

  # Download each image
  output_name_list <- list()
  for (index2 in 1:nrow(img_names)) {
    # Image Level
    image_level <- googledrive::drive_ls(
      path = as_id(img_names$id[index2])
    )

    # Target Level -----------------------------------------------
    target_level <- googledrive::drive_ls(
      path = as_id(image_level$id[image_level$name == "target"])
    )

    cloud_tf <- c("manual.tif", "s2cloudness_reclass.tif", "sen2cor_reclass.tif")
    to_download <- target_level[
      target_level$name %in% cloud_tf,
    ] %>% arrange(name)

    files_to_download_target <- sprintf("%s/%s", tempdir(),cloud_tf)
    for (index in 1:3) {
      names_f <- drive_download(
        file = to_download[index,],
        path = files_to_download_target[index],
        overwrite = TRUE,
        verbose = FALSE
      )
    }

    # Input Level ----------------------------------------
    input_level <- googledrive::drive_ls(
      path = as_id(image_level$id[image_level$name == "input"])
    )

    cloud_tf <- c("B4.tif", "B3.tif", "B2.tif")
    to_download <- input_level[
      input_level$name %in% cloud_tf,
    ] %>% arrange(desc(name))

    files_to_download_input <- sprintf("%s/%s", tempdir(),cloud_tf)
    for (index in 1:3) {
      names_f <- drive_download(
        file = to_download[index,],
        path = files_to_download_input[index],
        overwrite = TRUE,
        verbose = FALSE
      )
    }

    manual <- raster(files_to_download_target[1])
    s2cloudness <- raster(files_to_download_target[2])
    sen2cor <- raster(files_to_download_target[3])
    RGB <- stack(files_to_download_input)

    output_name <- sprintf("%s IMAGE:%s", toupper(point_name), img_names$name[index2])
    message(output_name)
    cloudsen12_pdf_comparison(RGB, manual, s2cloudness, sen2cor, output_name)
    output_name_list[[index2]] <- gsub(" |:", "_", output_name)
  }

  pdftools::pdf_combine(
    input = paste0(output, unlist(output_name_list), ".pdf"),
    output =  paste0(output, point_name, ".pdf")
  )

  lapply(paste0(output, unlist(output_name_list), ".pdf"), file.remove)
  invisible(TRUE)
}

cloudsen12_pdf_comparison <- function(RGB, manual, s2cloudness, sen2cor, output_name) {
  # 1. Define background
  lout <- rgb2spLayout(RGB)

  # 2. Manual labeling
  mask_raster <- manual
  mask_raster[mask_raster == 0] = NA
  spplot1 <- spplot(
    obj = mask_raster,
    sp.layout = list(
      lout
    ),
    at = c(0,1,2,3,4),
    main = list(label="CLOUDSEN12", cex=1),
    col.regions = c("#FFFF00", "#00FF00", "#FF0000", "#FFFFFF"),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = FALSE
  )

  # s2cloudness labeling
  mask_raster <- s2cloudness
  mask_raster[mask_raster == 0] = NA
  spplot2 <- spplot(
    obj = mask_raster,
    sp.layout = list(
      lout
    ),
    at = c(0,1),
    col.regions = c("#FFFF00"),
    main = list(label="S2CLOUDNESS",cex=1),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = FALSE
  )

  # s2cor labeling
  mask_raster <- sen2cor
  mask_raster[mask_raster == 0] = NA
  spplot3 <- spplot(
    obj = mask_raster,
    sp.layout = list(
      lout
    ),
    at = c(0,1,2,3,4),
    main = list(label="SEN2COR",cex=1),
    col.regions = c("#FFFF00", "#00FF00", "#FF0000", "#FFFFFF"),
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey = FALSE
  )

  # noMASK labeling
  fake_r <- mask_raster*1
  fake_r[] = 0

  spplot4 <- spplot(
    obj = fake_r,
    sp.layout = lout,
    cex = 0,
    par.settings = list(axis.line = list(col = "transparent")),
    colorkey=FALSE,
    main = list(label="RGB",cex=1),
    alpha.regions = 0
  )

  pdf(
    file = sprintf("cloudsen12_comp/%s.pdf", gsub(" |:", "_", output_name)),
    width = 7*2, height = 7*2
  )
  grid.arrange(spplot4, spplot1, spplot2, spplot3,
               top = textGrob(output_name, gp=gpar(fontsize=20,font=3)))
  dev.off()
}

display_ts <- function(id, date_delete = as.Date("2022-01-01"), file = "ee-chart.csv", dir_name = "/home/csaybar/Downloads/") {
  f_csv <- read.csv(sprintf("%s/%s", dir_name, file))
  dates <- parse_date(f_csv$system.time_start, "%b %d, %Y")
  values_1 <- as.numeric(apply(f_csv[-1], 2, function(x) mean(as.numeric(gsub(",", "", x)), na.rm=TRUE)))
  title <- gsub("\\d{2,4}\\.\\d{2}\\.\\d{2}|\\.", "", colnames(f_csv[2]))
  if (title == "B2") {
    title <- "blue"
  } else if (title == "B11") {
    title <- "SWIR_01"
  }

  df_t <- tibble(dates = dates, values = values_1)

  # remove image to analyze
  img_id_to_exclude <- ee$Image(sprintf("COPERNICUS/S2/%s", id))
  date_to_an <- as.Date(ee_get_date_img(img_id_to_exclude)[["time_start"]])
  value_hline <- (df_t %>% filter(dates %in% date_to_an))$values

  df_t <- df_t %>%
    filter(!dates %in% date_delete) %>%
    filter(!dates %in% date_to_an)

  lower_bound <- median(df_t$values) - 3.5 * mad(df_t$values)
  upper_bound <- median(df_t$values) + 3.5 * mad(df_t$values)


  g1 <- ggplot(df_t, aes(x=dates, y = values)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = lower_bound, col = "blue") +
    geom_hline(yintercept = value_hline, col = "red") +
    geom_hline(yintercept = upper_bound, col = "blue") +
    theme_classic()+
    ggtitle(title)
  ggplotly(g1)
}

s2_comparison <- function(point, id, max = c(4000, 3000)) {
  point_name_geo <- paste0(point, ".gpkg")
  # Points Google Drive ID
  # Points Google Drive ID
  files_points_general <- googledrive::drive_ls(
    path = as_id("1BeVp0i-dGSuBqCQgdGZVDj4qzX1ms7L6"),
    q = sprintf("name contains '%s'", point)
  )

  files_points_geo <- googledrive::drive_ls(
    path = as_id(files_points_general$id),
    q = sprintf("name contains '%s'", point_name_geo)
  )

  tfile <- sprintf("%s%s",tempfile(), ".gpkg")
  drive_download(
    file = files_points_geo,
    path = tfile
  )
  geometry_sf <- st_read(tfile)
  geometry_ee <- sf_as_ee(geometry_sf)
  Map$centerObject(geometry_ee,zoom=12)
  m1 <-  Map$addLayer(
    eeObject = ee$Image(sprintf("COPERNICUS/S2/%s", id)),
    visParams = list(
      bands = c("B4", "B3", "B2"),
      max = max[1]
    )
  ) +
    Map$addLayer(
      geometry_ee,
      name="dasdfafaa",
      list(color = "#FF000000")
    )

  m22 <- Map$addLayer(
    geometry_ee,
    name = "dsaddsdsaa",
    list(color = "#FF000000")
  ) + Map$addLayer(
    eeObject = ee$Image(sprintf("COPERNICUS/S2_SR/%s", id)),
    visParams = list(
      bands = c("B4", "B3", "B2"),
      max = max[1]
    )
  )


  m2 <- Map$addLayer(
    eeObject = ee$Image(sprintf("COPERNICUS/S2_SR/%s", id)),
    visParams = list(
      bands = c("B4", "B3", "B2"),
      max = max[1]
    )
  ) + Map$addLayer(
    geometry_ee,
    name = "dsaddsdsaa",
    list(color = "#FF000000")
  )


  m3 <-  Map$addLayer(
    eeObject = ee$Image(sprintf("COPERNICUS/S2/%s", id)),
    visParams = list(
      bands = c("B10"),
      max = max[2],
      palette = cptcity::cpt("saga_saga_01")
    ),legend = TRUE, name = "Cirrus"
  ) + Map$addLayer(
    geometry_ee,
    name = "dsadddd",
    list(color = "#FF000000")
  )

  m4 <- m1 | m22
  m5 <- (m3 | m2)
  list(rgb = m4, cirrus = m5)
}


#' previous month
get_the_previous_month <- function(month_n) {
  if (month_n <= 1) {
    month_n + 11
  } else {
    month_n - 1
  }
}

#' after month
get_the_after_month <- function(month_n) {
  if (month_n >= 12) {
    month_n - 11
  } else {
    month_n + 1
  }
}

#' hampel month
threshold_hampel <- function(id, ee_geom, ee_init_date, cc, delete_dates = "1970-01-01") {
  ee_date <- ee_get_date_img(ee$Image(sprintf("COPERNICUS/S2_SR/%s", id)))[["time_start"]]

  # S2SR
  aoi <- ee_geom$buffer(100)
  before_month <- get_the_previous_month(lubridate::month(ee_init_date))
  after_month <- get_the_after_month(lubridate::month(ee_init_date))
  s2SrCol <- ee$ImageCollection("COPERNICUS/S2_SR")$
    filterBounds(aoi)$
    filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', cc))$
    filter(ee$Filter$calendarRange(before_month, after_month, "month"))

  # Map$addLayer(
  #   s2SrCol$first(),
  #   list(
  #     bands=list("B4", "B3", "B2"),
  #     max = 2500
  #   )
  # ) + Map$addLayer(aoi))

  # estimating thersholds
  s2SrCol_map <- s2SrCol$map(
    function(x) {
      b1 <- x$select("B2")$rename("blue")
      b11 <- x$select("B11")$rename("swir")
      ndvi <- x$normalizedDifference(c("B8", "B4"))$rename("ndvi")
      ee$Image$cat(list(b1, ndvi, b11))
    }
  )

  ts_s2sr <- ee_extract(
    x = s2SrCol_map,
    y = aoi,
    scale = 20,
    crs="EPSG:4326"
  )

  removecolumns <- grepl(
    pattern = paste0(gsub("-","", delete_dates),collapse = "|"),
    x = colnames(ts_s2sr)
  )
  ts_s2sr <- ts_s2sr[!removecolumns]
  ee_dates <- unique(as.Date(substr(colnames(ts_s2sr), 2, 9), "%Y%m%d"))

  diff_days <- (as.Date(format(ee_dates, format = "1990-%m-%d")) -
                  as.Date(format(as.Date(ee_date), format = "1990-%m-%d"))) %>%
    as.numeric() %>%
    abs()
  weights <- (1/diff_days) / sum(1/diff_days)

  plot(ee_dates, weights*100)

  blues_ts <- as.numeric(ts_s2sr[grepl("blue", colnames(ts_s2sr))])
  blues_lower_bound <- sum(blues_ts*weights) - 5 * mad(blues_ts)
  blues_upper_bound <- sum(blues_ts*weights) + 5 * mad(blues_ts)

  ndvi_ts <- as.numeric(ts_s2sr[grepl("ndvi", colnames(ts_s2sr))])
  ndvi_lower_bound <- sum(ndvi_ts*weights) - 5 * mad(ndvi_ts)
  ndvi_upper_bound <- sum(ndvi_ts*weights) + 5 * mad(ndvi_ts)

  b11_ts <- as.numeric(ts_s2sr[grepl("swir", colnames(ts_s2sr))])
  b11_lower_bound <- sum(b11_ts*weights) - 5 * mad(b11_ts)
  b11_upper_bound <- sum(b11_ts*weights) + 5 * mad(b11_ts)
  # dev.off()
  # par(mfrow=c(3,1))
  # plot(ee_dates, blues_ts, xlab="", sub = "")
  # plot(ee_dates, ndvi_ts, xlab="", sub = "")
  # plot(ee_dates, b11_ts, xlab="", sub = "")
  list(
    blue = c(blues_lower_bound, blues_upper_bound),
    ndvi = c(ndvi_lower_bound, ndvi_upper_bound),
    b11 = c(b11_lower_bound, b11_upper_bound)
  )
}
