<h1 align="center">
  <br>
  <img src=https://user-images.githubusercontent.com/54723897/113879941-4e1af480-97bb-11eb-83f3-e0ec8772b7c4.gif width=500px>
  <br>    
</h1>

<h2 align="center">A global benchmark dataset for cloud semantic understanding</h2>


## Cloud visual inspector - Google Earth Engine App

<center>
  <a href=https://csaybar.users.earthengine.app/view/demo02#run=true;sensor=Sentinel-2%20SR;lon=-69.76606;lat=-48.99801;index=Blue;rgb=RED%2FGREEN%2FBLUE;initYear=2019;initMonth=2;initDay=12;cloud=5;chipwidth=1;imgid=20190212T142031_20190212T143214_T19FDF;llb1=763.743612800034;ulb1=1218.71887923383;llndvi=0.0688391460953984;ulndvi=0.160478652095398;llb11=2345.39890977067;ulb11=3520.32481478227;>
    <img src=https://user-images.githubusercontent.com/54723897/114320875-6458e580-9b18-11eb-9ad5-4fc45e2c6441.png>
  </a>
</center>

### Why we create this app?

This Google Earth Engine App helps labelers to recognize if a pixel is a cloud. Opaque clouds are easily to identify, however, semi-transparent clouds could be a challenge. In cloudSEN12 we considerate that a pixel is a cloud is it meet the following requirements.

(1) We obtain all the images that were obtained in the same season (temporal windows moving of 1 month) with a cloud coverage less than 5 % according to the cloudy pixel percentage taken from the original metadata.

(2) We manually eliminate some images that the algorithm does not automatically discard.

(3) We estimate the hampel filter considering the proximity of the images in the NDVI, Blue and SWIR band.

(4) If the pixel present a value higher than the Hampel threshold we considerate the pixel as a cloud.


### App parameters

- **run:** Render graphics after click?. By default true.
- **sensor:** Sensor data to be analyzed. By default `Sentinel-2 SR`.
- **lon:** Longitude data. If run is true, it can be obtained by clicking on the map. By default -121.68804.
- **lat:** Latitude data. If run is true, it can be obtained by clicking on the map. By default 36.46517.
- **rgb:** Image composition of image thumbnails. By default `SWIR1-NIR-GREEN`.
- **initYear:** Year acquisition time of the image to analyze. By default 2018.
- **initMonth:** Month acquisition time of the image to analyze. By default 8.
- **initDay:** Day acquisition time** of the image to analyze. By default 12.
- **cloud:** Cloudy pixel percentage** threshold. By default 5
- **chipwidth:** Size of the chip in the image thumbnail section. By default 2.
- **imgid:** Image id of the image to be analyzed. By default `20190212T142031_20190212T143214_T19FDF`.
- **llb1:** Blue Hampel lower threshold. By default -1.
- **ulb1:** Blue Hampel upper thershold. By default 1.
- **llndvi:** NDVI Hampel lower thershold. By default -1.
- **ulndvi:** NDVI Hampel upper thershold. By default 1.
- **llb11:** SWIR1 Hampel lower threshold. By default -1.
- **ulb11:** SWIR1 Hampel upper threshold. By default 1.

### Demo

[Try it yourself here](https://csaybar.users.earthengine.app/view/demo02#run=true;sensor=Sentinel-2%20SR;lon=-69.76606;lat=-48.99801;index=Blue;rgb=RED%2FGREEN%2FBLUE;initYear=2019;initMonth=2;initDay=12;cloud=5;chipwidth=1;imgid=20190212T142031_20190212T143214_T19FDF;llb1=763.743612800034;ulb1=1218.71887923383;llndvi=0.0688391460953984;ulndvi=0.160478652095398;llb11=2345.39890977067;ulb11=3520.32481478227;). If you prefer run the [cloudsen12_app.js](https://code.earthengine.google.com/eb7a6718eeb2170cac3428b52ffefdc5) in the Earth Engine code editor. Use the script `ee_viz_cloud.R` to automatically create the main parameters of the app.

### Credits
