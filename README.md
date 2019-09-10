# weather-lisp
A Common Lisp client for pulling weather data from the openweathermap.org API.

This library pulls both current world weather data, as well as short-
and long-term forecasts from the OpenWeatherMap.org public REST
API. The owner of this site makes this data freely available, but
there are restrictions on how often this data is accessed without
purchasing a subscription.

An API key is required to use the API. There are options for both free
and paid subscriptions. Information is available at:

https://openweathermap.org/appid#get

Note that I do not have a paid subscription, and have been unable to
test code functionality for the long-term forcasts, as they are not
available with a free subscription.

Documentation on supplied API parameters, as well as the returned
data, is available here:

https://openweathermap.org/api

There are three functions available in this package. (wx-current ...)
fetches current weather data. (wx-short-term-forecast ...) fetches
short-term weather forecast data, and (wx-long-term-forecast ...)
fetches long-term weather data. All three require your API key (which
OpenWeatherMap refers to as the APPID) as the first
parameter. Additional parameters are available for specifying the
location you want data for:

* :city-name - the name of the city
* :zip - zip code (defaults to "us" for country)
* :country-name - the name of the country (optional, can be used with
  :zip and :city-name)
* :city-id - ID number for specific city (list of cities can be pulled
  down from http://bulk.openweathermap.org/sample/ - file is
  city.list.json.gz)
* :cnt - number of days to forecast (only valid with
  wx-long-term-forecast).
* :units - temperature units. Can be :imperial, :metric,
  :kelvin. Defaults to :imperial.

The data returned is returned as an alist, and is documented in the
OpenWeatherMap API Documenation page, linked above.

````
CL-USER> (weather:wx-current "a7f68058f03a2cb7af0a6a2edf77a6af" :zip 98296)
http://api.openweathermap.org/data/2.5/weather?zip=98296&APPID=a7f68058f03a2cb7af0a6a2edf77a6af
((:COORD (:LON . -122.09) (:LAT . 47.86))
 (:WEATHER
  ((:ID . 711) (:MAIN . "Smoke") (:DESCRIPTION . "smoke") (:ICON . "50d"))
  ((:ID . 721) (:MAIN . "Haze") (:DESCRIPTION . "haze") (:ICON . "50d")))
 (:BASE . "stations")
 (:MAIN (:TEMP . 292.14) (:PRESSURE . 1013) (:HUMIDITY . 82)
  (:TEMP--MIN . 291.15) (:TEMP--MAX . 294.15))
 (:VISIBILITY . 6437) (:WIND (:SPEED . 1.13) (:DEG . 238.004))
 (:CLOUDS (:ALL . 1)) (:DT . 1504803360)
 (:SYS (:TYPE . 1) (:ID . 2931) (:MESSAGE . 0.1356) (:COUNTRY . "US")
  (:SUNRISE . 1504791374) (:SUNSET . 1504838084))
 (:ID . 0) (:NAME . "Snohomish") (:COD . 200))
CL-USER> (weather:wx-current "a7f68058f03a2cb7af0a6a2edf77a6af" :city-name "snohomish" :country-name "us")
http://api.openweathermap.org/data/2.5/weather?q=snohomish,us&APPID=a7f68058f03a2cb7af0a6a2edf77a6af
((:COORD (:LON . -122.1) (:LAT . 47.91))
 (:WEATHER
  ((:ID . 711) (:MAIN . "Smoke") (:DESCRIPTION . "smoke") (:ICON . "50d"))
  ((:ID . 721) (:MAIN . "Haze") (:DESCRIPTION . "haze") (:ICON . "50d")))
 (:BASE . "stations")
 (:MAIN (:TEMP . 292.15) (:PRESSURE . 1013) (:HUMIDITY . 82)
  (:TEMP--MIN . 291.15) (:TEMP--MAX . 294.15))
 (:VISIBILITY . 11265) (:WIND (:SPEED . 3.1) (:DEG . 200)) (:CLOUDS (:ALL . 1))
 (:DT . 1504805700)
 (:SYS (:TYPE . 1) (:ID . 2924) (:MESSAGE . 0.0051) (:COUNTRY . "US")
  (:SUNRISE . 1504791373) (:SUNSET . 1504838090))
 (:ID . 5810988) (:NAME . "Snohomish") (:COD . 200))
CL-USER> (weather:wx-current "a7f68058f03a2cb7af0a6a2edf77a6af" :city-id 5810988)
http://api.openweathermap.org/data/2.5/weather?id=5810988&APPID=a7f68058f03a2cb7af0a6a2edf77a6af
((:COORD (:LON . -122.1) (:LAT . 47.91))
 (:WEATHER
  ((:ID . 711) (:MAIN . "Smoke") (:DESCRIPTION . "smoke") (:ICON . "50d"))
  ((:ID . 721) (:MAIN . "Haze") (:DESCRIPTION . "haze") (:ICON . "50d")))
 (:BASE . "stations")
 (:MAIN (:TEMP . 292.15) (:PRESSURE . 1013) (:HUMIDITY . 82)
  (:TEMP--MIN . 291.15) (:TEMP--MAX . 294.15))
 (:VISIBILITY . 11265) (:WIND (:SPEED . 3.1) (:DEG . 200)) (:CLOUDS (:ALL . 1))
 (:DT . 1504805700)
 (:SYS (:TYPE . 1) (:ID . 2924) (:MESSAGE . 0.0052) (:COUNTRY . "US")
  (:SUNRISE . 1504791373) (:SUNSET . 1504838089))
 (:ID . 5810988) (:NAME . "Snohomish") (:COD . 200))
CL-USER> 
````

````
CL-USER> (weather:wx-short-term-forecast "a7f68058f03a2cb7af0a6a2edf77a6af" :city-id 5810988)
http://api.openweathermap.org/data/2.5/forecast?id=5810988&APPID=a7f68058f03a2cb7af0a6a2edf77a6af
((:COD . "200") (:MESSAGE . 0.0068) (:CNT . 40)
 (:LIST
  ((:DT . 1504818000)
   (:MAIN (:TEMP . 296.49) (:TEMP--MIN . 294.896) (:TEMP--MAX . 296.49)
    (:PRESSURE . 967.88) (:SEA--LEVEL . 1026.09) (:GRND--LEVEL . 967.88)
    (:HUMIDITY . 70) (:TEMP--KF . 1.59))
   (:WEATHER
    ((:ID . 804) (:MAIN . "Clouds") (:DESCRIPTION . "overcast clouds")
     (:ICON . "04d")))
   (:CLOUDS (:ALL . 92)) (:WIND (:SPEED . 1.13) (:DEG . 246))
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-07 21:00:00"))
  ((:DT . 1504828800)
   (:MAIN (:TEMP . 294.51) (:TEMP--MIN . 293.447) (:TEMP--MAX . 294.51)
    (:PRESSURE . 967.15) (:SEA--LEVEL . 1025.34) (:GRND--LEVEL . 967.15)
    (:HUMIDITY . 74) (:TEMP--KF . 1.06))
   (:WEATHER
    ((:ID . 804) (:MAIN . "Clouds") (:DESCRIPTION . "overcast clouds")
     (:ICON . "04n")))
   (:CLOUDS (:ALL . 92)) (:WIND (:SPEED . 1.05) (:DEG . 282.504))
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-08 00:00:00"))
  ((:DT . 1504839600)
   (:MAIN (:TEMP . 291.9) (:TEMP--MIN . 291.367) (:TEMP--MAX . 291.9)
    (:PRESSURE . 967.49) (:SEA--LEVEL . 1025.92) (:GRND--LEVEL . 967.49)
    (:HUMIDITY . 80) (:TEMP--KF . 0.53))
   (:WEATHER
    ((:ID . 802) (:MAIN . "Clouds") (:DESCRIPTION . "scattered clouds")
     (:ICON . "03n")))
   (:CLOUDS (:ALL . 48)) (:WIND (:SPEED . 0.77) (:DEG . 274.002))
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-08 03:00:00"))
  ((:DT . 1504850400)
   (:MAIN (:TEMP . 289.426) (:TEMP--MIN . 289.426) (:TEMP--MAX . 289.426)
    (:PRESSURE . 967.63) (:SEA--LEVEL . 1026.25) (:GRND--LEVEL . 967.63)
    (:HUMIDITY . 88) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 802) (:MAIN . "Clouds") (:DESCRIPTION . "scattered clouds")
     (:ICON . "03n")))
   (:CLOUDS (:ALL . 36)) (:WIND (:SPEED . 0.76) (:DEG . 46.0138))
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-08 06:00:00"))
  ((:DT . 1504861200)
   (:MAIN (:TEMP . 288.397) (:TEMP--MIN . 288.397) (:TEMP--MAX . 288.397)
    (:PRESSURE . 967.91) (:SEA--LEVEL . 1026.66) (:GRND--LEVEL . 967.91)
    (:HUMIDITY . 93) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 802) (:MAIN . "Clouds") (:DESCRIPTION . "scattered clouds")
     (:ICON . "03n")))
   (:CLOUDS (:ALL . 48)) (:WIND (:SPEED . 0.52) (:DEG . 111.502))
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-08 09:00:00"))
  ((:DT . 1504872000)
   (:MAIN (:TEMP . 287.75) (:TEMP--MIN . 287.75) (:TEMP--MAX . 287.75)
    (:PRESSURE . 968.26) (:SEA--LEVEL . 1027.07) (:GRND--LEVEL . 968.26)
    (:HUMIDITY . 95) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 802) (:MAIN . "Clouds") (:DESCRIPTION . "scattered clouds")
     (:ICON . "03n")))
   (:CLOUDS (:ALL . 32)) (:WIND (:SPEED . 0.89) (:DEG . 148.501))
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-08 12:00:00"))
  ((:DT . 1504882800)
   (:MAIN (:TEMP . 288.458) (:TEMP--MIN . 288.458) (:TEMP--MAX . 288.458)
    (:PRESSURE . 968.59) (:SEA--LEVEL . 1027.57) (:GRND--LEVEL . 968.59)
    (:HUMIDITY . 93) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 801) (:MAIN . "Clouds") (:DESCRIPTION . "few clouds")
     (:ICON . "02d")))
   (:CLOUDS (:ALL . 12)) (:WIND (:SPEED . 0.92) (:DEG . 145))
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-08 15:00:00"))
  ((:DT . 1504893600)
   (:MAIN (:TEMP . 291.394) (:TEMP--MIN . 291.394) (:TEMP--MAX . 291.394)
    (:PRESSURE . 968.95) (:SEA--LEVEL . 1027.55) (:GRND--LEVEL . 968.95)
    (:HUMIDITY . 85) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 802) (:MAIN . "Clouds") (:DESCRIPTION . "scattered clouds")
     (:ICON . "03d")))
   (:CLOUDS (:ALL . 48)) (:WIND (:SPEED . 1.08) (:DEG . 220.501))
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-08 18:00:00"))
  ((:DT . 1504904400)
   (:MAIN (:TEMP . 292.692) (:TEMP--MIN . 292.692) (:TEMP--MAX . 292.692)
    (:PRESSURE . 968.37) (:SEA--LEVEL . 1026.78) (:GRND--LEVEL . 968.37)
    (:HUMIDITY . 81) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 804) (:MAIN . "Clouds") (:DESCRIPTION . "overcast clouds")
     (:ICON . "04d")))
   (:CLOUDS (:ALL . 92)) (:WIND (:SPEED . 1.13) (:DEG . 235.005))
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-08 21:00:00"))
  ((:DT . 1504915200)
   (:MAIN (:TEMP . 292.189) (:TEMP--MIN . 292.189) (:TEMP--MAX . 292.189)
    (:PRESSURE . 967.72) (:SEA--LEVEL . 1026.03) (:GRND--LEVEL . 967.72)
    (:HUMIDITY . 81) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 92)) (:WIND (:SPEED . 1.01) (:DEG . 214.508))
   (:RAIN (:|3-H| . 0.02)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-09 00:00:00"))
  ((:DT . 1504926000)
   (:MAIN (:TEMP . 290.921) (:TEMP--MIN . 290.921) (:TEMP--MAX . 290.921)
    (:PRESSURE . 967.76) (:SEA--LEVEL . 1026.34) (:GRND--LEVEL . 967.76)
    (:HUMIDITY . 86) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 88)) (:WIND (:SPEED . 0.92) (:DEG . 204.506))
   (:RAIN (:|3-H| . 0.22)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-09 03:00:00"))
  ((:DT . 1504936800)
   (:MAIN (:TEMP . 289.909) (:TEMP--MIN . 289.909) (:TEMP--MAX . 289.909)
    (:PRESSURE . 968.5) (:SEA--LEVEL . 1027.24) (:GRND--LEVEL . 968.5)
    (:HUMIDITY . 84) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 88)) (:WIND (:SPEED . 0.96) (:DEG . 222.501))
   (:RAIN (:|3-H| . 0.15)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-09 06:00:00"))
  ((:DT . 1504947600)
   (:MAIN (:TEMP . 287.843) (:TEMP--MIN . 287.843) (:TEMP--MAX . 287.843)
    (:PRESSURE . 969.22) (:SEA--LEVEL . 1028.21) (:GRND--LEVEL . 969.22)
    (:HUMIDITY . 80) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 56)) (:WIND (:SPEED . 0.91) (:DEG . 207.5))
   (:RAIN (:|3-H| . 0.05)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-09 09:00:00"))
  ((:DT . 1504958400)
   (:MAIN (:TEMP . 284.815) (:TEMP--MIN . 284.815) (:TEMP--MAX . 284.815)
    (:PRESSURE . 969.9) (:SEA--LEVEL . 1029.39) (:GRND--LEVEL . 969.9)
    (:HUMIDITY . 83) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 801) (:MAIN . "Clouds") (:DESCRIPTION . "few clouds")
     (:ICON . "02n")))
   (:CLOUDS (:ALL . 20)) (:WIND (:SPEED . 0.86) (:DEG . 202.503)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-09 12:00:00"))
  ((:DT . 1504969200)
   (:MAIN (:TEMP . 284.14) (:TEMP--MIN . 284.14) (:TEMP--MAX . 284.14)
    (:PRESSURE . 971.25) (:SEA--LEVEL . 1030.73) (:GRND--LEVEL . 971.25)
    (:HUMIDITY . 91) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 803) (:MAIN . "Clouds") (:DESCRIPTION . "broken clouds")
     (:ICON . "04d")))
   (:CLOUDS (:ALL . 64)) (:WIND (:SPEED . 0.91) (:DEG . 210.501)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-09 15:00:00"))
  ((:DT . 1504980000)
   (:MAIN (:TEMP . 287.277) (:TEMP--MIN . 287.277) (:TEMP--MAX . 287.277)
    (:PRESSURE . 972.24) (:SEA--LEVEL . 1031.51) (:GRND--LEVEL . 972.24)
    (:HUMIDITY . 79) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 803) (:MAIN . "Clouds") (:DESCRIPTION . "broken clouds")
     (:ICON . "04d")))
   (:CLOUDS (:ALL . 64)) (:WIND (:SPEED . 0.97) (:DEG . 216.005)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-09 18:00:00"))
  ((:DT . 1504990800)
   (:MAIN (:TEMP . 288.924) (:TEMP--MIN . 288.924) (:TEMP--MAX . 288.924)
    (:PRESSURE . 973.19) (:SEA--LEVEL . 1032.23) (:GRND--LEVEL . 973.19)
    (:HUMIDITY . 66) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01d")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 1.07) (:DEG . 214.501)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-09 21:00:00"))
  ((:DT . 1505001600)
   (:MAIN (:TEMP . 289.288) (:TEMP--MIN . 289.288) (:TEMP--MAX . 289.288)
    (:PRESSURE . 973.32) (:SEA--LEVEL . 1032.29) (:GRND--LEVEL . 973.32)
    (:HUMIDITY . 62) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 801) (:MAIN . "Clouds") (:DESCRIPTION . "few clouds")
     (:ICON . "02n")))
   (:CLOUDS (:ALL . 12)) (:WIND (:SPEED . 1.07) (:DEG . 232.002)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-10 00:00:00"))
  ((:DT . 1505012400)
   (:MAIN (:TEMP . 286.42) (:TEMP--MIN . 286.42) (:TEMP--MAX . 286.42)
    (:PRESSURE . 974.82) (:SEA--LEVEL . 1034.1) (:GRND--LEVEL . 974.82)
    (:HUMIDITY . 74) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 88)) (:WIND (:SPEED . 0.37) (:DEG . 173.5))
   (:RAIN (:|3-H| . 0.26)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-10 03:00:00"))
  ((:DT . 1505023200)
   (:MAIN (:TEMP . 285.623) (:TEMP--MIN . 285.623) (:TEMP--MAX . 285.623)
    (:PRESSURE . 976.01) (:SEA--LEVEL . 1035.61) (:GRND--LEVEL . 976.01)
    (:HUMIDITY . 79) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 88)) (:WIND (:SPEED . 0.77) (:DEG . 109.502))
   (:RAIN (:|3-H| . 0.03)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-10 06:00:00"))
  ((:DT . 1505034000)
   (:MAIN (:TEMP . 285.244) (:TEMP--MIN . 285.244) (:TEMP--MAX . 285.244)
    (:PRESSURE . 976.87) (:SEA--LEVEL . 1036.63) (:GRND--LEVEL . 976.87)
    (:HUMIDITY . 89) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 100)) (:WIND (:SPEED . 1.02) (:DEG . 111.003))
   (:RAIN (:|3-H| . 0.46)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-10 09:00:00"))
  ((:DT . 1505044800)
   (:MAIN (:TEMP . 284.841) (:TEMP--MIN . 284.841) (:TEMP--MAX . 284.841)
    (:PRESSURE . 977.78) (:SEA--LEVEL . 1037.7) (:GRND--LEVEL . 977.78)
    (:HUMIDITY . 99) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10n")))
   (:CLOUDS (:ALL . 92)) (:WIND (:SPEED . 0.76) (:DEG . 135.001))
   (:RAIN (:|3-H| . 0.6)) (:SYS (:POD . "n"))
   (:DT--TXT . "2017-09-10 12:00:00"))
  ((:DT . 1505055600)
   (:MAIN (:TEMP . 285.248) (:TEMP--MIN . 285.248) (:TEMP--MAX . 285.248)
    (:PRESSURE . 978.81) (:SEA--LEVEL . 1038.91) (:GRND--LEVEL . 978.81)
    (:HUMIDITY . 99) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10d")))
   (:CLOUDS (:ALL . 88)) (:WIND (:SPEED . 0.96) (:DEG . 144.001))
   (:RAIN (:|3-H| . 0.35)) (:SYS (:POD . "d"))
   (:DT--TXT . "2017-09-10 15:00:00"))
  ((:DT . 1505066400)
   (:MAIN (:TEMP . 287.034) (:TEMP--MIN . 287.034) (:TEMP--MAX . 287.034)
    (:PRESSURE . 979.95) (:SEA--LEVEL . 1039.64) (:GRND--LEVEL . 979.95)
    (:HUMIDITY . 92) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 500) (:MAIN . "Rain") (:DESCRIPTION . "light rain")
     (:ICON . "10d")))
   (:CLOUDS (:ALL . 80)) (:WIND (:SPEED . 1.05) (:DEG . 205.003))
   (:RAIN (:|3-H| . 0.06)) (:SYS (:POD . "d"))
   (:DT--TXT . "2017-09-10 18:00:00"))
  ((:DT . 1505077200)
   (:MAIN (:TEMP . 289.365) (:TEMP--MIN . 289.365) (:TEMP--MAX . 289.365)
    (:PRESSURE . 980.03) (:SEA--LEVEL . 1039.5) (:GRND--LEVEL . 980.03)
    (:HUMIDITY . 80) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 801) (:MAIN . "Clouds") (:DESCRIPTION . "few clouds")
     (:ICON . "02d")))
   (:CLOUDS (:ALL . 20)) (:WIND (:SPEED . 0.42) (:DEG . 250.504)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-10 21:00:00"))
  ((:DT . 1505088000)
   (:MAIN (:TEMP . 290.452) (:TEMP--MIN . 290.452) (:TEMP--MAX . 290.452)
    (:PRESSURE . 979.92) (:SEA--LEVEL . 1039.14) (:GRND--LEVEL . 979.92)
    (:HUMIDITY . 74) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "02n")))
   (:CLOUDS (:ALL . 8)) (:WIND (:SPEED . 1.16) (:DEG . 302.002)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-11 00:00:00"))
  ((:DT . 1505098800)
   (:MAIN (:TEMP . 287.126) (:TEMP--MIN . 287.126) (:TEMP--MAX . 287.126)
    (:PRESSURE . 980.04) (:SEA--LEVEL . 1039.48) (:GRND--LEVEL . 980.04)
    (:HUMIDITY . 88) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "02n")))
   (:CLOUDS (:ALL . 8)) (:WIND (:SPEED . 0.78) (:DEG . 317)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-11 03:00:00"))
  ((:DT . 1505109600)
   (:MAIN (:TEMP . 284.284) (:TEMP--MIN . 284.284) (:TEMP--MAX . 284.284)
    (:PRESSURE . 980.22) (:SEA--LEVEL . 1039.92) (:GRND--LEVEL . 980.22)
    (:HUMIDITY . 93) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "02n")))
   (:CLOUDS (:ALL . 8)) (:WIND (:SPEED . 1.03) (:DEG . 66.5027)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-11 06:00:00"))
  ((:DT . 1505120400)
   (:MAIN (:TEMP . 283.609) (:TEMP--MIN . 283.609) (:TEMP--MAX . 283.609)
    (:PRESSURE . 979.69) (:SEA--LEVEL . 1039.55) (:GRND--LEVEL . 979.69)
    (:HUMIDITY . 89) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "02n")))
   (:CLOUDS (:ALL . 8)) (:WIND (:SPEED . 1.11) (:DEG . 87.506)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-11 09:00:00"))
  ((:DT . 1505131200)
   (:MAIN (:TEMP . 283.071) (:TEMP--MIN . 283.071) (:TEMP--MAX . 283.071)
    (:PRESSURE . 979.27) (:SEA--LEVEL . 1039.19) (:GRND--LEVEL . 979.27)
    (:HUMIDITY . 85) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "02n")))
   (:CLOUDS (:ALL . 8)) (:WIND (:SPEED . 0.96) (:DEG . 74.0015)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-11 12:00:00"))
  ((:DT . 1505142000)
   (:MAIN (:TEMP . 284.393) (:TEMP--MIN . 284.393) (:TEMP--MAX . 284.393)
    (:PRESSURE . 978.41) (:SEA--LEVEL . 1038.39) (:GRND--LEVEL . 978.41)
    (:HUMIDITY . 82) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01d")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 0.91) (:DEG . 72.0002)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-11 15:00:00"))
  ((:DT . 1505152800)
   (:MAIN (:TEMP . 291.621) (:TEMP--MIN . 291.621) (:TEMP--MAX . 291.621)
    (:PRESSURE . 977.69) (:SEA--LEVEL . 1037.08) (:GRND--LEVEL . 977.69)
    (:HUMIDITY . 73) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01d")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 0.96) (:DEG . 25.5055)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-11 18:00:00"))
  ((:DT . 1505163600)
   (:MAIN (:TEMP . 295.397) (:TEMP--MIN . 295.397) (:TEMP--MAX . 295.397)
    (:PRESSURE . 975.89) (:SEA--LEVEL . 1034.9) (:GRND--LEVEL . 975.89)
    (:HUMIDITY . 65) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01d")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 1.16) (:DEG . 309.003)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-11 21:00:00"))
  ((:DT . 1505174400)
   (:MAIN (:TEMP . 295.407) (:TEMP--MIN . 295.407) (:TEMP--MAX . 295.407)
    (:PRESSURE . 974.45) (:SEA--LEVEL . 1033.27) (:GRND--LEVEL . 974.45)
    (:HUMIDITY . 66) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01n")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 1.12) (:DEG . 311.5)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-12 00:00:00"))
  ((:DT . 1505185200)
   (:MAIN (:TEMP . 290.302) (:TEMP--MIN . 290.302) (:TEMP--MAX . 290.302)
    (:PRESSURE . 973.35) (:SEA--LEVEL . 1032.35) (:GRND--LEVEL . 973.35)
    (:HUMIDITY . 86) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01n")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 0.81) (:DEG . 324.501)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-12 03:00:00"))
  ((:DT . 1505196000)
   (:MAIN (:TEMP . 287.235) (:TEMP--MIN . 287.235) (:TEMP--MAX . 287.235)
    (:PRESSURE . 972.43) (:SEA--LEVEL . 1031.56) (:GRND--LEVEL . 972.43)
    (:HUMIDITY . 85) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01n")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 1.02) (:DEG . 66.0111)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-12 06:00:00"))
  ((:DT . 1505206800)
   (:MAIN (:TEMP . 286.85) (:TEMP--MIN . 286.85) (:TEMP--MAX . 286.85)
    (:PRESSURE . 971.32) (:SEA--LEVEL . 1030.54) (:GRND--LEVEL . 971.32)
    (:HUMIDITY . 76) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01n")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 1.03) (:DEG . 75.5001)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-12 09:00:00"))
  ((:DT . 1505217600)
   (:MAIN (:TEMP . 286.46) (:TEMP--MIN . 286.46) (:TEMP--MAX . 286.46)
    (:PRESSURE . 970.16) (:SEA--LEVEL . 1029.5) (:GRND--LEVEL . 970.16)
    (:HUMIDITY . 77) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01n")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 1.11) (:DEG . 78.5029)) (:RAIN)
   (:SYS (:POD . "n")) (:DT--TXT . "2017-09-12 12:00:00"))
  ((:DT . 1505228400)
   (:MAIN (:TEMP . 287.486) (:TEMP--MIN . 287.486) (:TEMP--MAX . 287.486)
    (:PRESSURE . 969.09) (:SEA--LEVEL . 1028.37) (:GRND--LEVEL . 969.09)
    (:HUMIDITY . 77) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "02d")))
   (:CLOUDS (:ALL . 8)) (:WIND (:SPEED . 1.06) (:DEG . 82)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-12 15:00:00"))
  ((:DT . 1505239200)
   (:MAIN (:TEMP . 294.804) (:TEMP--MIN . 294.804) (:TEMP--MAX . 294.804)
    (:PRESSURE . 968.34) (:SEA--LEVEL . 1027.08) (:GRND--LEVEL . 968.34)
    (:HUMIDITY . 70) (:TEMP--KF . 0))
   (:WEATHER
    ((:ID . 800) (:MAIN . "Clear") (:DESCRIPTION . "clear sky")
     (:ICON . "01d")))
   (:CLOUDS (:ALL . 0)) (:WIND (:SPEED . 0.07) (:DEG . 268.505)) (:RAIN)
   (:SYS (:POD . "d")) (:DT--TXT . "2017-09-12 18:00:00")))
 (:CITY (:ID . 5810988) (:NAME . "Snohomish")
  (:COORD (:LAT . 47.9129) (:LON . -122.0982)) (:COUNTRY . "US")))
CL-USER> 
````
