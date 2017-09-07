;;;; weather.lisp

(in-package #:weather)

;;; "weather" goes here. Hacks and glory await!

(defmacro stringify (thing)
  "If 'thing' is not a string, make it one (well, to the extent that
'format' is able to."
  `(if (typep ,thing 'string)
       ,thing
       (format nil "~A" ,thing)))

(defun get-weather (api-key type &key city-name country-name city-id lat lon zip cnt)
  "Fetch the weather data from the openweathermap API."
  (let ((query "") (uri ""))
    (cond (city-id
	   (setf query (concatenate 'string "id=" (stringify city-id)
				    (if (and cnt (equal type "daily")) (format nil "&cnt=~A" cnt) ""))))
	  (city-name
	   (setf query (concatenate 'string "q=" city-name
				    (if country-name (format nil ",~A" country-name) "")
				    (if (and cnt (equal type "daily")) (format nil "&cnt=~A" cnt) ""))))
	  (zip
	   (setf query (concatenate 'string "zip=" (stringify zip)
				    (if country-name (format nil ",~A" country-name) ""))))

	  ((and lat lon)
	   (setf query (concatenate 'string "lat=" (stringify lat) "&lon=" (stringify lon)
				    (if (and cnt (equal type "daily")) (format nil "&cnt=~A" cnt) "")))))

    (setf uri (concatenate 'string
			   "http://api.openweathermap.org/data/2.5/"
			   type "?" query
			   "&APPID=" (stringify api-key)))

    (format t "~A" uri)

    (json:decode-json-from-string
     (babel:octets-to-string
      (first
       (multiple-value-list
	(drakma:http-request uri
	 :method :get
	 :accept "application/json")))))))

(defun wx-current (api-key &key city-name country-name city-id lat lon zip)
  "Fetch current weather."
  (get-weather api-key "weather"
	       :city-name city-name
	       :country-name country-name
	       :city-id city-id
	       :lat lat
	       :lon lon
	       :zip zip))

(defun wx-short-term-forecast (api-key &key city-name country-name city-id lat lon zip)
  "Fetch the short-term weather forecast."
  (get-weather api-key "forecast"
	       :city-name city-name
	       :country-name country-name
	       :city-id city-id
	       :lat lat
	       :lon lon
	       :zip zip))

(defun wx-long-term-forecast (api-key &key city-name country-name city-id lat lon zip cnt)
  "Fetch the long-term weather forecast."
  (get-weather api-key "daily"
	       :city-name city-name
	       :country-name country-name
	       :city-id city-id
	       :cnt cnt
	       :lat lat
	       :lon lon
	       :zip zip))
