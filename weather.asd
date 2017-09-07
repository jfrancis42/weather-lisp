;;;; weather.asd

(asdf:defsystem #:weather
  :description "A Common Lisp client for pulling weather data from the openweathermap.org API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:cl-json
               #:drakma
               #:babel)
  :serial t
  :components ((:file "package")
               (:file "weather")))

