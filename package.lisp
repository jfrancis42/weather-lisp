;;;; package.lisp

(defpackage #:weather
  (:use #:cl)
  (:export :wx-current
	   :wx-short-term-forecast
	   :wx-long-term-forecast))
