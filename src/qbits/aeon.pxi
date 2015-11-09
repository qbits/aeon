(ns qbits.aeon
  (:require
   [qbits.aeon.ffi :as c]
   [pixie.ffi]
   [pixie.ffi-infer :as f]))

;; TODO
;; millisecond support
;; prolly leaks like crazy
;; tests!

(def default-format-buffer-size 80)

(defprotocol IDateTime
  (-copy [this])
  (ts [this])
  (year [this])
  (month [this])
  (day [this])
  (hour [this])
  (minute [this])
  (second [this])
  (week-day [this])
  (year-day [this])
  (epoch [this])
  (add-interval [this])
  (add [this x unit])
  (diff [this other])
  (format [this fmt]))

(deftype DateTime [struct-tm]

  IDateTime
  (year [this]
    (+ 1900 (:tm_year struct-tm)))

  (month [this]
    (:tm_mon struct-tm))

  (day [this]
    (:tm_mday struct-tm))

  (hour [this]
    (:tm_hour struct-tm))

  (minute [this]
    (:tm_min struct-tm))

  (second [this]
    (:tm_sec struct-tm))

  (week-day [this]
    (:tm_wday struct-tm))

  (year-day [this]
    (:tm_yday struct-tm))

  (epoch [this]
    (c/mktime struct-tm))

  (diff [this other]
    (- (epoch this)
       (epoch other)))

  (add-interval [this years months days hours minutes seconds]
    (let [nd (-copy this)
          nd-struct-tm (get-field nd :struct-tm)]
      (pixie.ffi/set! nd-struct-tm :tm_year (+ (:tm_year struct-tm) years))
      (pixie.ffi/set! nd-struct-tm :tm_mon (+ (:tm_mon struct-tm) months))
      (pixie.ffi/set! nd-struct-tm :tm_mday (+ (:tm_mday struct-tm) days))
      (pixie.ffi/set! nd-struct-tm :tm_hour (+ (:tm_hour struct-tm) hours))
      (pixie.ffi/set! nd-struct-tm :tm_min (+ (:tm_min struct-tm) minutes))
      (pixie.ffi/set! nd-struct-tm :tm_sec (+ (:tm_sec struct-tm) seconds))
      (c/mktime nd-struct-tm)
      nd))

  (add [this unit x]
    (let [nd (-copy this)
          nd-struct-tm (get-field nd :struct-tm)]
      (case unit
        :years
        (pixie.ffi/set! nd-struct-tm :tm_year (+ (:tm_year struct-tm) x))
        :months
        (pixie.ffi/set! nd-struct-tm :tm_mon (+ (:tm_mon struct-tm) x))
        :days
        (pixie.ffi/set! nd-struct-tm :tm_mday (+ (:tm_mday struct-tm) x))
        :hours
        (pixie.ffi/set! nd-struct-tm :tm_hour (+ (:tm_hour struct-tm) x))
        :minutes
        (pixie.ffi/set! nd-struct-tm :tm_min (+ (:tm_min struct-tm) x))
        :seconds
        (pixie.ffi/set! nd-struct-tm :tm_sec (+ (:tm_sec struct-tm) x)))
      (c/mktime nd-struct-tm)
      nd))

  (format [this fmt]
    (let [buf (buffer default-format-buffer-size)]
      (->> (c/strftime buf default-format-buffer-size fmt struct-tm)
           (set-buffer-count! buf))
      (let [s (transduce (map char) string-builder buf)]
        (dispose! buf)
        s)))

  (-copy [this]
    (let [nt (c/tm)]
      (pixie.ffi/set! nt :tm_sec (:tm_sec struct-tm))
      (pixie.ffi/set! nt :tm_min (:tm_min struct-tm))
      (pixie.ffi/set! nt :tm_hour (:tm_hour struct-tm))
      (pixie.ffi/set! nt :tm_mday (:tm_mday struct-tm))
      (pixie.ffi/set! nt :tm_mon (:tm_mon struct-tm))
      (pixie.ffi/set! nt :tm_year (:tm_year struct-tm))
      (pixie.ffi/set! nt :tm_wday (:tm_wday struct-tm))
      (pixie.ffi/set! nt :tm_yday (:tm_yday struct-tm))
      (pixie.ffi/set! nt :tm_isdst (:tm_isdst struct-tm))
      (->DateTime nt)))

  IDisposable
  (-dispose! [this]
    (dispose! struct-tm))

  IObject
  (-eq [this other]
    (zero? (diff this other)))

  (-hash [this]
    (hash (epochal struct-tm)))

  (-str [this]
    (-repr this))

  (-repr [this]
    (str "<qbits.aeon.DateTime \"" (str (format this "%Y-%m-%d %H:%M:%S")) "\">")))

(defn datetime
  ([i]
   (let [tt (c/time_t)]
     (if (number? i)
       (pixie.ffi/set! tt :val i)
       (c/time tt))
     (let [dt (->DateTime (pixie.ffi/cast (c/localtime tt)
                                          c/tm))]
       (dispose! tt)
       dt)))
  ([] (datetime nil)))
