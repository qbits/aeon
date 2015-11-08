(ns qbits.aeon
  (:require
   [pixie.ffi]
   [pixie.ffi-infer :as f]))

;; TODO
;; millisecond support
;; prolly leaks like crazy
;; tests!

(f/with-config {:library "c"
                :cxx-flags ["-lc"]
                :includes ["time.h"]}
  (def time_t (pixie.ffi/c-struct :time_t 8 [[:val CInt 0]]))
  (f/defcfn time)
  (f/defcfn gmtime)
  (f/defcfn mktime)
  (f/defcfn strftime)
  (f/defcfn localtime)
  (f/defcfn asctime)
  (f/defcfn difftime)
  (f/defcstruct tm [:tm_sec
                    :tm_min
                    :tm_hour
                    :tm_mday
                    :tm_mon
                    :tm_year
                    :tm_wday
                    :tm_yday
                    :tm_isdst]))


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
  (year [t]
    (+ 1900 (:tm_year struct-tm)))

  (month [t]
    (:tm_mon struct-tm))

  (day [t]
    (:tm_mday struct-tm))

  (hour [t]
    (:tm_hour struct-tm))

  (minute [t]
    (:tm_min struct-tm))

  (second [t]
    (:tm_sec struct-tm))

  (week-day [t]
    (:tm_wday struct-tm))

  (year-day [t]
    (:tm_yday struct-tm))

  (epoch [t] (mktime struct-tm))

  (diff [this other]
    (difftime (mktime (get-field this :struct-tm))
              (mktime (get-field other :struct-tm))))

  (add-interval [this years months days hours minutes seconds]
    (let [nd (-copy this)
          nd-struct-tm (get-field nd :struct-tm)]
      (pixie.ffi/set! nd-struct-tm :tm_year (+ (:tm_year struct-tm) years))
      (pixie.ffi/set! nd-struct-tm :tm_mon (+ (:tm_mon struct-tm) months))
      (pixie.ffi/set! nd-struct-tm :tm_mday (+ (:tm_mday struct-tm) days))
      (pixie.ffi/set! nd-struct-tm :tm_hour (+ (:tm_hour struct-tm) hours))
      (pixie.ffi/set! nd-struct-tm :tm_min (+ (:tm_min struct-tm) minutes))
      (pixie.ffi/set! nd-struct-tm :tm_sec (+ (:tm_sec struct-tm) seconds))
      (mktime nd-struct-tm)
      nd))

  (add [this unit x]
    (let [k (keyword (str "tm" unit))
          nd (-copy this)
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
      (mktime nd-struct-tm)
      nd))

  (format [this fmt]
    (let [buf (buffer default-format-buffer-size)]
      (->> (strftime buf default-format-buffer-size fmt struct-tm)
           (set-buffer-count! buf))
      (let [s (transduce (map char) string-builder buf)]
        (dispose! buf)
        s)))

  (-copy [this]
    ;; where is memcpy
    (let [nt (tm)]
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
    (hash (mktime struct-tm)))

  (-str [this]
    (-repr this))

  (-repr [this]
    (str "<qbits.aeon.DateTime \"" (format this "%Y-%m-%d %H:%M:%S") "\">")))

(defn new-datetime
  ([{:as opts
     :keys [gmt?]}]
   (let [tt (time_t)
         _ (time tt)
         dt (->DateTime (pixie.ffi/cast
                         (if gmt?
                           (gmtime tt)
                           (localtime tt))
                         tm))]
     (dispose! tt)
     dt))
  ([] (new-datetime nil)))

(defn epoch->datetime
  [i]
  (let [tt (time_t)]
    (time tt)
    (pixie.ffi/set! tt :val i)
    (let [dt (->DateTime (pixie.ffi/cast (gmtime tt) tm))]
      (dispose! tt)
      dt)))
