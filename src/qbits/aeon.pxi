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
  (-copy [t])
  (ts [t])
  (year [t])
  (month [t])
  (day [t])
  (hour [t])
  (minute [t])
  (second [t])
  (week-day [t])
  (year-day [t])
  (epoch [t])
  (add-interval [t])
  (format [t fmt]))

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

  (add-interval [this y m d h m s]
    (let [nd (-copy this)
          nd-struct-tm (get-field nd :struct-tm)]
      (pixie.ffi/set! nd-struct-tm :tm_year (+ (:tm_year struct-tm) y))
      (pixie.ffi/set! nd-struct-tm :tm_mon (+ (:tm_mon struct-tm) m))
      (pixie.ffi/set! nd-struct-tm :tm_mday (+ (:tm_mday struct-tm) d))
      (pixie.ffi/set! nd-struct-tm :tm_hour (+ (:tm_hour struct-tm) h))
      (pixie.ffi/set! nd-struct-tm :tm_min (+ (:tm_min struct-tm) m))
      (pixie.ffi/set! nd-struct-tm :tm_sec (+ (:tm_sec struct-tm) s))
      (mktime nd-struct-tm)
      nd))

  (format [this fmt]
    (let [buf (buffer 80)]
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
    (= 0
       (difftime (mktime (get-field this :struct-tm))
                 (mktime (get-field other :struct-tm)))))

  (-hash [this]
    (hash (mktime struct-tm)))

  (-str [this]
    (str (-repr this)))

  (-repr [this]
    (str "<qbits.aeon/DateTime \"" (format this "%Y-%m-%d %H:%M:%S") "\">")))

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
     ;; (dispose! tt)
     dt))
  ([] (new-datetime nil)))

(defn epoch->datetime
  [i]
  (let [tt (time_t)]
    (time tt)
    (pixie.ffi/set! tt :val i)
    (let [dt (->DateTime (pixie.ffi/cast (gmtime tt) tm))]
      ;; (dispose! tt)
      dt)))

;; (println (epoch->datetime 0))
;; (println (epoch->datetime 0))
;; (println (epoch->datetime 10))
;; (println (epoch->datetime 1446818738))


;; (defn
;;   ""
;;   []
;;   )


;; (let [d1 (new-datetime)
;;       d0 (-copy d1)
;;       d2 (-copy d1)]
;;   (println (epoch d1))
;;   (println (format d1 "%Y-%m-%d %H:%M:%S"))
;;   (println )
;;   (println (= d1 (add-interval d1 1 1 1 0 0 0 )))
;;   (println d2)

;;   (println d1)
;;   ;; (println (add-interval d1 1 1 1))
;;   (println d1)
;;   (println d2)
;;   (println (-hash d2) (-hash d1))
;; )
