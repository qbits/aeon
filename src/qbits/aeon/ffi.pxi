(ns qbits.aeon.ffi
  (:require
   [pixie.ffi]
   [pixie.ffi-infer :as f]))

(f/with-config {:library "c"
                :cxx-flags ["-lc"]
                :includes ["time.h"]}
  (def time_t (pixie.ffi/c-struct :time_t 8 [[:val CInt 0]]))
  (f/defcfn time)
  (f/defcfn gmtime)
  (f/defcfn mktime)
  (f/defcfn strftime)
  (f/defcfn localtime)
  (f/defcfn clock_gettime)
  (f/defconst CLOCK_MONOTONIC)
  (f/defcfn asctime)
  (f/defcfn difftime)
  (f/defcstruct timeval [:tv_sec :tv_usec])
  ;; (f/defcstruct timespec [:tv_sec :tv_nsec])

  (f/defcstruct tm [:tm_sec
                    :tm_min
                    :tm_hour
                    :tm_mday
                    :tm_mon
                    :tm_year
                    :tm_wday
                    :tm_yday
                    :tm_isdst]))
