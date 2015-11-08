(ns qbits.aeon.test.test-date-time
  (:require
   [qbits.aeon :refer :all]
   [pixie.test :as t]))

(def test-epoch 1262401445)
;; Human time (GMT): Sat, 02 Jan 2010 03:04:05 GMT
;; Human time (your time zone): 1/2/2010, 4:04:05 AM

(t/deftest test-fields
  (let [d1 (epoch->datetime test-epoch)]
    (t/assert= 2010 (year d1))
    (t/assert= 0 (month d1))
    (t/assert= 2 (day d1))
    (t/assert= 3 (hour d1))
    (t/assert= 4 (minute d1))
    (t/assert= 5 (second d1))))

(t/deftest test-format
  (let [d1 (epoch->datetime test-epoch)]
    (t/assert= "2010-01-02 03:04:05"
               (format d1 "%Y-%m-%d %H:%M:%S"))))

(t/deftest test-eq
  (let [d1 (epoch->datetime test-epoch)
        d2 (epoch->datetime test-epoch)]
    (t/assert= d1 d2)))

(t/deftest test-intervals
  (let [d1 (epoch->datetime test-epoch)]
    (t/assert= (year (add-interval d1 1 0 0 0 0 0)) 2011)
    (t/assert= (year (add d1 :years 1)) 2011)
    (t/assert= (month (add-interval d1 0 1 0 0 0 0)) 1)
    (t/assert= (month (add d1 :months 1)) 1)
    (t/assert= (day (add-interval d1 0 0 1 0 0 0)) 3)
    (t/assert= (day (add d1 :days 1)) 3)
    (t/assert= (hour (add-interval d1 0 0 0 1 0 0)) 4)
    (t/assert= (hour (add d1 :hours 1)) 4)
    (t/assert= (minute (add-interval d1 0 0 0 0 1 0)) 5)
    (t/assert= (minute (add d1 :minutes 1)) 5)
    (t/assert= (second (add-interval d1 0 0 0 0 0 1)) 6)
    (t/assert= (second (add d1 :seconds 1)) 6)
    (t/assert=  [2011 8] ((juxt year month) (add-interval d1 0 20 0 0 0 1)))))

(t/deftest test-diff-days
  (let [d (epoch->datetime test-epoch)]
    (t/assert= 0 (diff d d))
    (t/assert= -86400 (diff d (add-interval d 0 0 1 0 0 0)))))
