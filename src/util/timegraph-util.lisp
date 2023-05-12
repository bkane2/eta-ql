;; Sep 10/2021
;; ===========================================================
;;
;; Contains utility functions for creating and manipulating a timegraph
;; data structure for Eta dialogues, using the timegraph package.
;;

(in-package :eta)

(defun construct-timegraph (ds)
;````````````````````````````````
; Creates and returns a new timegraph structure
;
  (setf (ds-tg ds) (timegraph:make-timegraph))
) ; END construct-timegraph



(defun add-episode-to-timegraph (ep-name ds)
;````````````````````````````````````````````
; Adds an episode to the timegraph by asserting it to be during the overall
; dialogue episode, E.
; NOTE: in the case where we want the timegraph structure to persist across
; multiple dialogues, the name and temporal relations between the dialogue episode
; will need to be modified.
;
  (timegraph:assert-prop `(,ep-name :during E) (ds-tg ds))
) ; END add-episode-to-timegraph



(defun time-record-structure-to-timestamp (time-record)
;````````````````````````````````````````````````````````
; Converts a time record structure to a timestamp using the local-time package.
; e.g.,
; (time-record-structure-to-timestamp '($ DATE+TIME YEAR 2021 MONTH 9 DAY 21 HOUR 13 MINUTE 41 SEC 45))
; => @2021-09-21T13:41:45.000000-04:00
;
  (when (record-structure? time-record)
    (let (year month day hour minute sec nsec)
      (setq nsec 0)
      (setq sec (get-slot-record-structure :sec time-record))
      (setq minute (get-slot-record-structure :minute time-record))
      (setq hour (get-slot-record-structure :hour time-record))
      (setq day (get-slot-record-structure :day time-record))
      (setq month (get-slot-record-structure :month time-record))
      (setq year (get-slot-record-structure :year time-record))
      (when (and year month day hour minute sec nsec)
        (timegraph:encode-timestamp nsec sec minute hour day month year))))
) ; END time-record-structure-to-timestamp



(defun update-lower-bound-timegraph (ep-name time-record ds)
;`````````````````````````````````````````````````````````````
; Updates a timegraph episode with a lower quantitative bound, given a
; time record structure. This requires converting the record structure to
; a :local-time object.
;
  (timegraph:update-lower-bound-inst ep-name
    (time-record-structure-to-timestamp time-record) (ds-tg ds))
) ; END update-lower-bound-timegraph



(defun update-upper-bound-timegraph (ep-name time-record ds)
;`````````````````````````````````````````````````````````````
; Updates a timegraph episode with an upper quantitative bound, given a
; time record structure. This requires converting the record structure to
; a :local-time object.
;
  (timegraph:update-upper-bound-inst ep-name
    (time-record-structure-to-timestamp time-record) (ds-tg ds))
) ; END update-lower-bound-timegraph