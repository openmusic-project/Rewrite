;;;; To do a polyphonic quantization
;;;; Autho : Pierre Donat-Bouillud

(in-package :rw)

(defun distance (pitch1 pitch2)
  "Distance between to notes (pitch)"
  (abs (- pitch1 pitch2)))

(defun add-note (voices-pitch voices-onset voices-duration pitch onset duration)
  "Add a note to the voices by choosing or creating the right voice"
  (let ((chosen-voice -1)
        (min-distance 1000000000));should be enough but put max-fixnum instead of it ?
        ;we had put a note of offset 0 and duration 0 at the beginning so that first has always something to return
        (loop for i from 0
          for pitches in voices-pitch
          for onsets in voices-onset
          for durations in voices-duration
          do 
          (when (and (<= (+ (first onsets) (first durations)) onset) (< (distance (first pitches) pitch)  min-distance))
          (setf chosen-voice i)      
          (setf min-distance (distance (first pitches) pitch))))

        (if (/= chosen-voice -1) ; we had the note in an existing voice
          ;there must be not some many voices so elt on a list is not there a performance problem
            (let ((chosen-voice-pitch (elt voices-pitch chosen-voice))
                  (chosen-voice-onset (elt voices-onset chosen-voice))
                  (chosen-voice-duration (elt voices-duration chosen-voice)))
              (values (substitute (cons pitch chosen-voice-pitch) chosen-voice-pitch voices-pitch)
                      (substitute (cons onset chosen-voice-onset) chosen-voice-onset voices-onset)
                      (substitute (cons duration chosen-voice-duration) chosen-voice-duration voices-duration)))
                (values (cons (list pitch) voices-pitch )
                (cons (list onset) voices-onset )
                (cons (list duration) voices-duration )))))


(om::defmethod! voices-separation ((pitches list) (onsets list) (durations list) )
                :initvals '((6000) (0) (1024) )
                :indoc '("pitches" "onsets" "durations" ) 
                :icon 111  ; the icon
                :numouts 3
                :doc "Separates into monodic voices the polyphonic voice defined by <pitches>, <onsets> and <durations>. Tries to have a joint movement and
                     to minimize the number of voices.
                     Returns three lists, of pitches, onsets and durations, of all the created voices"
                (labels ((create-voices (voices-pi voices-on voices-du pitch onset duration acc-pi acc-on acc-du)
                           (if (or (null voices-pi) (null voices-on) (null voices-du));might be and because lists have the same size here
                                   (add-note acc-pi acc-on acc-du pitch onset duration)
                             (multiple-value-bind (pitches-te onsets-te durations-te) 
                                 (add-note acc-pi acc-on acc-du 
                                           pitch onset duration)
                               (create-voices (rest voices-pi) (rest voices-on) (rest voices-du) 
                                              (first voices-pi) (first voices-on) (first voices-du)
                                              pitches-te onsets-te durations-te)
                                              )))
                         (reverse-voices (voices-pi voices-on voices-du)
                           (unless (or (null voices-pi) (null voices-on) (null voices-du))    
                             (multiple-value-bind (pitch-voice onset-voice duration-voice)
                                 (reverse-voices (rest voices-pi) (rest voices-on) (rest voices-du))
                               (values
                                (cons (reverse (first voices-pi)) pitch-voice)
                                (cons (reverse (first voices-on)) onset-voice)
                                (cons (reverse (first voices-du)) duration-voice))))))
                  (multiple-value-call #'(lambda (a b c) (values (reverse a) (reverse b) (reverse c)))
                    (multiple-value-call #'reverse-voices
                      (create-voices (rest pitches) (rest onsets) (rest durations) (first pitches) (first onsets) (first durations) () () ())))))


;;Peut-être qu'on peut améliorer la stratégie de suppresion des chevauchements en cherchant à maximiser la durée... algo glouton
(om::defmethod! voices-remove-overlap ((pitches list) (onsets list) (durations list))
                :initvals '((6000) (0) (1024))
                :indoc '("pitches" "onsets" "durations")
                :icon 111
                :numouts 3
                :doc "For a polyphonic voice, removes overlapping part of notes, and keeps the part of the first note. If two notes
                strictly overlaps, the first one is taken."
                
                (labels ((create-voice (pitches-l onsets-l durations-l)
                           (if (or (null (rest pitches-l)) (null (rest onsets-l)) (null (rest durations-l)))
                               (values nil nil nil)
                             (multiple-value-bind (p o d) (create-voice (rest pitches-l) (rest onsets-l) (rest durations-l))
                               (let ((end-first-note (+ (first onsets-l) (first durations-l))))
                               (if (>  end-first-note (second onsets-l))
                                      (values (cons (second pitches-l) p)
                                              (cons end-first-note o)
                                              (cons (- (second durations-l) (- end-first-note (second onsets-l))) d))
                                 (values 
                                  (cons (second pitches-l) p)
                                  (cons (second onsets-l) o)
                                  (cons (second durations-l) d))))))))
                  (multiple-value-bind (pi-l on-l du-l)
                      (create-voice pitches onsets durations)
                      (values 
                       (cons (first pitches) pi-l)
                       (cons (first onsets) on-l)
                       (cons (first durations) du-l)))))

