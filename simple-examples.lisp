;; cl-pa.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-pa)

(defun play-noise (&key
                     (generator
                      (lambda (time-val)
                        (values (floor (* (expt 2 14) (sin (* 430 time-val time-val))))
                                (floor (* (expt 2 14) (cos (* 830 (cos time-val))))))))
                     (duration 2.0)
                     (volume 1.0))
  (let* ((ss (autowrap:alloc '(:struct (pai:sample-spec))))
         (err (autowrap:alloc :int))
         (sample-count (ceiling (* 48000 duration)))
         (buff (autowrap:alloc :short (* 2 1 sample-count)))
         (simple nil))
    (unwind-protect
         (progn
           (pai::sample-spec-init ss)
           (setf (pai:sample-spec.format ss) pai::+sample-s16le+)
           (setf (pai:sample-spec.rate ss) 48000)
           (setf (pai:sample-spec.channels ss) 2)
           (setf simple (pai:simple-new (pai:null-pointer)
                                        "player"
                                        pai:+stream-playback+
                                        (pai:null-pointer)
                                        "playback"
                                        ss
                                        (pai:null-pointer)
                                        (pai:null-pointer)
                                        err))

           (when (cffi:pointer-eq (autowrap:ptr simple) (pai:null-pointer))
             (check-error "simple-new" err))

           (format t "sample count ~a~%" sample-count)
           (format t "Returned: ~a ~a~%" simple (cffi:mem-ref (autowrap:ptr err) :int))
           (format t "Latency: ~a~%" (pai::simple-get-latency simple err))

           (loop
             :for idx :below sample-count :by 2
             :do
                (let ((time-val (* idx (/ 1.0 48000))))
                  (multiple-value-bind (left right) (funcall generator time-val)
                    (setf (cffi:mem-aref buff :int16 (+ 0 idx)) (floor (* (typecase volume
                                                                            (number volume)
                                                                            (t (funcall volume time-val)))
                                                                          (1- (expt 2 15)) left)))
                    (setf (cffi:mem-aref buff :int16 (+ 1 idx)) (floor (* (typecase volume
                                                                            (number volume)
                                                                            (t (funcall volume time-val)))
                                                                          (1- (expt 2 15)) right))))))
           (pai:simple-write simple buff sample-count err)
           (format t "Latency: ~a~%" (pai::simple-get-latency simple err))
           (pai:simple-drain simple err))
      (progn
        (when simple
          (pai:simple-drain simple err)
          (pai:simple-free simple))
      (autowrap:free ss)
      (autowrap:free err)
      (autowrap:free buff)))))

(defun play-file (file-name &key
                              (buffer-size (* 3 4096))
                              (use-posix #+sbcl t
                                         #-sbcl nil)
                              (sample-rate 48000)
                              (format pai:+sample-s24le+)
                              (channels 2))
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (with-input-from-file (inf file-name :element-type '(unsigned-byte 8))
    (let ((ss (autowrap:alloc '(:struct (pai:sample-spec))))
          (err (autowrap:alloc :int))
          (buff (autowrap:alloc :unsigned-char buffer-size))
          (simple nil))
      (unwind-protect
           (progn
             (pai:sample-spec-init ss)
             (setf (pai:sample-spec.format ss) format)
             (setf (pai:sample-spec.rate ss) sample-rate)
             (setf (pai:sample-spec.channels ss) channels)
             (setf simple (pai:simple-new (pai:null-pointer)
                                            "player"
                                            pai:+stream-playback+
                                            (pai:null-pointer)
                                            "playback"
                                            ss
                                            (pai:null-pointer)
                                            (pai:null-pointer)
                                            err))
             (when (cffi:pointer-eq (autowrap:ptr simple) (pai:null-pointer))
               (check-error "simple-new" err))
             (pai:simple-write simple buff buffer-size err)

             (cond
               #+sbcl(use-posix
                      (loop
                        :for bytes-read = (pai::posix-read (slot-value inf 'sb-impl::fd)
                                                           (autowrap:ptr buff)
                                                           buffer-size)
                        :do
                           (format t "Latency: ~a~%" (pai::simple-get-latency simple err))
                           (when (< (pai:simple-write simple buff bytes-read err) 0)
                             (check-error "simple-write" err))
                        :until (< bytes-read buffer-size)))
               (t
                (loop
                  :with sequence = (make-array buffer-size :element-type '(unsigned-byte 8))
                  :for bytes-read = (read-sequence sequence inf)
                  :do
                     (format t "Latency: ~a~%" (pai::simple-get-latency simple err))
                     (loop
                       :for idx :below bytes-read
                       :for value :across sequence
                       :do
                          (setf (cffi:mem-aref buff :uint8 idx) value))
                     (when (< (pai:simple-write simple buff bytes-read err) 0)
                       (check-error "simple-write" err))
                  :until (< bytes-read buffer-size))))
             (pai:simple-drain simple err))
      (progn
        (format t "Cleanup?")
        (when simple
          (pai:simple-drain simple err)
          (pai:simple-free simple))
        (autowrap:free ss)
        (autowrap:free err)
        (autowrap:free buff))))))

(defun play-record (file-name &key
                                (buffer-size (* 3 4096))
                                (timeout 5.0)
                                (use-read-sequence t))
  (with-input-from-file (inf file-name :element-type '(unsigned-byte 8))
    (let ((ss (autowrap:alloc '(:struct (pai:sample-spec))))
          (err (autowrap:alloc :int))
          (buff (autowrap:alloc :unsigned-char buffer-size))
          (simple nil))
      (unwind-protect
           (progn
             (pai:sample-spec-init ss)
             (setf (pai:sample-spec.format ss) pai::+sample-s24le+)
             (setf (pai:sample-spec.rate ss) 96000)
             (setf (pai:sample-spec.channels ss) 2)
             (setf simple (pai:simple-new (pai:null-pointer)
                                            "player"
                                            pai:+stream-playback+
                                            (pai:null-pointer)
                                            "playback"
                                            ss
                                            (pai:null-pointer)
                                            (pai:null-pointer)
                                            err))
             (when (cffi:pointer-eq (autowrap:ptr simple) (pai:null-pointer))
               (check-error "simple-new" err))
             (pai:simple-write simple buff buffer-size err)
             (cond
               (use-read-sequence
                (loop
                  :with sequence = (make-array buffer-size :element-type '(unsigned-byte 8))
                  :for bytes-read = (read-sequence sequence inf)
                  :do
                     (format t "Latency: ~a~%" (pai::simple-get-latency simple err))
                     (loop
                       :for idx :below bytes-read
                       :for value :across sequence
                       :do
                          (setf (cffi:mem-aref buff :uint8 idx) value))
                     (when (< (pai:simple-write simple buff bytes-read err) 0)
                       (check-error "simple-write" err))
                  :until (< bytes-read buffer-size)))
               #+sbcl(t
                      (loop
                        :for bytes-read = (pai::posix-read (slot-value inf 'sb-impl::fd)
                                                           (autowrap:ptr buff)
                                                           buffer-size)
                        :do
                           (format t "Latency: ~a~%" (pai::simple-get-latency simple err))
                           (when (< (pai:simple-write simple buff bytes-read err) 0)
                             (check-error "simple-write" err))
                        :until (< bytes-read buffer-size)))
               #-sbcl(t
                      (error "Only read-sequence supported!")))
             (pai:simple-drain simple err))
      (progn
        (format t "Cleanup?")
        (when simple
          (pai:simple-free simple))
        (autowrap:free ss)
        (autowrap:free err)
        (autowrap:free buff))))))
