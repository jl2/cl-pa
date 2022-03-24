
# Table of Contents

1.  [cl-pa](#org744d892)
    1.  [About The PAI Package](#org3ca749c)
    2.  [cl-pa package](#org3a44413)
    3.  [License](#org4d327f1)


<a id="org744d892"></a>

# cl-pa

A Common Lisp binding to Pulse Audio.


<a id="org3ca749c"></a>

## About The PAI Package

The pai package contains a direct binding to libpulse and libpulse-simple.

    (ql:quickload :pai)
    (defun play-noise (&key (duration 8.0))
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
                    (let ((t-val (* idx (/ 1.0 48000))))
                      (setf (cffi:mem-aref buff :int16 (+ 0 idx)) (floor (* (expt 2 14) (sin (* 430 t-val t-val)))))
                      (setf (cffi:mem-aref buff :int16 (+ 1 idx)) (floor (* (expt 2 14) (cos (* 830 (cos t-val))))))))
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
    (play-noise :duration 8.0)


<a id="org3a44413"></a>

## cl-pa package

The cl-pa (nickname 'pa') will eventually offer a higher level API.

The aim of the project is to:

1.  Let the user pass normal Common Lisp functions and avoid using(cffi:defcallback &#x2026;)
2.  Provide query functions to retrieve sources, sinks, sound cards, etc.
3.  Provide utility functions to change volume, mute sources, mount sinks, etc.

    (ql:quickload :cl-pa)
    (cl-pa:get-server-info)
    (cl-pa:play-noise :duration 30.0
                      :volume (lambda (time-val) (+ 0.5 (* 0.5 (sin (* 0.5 time-val)))))
                      :generator (lambda (time-val)
                                   (values (sin (* 830 time-val))
                                           (cos (* 430 time-val (cos (* pi time-val)))))))


<a id="org4d327f1"></a>

## License

ISC

Copyright (c) 2022 Jeremiah LaRocco <jeremiah<sub>larocco</sub>@fastmail.com>

