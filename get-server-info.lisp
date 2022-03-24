;; get-server-info.lisp
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

(defparameter *server-info-cb-done* nil)
(defparameter *context* nil)

(defparameter *server-info-cb*
  (lambda (c info u)
    (declare (ignorable c info u))
    (let ((sinfo (pai::make-server-info :ptr info)))
      (format t "*server-info-cb* lambda ~a ~a ~a~%" c info u)
      (format t "User name: ~a~%" (cffi:foreign-string-to-lisp (pai:server-info.user-name sinfo)))
      (format t "Host name: ~a~%" (cffi:foreign-string-to-lisp (pai:server-info.host-name sinfo)))
      (format t "Server version: ~x~%"  (cffi:foreign-string-to-lisp (pai:server-info.server-version sinfo)))
      (format t "Server name: ~a~%" (cffi:foreign-string-to-lisp (pai:server-info.server-name sinfo)))
      (format t "Default sink name: ~a~%" (cffi:foreign-string-to-lisp (pai:server-info.default-sink-name sinfo)))
      (format t "Default source name: ~a~%" (cffi:foreign-string-to-lisp (pai:server-info.default-source-name sinfo)))
      (format t "Cookie: ~a~%" (cffi:foreign-string-to-lisp (pai:server-info.host-name sinfo)))
      (let ((sample-spec (pai:server-info.sample-spec sinfo)))
        (format t "Default sample format: ~a~%" (pai:sample-format-to-string (pai:sample-spec.format sample-spec)))
        (format t "Default sample rate: ~a~%" (pai:sample-spec.rate sample-spec))
        (format t "Default sample channels: ~a~%" (pai:sample-spec.channels sample-spec))
        (format t "Tile size: ~a~%" (pai:context-get-tile-size *context* sample-spec))))
    (setf *context* nil)
    (setf *server-info-cb-done* t)))

(cffi:defcallback server-info-cb :void ((context :pointer)
                                        (info :pointer)
                                        (userdata :pointer))
  (format t "server-info-cb ~a ~a ~a~%" context info userdata)
  (funcall *server-info-cb* context info userdata))

(cffi:defcallback server-info-handle-context-state :void ((context :pointer)
                                                          (userdata :pointer))
  (declare (ignorable userdata context))
  (let ((state (pai:context-get-state context)))
    (cond ((= pai:+context-ready+ state)
           (format t "Ready!~%")
           (setf *server-info-cb-done* nil)
           (setf *context* context)
           (pai:context-get-server-info context (cffi:callback server-info-cb) userdata)
           )
          ((= pai:+context-connecting+ state)
           (format t "Connecting!~%"))
          ((= pai:+context-authorizing+ state)
           (format t "Authorizing!~%"))
          ((= pai:+context-setting-name+ state)
           (format t "Setting name!~%")
           0)

          ((= pai:+context-terminated+ state)
           (format t "Terminated!~%")
           1)
          ((= pai:+context-failed+ state)
           (format t "Failed!~%")
           1)

          (t
           (format t "Context state unknown: ~a~%" state)))
    ))

(defun get-server-info ()
  ;; TODO: Find out why this doesn't work
  ;; (let* ((main-loop (pai:threaded-mainloop-new))
  ;;        (main-loop-api (pai:threaded-mainloop-get-api main-loop)))

  (let* ((main-loop (pai:mainloop-new))
         (main-loop-api (pai:mainloop-get-api main-loop)))

    (unwind-protect
        (progn
          (format t "~a ~a~%" main-loop main-loop-api)
          (pai:signal-init main-loop-api)
          (setf *server-info-cb-done* nil)
          (pai:signal-new pai:+sigint+ (cffi:callback handle-sig-int) (pai::mainloop-api-ptr main-loop-api))
          (pai:signal-new pai:+sigterm+ (cffi:callback handle-sig-term) (pai::mainloop-api-ptr main-loop-api))
          (pai:signal-new pai:+sigabrt+ (cffi:callback handle-sig-abrt) (pai::mainloop-api-ptr main-loop-api))
          ;;(pai:threaded-mainloop-start main-loop)

          (let ((context (pai:context-new main-loop-api "common-lisp-pa"))
                (ss (autowrap:alloc '(:struct (pai:sample-spec)))))
            (pai:context-ref context)
            (format t "~a ~a~%" main-loop context)

            (pai:context-set-state-callback context (cffi:callback server-info-handle-context-state) main-loop)
            (pai:context-connect context (pai:null-pointer) pai:+context-noautospawn+ (pai:null-pointer))

            (pai:sample-spec-init ss)
            (setf (pai:sample-spec.format ss) pai::+sample-s24le+)
            (setf (pai:sample-spec.rate ss) 96000)
            (setf (pai:sample-spec.channels ss) 2)
            ;;(pai:mainloop-run main-loop (cffi:null-pointer))

            (format t "Server: ~a~%" (pai:context-get-server context))
            (format t "Protocol version: ~a~%" (pai:context-get-server-protocol-version context))
            (format t "State: ~a~%" (pai:context-get-state context))
            (format t "Tile size: ~a~%" (pai:context-get-tile-size context ss))

            (pai:context-get-server-info context (cffi:callback server-info-cb) (cffi:make-pointer 9))
            ;;(pai:threaded-mainloop-stop main-loop)
            (loop :with ret-val = (autowrap:alloc :pointer 1)
                  :for iter-result =  (pai:mainloop-iterate main-loop 1 ret-val)
                  :do
                     (format t "mainloop-iterate returned: ~a ret-val = ~a~%" iter-result (cffi:mem-ref ret-val :pointer))
                  :until *server-info-cb-done*
                  :finally
                     (autowrap:free ret-val))
            (pai:context-disconnect context)
            (autowrap:free ss)
            (pai:context-unref context)

            (pai:signal-done)
            ))
      (progn
        (format t "Unwinding!~%")
        (pai:signal-done)
        (pai:mainloop-free main-loop)))))
