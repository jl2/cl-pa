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
(use-package :pai)

(defparameter *source-info-list-callback*
  (lambda (context source-info user-data)
    (declare (ignorable context source-info user-data))
    (format t "*server-info-list-callback* lambda ~a ~a ~a~%" context source-info user-data)
    ))

(cffi:defcallback source-info-list-callback :void ((context :pointer)
                                                   (source-info :pointer)
                                                   (userdata :pointer))
  (format t "source-info-list-callback ~a ~a ~a~%" context source-info userdata)
  (funcall *source-info-list-callback* context source-info userdata))


(cffi:defcallback handle-context-state :void ((context :pointer)
                                              (userdata :pointer))
  (declare (ignorable userdata context))
  (let ((state (pai:context-get-state context)))
    (cond ((= pai:+context-ready+ state)
           (format t "Ready!~%"))
          ((= pai:+context-connecting+ state)
           (format t "Connecting!~%"))
          ((= pai:+context-authorizing+ state)
           (format t "Authorizing!~%"))
          ((= pai:+context-setting-name+ state)
           (format t "Setting name!~%"))

          ((= pai:+context-terminated+ state)
           (format t "Terminated!~%"))
          ((= pai:+context-failed+ state)
           (format t "Failed!~%"))

          (t
           (format t "Context state unknown: ~a~%" state)))))

(defun list-all-sources ()
  (let* ((main-loop (pai:mainloop-new))
         (main-loop-api (pai:mainloop-get-api main-loop)))
    (unwind-protect
         (progn

           (format t "~a ~a~%" main-loop main-loop-api)
           (pai:signal-init main-loop-api)

           (pai:signal-new pai:+sigint+ (cffi:callback handle-sig-int) (pai::mainloop-api-ptr main-loop-api))
           (pai:signal-new pai:+sigterm+ (cffi:callback handle-sig-term) (pai::mainloop-api-ptr main-loop-api))
           (pai:signal-new pai:+sigabrt+ (cffi:callback handle-sig-abrt) (pai::mainloop-api-ptr main-loop-api))
           ;;(pai:threaded-mainloop-start main-loop)

           (let ((context (pai:context-new main-loop-api "common-lisp-pa")))

             (pai:context-ref context)

             (pai:context-set-state-callback context (cffi:callback handle-context-state) (cffi:null-pointer))
             (pai:context-connect context (pai:null-pointer) pai:+context-noautospawn+ (pai:null-pointer))

             (format t "Server: ~a~%" (pai:context-get-server context))
             (format t "Protocol version: ~a~%" (pai:context-get-server-protocol-version context))
             (format t "State: ~a~%" (pai:context-get-state context))
             (pai:mainloop-run main-loop (cffi:null-pointer))
             ;; (check-operation
             ;;  (pai:context-get-source-info-list context  (cffi:callback source-info-list-callback) (cffi:make-pointer 9)))
             (sleep 2)
             (pai:context-disconnect context)
             (pai:context-unref context)))
      (progn
        (pai:signal-done)
        (pai:mainloop-free main-loop)
        ))))
