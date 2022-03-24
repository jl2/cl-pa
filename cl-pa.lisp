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

(declaim (optimize (speed 2) (safety 3) (debug 3)))
(defun check-error (fname err)
  (let ((err-val (cffi:mem-ref (autowrap:ptr err) :int)))
    (when (> err-val 0)
    (error "~a failed with error ~a (~a)"
           fname
           err-val
           (autowrap:enum-key 'pai::error-code-t err-val)))))


(cffi:defcallback handle-sig-int :void ((context :pointer)
                                        (source-info :pointer)
                                        (userdata :pointer))
  (format t "~a ~a ~a~%" context source-info userdata))


(cffi:defcallback handle-sig-term :void ((context :pointer)
                                        (source-info :pointer)
                                        (userdata :pointer))
  (format t "~a ~a ~a%" context source-info userdata))


(cffi:defcallback handle-sig-abrt :void ((context :pointer)
                                        (source-info :pointer)
                                        (userdata :pointer))
  (format t "~a ~a ~a%" context source-info userdata))


(defun check-operation (operation)
  (cond
    ((cffi:pointer-eq (pai::operation-ptr operation) (cffi:null-pointer))
     (error "operation was a null pointer~%"))
    (t
     (format t "Operation state: ~a~%" (pai:operation-get-state operation))))
  operation)
