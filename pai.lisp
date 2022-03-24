;; pai.lisp
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

;; The pai package contains the low level C wrapper library.
;; For now it's auto-generated with autowrap, but I'll look at a minimal CFFI
;; if autowrap becomes unwieldy or has too much overhead.

;; The cl-pa (pa) package provides a Lisp-friendly interface.

(defpackage :pai
  (:nicknames)
  (:use #:cl #:j-utils #:alexandria)
  (:import-from :cffi :null-pointer)
  (:export #:null-pointer))

(in-package :pai)

(cffi:define-foreign-library pulse-lib
    (:darwin (error "No Pulse Audio on OSX!"))
    (:unix (:or "libpulse" "libpulse.so" "pulse"))
    (t (:default "libpulse")))
(cffi:use-foreign-library pulse-lib)

(cffi:define-foreign-library pulse-simple-lib
    (:darwin (error "No Pulse Audio on OSX!"))
    (:unix (:or "libpulse-simple" "libpulse-simple.so" "pulse-simple"))
    (t (:default "libpulse-simple")))
(cffi:use-foreign-library pulse-simple-lib)


(cffi:defcfun ("read" posix-read) :unsigned-long
  "Posix read function."
  (fd :int)
  (buffer :pointer)
  (length :unsigned-long))

(cffi:defcfun ("write" posix-write) :unsigned-long
  "Posix read function."
  (fd :int)
  (buffer :pointer)
  (length :unsigned-long))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pa-renamer (string matches regex)
    (declare (ignorable string matches regex))
    (aref matches 0)))


(autowrap:c-include
 (asdf:system-relative-pathname :cl-pa "include/cl-pa.h")
 :spec-path '(pai specs)
 :trace-c2ffi t
 :release-p t
 :symbol-regex (("^pa_(.*)" (:case-insensitive-mode t) #'pa-renamer))
 :symbol-exceptions (("t" . "pat")
                     ("pa_strerror" . "PA-STRERROR")
                     ("pa_make_stream" . "PA-MAKE_STREAM")
                     ("pa_stream" . "PA-STREAM"))
 :exclude-arch ("i686-pc-linux-gnu"
                "i686-pc-windows-msvc"
                "x86_64-pc-windows-msvc"
                "i686-apple-darwin9"
                "x86_64-apple-darwin9"
                "i386-unknown-freebsd"
                "x86_64-unknown-freebsd"
                "i386-unknown-openbsd"
                "x86_64-unknown-openbsd"
                "arm-pc-linux-gnu"
                "arm-unknown-linux-androideabi"
                "aarch64-unknown-linux-android"
                "i686-unknown-linux-android"
                "x86_64-unknown-linux-android")
 :exclude-definitions (
                       ;;"^va_list$"
                       ;;"^__PTHREAD.*"
                       "^sigevent$"
                       "Random" "Signal" ;;"long-double"
                       "^acos$" "^asin$" "^atan$" "^cos$" "^sin$" "^tan$" "^div$" "^ldiv$" "^lldiv$"
                       "^imaxdiv$"
                       "^gettimeofday$"
                       "^log$" "^exp$" "^acosh$" "^cosh$" "^asinh$" "^sinh$"
                       "^tanh$" "^atanh$"  "^sqrt$" "^floor$" "^round$"
                       "^time$" "^close$" "^open$" "^write$" "^read$"
                       "^sleep$" "^truncate$" "^ceil$"
                       "^abs$" "^abort$" "^random$" "^remove$" "^signal$"
                       "^t$"))
