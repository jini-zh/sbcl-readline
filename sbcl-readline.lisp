;;; SBCL interface to GNU Readline
;;; (c) 2009 2010 by Evgeniy Zhemchugov <jini.zh@gmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defpackage readline
  (:use :cl :cl-user)
  (:export *history-file* 
           *history-size*
           *ps1*
           *ps2*
           *debug-ps1*
           *debug-ps2*
           stifle-history
           unstifle-history
           readline))

(in-package readline)

(require 'asdf)
(require 'cffi)

#| how to implement callback through sbcl native routines?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case (load-shared-object #p"libncursesw.so")
    (t (load-shared-object #p"libncurses.so")))
  (load-shared-object #p"libreadline.so"))


(define-alien-routine "readline" c-string (prompt c-string))
(define-alien-routine "add_history" void (command c-string))
(define-alien-routine "read_history" int (file c-string))
(define-alien-routine "write_history" int (file c-string))

(define-alien-variable "rl_attempted_completion_function"
                       (* (function (* c-string) c-string int int)))
(define-alien-variable "rl_line_buffer" c-string)
(define-alien-variable "rl_attempted_completion_over" int)
|#

(cffi:define-foreign-library ncurses (t (:default "libncursesw" "libncurses")))
(cffi:define-foreign-library readline (t (:or "libreadline.so.6" "libreadline")))

(cffi:use-foreign-library ncurses)
(cffi:use-foreign-library readline)

(cffi:defcfun ("readline" rl-readline) :string (prompt :string))
(cffi:defcfun "add_history"      :void   (command :string))
(cffi:defcfun "read_history"     :int    (file    :string))
(cffi:defcfun "write_history"    :int    (file    :string))
(cffi:defcfun "stifle_history"   :void   
  "Stifle the history list, remembering only the last MAX entries"
  (max :int))
(cffi:defcfun "unstifle_history" :int)
(cffi:defcfun "_rl_enable_paren_matching" :int (on-or-off :int))
(cffi:defcfun "rl_display_match_list" :void 
              (matches :pointer) (len :int) (max :int))
(cffi:defcfun "rl_redisplay" :void)
(cffi:defcfun "rl_forced_update_display" :int)

(cffi:defcvar "rl_attempted_completion_function" :pointer)
(cffi:defcvar "rl_completion_display_matches_hook" :pointer)

(cffi:defcvar "rl_line_buffer" :string)
(cffi:defcvar "rl_point" :int)
(cffi:defcvar "rl_attempted_completion" :int)
(cffi:defcvar "rl_completion_quote_character" :int)
(cffi:defcvar "rl_completion_suppress_quote" :int)
(cffi:defcvar "rl_basic_word_break_characters" :string)
(cffi:defcvar "rl_basic_quote_characters" :string)
(cffi:defcvar "rl_attempted_completion_over" :int)
(cffi:defcvar "rl_completion_suppress_append" :int)
(cffi:defcvar "rl_completion_query_items" :int)

(let ((line-number 0))
  (defun default-prompt ()
    (format nil "~a[~a]: "
            (car (sort (append
                         (package-nicknames *package*)
                         (list (package-name *package*)))
                       #'<
                       :key #'length))
            (incf line-number))))

(defvar *history-file* 
  (format nil "~a/.sbcl-history" (sb-ext:posix-getenv "HOME"))
  "File to keep history in")
(defvar *history-size* 200
  "Number of commands to keep in history")

(defvar *ps1* #'default-prompt
  "Either a string or a function that returns a string to be used when prompting user for the next command. The function have to take no arguments")

(defvar *ps2* "> "
  "Either a string or a function that returns a string to be used when prompting user for the rest of incomplete command. The function have to take no arguments")

(defvar *debug-ps1*
  ; #'sb-debug::debug-prompt will be overwritten later
  (let ((p #'sb-debug::debug-prompt))
    (lambda ()
      ; strip the newline in the beginning
      (subseq (with-output-to-string (s) (funcall p s)) 1)))
  "Either a string or a function that returns a string to be used when prompting user for the next command in debug mode. The function have to take no arguments")

(defvar *debug-ps2* "* "
  "Either a string or a function that returns a string to be used when prompting user for the rest of inomplete command in debug mode. The function have to take no arguments")

(defun prompt (ps)
  (etypecase ps
    (string ps)
    (function (funcall ps))))

(defun readline (&key (ps1 "* ") (ps2 "> ") (eof-value nil eof-error-p))
  "Prompts user for a command and returns the result as a Lisp form"
  (loop with result
        with pos = 0
        for line = (rl-readline (prompt ps1)) then (rl-readline (prompt ps2))
        for cmd = line then (concatenate 'string 
                                         cmd 
                                         #.(make-string 1 
                                             :initial-element #\newline)
                                         (or line ""))
        unless line do
          (if eof-error-p
            (return eof-value)
            (error 'end-of-file :stream *standard-input*))
        do (unwind-protect
             (handler-case
               (loop with form
                     with eof = '#:eof
                     do (multiple-value-setq (form pos) 
                          (read-from-string cmd nil eof :start pos))
                     until (eq form eof)
                     do (push form result)
                     finally 
                       (return-from readline 
                                    (values-list (nreverse result))))
               (end-of-file ()))
             (unless (= (length cmd) 0)
               (add-history cmd)))))

(defun parse-symbol (string &key (start 0) (end (length string)))
  (let* ((colon (position #\: string :start start :end end))
         (internal (and colon
                        (< (1+ colon) end)
                        (char= (char string (1+ colon)) #\:))))
    (values (cond
              ((not colon) start)
              (internal (+ colon 2))
              (t (1+ colon)))
            internal
            (cond
              ((not colon) *package*)
              ((= colon start) 'keyword)
              (t (find-package
                   (read-from-string string t nil :start start :end colon)))))))

(defun whitespacep (char)
  (member char '(#\Space #\Newline #\Linefeed #\Tab #\Return #\Page)))

(defun function-signature (f)
  (loop with optional = nil
        with key = nil
        with rest = nil
        with allow-other-keys = nil
        for arg in (sb-kernel:%fun-lambda-list f)
        if (eq arg '&key)
         do (setf key t)
        else if (eq arg '&optional)
         do (setf optional 0)
        else if (eq arg '&rest)
         do (setf rest t)
        else if (eq arg '&allow-other-keys)
         do (setf allow-other-keys t)
        else if optional
         count t into opt-num
        else if key
         collect (intern (symbol-name
                           (cond ((atom arg) arg)
                                 ((atom (car arg)) (car arg))
                                 (t (caar arg))))
                         'keyword)
                 into keys
        else
         count t into req-num
        finally 
         (return (values ;(sb-kernel:%simple-fun-name f)
                         req-num opt-num rest keys allow-other-keys))))

(defun find-symbols (key package &optional external-only)
  (let ((result (list)))
    (if external-only
      (do-external-symbols (s package result)
        (if (funcall key s)
          (push s result)))
      (do-symbols (s package result)
        (if (funcall key s)
          (push s result))))))

(defun find-function-keywords (function key arg)
  (multiple-value-bind (#|name|# req-num opt-num rest keys allow-other-keys)
                       (function-signature function)
    (declare (ignorable rest))
    (if (or allow-other-keys 
            (< arg (+ req-num opt-num))
            (oddp (- arg req-num opt-num)))
      (find-symbols key 'keyword)
      (delete-if-not key keys))))

(defun find-functions (key package &optional external-only)
  (find-symbols (lambda (symbol)
                  (and (fboundp symbol) (funcall key symbol)))
                package
                external-only))

(defun find-values (key package &optional external-only)
  (find-symbols (lambda (symbol)
                  (and (boundp symbol) (funcall key symbol)))
                package
                external-only))

(defun find-packages (key)
  (delete-if-not key 
                 (mapcan (lambda (p)
                           (cons (package-name p)
                                 (copy-seq (package-nicknames p))))
                         (list-all-packages))))

(defun find-function (string &key (start 0) (end (length string)))
  (multiple-value-bind (symbol-start internal package)
                       (parse-symbol string :start start :end end)
    (let ((f (find-symbol 
               (nstring-upcase (subseq string symbol-start end))
               package)))
      (when (fboundp f)
        (or (macro-function f)
            (symbol-function f))))))

(defun find-parent-function (string start)
  (loop with level = 0
        with end = start
        with arg = -1
        with space = nil
        for i from (1- start) downto 0
        for c = (char string i)
        do (if (whitespacep c)
             (progn
               (when (not space)
                 (incf arg)
                 (setf space t))
               (setf end i))
             (progn
               (setf space nil)
               (cond 
                 ((char= c #\()
                  (if (= level 0)
                    (return 
                      (values (find-function string :start (1+ i) :end end)
                              arg))
                    (decf level)))
                 ((char= c #\)) (incf level)))))))

(defun quoted (string end)
  "Tests whether quotes (\") are balanced in string and returns index of the
  last opened quote if not"
  (loop 
    with escaped
    with result
    for i from 0 below end
    for c = (char string i)
    do (if (char= c #\\)
         (setf escaped (not escaped))
         (when (and (not escaped) (char= c #\"))
           (setf result (if result nil i))))
    finally (return result)))

(defun symbol-name* (symbol &optional prefix)
  (if prefix
    (nstring-downcase (concatenate 'string prefix (symbol-name symbol)))
    (string-downcase (symbol-name symbol))))

(defun mapcar! (function sequence &rest sequences)
  (apply #'map-into sequence function sequence sequences))

(defun complete (string start end)
  (let ((q (quoted string start)))
    (when q
      (unless (and (> q 1) (string= string "#p" :start1 (- q 2) :end1 q))
        (setf *rl-attempted-completion-over* 1))
      (return-from complete nil)))
  (setf *rl-attempted-completion-over* 1)
  (multiple-value-bind (symbol-start internal package)
                       (parse-symbol string :start start :end end)
    (let ((functional (or (and (> start 0)
                               (char= (char string (1- start)) #\())
                          (and (> start 1)
                               (string= (subseq string (- start 2) start) 
                                        "#'"))))
          arg)
      (unless functional
        (multiple-value-setq (functional arg) 
          (find-parent-function string start)))
      (labels ((name-cmp (name)
                         (string-equal string name
                                       :start1 symbol-start :end1 end
                                       :end2 (min (length name) 
                                                  (- end symbol-start))))
               (symbol-name-cmp (symbol) (name-cmp (symbol-name symbol))))
        (unless package ; bad package name
          (return-from complete nil))
        (sort
          (if (eq package 'keyword)
            (mapcar! (lambda (symbol) (symbol-name* symbol ":"))
                     (if (functionp functional)
                       (find-function-keywords 
                         functional #'symbol-name-cmp arg)
                       (find-symbols #'symbol-name-cmp 'keyword)))
            (nconc
              (mapcar! (if (eq package *package*)
                         #'symbol-name*
                         (let ((package-part 
                                 (subseq string start symbol-start)))
                           (lambda (symbol)
                             (symbol-name* symbol package-part))))
                       (let ((external-only (not (or internal
                                                     (eq package *package*)))))
                         (if (eq functional t)
                           (find-functions 
                             #'symbol-name-cmp package external-only)
                           (find-symbols ; find-values?
                             #'symbol-name-cmp package external-only))))
              (when (eq package *package*)
                (mapcar! (lambda (name)
                           (nstring-downcase (concatenate 'string name ":")))
                         (find-packages #'name-cmp)))))
          #'string-lessp)))))

(defun string-start-intersection (&rest strings)
  (if (atom strings)
    strings
    (subseq (car strings)
            0 
            (reduce #'min strings
                    :key (lambda (s) 
                           (or (string-lessp (car strings) s)
                               (length s)))))))

(cffi:defcallback rl-complete :pointer 
                  ((word :string) (start :int) (end :int))
  (declare (ignorable word))
  (when (= *rl-completion-quote-character* (char-code #\'))
    (setf *rl-completion-suppress-quote* 1))
  (let ((result (complete *rl-line-buffer* start end)))
    (if result
      (progn
        (when (and (not (cdr result)) 
                   (char= (char (car result) (1- (length (car result)))) #\:))
          (setf *rl-completion-suppress-append* 1))
        (cffi:foreign-alloc :string 
                            :null-terminated-p t 
                            :initial-contents
                            (cons 
                              (apply #'string-start-intersection
                                     result)
                              result)))
      (cffi:null-pointer))))

(cffi:defcallback rl-completion-display-matches-hook :void 
                  ((matches :pointer) (num-matches :int) (max-length :int))
  (let ((f (and (> *rl-point* 0)
                (whitespacep (char *rl-line-buffer* (1- *rl-point*)))
                (find-parent-function *rl-line-buffer* *rl-point*))))
    (cond
      (f
        (terpri)
        (describe f)
        (rl-forced-update-display))
      ((or (< *rl-completion-query-items* 0)
           (> *rl-completion-query-items* num-matches)
           (if (y-or-n-p "~%Display all ~d possibilities?" num-matches)
             t
             (progn
               (terpri)
               (rl-forced-update-display)
               nil)))
       (rl-display-match-list matches num-matches max-length)
       (rl-forced-update-display)))))

(setf *rl-attempted-completion-function* (cffi:callback rl-complete)
      *rl-basic-word-break-characters* #.(format nil " ~c~c\"#',@(|" 
                                                 #\tab #\newline)
      *rl-basic-quote-characters* "\""
      *rl-completion-display-matches-hook* 
      (cffi:callback rl-completion-display-matches-hook))

(-rl-enable-paren-matching 1)

(when (probe-file *history-file*)
  (read-history *history-file*))
(push (lambda () 
        (stifle-history *history-size*)
        (write-history *history-file*)) 
      sb-ext:*exit-hooks*)

(handler-bind ((sb-ext:symbol-package-locked-error
                 (lambda (c) 
                   (declare (ignorable c)) 
                   (invoke-restart 'continue))))
  (macrolet ((describe-readline-reader (buffer input output ps1 ps2 on-eof)
               `(if ,buffer
                  (pop ,buffer)
                  (let ((*standard-input*  ,input)
                        (*standard-output* ,output)
                        (eof '#:eof))
                    (loop
                      (let ((result (multiple-value-list
                                      (readline :ps1 ,ps1
                                                :ps2 ,ps2
                                                :eof-value eof))))
                        (cond ((eq (car result) eof) 
                               (terpri)
                               ,on-eof)
                              (result
                                (setf ,buffer (cdr result))
                                (return (car result))))))))))
    (setf sb-int:*repl-prompt-fun* 
          (lambda (input) 
            (declare (ignorable input))
            (fresh-line))
          sb-int:*repl-read-form-fun*
          (let (buffer)
            (lambda (input output)
              (describe-readline-reader 
                buffer input output *ps1* *ps2* (sb-ext:quit))))
          (symbol-function 'sb-debug::debug-prompt)
          (lambda (stream)
            (sb-thread::get-foreground)
            (fresh-line stream))
          (symbol-function 'sb-debug::debug-read)
          (let (buffer)
            (lambda (stream)
              (describe-readline-reader
                buffer stream stream *debug-ps1* *debug-ps2* (abort)))))))
