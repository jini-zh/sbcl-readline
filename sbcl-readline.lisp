;;; SBCL interface to GNU Readline
;;; (c) 2009--2017 by Evgenii Zhemchugov <jini.zh@gmail.com>
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
  (:export *complete*
           *history-file*
           *history-size*
           *ps1*
           *ps2*
           *debug-ps1*
           *debug-ps2*
           stifle-history
           unstifle-history
           readline-lisp
           readline
           set-keyboard-input-timeout
           get-event-hook
           set-event-hook
           printf
           with-default-rl))

(in-package readline)

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

(cffi:define-foreign-library ncurses 
                             (:windows "pdcurses.dll")
                             (t (:or "libncursesw.so.6" "libncursesw.so.5")))
(cffi:define-foreign-library readline 
                             (:windows (:or "readline.dll" "readline5.dll"))
                             (t (:or "libreadline.so.6")))

(cffi:use-foreign-library ncurses)
(cffi:use-foreign-library readline)

(cffi:defcfun readline :string (prompt :string))
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
(cffi:defcfun ("rl_set_keyboard_input_timeout" set-keyboard-input-timeout) 
              :int 
              "While waiting for keyboard input, Readline will wait for u microseconds before calling any function assigned with set-event-hook. u must be greater than or equal to zero (a zero-length timeout is equivalent to poll). The default waiting period is one-tenth of a second. Returns the old timeout value."
              (u :int))

(cffi:defcfun "strlen" :unsigned-long (s :pointer))

(cffi:defcvar "rl_attempted_completion_function"   :pointer)
(cffi:defcvar "rl_attempted_completion"            :int)

(cffi:defcvar "rl_attempted_completion_over"       :int
  "Non-zero means to suppress normal filename completion after the user-specified completion function has been called.")

(cffi:defcvar "rl_basic_quote_characters"          :string)

(cffi:defcvar "rl_basic_word_break_characters"     :string
  "The basic list of characters that signal a break between words for the completer routine.  The initial contents of this variable is what breaks words in the shell, i.e. \" \\n\\t\\\"\\\\'`@$>.")

(cffi:defcvar "rl_completer_word_break_characters"  :string
  "The list of characters that signal a break between words for rl_complete_internal.  The default list is the contents of rl_basic_word_break_characters.")

(cffi:defcvar "rl_completion_append_character" :int
  "Character appended to completed words when at the end of the line.  The default is a space.  Nothing is added if this is '\0'.")

(cffi:defcvar "rl_completion_display_matches_hook" :pointer
  "If non-zero, then this is the address of a function to call when completing a word would normally display the list of possible matches.  This function is called instead of actually doing the display.  It takes three arguments: (char **matches, int num_matches, int max_length) where MATCHES is the array of strings that matched, NUM_MATCHES is the number of strings in that array, and MAX_LENGTH is the length of the longest string in that array.")

(cffi:defcvar "rl_completion_query_items"          :int)
(cffi:defcvar "rl_completion_quote_character"      :int)
(cffi:defcvar "rl_completion_suppress_append"      :int)

(cffi:defcvar "rl_completion_suppress_quote"       :int
  "If non-zero, the completion functions don't append any closing quote.  This is set to 0 by rl_complete_internal and may be changed by an application-specific completion function.")

(cffi:defcvar "rl_event_hook"                      :pointer)
(cffi:defcvar "rl_line_buffer"                     :string)
(cffi:defcvar "rl_point"                           :int)
(cffi:defcvar "rl_prompt"                          :pointer)
(cffi:defcvar "rl_readline_state"                  :int)

; taken from /usr/include/readline/readline.h
(cffi:defcenum state
  (:initializing #x0000001)  ; initializing
  (:initialized  #x0000002)  ; initialization done
  (:termprepped  #x0000004)  ; terminal is prepped
  (:readcmd      #x0000008)  ; reading a command key
  (:metanext     #x0000010)  ; reading input after ESC
  (:dispatching  #x0000020)  ; dispatching to a command
  (:moreinput    #x0000040)  ; reading more input in a command function
  (:isearch      #x0000080)  ; doing incremental search
  (:nsearch      #x0000100)  ; doing non-inc search
  (:search       #x0000200)  ; doing a history search
  (:numericarg   #x0000400)  ; reading numeric argument
  (:macroinput   #x0000800)  ; getting input from a macro
  (:macrodef     #x0001000)  ; defining keyboard macro
  (:overwrite    #x0002000)  ; overwrite mode
  (:completing   #x0004000)  ; doing completion
  (:sighandler   #x0008000)  ; in readline sighandler
  (:undoing      #x0010000)  ; doing an undo
  (:inputpending #x0020000)  ; rl_execute_next called
  (:ttycsaved    #x0040000)  ; tty special chars saved
  (:callback     #x0080000)  ; using the callback interface
  (:vimotion     #x0100000)  ; reading vi motion arg
  (:multikey     #x0200000)  ; reading multiple-key command
  (:vicmdonce    #x0400000)  ; entered vi command mode at least once
  (:redisplaying #x0800000)) ; updating terminal display

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
  "Either a string or a function that returns a string to be used when prompting user for the next command. The function has to take no arguments")

(defvar *ps2* "> "
  "Either a string or a function that returns a string to be used when prompting user for the rest of incomplete command. The function has to take no arguments")

(defvar *debug-ps1*
  ; #'sb-debug::debug-prompt will be overwritten later
  (let ((p #'sb-debug::debug-prompt))
    (lambda ()
      ; strip the newline in the beginning
      (subseq (with-output-to-string (s) (funcall p s)) 1)))
  "Either a string or a function that returns a string to be used when prompting user for the next command in debug mode. The function has to take no arguments")

(defvar *debug-ps2* "* "
  "Either a string or a function that returns a string to be used when prompting user for the rest of inomplete command in debug mode. The function has to take no arguments")

(defvar *event-hook* nil)

(defun prompt (ps)
  (etypecase ps
    (string ps)
    (function (funcall ps))))

(defun readline-lisp (&key (ps1 "* ") (ps2 "> ") (eof-value nil eof-error-p))
  "Prompts the user for a command and returns the result as a Lisp form"
  (let ((cmd "") result (pos 0))
    (unwind-protect
      (flet ((read1 (ps)
               (let ((line (readline (prompt ps))))
                 (unless line
                   (if eof-error-p
                     (return-from readline-lisp eof-value)
                     (error 'end-of-file :stream *standard-output*)))
                 line)))
        (setf cmd (read1 ps1))
        (loop
          (handler-case
            (loop
              (let ((eof '#:eof))
                (multiple-value-bind (form p) (read-from-string cmd nil eof
                                                                :start pos)
                  (when (eq form eof)
                    (return-from readline-lisp (values-list (nreverse result))))
                  (push form result)
                  (setf pos p))))
            (end-of-file ()))
          (setf cmd
                (concatenate 'string
                             cmd
                             #.(make-string 1 :initial-element #\newline)
                             (read1 ps2)))))
      (add-history cmd))))

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
        else if (eq arg 'sb-int:&more)
         do (progn
              (setf allow-other-keys t
                    rest t)
              (loop-finish))
        else if (and optional (not rest) (not key))
         count t into opt-num
        else if key
         collect (intern (symbol-name
                           (cond ((atom arg) arg)
                                 ((atom (car arg)) (car arg))
                                 (t (caar arg))))
                         'keyword)
                 into keys
        else unless rest
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
    (if (or allow-other-keys (and rest (not keys))
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

(defun find-function (string &key (start 0) (end (length string)) as-symbol)
  (multiple-value-bind (symbol-start internal package)
                       (parse-symbol string :start start :end end)
    (declare (ignore internal))
    (let ((f (find-symbol 
               (nstring-upcase (subseq string symbol-start end))
               package)))
      (when (fboundp f)
        (if as-symbol
          f
          (or (macro-function f)
              (symbol-function f)))))))

(defun find-parent-function (string start &key as-symbol)
  (loop with level = 0
        with end = start
        with arg = -1
        with space = nil
        for i from (1- start) downto 0
        for c = (char string i)
        do (if (whitespacep c)
             (progn
               (when (and (= level 0) (not space))
                 (incf arg)
                 (setf space t))
               (setf end i))
             (progn
               (setf space nil)
               (cond 
                 ((char= c #\()
                  (if (= level 0)
                    (return 
                      (values (find-function string
                                             :start (1+ i)
                                             :end end
                                             :as-symbol as-symbol)
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

(defun complete-symbol (string start end)
  (multiple-value-bind (symbol-start internal package)
                       (parse-symbol string :start start :end end)
    (let ((functional (or (and (> start 0)
                               (char= (char string (1- start)) #\())
                          (and (> start 1)
                               (string= string "#'"
                                        :start1 (- start 2)
                                        :end1   start))))
          (min-name-length (- end symbol-start))
          arg)
      (unless functional
        (multiple-value-setq (functional arg) 
          (find-parent-function string start)))
      (labels ((name-cmp (name)
                 (and (>= (length name) min-name-length)
                      (string-equal string name
                                    :start1 symbol-start
                                    :end1   end
                                    :end2   min-name-length)))
               (symbol-name-cmp (symbol) (name-cmp (symbol-name symbol))))
        (unless package ; bad package name
          (return-from complete-symbol nil))
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

(defun rl-complete (string start end)
  (when (= *rl-completion-quote-character* (char-code #\'))
    (setf *rl-completion-suppress-quote* 1))
  (let ((q (quoted string start)))
    (when q
      (unless (and (> q 1) (string-equal string "#p" :start1 (- q 2) :end1 q))
        (setf *rl-attempted-completion-over* 1))
      (return-from rl-complete nil)))
  (setf *rl-attempted-completion-over* 1)
  (let ((result (complete-symbol string start end)))
    (when (and result
               (not (cdr result))
               (char= (char (car result) (1- (length (car result)))) #\:))
      (setf *rl-completion-suppress-append* 1))
    result))

(defvar *complete* #'rl-complete)

(defun string-start-intersection (&rest strings)
  (if (cdr strings)
    (subseq (car strings)
            0 
            (reduce #'min strings
                    :key (lambda (s) 
                           (or (string-lessp (car strings) s)
                               (length s)))))
    (car strings)))

#-sb-unicode
(progn
  (declaim (inline int->string-position))
  (defun int->string-position (string int &optional (start 0))
    (declare (ignore string))
    (+ start int)))

#+sb-unicode
(defun int->string-position (string int &optional (start 0))
  (flet ((char-byte-size (char)
           (let ((code (char-code char)))
             (cond
                ; max Unicode code point is #x10ffff
               ((> code #xffff) 3)
               ((> code #xff)   2)
               (t               1)))))
    (loop
      with i = 0
      for j from start below (length string)
      until (>= i int)
      do (incf i (char-byte-size (char string j)))
      finally (return j))))

(cffi:defcallback rl-attempted-completion :pointer
                  ((word :string) (c-start :int) (c-end :int))
  (declare (ignore word))
  (if *complete*
    (let ((result (let* ((start (int->string-position *rl-line-buffer*
                                                      c-start))
                         (end   (int->string-position *rl-line-buffer*
                                                      (- c-end c-start)
                                                      start)))
                    (funcall *complete* *rl-line-buffer* start end))))
      (if result
        (cffi:foreign-alloc
          :string
          :null-terminated-p t
          :initial-contents (cons (apply #'string-start-intersection result)
                                  result))
        (cffi:null-pointer)))
    (cffi:null-pointer)))

(cffi:defcallback rl-completion-display-matches-hook :void
                  ((matches :pointer) (num-matches :int) (max-length :int))
  (let ((f (when (plusp *rl-point*)
             (let* ((line *rl-line-buffer*)
                    (point (int->string-position line *rl-point*)))
               (when (whitespacep (char line (1- point)))
                 (find-parent-function line point :as-symbol t))))))
    (cond
      (f
        (terpri)
        (describe f)
        (rl-forced-update-display))
      ((or (not (<= 0 *rl-completion-query-items* num-matches))
           (if (y-or-n-p "~%Display all ~d possibilities?" num-matches)
             t
             (progn
               (terpri)
               (rl-forced-update-display)
               nil)))
       (rl-display-match-list matches num-matches max-length)
       (rl-forced-update-display)))))

(defun set-rl-options (&optional (set :sbcl))
  (ecase set
    (:sbcl
      (setf *rl-basic-word-break-characters*
            #.(format nil " ~c~c\"#',@(|" #\tab #\newline)
            *rl-basic-quote-characters*
            "\""
            *rl-completion-display-matches-hook*
            (cffi:callback rl-completion-display-matches-hook))
      (-rl-enable-paren-matching 1))
    (:default
      (setf *rl-basic-word-break-characters*
            #.(format nil " ~c~c\"\\'`@$>" #\tab #\newline)
            *rl-basic-quote-characters*
            "'\"`"
            *rl-completion-display-matches-hook*
            (cffi:null-pointer))
      (-rl-enable-paren-matching 0)))
  (setf *rl-completer-word-break-characters* *rl-basic-word-break-characters*))

(set-rl-options)
(setf *rl-attempted-completion-function*
      (cffi:callback rl-attempted-completion))

(defmacro with-default-rl (&body body)
  `(unwind-protect
     (progn
       (set-rl-options :default)
       ,@body)
     (set-rl-options)))

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
                                      (readline-lisp :ps1 ,ps1
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
            (lambda (stream eof-restart)
              (describe-readline-reader
                buffer stream stream *debug-ps1* *debug-ps2*
                (invoke-restart eof-restart)))))))

(defun set-event-hook (function)
  "Call the function periodically when Readline is waiting for terminal input. By default, this will be called at most ten times a second if there is no keyboard input. NIL removes the hook."
  (setf *event-hook* function
        *rl-event-hook* (if function
                          (cffi:callback event-hook)
                          (cffi:null-pointer)))
  nil)

(cffi:defcallback event-hook :int ()
  (handler-case 
    (funcall *event-hook*)
    (condition (c)
      (princ c)
      (set-event-hook nil)))
  0)

(defun get-event-hook ()
  "Get the function periodically called by Readline while waiting for terminal input"
  *event-hook*)

(defun in-state (state)
  (not (zerop (logand *rl-readline-state*
                      (cffi:foreign-enum-value 'state state)))))

(defun printf (format &rest data)
  (if (in-state :readcmd)
    (progn
      (write-char #\return)
      (dotimes (i (+ (strlen *rl-prompt*) *rl-point*))
        (write-char #\space))
      (write-char #\return)
      (apply #'format t format data)
      (write-char #\newline)
      (rl-forced-update-display))
    (apply #'format t format data)))
