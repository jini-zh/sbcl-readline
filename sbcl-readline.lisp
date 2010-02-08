;;; SBCL interface to GNU Readline
;;; author: Evgeniy Zhemchugov <jini.zh@gmail.com>
;;;
;;; (c) 2009

(defpackage readline
  (:use :cl :cl-user :sb-alien)
  (:export *history-file* 
           *history-size*
           *line-number*
           *prompt*
           stifle-history
           unstifle-history
           readline))

(in-package readline)

(declaim (optimize (safety 3) (debug 3)))

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
(cffi:defcfun "stifle_history"   :void   (max     :int))
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

(defun default-prompt ()
  (format nil "~a[~a]: "
          (car (sort (append
                       (package-nicknames *package*)
                       (list (package-name *package*)))
                     #'<
                     :key #'length))
          *line-number*))

(defvar *line-number* 1)
(defvar *history-file* 
  (format nil "~a/.sbcl-history" (sb-ext:posix-getenv "HOME")))
(defvar *history-size* 200)
(defvar *prompt* #'default-prompt)
(defvar *toplevel* t)

(defun repl-read-form-fun (input output)
  (let ((*standard-input*  input)
        (*standard-output* output))
    (loop with form
          with line = (rl-readline (funcall *prompt*))
          initially (unless line (terpri) (sb-ext:quit))
          until (or form (string= line ""))
          do (handler-case (setf form (read-from-string line))
               (end-of-file ()
                 (setf line (concatenate 'string
                                         line
                                         #.(make-string 1
                                             :initial-element #\newline)
                                         (rl-readline "")))))
          finally (if (string/= line "")
                    (progn
                      (add-history line)
                      (incf *line-number*)
                      (return form))
                    (return (values))))))

(defun readline (&key (prompt "> ")
                      (input *standard-input*) 
                      (output *standard-output*))
  (let ((*prompt* (if (stringp prompt) (lambda () prompt) prompt)))
    (repl-read-form-fun input output)))

(defun whitespacep (char)
  (member char '(#\Space #\Newline #\Linefeed #\Tab #\Return #\Page)))

(defun function-signature (f)
  (loop with optional = nil
        with key = nil
        with rest = nil
        with allow-other-keys = nil
        for arg in (sb-kernel:%simple-fun-arglist f)
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
         collect (intern (symbol-name (if (atom arg) arg (car arg))) 'keyword) 
                 into keys
        else
         count t into req-num
        finally 
         (return (values (sb-kernel:%simple-fun-name f)
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
  (multiple-value-bind (name req-num opt-num rest keys allow-other-keys)
                       (function-signature function)
    (if (or allow-other-keys (< arg (+ req-num opt-num)))
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

(defun find-function (name)
  (let ((f (find-symbol (string-upcase name))))
    (when (fboundp f)
      (or (macro-function f) (symbol-function f)))))

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
                 ((char= c #\() (if (= level 0)
                                  (return 
                                    (values
                                      (find-function (subseq string (1+ i) end))
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
  (let* ((colon (position #\: string :start start :end end))
         (double-colon (and colon 
                            (< (1+ colon) end) 
                            (char= (char string (1+ colon)) #\:)))
         (package (cond
                    ((not colon) *package*)
                    ((= colon start) 'keyword)
                    (t (find-package
                         (read-from-string string t nil 
                                           :start start 
                                           :end colon)))))
         (symbol-start (if colon
                         (if double-colon (+ colon 2) (1+ colon))
                         start))
         (functional (or (and (> start 0)
                              (char= (char string (1- start)) #\())
                         (and (> start 1)
                              (string= (subseq string (- start 2) start) "#'"))))
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
                       (let ((package-part (subseq string start symbol-start)))
                         (lambda (symbol)
                           (symbol-name* symbol package-part))))
                     (let ((external-only (not (or double-colon 
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
        #'string-lessp))))

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
           (if (y-or-n-p "Display all ~d possibilities?" num-matches)
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

(setf sb-int:*repl-prompt-fun* (lambda (input) (fresh-line)))
(setf sb-int:*repl-read-form-fun* #'repl-read-form-fun)

(when (probe-file *history-file*)
  (read-history *history-file*))
(push (lambda () 
        (stifle-history *history-size*)
        (write-history *history-file*)) 
      sb-ext:*exit-hooks*)
