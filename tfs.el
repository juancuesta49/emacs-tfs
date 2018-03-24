(defcustom tfs-tf  "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\Common7\\IDE\\TF.exe"
  "location of the tf.exe command.  Defaults to \"c:\\Program Files\\Microsoft Visual Studio 9.0\\common7\\ide\\tf.exe\""
  :group 'tfs)

(defvar tfs-status-mode-map (make-sparse-keymap)
  "Keymap while temp-mode is active.")

(define-minor-mode tfs-status-mode "Local mode for tfs status command" :init-value nil tfs-status-mode-map)

(defcustom tfs--tfpt-exe  "c:\\Program Files\\Microsoft Team Foundation Server 2008 Power Tools\\TFPT.exe"
  "location of the tfpt.exe command.  Defaults to \"c:\\Program Files\\Microsoft Team Foundation Server 2008 Power Tools\\TFPT.exe\""
  :group 'tfs)


(defcustom tfs--login "/login:domain\\user,password"
  "/login option for all TFS activity."
  :group 'tfs)

(defcustom tfs--buffer-name "*TFS Messages*"
  "name of buffer for TFS Messages"
  :group 'tfs)

(defun tfs--maybe-add-login ()
  "If login details aren't supplied, don't try using them"
 (if (equal tfs--login "/login:domain\\user,password")
     () ; return the empty list
   (list tfs--login)))

(defun tfs--append-to-message-buffer (text)
  "Append text to the TFS Messages buffer.  Intended for internal use only."
  (let ((buf (current-buffer))
        (tfsbuffer (get-buffer-create tfs/buffer-name)))
    (set-buffer tfsbuffer)
    (goto-char (point-max))
    (insert text)
    (set-buffer buf)))

(defun make-command (action
                     format
                     &rest ARGS)
  (if format
      (list tfs-tf (symbol-name action) (append ARGS (list (concat "/format:" (symbol-name action))) (tfs--maybe-add-login)))
      (list tfs-tf (symbol-name action)(append ARGS (tfs--maybe-add-login)))))

(defun get-command (command)

  (car command))

(defun get-action (command)
  "Gets the actual action associated with command (e.g status/history/diff/checkout)"
  (cadr command))

(defun get-args (command)
  "Gets argument list from command made with make-command"
  (append (list (cadr command)) (cl-caddr command)))

(defun get-buffer-name (command)
  "Computes a name of buffer from command made with make-command"
  (concat "*TFS-" (get-action command) "*"))

(defun execute (command)
  "This function is used to execute commands made with make-command"
  (apply 'call-process
         (get-command command)
         nil
         (get-buffer-name command)
         nil
         (get-args command)))

(defun tfs--execute-command (command format-function kbdings-map)
  "tfs--execute-command is helper function to execute every tf command that has output
and/or actions associated with it"
  (let* ((exitcode nil)                 ;
         (command-bufname (get-buffer-name command))
         (buffer (get-buffer-create command-bufname)))
    (with-current-buffer buffer
      (erase-buffer)
      (tfs--append-to-message-buffer (concat (get-action command) ":\t"
                                             (prin1-to-string command) "\n"))
      (setq exitcode (execute command))
      (cl-loop for kbding in kbdings-map
               do (define-key tfs-status-mode-map (kbd (car kbding)) `,(cdr kbding)))
      (tfs-status-mode 1))
    (cond ((equal exitcode 0) (funcall format-function buffer)
                              (display-buffer command-bufname t))
          (t (error "Get TFS status was unsuccessful (%S)" exitcode)))))

(defun tfs--determine-file (filename prompt)
  "determine the name of the file to use in a TF command."
  (let ((is-tfs-enabled
         (and (fboundp 'tfs-mode)    ;; function
              (boundp 'tfs-mode)     ;; variable
              tfs-mode)))
    (cond
     ((stringp filename) filename)
     ((eq major-mode 'dired-mode) (dired-get-filename))
     ((is-tfs-enabled) tfs--get-file-name)
     (buffer-file-name buffer-file-name)
     (t (expand-file-name (read-file-name "File to checkout: "))))))

(defun tfs--write-to-buffer (lines buffer)
  "Writes list of strings to buffer in seperate lines"
  (with-current-buffer buffer
    (cl-loop for line in lines
             do (insert (concat line "\n")))))

;; TFS STATUS related functions
(defun tfs--get-status-color (properties)
  "Maps change mode to appropriate color"
  (cond ((member "edit" properties) "yellow")
        ((member "add" properties) "green")
        ((member "delete" properties) "red")
        ((member "remove" properties) "red")
        (t "pink")))

(defun tfs--reformat-status-output (buffer)
  "This function is used to reformat /format:detailed output from TFS output to more emacs
friendly format"
  (defun tfs--parse-tfs-status (string-result)
    "Function that recieves raw output from tf status and converts it
to list of list of strings"
    (defun tfs--parse-tfs-status-file (string-result)
      (cond ((null string-result) nil)
            ((string-match-p "\\s-+Change.*:.*" (car string-result))
             (cons (split-string (substring (car string-result)
                                            (+ (cl-search ":"
                                                          (car string-result))
                                               2))
                                 ", ")
                   (tfs--parse-tfs-status-file (cdr string-result))))
            ((string-match-p "\\s-+Workspace.*:.*" (car string-result))
             (cons (substring (car string-result)
                              (+ (cl-search ":"
                                            (car string-result))
                                 2))
                   (tfs--parse-tfs-status-file (cdr string-result))))
            ((string-match-p "\\s-+Local item.*:.*" (car string-result))
             (cons (substring (car string-result)
                              (+ (cl-search "]" (car string-result)) 2))
                   (tfs--parse-tfs-status-file (cdr string-result))))
            (t (tfs--parse-tfs-status-file (cdr string-result)))))
    (mapcar (lambda (file-info)
              (tfs--parse-tfs-status-file (split-string file-info
                                                        "\n")))
            (split-string string-result "\n\n")))
  (with-current-buffer buffer
    (let* ((parsed-output (tfs--parse-tfs-status (substring-no-properties (buffer-string))))
           (lines (mapcar (lambda (file-info)
                            (propertize (concat "*\t"
                                                (cadr file-info)
                                                " ["
                                                (mapconcat 'identity
                                                           (car file-info)
                                                           ", ")
                                                "]")
                                        'font-lock-face
                                        `(:foreground ,(tfs--get-status-color (car file-info)))))
                          parsed-output)))
      (erase-buffer)
      (insert "*** TF STATUS RESULTS ***\n\n\n")
      (font-lock-mode t)
      (tfs--write-to-buffer lines buffer))))

(defun tfs-status-goto-file-at-point (select-buffer)
  "Function that is used in tfs status mode to jump to buffer"
  (interactive)
  (let* ((line (thing-at-point 'line t))
        (buffer (switch-to-buffer-other-window (find-file-noselect(substring line
                                                                 (+ (cl-search "*\t" line) 2)
                                                                 (- (cl-search "[" line) 1))))))
    (if select-buffer
        (set-buffer buffer))))

(defun tfs-status ()
  "This function returns status of workspace file from which it is invoked belongs to
It creates new buffer with tfs-status-mode map. Modify this map to change default behaviour of keybindings
"
  (interactive)
  (tfs--execute-command (make-command 'status 'detailed)
                        #'tfs--reformat-status--output
                        '(("o" . (lambda () (tfs-status-goto-file-at-point t)))
                          ("C-o" . (lambda () (tfs-status-goto-file-at-point nil)))
                          ("g" . ))
