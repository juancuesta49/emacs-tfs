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

(defun tfs--get-color (properties)
  (cond ((member "edit" properties) "yellow")
        ((member "add" properties) "green")
        ((member "delete" properties) "red")
        ((member "remove" properties) "red")
        (t "pink")))

(defun make-command (action
                     detailed
                     &rest ARGS)
  (if detailed
      (list tfs-tf (symbol-name action) (append ARGS (list "/format:detailed") (tfs--maybe-add-login)))
      (list tfs-tf (symbol-name action)(append ARGS (tfs--maybe-add-login)))))

(defun get-command (command)
  (car command))

(defun get-action (command)
  (cadr command))

(defun get-args (command)
  (append (list (cadr command)) (cl-caddr command)))

(defun get-buffer-name (command)
  command
  (concat "*TFS-" (get-action command) "*"))

(defun send-command (command)
  (apply 'call-process
         (get-command command)
         nil
         (get-buffer-name command)
         nil
         (get-args command)))

(defun tfs--format-buffer (format-line-function buffer)
  (with-current-buffer buffer
    (let ((buffer-s (split-string (buffer-string) "\n" t)))
      (erase-buffer)
      (insert (mapconcat format-line-function
                         buffer-s
                         "\n")))))

(defun tfs--execute-command (command format-function kbdings-map)
  (let* ((exitcode nil)                 ;
         (command-bufname (get-buffer-name command))
         (buffer (get-buffer-create command-bufname)))
    (with-current-buffer buffer
      (erase-buffer)
      (tfs--append-to-message-buffer (concat (get-action command) ":\t"
                                             (prin1-to-string command) "\n"))
      (setq exitcode (send-command command))
      (cl-loop for kbding in kbdings-map
               do (define-key tfs-status-mode-map (kbd (car kbding)) `,(cdr kbding)))
      (tfs-status-mode 1))
    (if (equal exitcode 0)
        (progn
          (funcall format-function buffer)
          (display-buffer command-bufname t))
      (error "Get TFS status was unsuccessful (%S)" exitcode))))

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

(defun tfs--get-file-name ()
  (s-trim (thing-at-point 'filename t)))



(defun tfs--parse-tfs-status (string-result)
  (defun tfs--parse-tfs-status-file (string-result)
    (cond ((null string-result) nil)
          ((string-match-p "\\s-+Change.*:.*" (car string-result))
           (cons (split-string (substring (car string-result)
                                          (+ (cl-search ":"
                                                        (car string-result))
                                             2))
                               ", ")
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

(defun tfs--status-reformat-output (buffer)
  (with-current-buffer buffer
    (let* ((parsed-output (tfs--parse-tfs-status (substring-no-properties (buffer-string))))
           (lines (mapcar (lambda (file-info)
                            (propertize (concat "*\t"
                                                (cadr file-info)
                                                " ["
                                                (mapconcat 'identity
                                                           (car file-info)
                                                           ", ")
                                                "]\n")
                                        'font-lock-face
                                        `(:foreground ,(tfs--get-color (car file-info)))))
                          parsed-output)))
      (erase-buffer)
      (insert "*** TF STATUS RESULTS ***\n\n\n")
      (font-lock-mode t)
      (tfs--write-to-buffer lines buffer))))

(defun tfs--write-to-buffer (lines buffer)
  (with-current-buffer buffer
    (cl-loop for line in lines
             do (insert line))))

(defun goto-file-at-point ()
  (interactive)
  (let ((line (thing-at-point 'line t)))
      (switch-to-buffer (find-file-noselect
                     (substring line
                                (+ (cl-search "*\t" line) 2)
                                (- (cl-search "[" line) 1))))))
(defun tfs-status ()
  (interactive)
  (tfs--execute-command (make-command 'status t)
                        'tfs--status-reformat-output
                        '(("o" . goto-file-at-point))))
