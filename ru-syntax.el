;; ru-syntax.el --- Minor mode for GNU Emacs

;; Author: Sergey Kruk <sergey.kruk@gmail.com>
;; Created: 2017-04-04

;; Underline russian misspelled words.

(defgroup ru-syntax nil
  "Minor mode for syntax in russian texts."
  :group 'editing)

;;;###autoload
(defcustom ru-syntax-mode nil
  "Toggle ru-syntax-mode."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "1.0.0"
  :type 'boolean
  :group 'ru-syntax)

(defun ru-syntax-enchant ()
  (ignore-errors
    (progn
      (remove-text-properties (point-min) (point-max) '(font-lock-face nil))
      (mapcar 'ru-syntax-underline-word (ru-syntax-get-misspelled-words-list)))))
      
(defun ru-syntax-underline-word (word)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward word nil 't)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'ru-syntax-underlined))))

(defun ru-syntax-get-misspelled-words-list ()
  (let ((selfbuf (current-buffer))
	(start (point-min))
	(end (point-max)))
    (with-temp-buffer
      (insert-buffer-substring selfbuf start end)
      (goto-char (point-min))
      (while (search-forward "ё" nil t)
        (replace-match "е"))
      (goto-char (point-min))
      (while (re-search-forward "\\s-[a-zA-Z0-9\\-]+\\s-" nil t)
        (replace-match " "))
      (shell-command-on-region
       (point-min) (point-max)
       "enchant -d ru -l" nil t)
      (split-string (buffer-string) "\n" t))))

(defface ru-syntax-underlined
  '((t (:underline "purple")))
  "Face for underline"
  :group 'ru-syntax)

(defvar ru-syntax--timer nil)
(defvar ru-syntax--timer-idle .3)

;;;###autoload
(define-minor-mode ru-syntax-mode
  "Minor mode that underlines misspelled russian words."
  :lighter "__")

;;;###autoload
(defun turn-on-ru-syntax ()
  (ru-syntax-mode +1)
  (unless ru-syntax--timer
    (setq ru-syntax--timer
          (run-with-idle-timer ru-syntax--timer-idle t #'ru-syntax-enchant))))

;;;###autoload
(defun turn-off-ru-syntax ()
  (ru-syntax-mode -1)
  (when ru-syntax--timer
    (cancel-timer ru-syntax--timer)
    (remove-text-properties (point-min) (point-max) '(font-lock-face nil))
    (setq ru-syntax--timer nil)))

(provide 'ru-syntax)
