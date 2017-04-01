(defgroup ru-typo nil
  "Minor mode for typography in russian texts."
  :group 'editing)

;;;###autoload
(defcustom ru-typo-mode nil
  "Toggle ru-typo-mode."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "1.0"
  :type 'boolean
  :group 'ru-typo
  :require 'ru-typo)

(defcustom ru-typo-left-fir-quote #xAB
  "Left fir quote character \"«\""
  :type 'character
  :group 'ru-typo)

(defcustom ru-typo-right-fir-quote #xBB
  "Right fir quote character \"»\""
  :type 'character
  :group 'ru-typo)

(defcustom ru-typo-left-paw-quote #x201E
  "Left paw quote character \"„\""
  :type 'character
  :group 'ru-typo)

(defcustom ru-typo-right-paw-quote #x201C
  "Right paw quote character \"“\""
  :type 'character
  :group 'ru-typo)

(defcustom ru-typo-nbspace #xA0
  "Non-breakable space (nbspace)"
  :type 'character
  :group 'ru-typo)

(defcustom ru-typo-mdash #x2014
  "Long dash character (mdash) \"\""
  :type 'character
  :group 'ru-typo)

(defcustom ru-typo-quotes-left-context (concat "^\\|\\s-\\|\\s(\\|[" (string ru-typo-right-fir-quote ru-typo-right-paw-quote) "]")
  "Regular expression to define left context of
the left quote"
  :type 'regexp
  :group 'ru-typo)

(defcustom ru-typo-mdash-left-context " -"
  "Regular expression to define left context of
dash to turn it into mdash"
  :type 'regexp
  :group 'ru-typo)

(defun ru-typo-quotes-insert-any (left right)
  (insert-char (if (looking-back ru-typo-quotes-left-context (- (point) 1)) left right)))
  
(defun ru-typo-quotes-insert-double ()
  (interactive)
  (ru-typo-quotes-insert-any ru-typo-left-fir-quote ru-typo-right-fir-quote))

(defun ru-typo-quotes-insert-single ()
  (interactive)
  (ru-typo-quotes-insert-any ru-typo-left-paw-quote ru-typo-right-paw-quote))

(defun ru-typo-mdash-insert ()
  (interactive)
  (insert (if (looking-back ru-typo-mdash-left-context (- (point) 2))
	      (progn (delete-char -2) (string ru-typo-nbspace ru-typo-mdash))
	    "-")))

;;;###autoload
(define-minor-mode ru-typo-mode
  "Minor mode that makes some russian typography replacements.
For now: quotes and long dash."
  :lighter (:eval (string ? (decode-char 'ucs ru-typo-left-fir-quote)
			  (decode-char 'ucs ru-typo-right-fir-quote)))
  :keymap '(("'" . ru-typo-quotes-insert-single)
	    ("\"" . ru-typo-quotes-insert-double)
	    ("-" . ru-typo-mdash-insert)))

;;;###autoload
(defun turn-on-ru-typo ()
  (ru-typo-mode +1))

;;;###autoload
(defun turn-off-ru-typo ()
  (ru-typo-mode -1))

(provide 'ru-typo)
