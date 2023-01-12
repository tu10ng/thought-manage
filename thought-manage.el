;;; thought-manage.el --- thought management
;;; Commentary:

;; 

;;; Code:


;; Customization

(defgroup thought-manage nil
  "Major mode for thought manage"
  :prefix "thought-manage-"
  :group 'text)



;; font lock
(require 'font-lock)

(defgroup thought-manage-faces nil
  "faces used in thought-manage mode"
  :group 'thought-manage
  :group 'faces)

(defface thought-manage-incident-face
  '((t (:inherit underline)))
  "face for incident"
  :group 'thought-manage-faces)

(defvar thought-manage-incident-face 'thought-manage-incident-face
  "see `outline-font-lock-faces'")

(defvar thought-manage-font-lock-keywords
  '(("^<-\\(.*\\)" 1 thought-manage-incident-face)))


;; major mode

(defvar thought-manage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'thought-manage-show)
    (define-key map (kbd "<backtab>") #'thought-manage-fold-all)
    map))

(define-derived-mode thought-manage-mode text-mode "Thought Manage"
  "Set major mode for thought manage

\\{thought-manage-mode-map}
"
  (setq-local font-lock-defaults
              '(thought-manage-font-lock-keywords))
  (add-hook 'change-major-mode-hook #'thought-manage-show-all))


;; helper functions

(defvar thought-manage-thought-regexp "^[^ \n]+?"
  "regular expression to match the the frist char in any line in a thought. the reverse match is the blank lines.")

(defvar thought-manage-incident-regexp "^<-.*$")

(defun thought-manage-at-current-thought-p ()
  "return t if point is at the beggining of current-thought"
  (save-excursion
    (and (bolp)
         (or (bobp)
             (save-excursion
               (previous-line)
               (not (looking-at-p thought-manage-thought-regexp))))
         (looking-at-p thought-manage-thought-regexp))))

(defun thought-manage-at-thought-end-p ()
  (save-excursion
    (and (bolp)
         (and (not (bobp))
              (save-excursion
                (previous-line)
                (looking-at-p thought-manage-thought-regexp)))
         (not (looking-at-p thought-manage-thought-regexp)))))

(defun thought-manage-at-thought-p ()
  (looking-at-p thought-manage-thought-regexp))

(defun thought-manage-thought-end-point ()
  (save-excursion
    (beginning-of-line)
    (while (not (thought-manage-at-thought-end-p))
      (next-line))
    (point)))

(defun thought-manage-back-to-current-thought ()
  (beginning-of-line)
  (while (not (thought-manage-at-current-thought-p))
    (previous-line)))


;; fold

(defun thought-manage-fold-region (from to flag)
  "hide or show lines from FROM to TO, according to FLAG.
see `org-fold-core-region'"
  (with-silent-modifications
    (if flag
        (put-text-property from to 'invisible t)
	  (remove-text-properties from to (list 'invisible nil)))))

(defun thought-manage-show-all ()
  "Show all contents. maybe useless"
  (interactive)
  (thought-manage-fold-region (point-min) (point-max) nil))

(defun thought-manage-fold ()
  (interactive)
  (when (thought-manage-at-thought-p)
    (thought-manage-back-to-current-thought)
    (thought-manage-fold-region
     (1+ (line-end-position))
     (thought-manage-thought-end-point)
     t)))

(defun thought-manage-count-incident ()
  (save-excursion
    (let ((count 0))
      (while (re-search-forward
              thought-manage-incident-regexp
              (save-excursion
                (goto-char (thought-manage-thought-end-point))
                (previous-line)
                (line-end-position))
              :move)
        (setq count (1+ count)))
      count)))

(defun thought-manage-show ()
  "keep current folding level, unfold thought and fold again with 1+ folding level"
  (interactive)
  (save-excursion
    (thought-manage-back-to-current-thought)
    (let* ((current-level (thought-manage-count-incident))
           (next-level (1+ current-level))
           (start-point (save-excursion
                          (goto-char (line-end-position))
                          (thought-manage-fold-region (point)
                                                      (thought-manage-thought-end-point)
                                                      nil)
                          (when (<= next-level (thought-manage-count-incident))
                            (dotimes (_ next-level)
                              (re-search-forward
                               thought-manage-incident-regexp
                               (thought-manage-thought-end-point))))
                          (1+ (point))))
           (end-point (thought-manage-thought-end-point)))
      (thought-manage-fold-region start-point end-point t))))

(defun thought-manage-fold-all ()
  "see `org-cycle-overview'"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (thought-manage-at-current-thought-p)
        (thought-manage-fold))
      (let ((next-line-add-newlines nil))
        (forward-visible-line 1)))))


