;;; thought-manage.el --- thought management
;;; Commentary:

;; 


;;; Code:

(require 'org-fold-core)

;; Customization

(defgroup thought-manage nil
  "Major mode for thought manage"
  :prefix "thought-manage-"
  :group 'text)

;; thought-manage-mode-hook

;; 


;; font lock
(require 'font-lock)

(defgroup thought-manage-faces nil
  "faces used in thought-manage mode"
  :group 'thought-manage
  :group 'faces)

(defface thought-manage-incident-faces
  '((t (:inherit underline)))
  "face for incident"
  :group 'thought-manage-faces)

;;

(defvar current-thought-regexp-back "^\s*\n"
  "regular expression to match the line before a current-thought")

(defvar current-thought-regexp-at ".*"
  "regular expression to match the beginning of a current-thought")

(defvar thought-end current-thought-regexp-back)

(defun thought-manage-at-current-thought-p ()
  "return t if point is on current-thought"
  (save-excursion
    (beginning-of-line)
    (and (bolp)
         (looking-back current-thought-regexp-back)
         (looking-at current-thought-regexp-at))))

(defun thought-manage-back-to-current-thought ()
  (beginning-of-line)
  (while (not (thought-manage-at-current-thought-p))
    (previous-line)))

(defun thought-manage-fold ()
  (interactive)
  (thought-manage-back-to-current-thought)
    (thought-manage-fold-region
     (line-end-position)
     (save-excursion
       (re-search-forward thought-end)
       (previous-line 2)
       (line-end-position))
     t))

;; (local-set-key (kbd "TAB") #'thought-manage-fold)
;; (local-set-key (kbd "<backtab>") #'thought-manage-show-all)

(defun thought-manage-show ()
  "keep current folding level, unfold thought and fold again with 1+ folding level")

(defun thought-manage-fold-all ())

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

