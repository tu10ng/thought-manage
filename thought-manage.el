;;; thought-manage.el --- thought management
;;; Commentary:

;; 

;;; Code:


;; Customization

(defgroup thought-manage nil
  "Major mode for thought manage"
  :prefix "thought-manage-"
  :group 'text)

(defcustom thought-manage-disable-auto-remove-blank-line nil
  "disable auto remove multiple blank lines after save"
  :type 'boolean)

(defcustom thought-manage-show-everything-startup nil
  "do not recommend for philosophy"
  :type 'boolean)

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
    (define-key map (kbd "C-c y n") #'thought-manage-add-thought-to-thought)
    (define-key map (kbd "C-c y i") #'thought-manage-add-thought-init)
    (define-key map (kbd "C-c y a") #'thought-manage-show-all)    
    map))

;;;###autoload
(define-derived-mode thought-manage-mode text-mode "Thought Manage"
  "major mode for thought management

file extension types are .thought or .tm

the following commands are available:

\\{thought-manage-mode-map}
"
  (setq-local font-lock-defaults
              '(thought-manage-font-lock-keywords))
  (unless thought-manage-show-everything-startup
    (thought-manage-fold-all))
  (add-hook 'change-major-mode-hook #'thought-manage-show-all 0 t)
  (unless thought-manage-disable-auto-remove-blank-line
    (add-hook 'after-save-hook #'thought-manage-remove-blank-lines 0 t)))

(add-to-list 'auto-mode-alist
             '("\\.\\(?:tm\\|thought\\)\\'" . thought-manage-mode))


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
  "return t if point is at the beggining of the blank line which is just after the thought"
  (save-excursion
    (and (bolp)
         (and (not (bobp))
              (save-excursion
                (previous-line)
                (looking-at-p thought-manage-thought-regexp)))
         (not (looking-at-p thought-manage-thought-regexp)))))

(defun thought-manage-at-thought-p ()
  "return t if point is in the thought, in other words, not on blank line.
`thought-manage-at-thought-p' is t when `thought-manage-at-thought-end-p' is t because its on the blank line."
  (looking-at-p thought-manage-thought-regexp))

(defun thought-manage-thought-end-point ()
  (save-excursion
    (beginning-of-line)
    (while (not (thought-manage-at-thought-end-p))
      (next-line))
    (point)))

(defun thought-manage-back-to-current-thought ()
  "move point to the beggining of the first line of thought(current thought)."
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
  "Show all contents. maybe useless in design philosophy but useful in playing around"
  (interactive)
  (thought-manage-fold-region (point-min) (point-max) nil))

(defun thought-manage-fold ()
  "fold from the beginning of the thought, ends on the beginning of the first blank line encounterd"
  (interactive)
  (when (thought-manage-at-thought-p)
    (thought-manage-back-to-current-thought)
    (thought-manage-fold-region
     (1+ (line-end-position))
     (thought-manage-thought-end-point)
     t)))

(defun thought-manage-count-incident ()
  "how many incident is shown under current visibility.
0 after called `thought-manage-folded'"
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
  "show one more old thought & incident by showing 1+ current folding level"
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


;; add thought

(defun thought-manage-add-thought-to-thought (thought incident)
  "add thought and incident to the thought at point, and move the whole thought to the beggining of the file"
  (interactive "sthought:
sincident: ")
  (thought-manage-back-to-current-thought)
  (insert (format "%s\n<-%s\n" thought incident))  
  (thought-manage-back-to-current-thought)  
  (thought-manage-move-thought-to-front)
  )

(defun thought-manage-move-thought-to-front ()
  "move whole thought to beggining of the file for design philosophy.
see `move-text-region'"
  (save-excursion
    (thought-manage-back-to-current-thought)
    (let ((thought (delete-and-extract-region
                    (point)
                    (thought-manage-thought-end-point))))
      (goto-char (point-min))
      (insert thought))))

(defun thought-manage-add-thought-init (thought incident)
  "init a thought at the beggining of the file"
  (interactive "sthought:
sincident: ")
  (goto-char (point-min))
  (insert (format "%s\n<-%s\n\n" thought incident)))


;; utils
(defun thought-manage-remove-blank-lines ()
  "remove abundant blank lines, leave one behind"
  (interactive)
  (save-excursion
    (replace-regexp "^[ ]*\n+" "\n" nil (point-min) (point-max))))
