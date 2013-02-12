;;; dayone-journal.el --- Browse and edit a Day One journal package from Emacs

;;; Copyright (C) 2012 Rob Tillotson <rob@pyrite.org>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation  and/or other materials provided with the distribution.
;; 3. Neither the names of the copyright holders nor the names of any
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Version: 0.1
;;; Author: Rob Tillotson <rob@pyrite.org>
;;; Keywords: plain text, journal, Day One
;;; URL: http://github.com/robtillotson/dayone-journal-el

;; This file is not part of GNU Emacs.

;;; Status:
;;
;; Very incomplete.  Use at your own risk. :)
;;

;;; Commentary:
;;
;; Day One is a journal app for the iPad and Mac OS X, with a clean and
;; useful design, Markdown support, and many other useful features.
;; Unfortunately, there is no official Day One client for any non-Apple
;; platform yet, but since it uses a relatively straightforward XML based
;; storage format based on standard Apple plists, it isn't hard to work
;; with its data.
;;
;; This package is intended to be a way to perform basic operations on
;; a Day One journal on platforms with Emacs.  It isn't intended as a
;; complete replacement, but to be just functional enough to view the
;; journal and do simple editing and addition of new text entries.
;;
;; Finally, I would like to thank Mr. Jason Blevins <jrblevin@sdf.org>,
;; whose Deft mode provided most of the ideas and examples of how to
;; do the navigation UI for this package, in addition to being an
;; indispensable part of my daily workflow.

(require 'uuid)
(require 'xml)

(defconst dayone-buffer "*Day One*"
  "Day One buffer name.")

(defvar dayone-mode-hook nil)

(defgroup dayone-journal nil
  "Emacs Day One mode."
  :group 'local)

(defcustom dayone-journal-path (expand-file-name "~/Dropbox/Apps/Day One/Journal.dayone")
  "Location of the Day One journal package."
  :type 'directory
  :safe 'stringp
  :group 'dayone-journal)

;; Faces

(defgroup dayone-faces nil
  "Day One faces."
  :group 'dayone-journal
  :group 'faces)

(defface dayone-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Day One header."
  :group 'dayone-faces)

(defface dayone-date-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Day One dates."
  :group 'dayone-faces)

(defface dayone-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for Day One entry summaries."
  :group 'dayone-faces)

(defface dayone-tag-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Day One tags."
  :group 'dayone-faces)


;;
;; Simple recursive parser for Apple XML plists as used in Day One.
;;

(defun dayone-parse-plist-file (filename)
  "Load an Apple XML property list from a file."
  (let ((xml (car (xml-parse-file filename))))
    (dayone-plist-parse-node (car (xml-get-children xml 'dict)))))

(defun dayone-plist-remove-strings (seq)
  "Remove strings from a list."
  (delq nil (mapcar (lambda (x) (and (not (stringp x)) x)) seq)))

;; Return the value of a plist node.  All types except dict are extremely straightforward.
(defun dayone-plist-parse-node (node)
  "Return the value of a node in an Apple XML property list structure.
Dicts are returned as alists, arrays as regular lists, dates as elisp time objects, and
everything else as an appropriate primitive type."
  (let ((name (xml-node-name node))
        (children (xml-node-children node)))
    (cond ((eq name 'key) (apply 'concat children))
          ((eq name 'string) (apply 'concat children))
          ((eq name 'date) (date-to-time (apply 'concat children)))
          ((eq name 'array)
           (mapcar 'dayone-plist-parse-node (dayone-plist-remove-strings (xml-node-children node))))
          ((eq name 'dict) (dayone-plist-parse-dict node))
          ((eq name 'true) t)
          ((eq name 'false) nil))))

;; Return the contents of a dict as a hash table, using dayone-plist-parse-node to
;; recursively handle its contents.
(defun dayone-plist-parse-dict (node)
  "Convert a dict in an XML property list to an alist."
  (let ((children (dayone-plist-remove-strings (xml-node-children node)))
        (dict (make-hash-table :test 'equal))
        (key nil))
    (mapc (lambda (child)
            (let ((name (xml-node-name child))
                  (children (xml-node-children child)))
              (if (eq name 'key) (setq key (dayone-plist-parse-node child))
                (puthash key (dayone-plist-parse-node child) dict))))
          children)
    dict))

;;
;; Day One package handling
;;
(setq dayone-entries nil)

(defun dayone-entry-file-names ()
  "Return the filenames of all entries in the Day One journal package."
  (let ((entries-path (concat (file-name-as-directory dayone-journal-path) "entries")))
    (directory-files entries-path t "\\.doentry$")))

(defun dayone-compare-entries-by-date (e0 e1)
  "Comparison function to use when sorting Day One entries by date."
  (let ((date0 (gethash "Creation Date" e0))
        (date1 (gethash "Creation Date" e1)))
    (time-less-p date0 date1)))

(defun dayone-load-entries ()
  "Load all entries from the Day One journal package.
The return value is an alist mapping IDs to entries (which are themselves alists),
sorted by the entry creation date.  The return value is also cached in the
dayone-entries variable."
  (let ((entries nil))
    (mapc (lambda (filename)
            (let ((entry (dayone-parse-plist-file filename))
                  (uid (when (string-match "\\\([0-9a-zA-Z]+\\\)\\.doentry$" filename)
                         (match-string 1 filename))))
              (setq entries (cons entry entries))))
          (dayone-entry-file-names))
    (setq dayone-entries (sort entries 'dayone-compare-entries-by-date))))

(defun dayone-has-entry-p (uuid)
  (member uuid (mapcar (lambda (x) (gethash "UUID" x)) dayone-entries)))

(defun dayone-new-entry ()
  (let ((uuid nil))
    ;; There should not be collisions, but...
    (while (or (not uuid) (dayone-has-entry-p uuid))
      (setq uuid (upcase (replace-regexp-in-string "\\-" "" (uuid-string)))))
    (list (cons "UUID" uuid)
          (cons "Creation Date" (current-time))
          (cons "Entry Text" "")
          (cons "Tags" nil)
          (cons "Starred" nil))))

(defun dayone-value-to-xml (value type)
  (cond ((stringp value) (concat "<string>" (xml-escape-string value) "</string>"))
        ((eq type 'date) (concat "<date>" (format-time-string "%Y-%m-%dT%TZ" value t) "</date>"))
        ((hash-table-p value) (dayone-dict-to-xml value))
        ((or (eq type 'array) (listp value)) (dayone-list-to-xml value))
        ((eq value t) "<true/>")
        ((eq value nil) "<false/>")))

(defun dayone-entry-to-xml (entry)
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n")
    (insert "<plist version=\"1.0\">\n")
    (insert "<dict>\n")
    (dolist (item entry)
      )
    (insert "</dict>\n")
    (insert "</plist>\n")))
;;
;; Entry editing mode.
;;
;; Since Day One uses Markdown for text formatting, entries use a major mode derived
;; from markdown-mode.  Metadata is inserted into the buffer as specially formatted
;; headings, and stripped (and put into the entry plist as appropriate) from the
;; text as the entry is saved.
;;
(defun dayone-open-entry (entry)
  "Create and switch to a new buffer containing a single Day One entry."
  (let* ((uuid (gethash "UUID" entry))
         (date (format-time-string "%Y-%m-%d %H:%M" (gethash "Creation Date" entry)))
         (text (gethash "Entry Text" entry))
         (tags (gethash "Tags" entry))
         (starred (gethash "Starred" entry))
         (bufname (concat "*Day One: " date "*"))
         (buffer (get-buffer-create bufname)))
    (set-buffer buffer)
    (erase-buffer)
    (make-local-variable 'dayone-entry-uuid)
    (setq dayone-entry-uuid uuid)
    ;; Create a special title line to allow editing tags and stars.
    (insert "# %% Date: " date "\n")
    (insert "# %% Tags: " (mapconcat 'identity tags ", ") "\n")
    (insert "# %% Starred: " (if starred "Yes" "No") "\n")
    (insert "\n")
    ;;
    (insert text)
    (dayone-entry-mode)
    (visual-line-mode t)
    (switch-to-buffer buffer)
    ))

(defun dayone-save-entry ()
  )

(define-derived-mode dayone-entry-mode markdown-mode "Day One Entry"
  "Major mode for Day One entries.
\\{dayone-entry-mode-map}")

(define-key dayone-entry-mode-map (kbd "C-c C-c") 'dayone-save-entry)

;;
;; Navigation buffer.  Thanks to deft.el for the example of how to do this.
;;
;; Uses the widget library to make a clickable line for each entry.
;;
(defun dayone-buffer-setup ()
  "Set up the journal browser buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert (propertize "Day One" 'face 'dayone-header-face))
  (widget-insert "\n\n")
  (if dayone-entries
      (progn
        (mapc 'dayone-entry-widget dayone-entries)))
  (use-local-map dayone-mode-map)
  (widget-setup)
  (goto-char 1)
  (forward-line 2))

(defun dayone-entry-widget (entry)
  "Insert a line into the navigation buffer representing a single entry."
  (when entry
    (let* ((uuid (gethash "UUID" entry))
           (date (format-time-string "%Y-%m-%d %H:%M" (gethash "Creation Date" entry)))
           (text (gethash "Entry Text" entry))
           (tags (gethash "Tags" entry))
           (tagstr (mapconcat 'identity (mapcar (lambda (x) (concat "#" x)) tags) " "))
           (tagstr-width (length tagstr))
           (starred (gethash "Starred" entry))
           (width (window-width))
           (line-width (- width tagstr-width))
           (date-width (length date))
           (summary (replace-regexp-in-string "[\n\t]" " " text))
           (summary-width (min (length summary)
                               (- line-width date-width 5)))
           )
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :button-face 'dayone-date-face
                     :format "%[%v%]"
                     :tag entry
                     :notify (lambda (widget &rest ignore)
                               (dayone-open-entry (widget-get widget :tag)))
                     date)
      (widget-insert "  ")
      (when (> summary-width 0)
        (widget-insert (propertize (substring summary 0 summary-width) 'face 'dayone-summary-face))
        (widget-insert " "))
      (when tagstr
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize tagstr 'face 'dayone-tag-face)))
      (widget-insert "\n"))))

(defvar dayone-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'dayone-open-entry)
    (define-key map (kbd "C-c C-q") 'quit-window)
    (define-key map (kbd "C-c C-g") `dayone-refresh)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map (kbd "<tab>") 'widget-forward)
    (define-key map (kbd "<backtab>") 'widget-backward)
    (define-key map (kbd "<S-tab>") 'widget-backward)
    map)
  "Keymap for Day One mode.")

(defun dayone-mode ()
  "Major mode for browsing a Day One journal.

\\{dayone-mode-map}."
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map dayone-mode-map)
  (dayone-load-entries)
  (setq major-mode 'dayone-mode)
  (setq mode-name "Day One")
  (dayone-buffer-setup)
  (run-mode-hooks 'dayone-mode-hook))

(put 'dayone-mode 'mode-class 'special)

(defun dayone ()
  (interactive)
  (switch-to-buffer dayone-buffer)
  (if (not (eq major-mode 'dayone-mode))
      (dayone-mode)))

(provide 'dayone-journal)
