;;; hl-freq.el ---  Highlight words that are frequent

;; Copyright (C) 2015  Chiyano

;; Author: Chiyano <chiyanop at gmail dot com>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'pp)
(require 'cl)

(defgroup hl-freq nil
  "Options for hl-freq-minor-mode.")

(defgroup hl-freq-faces nil
  "Faces for hl-freq-minor-mode."
  :group :hl-freq)

(defcustom hl-freq-file "~/.emacs.d/hl-freq.dat"
  "File to store `hl-freq-hash-table'."
  :group 'hl-freq :type 'integer)

(defcustom hl-freq-font-lock-exclude-modes '(prog-mode)
  "Modes excluded from font-lock."
  :group 'hl-freq :type 'list)

(defface hl-freq-very-low
  '((t (:foreground "black" :background "white")))
  "hl-freq font most lower."
  :group 'hl-freq-faces)

(defface hl-freq-low
  '((t (:foreground "black" :background "yellow")))
  "hl-freq font most lower."
  :group 'hl-freq-faces)

(defface hl-freq-medium
  '((t (:foreground "black" :background "cyan")))
  "hl-freq font most lower."
  :group 'hl-freq-faces)

(defface hl-freq-high
  '((t (:foreground "white" :background "magenta")))
  "hl-freq font most lower."
  :group 'hl-freq-faces)

(defface hl-freq-very-high
  '((t (:foreground "white" :background "red")))
  "hl-freq font most lower."
  :group 'hl-freq-faces)

(defvar hl-freq-hash-table nil
  "Hash table contains frequent text.")

(defun hl-freq-face (keyword)
  "Return symbol of face on keyword."
  (let* ((n (hl-freq-keyword-n keyword)))
    (cond
     ((> n 12) 'hl-freq-very-high)
     ((> n 8) 'hl-freq-high)
     ((> n 5) 'hl-freq-medium)
     ((> n 2) 'hl-freq-low)
     ((> n 0) 'hl-freq-very-low))))

(defun hl-freq-map-all-buffers (func)
  "Map font-lock to all buffers."
  (mapc (lambda (buf)
          (with-current-buffer buf
            (or
             (reduce (lambda (result mode)
                       (or result
                           (derived-mode-p mode)))
                     (cons nil hl-freq-font-lock-exclude-modes))
             (funcall func))))
        (buffer-list)))

(defun hl-freq-set-font-lock (keyword)
  "Set font-lock on KEYWORD."
  (let ((face (hl-freq-face keyword)))
    (hl-freq-map-all-buffers
     (lambda ()
       (font-lock-add-keywords nil (list (list keyword 'quote face)))))))

(defun hl-freq-remove-font-lock (keyword)
  "Remove font-lock from KEYWORD."
  (let ((face (hl-freq-face keyword)))
    (hl-freq-map-all-buffers
     (lambda ()
       (font-lock-remove-keywords nil (list (list keyword 'quote face)))))))

(defun hl-freq-apply-font-lock ()
  "Apply font-lock to all keywords."
  (maphash
   (lambda (keyword v)
     (hl-freq-set-font-lock keyword)
     (font-lock-fontify-buffer))
   hl-freq-hash-table))

(defun hl-freq-apply-font-lock-current-buffer ()
  "Apply font-lock to current buffer."
  (with-current-buffer (current-buffer)
    (hl-freq-apply-font-lock)))

(defun hl-freq-apply-font-lock-all-buffers ()
  "Apply font-lock to all buffers."
  (hl-freq-map-all-buffers
   (lambda ()
     (hl-freq-apply-font-lock))))

(defun hl-freq-make-hash-table ()
  "Make hash table."
  (make-hash-table :test 'equal))

(defun hl-freq-keyword-property (keyword)
  "Return KEYWORD property."
  (gethash keyword hl-freq-hash-table))

(defun hl-freq-keyword-n (keyword)
  "Return count of KEYWORD."
  (or (plist-get (hl-freq-keyword-property keyword) :n) 0))

(defun hl-freq-set-keyword-n (keyword n)
  "Set N into `hl-freq-hash-table' as count of KEYWORD."
  (hl-freq-remove-font-lock keyword)
  (puthash keyword `(:n ,n) hl-freq-hash-table)
  (hl-freq-set-font-lock keyword)
  (font-lock-fontify-buffer))

(defun hl-freq-increment-keyword-n (keyword)
  "Increment count of KEYWORD."
  (let ((n (hl-freq-keyword-n keyword)))
    (hl-freq-set-keyword-n keyword (1+ n))))

(defun hl-freq-remove-keyword (keyword)
  "Remove KEYWORD from `hl-freq-hash-table'."
  (hl-freq-remove-font-lock keyword)
  (remhash keyword hl-freq-hash-table)
  (font-lock-fontify-buffer))

(defun hl-freq-word-list ()
  "Return sorted word list."
  (let ((list))
    (maphash (lambda (k v) (push (cons k v) list)) hl-freq-hash-table)
    (sort list
          (lambda (a b)
            (> (plist-get (cdr a) :n)
               (plist-get (cdr b) :n))))))

(defun hl-freq-serialize ()
  "Serialize `hl-freq-hash-table'."
  (let (alist)
    (maphash (lambda (k v)
               (push (cons k v) alist))
             hl-freq-hash-table)
    alist))

(defun hl-freq-deserialize (sexp)
  "Deserialize SEXP to load it into `hl-freq-hash-table'."
  (let ((hash-table (hl-freq-make-hash-table)))
    (condition-case nil
        (mapc
         (lambda (cons)
           (puthash (car cons) (cdr cons) hash-table))
         sexp)
      (error (message "Invalid hl-freq-hash-table.") nil))
    hash-table))

(defun hl-freq-save ()
  "Store `hl-freq-hash-table' to file specified by `hl-freq-file'."
  (ignore-errors
    (with-temp-buffer
      (pp (hl-freq-serialize) (current-buffer))
      (write-region (point-min) (point-max) hl-freq-file))))

(defun hl-freq-load ()
  "Load `hl-freq-serialize' from file specified by `hl-freq-file'."
  (if (file-exists-p hl-freq-file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents hl-freq-file)
          (goto-char (point-min))
          (hl-freq-deserialize (read (current-buffer)))))
    (hl-freq-make-hash-table)))

(eval-after-load 'hl-freq
  '(progn
    (setq hl-freq-hash-table (hl-freq-load))
    (hl-freq-apply-font-lock-all-buffers)))

(add-hook 'kill-emacs-hook 'hl-freq-save)

(add-hook 'after-change-major-mode-hook 'hl-freq-apply-font-lock-current-buffer)

(provide 'hl-freq)

;;; hl-freq.el ends here
