;;; cppref.el --- A Simple C++ Reference Viewer

;; Copyright (C) 2009 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi, <kentarok@gmail.com>
;; Modified by: whitypig <whitypig@gmailcom>
;; Keywords: C++

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * Description

;; cppref.el is a port of Perl's cppref command, a simple C++
;; reference viewer working on a terminal.

;; * Usage
;;
;; cppref.el requires cl and emacs-w3m installed in advance. So, add
;; the lines below into your .emacs:
;;
;;   (require 'cppref)
;;
;; Althogh cppref.el automatically find out the place of
;; documentation, if you want to put your the directory at some other
;; place, you must add one more line like below:
;;
;;   (setq cppref-doc-dir "/path/to/dir") ;; doesn't end with "/"
;;
;; Then run `cppref' command and type like "vector::begin",
;; "io::fopen", or so.

;;; Acknowledment:

;; cppref.el is Emacs version of Kazuho Oku's cppref command
;; http://search.cpan.org/dist/cppref/

;; The documents are from http://www.cppreference.com/ (under Creative
;; Commons Attribution 3.0 license).

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'w3m-load)
  (load "find-func") ;; for `find-library-name' to be loaded.
  )

(defvar cppref-doc-dir nil
  "Your local directory in which C++ references are placed")

(defvar cppref-open-in-the-same-window t
  "non nil means that a reference is opened in the current window.
When nil, it try to open a reference in some other window, or
already-opened w3m buffer window.")

(defun cppref (name)
  "Show C++ reference along with arg `name' using w3m web
browser."
  (interactive (cppref-read-primary-args))
  (cppref-init-doc-dir)
  (let ((candidates nil)
        (reference nil))
    (when (string-equal name "")
      (setq name "start"))

    ;; replace "class::method" to "class/method"
    (setq name (replace-regexp-in-string "::" "/" name))

    ;; directory index is like ***/start.html
    (when (file-directory-p (concat cppref-doc-dir "/" name))
      (setq name (concat name "/start")))

    (setq candidates
          (let ((file (concat cppref-doc-dir "/" name ".html")))
            (if (file-exists-p file)
                (list file)
              (cppref-find-reference cppref-doc-dir name))))

    (cond
     ((not candidates)
      (error (concat "no document found for " name)))
     ((= 1 (length candidates))
      (setq reference (car candidates)))
     ((> (length candidates) 1)
      (setq reference (cppref-select-from-multiple-choices
                       candidates))))

    (cppref-visit-reference reference cppref-open-in-the-same-window)))

(defun cppref-read-primary-args ()
  "Return a string entered from minibuffer or default"
  (list (let* ((default (symbol-at-point))
               (input
                (read-from-minibuffer (if default
                                          (format "Search for (default %s): " default)
                                        "Search for: ")
                                      nil       ;; initial-contents (obsolete)
                                      nil       ;; keymap
                                      nil       ;; read
                                      nil       ;; hist
                                      default   ;; default
                                      )))
          (if (string= input "")
              (format "%s" default)
            (format "%s" input)))))

(defun cppref-select-from-multiple-choices (choices)
  (completing-read "multiple choices. push tab key. select :" choices nil t ""))

(defun cppref-init-doc-dir ()
  (if (not cppref-doc-dir)
      (let* ((library-path (find-library-name "cppref"))
             (library-root (file-name-directory library-path)))
        (setq cppref-doc-dir (concat library-root "docs")))))

(defun cppref-visit-reference (reference open-in-the-same-window)
  (cond
   (cppref-open-in-the-same-window (w3m-find-file reference))
   (t (cppref-visit-reference-in-other-window reference))))

(defun cppref-find-reference (dir name)
  (let ((candidates '())
        (reference  nil)
        (absolute-path nil))
    (loop for fn
          in (directory-files dir)
          do (setq absolute-path (concat dir "/" fn))
          (if (file-directory-p absolute-path)
              (when (and (not (string-equal fn "."))
                         (not (string-equal fn "..")))
                (if (string-match (concat name "$") absolute-path)
                    (push (concat absolute-path "/start.html") candidates)
                  (setq candidates
                        (append (cppref-find-reference absolute-path name)
                                candidates))))
            (when (string-match (concat name "\\.html$") absolute-path)
              (push absolute-path candidates))))
    candidates))

(defun cppref-visit-reference-in-other-window (reference)
  "Open reference in some other window."
  (let ((w (get-buffer-window "*w3m*")))
    (cond
     (w (select-window w)
        (w3m-find-file reference))
     ((one-window-p)
      ;; There is only one window in the current frame.
      ;; So, let's split this window horizontally.
      ;; TODO: make a user choose which way to split window.
      ;; TODO: need to be refactored.
      (split-window-horizontally)
      (select-window (next-window))
      (w3m-find-file reference))
     (t
      (select-window (next-window))
      (w3m-find-file reference)))))

(provide 'cppref)
;;; cppref.el ends here
