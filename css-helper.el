;;; css-helper.el --- CSS Mode helper functions
;;;
;; Author: Tung Dao <me@tungdao.com>
;; Version: 0.1
;;
;; This file is NOT part of GNU Emacs
;;
;;; License: BSD http://opensource.org/licenses/BSD-3-Clause
;;
;; Copyright (c) 2012, Tung Dao
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.
;; * Neither the name of the <ORGANIZATION> nor the names of its contributors may
;; be used to endorse or promote products derived from this software without
;; specific prior written permission.
;;
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
;;
;;
;;; Commentary:
;;
;; Helper functions provided:
;; `css-helper-explain'
;;
;;; Installation:
;; Add the to your .emacs or similar:
;;
;; (autoload 'css-helper-explain "css-helper")
;; (add-hook 'css-mode-hook
;;   (lambda ()
;;     (local-set-key (kbd "C-c C-s" #'css-helper-explain))))
;;
;;; TODO:
;; - test suite
;; - simplify specificity calculation
;; - elpa
;; - sass support
;;
;;; Changelog
;;
;; v0.1, 2012-12-10
;; - Initial version
;;

(defconst css-helper-version "v0.1")

(defconst css-helper-patterns
  '(:id "#[a-zA-Z0-9-_]+"
    :class "\\.[a-zA-Z0-9-_]+"
    :tag "^[^#\\.:\\[][a-zA-Z0-9-_]+"
    :attr "\\[[^\\[]+\\]"
    :pseudo-class-or-element ":[a-zA-Z0-9-_]+"
    :pseudo-class ""
    :pseudo-element ":first-line\\|:first-letter\\|:before\\|:after"))

(defun css-helper-match-pattern-p (type part)
  "Check a part against a pattern in the patterns list given by type."
  (string-match (plist-get css-helper-patterns type) part))

(defun css-helper-matcher (type)
  "Returns matcher function for a given type. Behave smartly for `:pseudo-class'
because Emacs regex doesn't do look ahead, so we cannot find them directly."
  (if (eq type :pseudo-class)
      (lambda (part)
        (and (css-helper-match-pattern-p :pseudo-class-or-element part)
             (not (css-helper-match-pattern-p :pseudo-element part))
             part))
    (lambda (part)
      (and (css-helper-match-pattern-p type part) part))))

(defun css-helper-filter-by (type parts)
  (delq nil (mapcar (css-helper-matcher type) parts)))

(defun css-helper-categorize (parts)
  (let* ((categories (make-hash-table :test 'equal)))
    (dolist (type css-helper-patterns)
      (when (symbolp type)
        (puthash type
                 (css-helper-filter-by type parts)
                 categories)))
    categories))

(defun css-helper-parse-selector (selector)
  (let* ((chunker "\\([#\\.:]?[a-zA-Z0-9-_]+\\|\\[[^\\[]+\\]\\)")
         (parts '())
         (last-match 0))
    (while (string-match chunker selector last-match)
      (push (match-string 1 selector) parts)
      (setq last-match (match-end 0)))
    parts))

(defun css-helper-calculate-specificity (selector)
  "Returns the specificity of a selector after parsed into parts.
See http://www.w3.org/TR/CSS21/cascade.html#specificity for more detail.
"
  (let* ((parts (css-helper-parse-selector selector))
         (categories (css-helper-categorize parts))
         (ids (gethash :id categories))
         (class-likes (append (gethash :attr categories)
                              (gethash :class categories)
                              (gethash :pseudo-class categories)))
         (tag-likes (append (gethash :tag categories)
                            (gethash :pseudo-element categories))))
    (mapcar #'length (list ids class-likes tag-likes))))

(defun css-helper-calculate-specificity (selector)
  (let* ((parts (css-helper-parse-selector selector)))
    (dolist (part parts)
      (cond )))

(defun css-helper-detect-category-and-key (selector)
  (let* ((parts (split-string selector " "))
         (last (car (last parts))))
    (cond ((css-helper-match-pattern-p :id last) `("id" ,last))
          ((css-helper-match-pattern-p :class last) `("class" ,last))
          ((css-helper-match-pattern-p :tag last) `("tag" ,last))
          (t '("universal" "*")))))

;;;###autoload
(defun css-helper-explain ()
  "SQL EXPLAIN, for CSS selectors.
Inspired by https://github.com/josh/css-explain/"
  (interactive)
  (let* ((selector (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
         (parts (split-string selector " "))
         (specificity (css-helper-calculate-specificity parts))
         (category-and-key (css-helper-detect-category-and-key parts)))
    ;; Vector conversion just for better string representation.
    (message "%s %s"
             (apply #'vector specificity)
             (apply #'vector category-and-key))))

;;; Test

;; (defun css-helper-test-explain (selector)
;;   (let* ((specificity (css-helper-calculate-specificity selector))
;;          (category-and-key (css-helper-detect-category-and-key selector)))
;;     (list selector specificity category-and-key)))

;; (css-helper-test-explain "li.active ~ strong[type=foo]:hover .foo")


(provide 'css-helper)

;;; css-helper.el ends here
