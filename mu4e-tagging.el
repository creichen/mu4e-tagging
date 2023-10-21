;;; mu4e-tagging.el --- Minor mode for tag management in mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2023 Christoph Reichenbach

;; Author: Christoph Reichenbach <creichen@creichen.net>
;; Maintainer: Christoph Reichenbach <creichen@creichen.net>
;; Created: 12 Oct 2023

;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.0") (compat "29.1"))

;; Keywords: mail
;; URL: https://github.com/creichen/mu4e-tagging

;; This file is not part of GNU Emacs.

;; mu4e-tagging is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds a minor mode to simplify tagging and tag-based
;; search within `mu4e-headers-mode'.

;; Extends `mu4e' and requires `dash' and `compat-29'.

;; Terminology:
;; - tags: all the things managed by mu4e-tagging-mode, comprising
;;   - flags: toggles that are independent of each other
;;   - categories: selections that are mutually exclusive.
;;
;; Mails may violate the "categories" restriction and/or include
;; unknown tags; we handle these cases gracefully.
;;
;; - A mail is:
;;   - cagetorised ("categorized" in function/var definitions) iff it
;;     has at least one category
;;   - uncategorised ("uncategorized" in code) otherwise

;;; Code:

(require 'dash)
(require 'mu4e)
(require 'compat-29)
(require 'crm)
(setq mu4e-tagging--supports-vtable
      (require 'vtable nil 'optional))

(defgroup mu4e-tagging
  nil
  "Configuration options and faces for `mu4e-tagging-mode`."
  :group 'mu4e-headers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tag updates

(defun mu4e-tagging-decategorize ()
  "Remove any category tags for the message at point."
  (interactive)
  (let ((tags-remove (mapconcat (lambda (n) (concat "-" n))
                                (mu4e-tagging-known-category-tags)
                               ","))
        (msg (mu4e-message-at-point)))
    (mu4e-action-retag-message msg tags-remove)))


(defun mu4e-tagging-keystring (key)
  "Translate key binding to satisfy `key-valid-p'.

KEY may be a string, character, or key vector."
  (cond ((key-valid-p key)
         key)
        ((vectorp key)
         (key-description key))
        ((characterp key)
         (key-description (vector key)))
        (t (error "Invalid key: " key))))


(defun mu4e-tagging-keyvec (key)
  "Translate a key binding into a vector of characters.

KEY may be a string, character, or key vector."
  (let* ((key-vec-or-string key)
         (key-tag (cond ((vectorp key-vec-or-string)
                         key-vec-or-string)
                        ((characterp key-vec-or-string)
                         (vector key-vec-or-string))
                        (t
                         (key-parse key-vec-or-string)))))
    (when (eq 0 (length key-tag))
      (warn "mu4e-tagging: empty key binding (%s); misconfigured?"
            key-vec-or-string))
    key-tag))


(defun mu4e-tagging-alloc-keys (tag-name tag-pbody)
  "Allocate and bind keys for TAG-NAME, ensuring uniqueness.

Returns TAG-PBODY with :key possibly updated.  Throws
'keybind-impossible if
1. :key in tag-pbody is already bound,
2. :key is not in tag-pbody and the function couldn't guess an
    unused binding, or
3. :key is somehow invalid."
  (let* ((requested-key (plist-get tag-pbody :key))
         (str-key (cond
                   ((null requested-key)
                    nil)
                   ((stringp requested-key)
                    requested-key)
                   (t
                    (key-description (if (vectorp requested-key)
                                         requested-key
                                       ;; otherwise single character
                                       (vector requested-key))))))
         (key (or
               str-key
               (cl-loop for c across (concat tag-name
                                             (plist-get tag-pbody
                                                        :short)
                                             ;; backup list of keys:
                                             "1234567890")
                        when (not (keymap-lookup
                                   mu4e-tagging--dyn-keymap
                                   (kbd (string c))))
                        return (key-description (vector c)))))
         (prefix-key-vec (mu4e-tagging-keyvec
                            mu4e-tagging-untag-prefix))
         (query-key-vec (mu4e-tagging-keyvec
                           mu4e-tagging-query-prefix)))
    (when (or
           (null key)
           (equal (string-to-char key)
                  mu4e-tagging-untag-prefix)
           (keymap-lookup mu4e-tagging-mode-map (kbd key)))
      (throw 'keybind-impossible (list tag-name :key requested-key)))
    ;; Otherwise the keybind is available
    (let* ((key-tag (mu4e-tagging-keyvec key))
           (key-untag (vconcat prefix-key-vec key-tag))
           ;; keys for querying
           (key-query-require (vconcat query-key-vec key-tag))
           (key-query-block (vconcat query-key-vec
                                     prefix-key-vec
                                     key-tag))
           ;; update :key accordingly
           (tag-pbody-new (plist-put tag-pbody
                                     :key
                                     (mu4e-tagging-keystring key-tag)))
           (taginfo (cons tag-name tag-pbody-new)))
      ;; Bind the key to call the generic interceptor functions
      (define-key mu4e-tagging--dyn-keymap
                  key-tag
                  #'mu4e-tagging--interceptor-tag)
      (define-key mu4e-tagging--dyn-keymap
                  key-untag #'mu4e-tagging--interceptor-untag)
      (define-key mu4e-tagging--dyn-keymap
                  key-query-require
                  #'mu4e-tagging--interceptor-query-require)
      (define-key mu4e-tagging--dyn-keymap
                  key-query-block
                  #'mu4e-tagging--interceptor-query-block)

      ;; Set up reverse lookup keymaps so the interceptors can figure
      ;; out why they were called
      (puthash key-tag
               taginfo
               mu4e-tagging--key-action-table)
      (puthash key-untag
               taginfo
               mu4e-tagging--key-action-table)
      (puthash key-query-require
               taginfo
               mu4e-tagging--key-action-table)
      (puthash key-query-block
               taginfo
               mu4e-tagging--key-action-table)
      ;; Return updated tag-body plist
      tag-pbody-new)))

(defun mu4e-tagging-alloc-default-keys ()
  "Install default dynamic key bindings."
  (define-key mu4e-tagging--dyn-keymap
              (mu4e-tagging-keyvec mu4e-tagging-uncategorized-suffix)
              'mu4e-tagging-decategorize))

;; Current list of all category tags
(setq mu4e-tagging-categories-var nil)

(defun mu4e-tagging-update-tags ()
  "Extract all tag information from the user specification.

Plugs in defaults as needed."
  (clrhash mu4e-tagging--key-action-table)
  (clrhash mu4e-tagging--known-tags)
  ;(setq mu4e-tagging-known-category-tags nil)
  (mu4e-tagging-clear-tagging-dyn-keymap)
  (let ((prefix-key-vec (if (vectorp mu4e-tagging-untag-prefix)
                            mu4e-tagging-untag-prefix
                          (vector mu4e-tagging-untag-prefix)))
        (tag-aspecs-list (if (boundp 'mu4e-tagging-tags)
                              mu4e-tagging-tags
                            nil))
        (tag-infos-categories nil)
        (tag-infos-flags nil))
    (dolist (tag-aspec tag-aspecs-list)
      (-let* (((tag-name . tag-pbody-orig) tag-aspec)
              (tag-short-orig (plist-get tag-pbody-orig
                                         :short))
              (tag-short (if tag-short-orig
                             tag-short-orig
                           tag-name))
              (tag-pbody-pre (plist-put tag-pbody-orig
                                        :short
                                        tag-short))
              ;; alloc-keys ensures that tag-pbody contains the
              ;; correct :key value
              (tag-pbody (mu4e-tagging-alloc-keys tag-name
                                                  tag-pbody-pre))
              (is-flag (plist-get tag-pbody :flag))
              (tag-info (cons tag-name tag-pbody)))
        (puthash tag-name tag-pbody mu4e-tagging--known-tags)
        (if is-flag
            ; flag:
            (push tag-info tag-infos-flags)
          ; category:
          (progn
            ;(push tag-name mu4e-tagging-known-category-tags)
            (push tag-info tag-infos-categories)))))
    ;; Add the remaining dynamic key bindings
    (mu4e-tagging-alloc-default-keys)
    ;; Store the results
    (setq mu4e-tagging-categories-var (reverse tag-infos-categories))
    (setq mu4e-tagging-flags-var (reverse tag-infos-flags))))


(defun mu4e-tagging-known-category-tags ()
  "All tags that are category tags (list of strings)."
  (mapcar 'car mu4e-tagging-categories-var))

(setq mu4e-tagging-tag-info-window nil)
(setq mu4e-tagging-mail-info-window nil)

(setq mu4e-tagging-tag-info-window-buf nil)
(setq mu4e-tagging-mail-info-window-buf nil)

(defun mu4e-tagging-mail-info-buf ()
  "Return the buffer used for showing mail information.

Shows the buffer in the designated window, if it still exists."
  (when mu4e-tagging-mail-info-window
    (let ((buf (get-buffer-create " *mu4e-tagging-mail-info*")))
      (unless
          mu4e-tagging-mail-info-window-buf
        (set-window-buffer mu4e-tagging-mail-info-window buf t)
        (setq mu4e-tagging-mail-info-window-buf buf))
      buf)))


(defun mu4e-tagging-tag-info-buf ()
  "Return the buffer used for showing tag information.

Shows the buffer in the designated window, if it still exists."
  (when mu4e-tagging-tag-info-window
    (let ((buf (get-buffer-create " *mu4e-tagging-tag-info*")))
      (unless
          mu4e-tagging-tag-info-window-buf
        (set-window-buffer mu4e-tagging-tag-info-window buf t)
        (setq mu4e-tagging-tag-info-window-buf buf))
      buf)))

(defun mu4e-tagging-enable-handler ()
  "Activate the minor mode: create info windows, add mu4e hooks.

Currently, the affected hooks are `mu4e-search-hook',
`mu4e-message-changed-hook', and `mu4e-main-mode-hook'.

However, `mu4e-tagging-query-submode-enable' may add more hooks
on demand."
  (let ((tag-info-window
         (if (window-live-p mu4e-tagging-tag-info-window)
             mu4e-tagging-tag-info-window
           (split-window-below (- -2 (hash-table-count
                                        mu4e-tagging--known-tags)))))
        (mail-info-window
         (if (window-live-p mu4e-tagging-mail-info-window)
             mu4e-tagging-mail-info-window
           (split-window-right -80))))
    (setq mu4e-tagging-tag-info-window tag-info-window)
    (setq mu4e-tagging-mail-info-window mail-info-window)
    (mu4e-tagging-mail-info-buf)
    (mu4e-tagging-tag-info-buf)
    (mu4e-tagging-mail-at-point-changed)
    (add-hook 'mu4e-main-mode-hook
              #'mu4e-tagging-disable)
    (add-hook 'mu4e-message-changed-hook
              #'mu4e-tagging-mail-at-point-changed)
    (add-hook 'mu4e-search-hook
              #'mu4e-tagging--query-submode-auto-disable)))

(defun mu4e-tagging-disable-handler ()
  "Deactivate minor mode: close info windows, remove mu4e hooks.

Also call `mu4e-tagging-query-submode-disable'."
  (remove-hook 'mu4e-search-hook
               #'mu4e-tagging--query-submode-auto-disable)
  (remove-hook 'mu4e-main-mode-hook
               #'mu4e-tagging-disable)
  (remove-hook 'mu4e-message-changed-hook
               #'mu4e-tagging-mail-at-point-changed)
  (mu4e-tagging-query-submode-disable)
  (unwind-protect
      (when (eq mu4e-tagging-tag-info-window-buf
              (window-buffer mu4e-tagging-tag-info-window))
          (delete-window mu4e-tagging-tag-info-window)))
  (unwind-protect
      (when (eq mu4e-tagging-mail-info-window-buf
              (window-buffer mu4e-tagging-mail-info-window))
          (delete-window mu4e-tagging-mail-info-window)))
  (setq mu4e-tagging-tag-info-window nil)
  (setq mu4e-tagging-mail-info-window nil)
  (setq mu4e-tagging-tag-info-window-buf nil)
  (setq mu4e-tagging-mail-info-window-buf nil))

(defun mu4e-tagging-disable ()
  "Ensure that `mu4e-tagging-mode' is disabled."
  (interactive)
  (if mu4e-tagging
      ;; switch mode off, which automatically calls disbale-handler
      (mu4e-tagging 'toggle)
    ;; no need to switch mode off, but must still call disbale-handler
    (mu4e-tagging-disable-handler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying and categorising tags

(defvar mu4e-tagging--known-tags
  (make-hash-table :test 'equal)
  "All tags managed by `mu4e-tagging-mode'.

Automatically constructed from `mu4e-tagging-tags'.  Maps to
plist with tag properties.")

(defun mu4e-tagging-info (tagname)
  "Retrieve the tag plist information for TAGNAME."
  (gethash tagname mu4e-tagging--known-tags '()))

(defun mu4e-tagging-category-tag-p (tagname)
  "Check if TAGNAME isa category tag."
  (let ((plist (mu4e-tagging-info tagname)))
    (and plist
         (not (plist-get plist
                         :flag)))))

(defun mu4e-tagging-flag-tag-p (tagname)
  "Check if TAGNAME is a flag."
  (let ((plist (mu4e-tagging-info tagname)))
    (and plist
         (plist-get plist
                    :flag))))

(defun mu4e-tagging-no-tag-p (tagname)
  "Check if TAGNAME is not managed by `mu4e-tagging-mode'."
  (not (mu4e-tagging-info tagname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pretty-printing tags

(defun mu4e-tagging-propertized-name (tag-name &rest short)
  "Propertized tag string for TAG-NAME.

If SHORT is non-NIL, uses :short instead of :tag as tag text."
  (let* ((tinfo (mu4e-tagging-info tag-name))
         (name (if (and short (car short))
                   (plist-get tinfo :short)
                 tag-name))
         (fg (plist-get tinfo :foreground))
         (bg (plist-get tinfo :background))
         (box (plist-get tinfo :box))
         (weight (plist-get tinfo :weight))
         (face                      (append
                              (if fg (list :foreground fg))
                              (if bg (list :background bg))
                              (cond ((null box) nil)
                                    ((eq box t) '(:box
                                                    (:line-width -1)))
                                    (t (list :box box)))
                              (if weight (list :weigth weight)))))
    (propertize name 'face (list face))))

(setq mu4e-tagging--short-tags
               '(:short-tags . (:name "Short Tags"
                                      :shortname "mTags" ; "milli-Tags"
                                      :function
                                        mu4e-tagging-render-tags)))

(defun mu4e-tagging--on-load ()
  "Add :short-tags as mu4e header field type.

Run auntomatically."
  (add-to-list 'mu4e-header-info-custom
               mu4e-tagging--short-tags))

(defun mu4e-tagging--on-unload ()
  "Remove :short-tags as mu4e header field type.

Run if `mu4e-tagging-mode' is unloaded."
  (setq mu4e-header-info-custom
        (remove mu4e-tagging--short-tags)
         mu4e-header-info-custom))

(defun mu4e-tagging-render-tags (msg)
  "Return propertized string to highlight tags for MSG.

Called from mu4e via `mu4e-header-info-custom'."
  (let* (
         (tags (mu4e-message-field msg :tags))
         (category-tags (-filter 'mu4e-tagging-category-tag-p tags))
         (flag-tags (-filter 'mu4e-tagging-flag-tag-p tags))
         (unknown-tags (-filter 'mu4e-tagging-no-tag-p tags))
         (category-part (mapconcat (lambda (tag)
                                     (mu4e-tagging-propertized-name
                                        tag
                                        t))
                                   category-tags "_"))
         (flag-part (mapconcat (lambda (tag)
                                 (mu4e-tagging-propertized-name tag t))
                               flag-tags ""))
         (unknown-part (mapconcat (lambda (tag) tag)
                                  unknown-tags ","))
         (flag-and-unknown-part (concat flag-part
                                        (if unknown-tags
                                            (concat " " unknown-part)
                                          ""))))
    (concat category-part
            (if (and flag-tags
                     (not (string-empty-p flag-and-unknown-part)))
                ":"
              "")
            flag-and-unknown-part)))

(defun mu4e-tagging-mail-at-point-changed (&rest _)
  "Handle changed selection to the mail at point.

Hook function that handles situations in which the mail at point
changed and we need to update the info buffers."
  (when (or mu4e-tagging-tag-info-window mu4e-tagging-mail-info-window)
    ;; only react if at least one of the info windows is open
    (mu4e-tagging-update-mail-info-buf)
    (mu4e-tagging-update-tag-info-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query submode

;; (name to '+ or '-)
(setq mu4e-tagging-query-flags (make-hash-table :test 'equal))
;; category tag (string) or 'uncategorized
(setq mu4e-tagging-query-category nil)
;; backup for  mu4e-query-rewrite-function
(setq mu4e-tagging-query-rewrite-function-backup nil)

(setq mu4e-tagging-query-rewrite-last-original-query nil)
(setq mu4e-tagging-query-rewrite-last-rewritten-query nil)


(defun mu4e-tagging--query-flags-alist ()
  "A sorted alist of all tagging query flags.

This alist is equivalent to the contents of the hashtable in
variable `mu4e-tagging-query-flags' but in the form of a sorted
alist, with tag names as keys and '+ or '- as values."
  (let ((tags nil))
    (maphash (lambda (k v)
               (push (cons k v) tags))
             mu4e-tagging-query-flags)
    (sort tags (lambda (a b) (string< (car a) (car b))))
    tags))


(defun mu4e-tagging--query-rewrite (initial-query)
  "Rewrite mu4e query to account for `mu4e-tagging-mode' filters.

Updates `mu4e-tagging-query-rewrite-last-original-query' with
INITIAL-QUERY and
`mu4e-tagging-query-rewrite-last-rewritten-query' with the
function return value, cf. `mu4e-tagging--query-rewrite-mu4e'."
  (if (null mu4e-tagging-query-rewrite-function-backup)
      (progn
        ;(message "mu4e-tagging--query-rewrite outside query-submode")
        initial-query)
    ;; otherwise
    (let* ((query (if (eq #'mu4e-tagging--query-rewrite
                          mu4e-tagging-query-rewrite-function-backup)
                      ;; sanity check; this shouldn't happen
                      initial-query
                    (apply mu4e-tagging-query-rewrite-function-backup
                           (list initial-query))))
           (cat-filter mu4e-tagging-query-category)
           (predicates-for-category
              (cond
               ;; Filter: yield only mails without categories
               ((eq 'uncategorized cat-filter)
                (mapcar (lambda (cat)
                          (concat "(NOT x:"
                                  cat
                                  ")"))
                        (mu4e-tagging-known-category-tags)))
               ;; Filter: yield only mails with that category
               ((stringp cat-filter)
                (list (concat "x:" cat-filter)))
               ;; Don't filter by category
               (t
                nil)))
           (predicates-for-flags
            (mapcar (lambda (pair)
                        (let* ((tag (car pair))
                               (q (concat "x:" tag))
                               (dir (cdr pair)))
                          (if (eq '+ dir)
                              q
                            (concat "(NOT " q ")"))))
                      (mu4e-tagging--query-flags-alist)))
           (predicates (append predicates-for-category
                               predicates-for-flags))
           (result (string-join (cons
                    (concat "(" query ")")
                    predicates)
                   " AND ")))
      (setq mu4e-tagging-query-rewrite-last-original-query
            initial-query)
      (setq mu4e-tagging-query-rewrite-last-rewritten-query
            result)
      ;; (message "Query rewritten to: %s" result)
      result)))


(defun mu4e-tagging--query-rewrite-mu4e (initial-query)
  "Rewrite INITIAL-QUERY via `mu4e-tagging--query-rewrite'.

Since re-running a mu4e query will re-run the query returned by
`mu4e-tagging--query-rewrite', we need to detect this situation
and instead rewrite the original, un-rewritten query
\(from `mu4e-tagging-query-rewrite-last-original-query') to avoid
generating increasingly complex queries with redundant filters."
  (if (eq initial-query
          mu4e-tagging-query-rewrite-last-rewritten-query)
      (mu4e-tagging--query-rewrite
         mu4e-tagging-query-rewrite-last-original-query)
    (mu4e-tagging--query-rewrite
       initial-query)))

(defun mu4e-tagging-query-submode-p ()
  "Check wither query submode is enabled."
  (interactive)
  mu4e-tagging-query-rewrite-function-backup)


(defun mu4e-tagging-query-submode-enable ()
  "Ensure that query submode is active.

Start automatically rewriting mu4e queries.
This updates `mu4e-tagging-query-rewrite-function-backup'
and `mu4e-query-rewrite-function'."
  (interactive)
  (when (not (mu4e-tagging-query-submode-p))
    ;; removes query-rewrite function if enabled
    (mu4e-tagging-query-submode-reset)
    ;; so now we should definitely be able to set it
    (setq mu4e-tagging-query-rewrite-function-backup
          mu4e-query-rewrite-function)
    (setq mu4e-query-rewrite-function
          #'mu4e-tagging--query-rewrite-mu4e)))

(defun mu4e-tagging-query-submode-disable ()
  "Ensure that query submode is not active.

This will stop rewrites to mu4e queries."
  (interactive)
  ;; removes the query-rewrite function
  (when (mu4e-tagging-query-submode-p)
    (mu4e-tagging-query-submode-reset)))

(defun mu4e-tagging--query-submode-auto-disable (&rest _)
  "Disable query-submode if appropriate.

Checks if either `mu4e-tagging-mode' itself has been disabled or
the query hook is disabled and either re-activates
`mu4e-tagging-mode' (if `mu4e' switched buffers) or deactivates
query rewriting (if `mu4e-tagging-mode' is disabled or we are
outside of `mu4e-headers-mode')."
  (interactive)
  (unless (and mu4e-tagging-mode
               mu4e-tagging-query-rewrite-function-backup)
    (if (and (eq major-mode 'mu4e-headers-mode)
             (not mu4e-tagging-mode))
        ;; We got disabled by accident?
        (progn
          (mu4e-tagging t))
      ;; We really aren't needed any more
      (mu4e-tagging-query-submode-disable))))

(defun mu4e-tagging-query-submode-reset (&rest _)
  "Reset filters in query-submode, uninstall mu4e-query rewriter."
  (interactive)
  (when mu4e-tagging-query-rewrite-function-backup
    (setq mu4e-query-rewrite-function
          mu4e-tagging-query-rewrite-function-backup)
    (setq mu4e-tagging-query-rewrite-function-backup nil))
  (clrhash mu4e-tagging-query-flags)
  (setq mu4e-tagging-query-category nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query submode M-x UI

(defvar mu4e-tagging-query-separator ","
  "String that separates lists of items.
Must match the regex in `crm-separator`.")

(setq mu4e-tagging-query-category-history nil)
(setq mu4e-tagging-query-tags-history nil)

(defun mu4e-tagging--query-current-category-string ()
  "String representation of current query filter category.

Derived from variable `mu4e-tagging-query-category', as parsed by
`mu4e-tagging--query-category-parse'."
  (cond
   ((null mu4e-tagging-query-category)
    "")
   ((eq 'uncategorized mu4e-tagging-query-category)
    "-")
   (t
    mu4e-tagging-query-category)))

(defun mu4e-tagging--query-category-parse (name)
  "Parse query category string NAME.

If NAME is \\=\"\\=\", interpret as nil (no category filter).
If NAME is \\=\"-\\=\", parse as 'uncategorized (filter for
uncategorized mails only).
Otherwise NAME is the tag name of the category to filter for."
  (cond
   ((equal "" name)
    nil)
   ((equal "-" name)
    'uncategorized)
   (t
    name)))

(defun mu4e-tagging--query-current-flags-string ()
  "String representation of all query filter flags.

Derived from variable `mu4e-tagging-query-flags', separated by
`mu4e-tagging-query-separator'."
  (mapconcat
   (lambda (pair)
     (let ((tag (car pair))
           (dir (cdr pair)))
       (concat (symbol-name dir) tag)))
   (mu4e-tagging--query-flags-alist)
   mu4e-tagging-query-separator))

(defun mu4e-tagging--query-flag-parse (pmtag)
  "Parse PMTAG as single flag preceded by \"+\" or \"-\" into a pair.

For a specification of the form \"+foo\" or \"-foo\", yield
pair (\"foo\" . '+) or (\"foo\" . '-), suitable for alist
storage, or nil on error."
  (if (< (length pmtag) 2)
      nil
    (let ((dir-string (substring pmtag 0 1))
          (tag (substring pmtag 1)))
      (if (or
           (equal "-" dir-string)
           (equal "+" dir-string))
          ;; well-formed:
          (cons tag (intern dir-string))))))


(defun mu4e-tagging--query-flags-parse (pmtags)
  "Parse list or string of flags preceded by \"+\" / \"-\" into alist.

If PMTAGS is a string, the markers must be separated by
`mu4e-tagging-query-separator'.
The result is an alist of tags and + and - symbols, as returned
by `mu4e-tagging--query-flags-alist' (but not necessarily sorted)."
  (-filter #'identity ; remove any nil
           (mapcar #'mu4e-tagging--query-flag-parse
                   (if (listp pmtags)
                       pmtags
                     (split-string pmtags
                                   mu4e-tagging-query-separator)))))


(defun mu4e-tagging-query-category (category)
  "Select the tag CATEGORY to filter for query-submode.

Enable query-submode, if needed."
  (interactive
   (list
    (completing-read "Tag category (\"-\" for uncategorised): "
                     (cons "" (cons "-" (mu4e-tagging-categories-get)))
                     nil
                     t
                     (mu4e-tagging--query-current-category-string)
                     'mu4e-tagging-query-category-history)))

  (mu4e-tagging-query-submode-enable)
  (setq mu4e-tagging-query-category
        (mu4e-tagging--query-category-parse category)))

(defun mu4e-tagging-query-flags (tags)
  "Select the TAGS to filter for query-submode.

Enable query-submode, if needed.
If TAGS is a string, the markers must be separated by
`mu4e-tagging-query-separator'."
  (interactive
   (list
    (completing-read-multiple "Flags to require / disallow: "
                              (let ((flags
                                     (mu4e-tagging--flags-get-alist)))
                                (append (mapcar (lambda (flag)
                                                  (concat "+"
                                                          (car flag)))
                                                flags)
                                        (mapcar (lambda (flag)
                                                  (concat "-"
                                                          (car flag)))
                                                flags)))
                              nil
                              nil
                              (mu4e-tagging--query-current-flags-string)
                              'mu4e-tagging-query-flags-history)))
  (mu4e-tagging-query-submode-enable)
  (clrhash mu4e-tagging-query-flags)
  (dolist (binding (mu4e-tagging--query-flags-parse tags))
    (puthash (car binding) (cdr binding) mu4e-tagging-query-flags)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query submode quick UI

(defun mu4e-tagging-query-rerun ()
  "Called after changing the query filter configuration.
Currently hardwired to run `mu4e-search-retun`."
  ;; (message "filtering: [%s] %s"
  ;;            (mu4e-tagging--query-current-flags-string)
  ;;            (mu4e-tagging--query-current-category-string))
  (mu4e-search-rerun))

(defun mu4e-tagging-query-clear ()
  "Clears the category filter, i.e., stops filtering by category."
  (interactive)
  (mu4e-tagging-query-category "")
  (clrhash mu4e-tagging-query-flags)
  (mu4e-tagging-query-rerun))

(defun mu4e-tagging-query-require-uncategorized ()
  "Set the category filter to only show uncategorized mails."
  (interactive)
  (mu4e-tagging-query-category "-")
  (mu4e-tagging-query-rerun))

(defun mu4e-tagging--interceptor-query-require ()
  "Set category filter to show category or to require flag.

Decodes the category or flag by looking at the most recent
command.  For flags, if the flag is currently blocked, it will be
ignored instead of being required."
  (interactive)
  (-let* (((tag-name . taginfo)
             (gethash (this-command-keys-vector)
                      mu4e-tagging--key-action-table))
          (is-flag (plist-get taginfo :flag)))
    (if (not is-flag)
        ;; category?
        (mu4e-tagging-query-category tag-name)
      ;; flag?
      (progn
        (mu4e-tagging-query-submode-enable)
        (let* ((last-bind (gethash tag-name
                                   mu4e-tagging-query-flags)))
          (if (eq '- last-bind)
              ;; From -flag to ignoring the flag
              (remhash tag-name mu4e-tagging-query-flags)
            ;; otherwise +flag
            (puthash tag-name '+ mu4e-tagging-query-flags))))))
  (mu4e-tagging-query-rerun))

(defun mu4e-tagging--interceptor-query-block ()
  "Set category filter to stop filtering categories, or block flag.

Decodes the category or flag by looking at the most recent
command.  For flags, if the flag is currently required, it will
be ignored instead of being blocked."
  (interactive)
  (-let* (((tag-name . taginfo)
             (gethash (this-command-keys-vector)
                      mu4e-tagging--key-action-table))
          (is-flag (plist-get taginfo :flag)))
    (if (not is-flag)
        ;; category?
        (mu4e-tagging-query-category "")
      ;; flag?
      (progn
        (mu4e-tagging-query-submode-enable)
        (let* ((last-bind (gethash tag-name
                                   mu4e-tagging-query-flags)))
          (if (eq '+ last-bind)
              ;; From +flag to ignoring the flag
              (remhash tag-name mu4e-tagging-query-flags)
            ;; otherwise -flag
            (puthash tag-name '- mu4e-tagging-query-flags))))))
  (mu4e-tagging-query-rerun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mail-info buffer

(defun mu4e-tagging-update-mail-info-buf ()
  "Update the tagging-related mail-info buffer displayed to the user."
  (let ((buf (mu4e-tagging-mail-info-buf)))
    (when buf
      (let* ((msg (mu4e-message-at-point))
             (path (when msg
                     (mu4e-message-readable-path))))
        (save-excursion
          (with-current-buffer buf
            (read-only-mode)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (if msg
                  (progn
                    (insert "Mailinfo\n")
                    (insert (format "%s" msg))
                    (insert-file-contents path))
                ;; if msg is NIL:
                (insert "Nomail")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tag-info buffer

(defface mu4e-tagging-separator
  '((t (:inherit shadow)))
  "Low-intensity separator characters for `mu4e-tagging-mode`."
  :group 'mu4e-tagging)

(defface mu4e-tagging-filter-disabled
  '((t (:inherit widget-inactive)))
  "Indicates that a filter is disabled in `mu4e-tagging-mode`."
  :group 'mu4e-tagging)

(defface mu4e-tagging-filter-require
  '((((class color) (background light))
     :background "dark green"
     :foreground "greenyellow"
     :weight ultra-bold)
    (((class color) (background dark))
     :background "greenyellow"
     :foreground "black"
     :weight ultra-bold)
    (t :weight ultra-bold))
  "Indicates that `mu4e-tagging-mode' is filtering to require a tag."
  :group 'mu4e-tagging)

(defface mu4e-tagging-filter-block
  '((((class color) (background light))
     :background "LightSalmon3"
     :foreground "yellow"
     :weight ultra-bold)
    (((class color) (background dark))
     :background "LightSalmon1"
     :foreground "black"
     :weight ultra-bold)
    (t :weight ultra-bold :inverse-video t))
  "Indicates that `mu4e-tagging-mode' is filtering out a tag."
  :group 'mu4e-tagging)

(define-derived-mode mu4e-tagging-tags-info-mode
  tabulated-list-mode
  "mu4e-tag info"
  "Major mode to support `mu4e-tagging-mode'.

Used to display active tags and key bindings.")

(defun mu4e-tagging--show-keybind (keybind)
  "Visualises KEYBIND, which must satisfy `key-valid-p'.

The result is a propertized string, using the faces for
`help-key-binding' and `mu4e-tagging-separator'."
  (concat
   (propertize "[" 'face 'mu4e-tagging-separator)
   (propertize (key-description (mu4e-tagging-keyvec keybind))
               'face 'help-key-binding)
   (propertize "]" 'face 'mu4e-tagging-separator)))

(defun mu4e-tagging--column-widths (rows num-columns)
  "Computes the maximum widths for a table of strings.

ROWS must be a list of lists that each represent one row.  The
rows in turn must be a list of strings, where each string
represents data to be shown in that specific column of the row.
No row may contain more than NUM-COLUMNS strings.

Returns a vector of length num-columns with the largest string
length observed for each column."
  (let ((widths (make-vector num-columns 0)))
    (dolist (row rows)
      (seq-do-indexed (lambda (elt index)
                        (aset widths index
                              (max (length elt)
                                   (aref widths index))))
                      row))
    widths))

(defun mu4e-tagging--list-tag-info (taginfo)
  "For a TAGINFO of (NAME . PLIST), compute tag into table contents.

The result is a list of five strings for the tag info table:
1. Key binding
2. Flag or not
3. Short tag name
4. Current filter status
5. Long tag name."
  (-let* (((tag-name . tag-pinfo) taginfo)
          (keybind (plist-get tag-pinfo :key))
          (is-flag (plist-get tag-pinfo :flag))
          ;(tagname (plist-get tag-pinfo :tag))
          (short-tagstring (mu4e-tagging-propertized-name tag-name t))
          ;;(tagstring (mu4e-tagging-propertized-name tag-name nil))
          (filter-status (if is-flag
                             (mu4e-tagging--propertized-info
                              (gethash
                                 tag-name mu4e-tagging-query-flags))
                           ;; categorical
                           (mu4e-tagging--propertized-info
                            (eq mu4e-tagging-query-category
                                tag-name)))))
    (list
     (mu4e-tagging--show-keybind keybind)
     (if is-flag "f" "")
     short-tagstring
     filter-status
     tag-name)))

(defun mu4e-tagging--propertized-info (mode)
  "Translate tag MODE into propertized string.

MODE must be one of t, nil, '+, or '-.  THe resultant string is a
human-readable representation of whether the tag is to describe
whether the given tag is required (t or '+), blocked ('-), or
ignored (nil) by the filter."
  (let ((string (cond ((eq mode t)
                       "required")
                      ((eq mode '+)
                       "required")
                      ((eq mode '-)
                       "blocked")
                      (t
                       "shown"))))
    (propertize string 'face
                (cond ((eq mode t)
                       'mu4e-tagging-filter-require)
                      ((eq mode '+)
                       'mu4e-tagging-filter-require)
                      ((eq mode '-)
                       'mu4e-tagging-filter-block)
                      (t
                       'mu4e-tagging-filter-disabled)))))

(defun mu4e-tagging-update-tag-info-buf ()
  "Update the tagging-related tag-info buffer displayed to the user."
  (let ((buf (mu4e-tagging-tag-info-buf)))
    (when buf
      (let ((msg (mu4e-message-at-point)))
        (save-excursion
          (with-current-buffer buf
            (read-only-mode)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (mu4e-tagging-tags-info-mode)
              (let* ((table-data-main
                      (mapcar #'mu4e-tagging--list-tag-info
                              (append
                               (mu4e-tagging-categories-get)
                               (mu4e-tagging-flags-get))))
                     ;; prepend "uncategorised" row
                     (table-data
                        (cons
                         (list
                          (mu4e-tagging--show-keybind
                             mu4e-tagging-uncategorized-suffix)
                          ""
                          ""
                          (mu4e-tagging--propertized-info
                             (eq mu4e-tagging-query-category
                                 'uncategorized))
                          "(uncategorised)")
                         table-data-main))
                     (column-widths (mu4e-tagging--column-widths
                                     table-data 5))
                     (query-keybind (mu4e-tagging--show-keybind
                                     mu4e-tagging-query-prefix)))
                (setq tabulated-list-format
                      (vector
                       `("Key"          ,(max 3 (aref column-widths
                                                      0))
                                        t)
                       `("Flag"         ,(max 4 (aref column-widths
                                                      1))
                                        t)
                       `("Tag"          ,(max 3 (aref column-widths 2))
                                        t
                                        :right-align t)
                       `(,query-keybind ,(max 8 (length query-keybind)
                                              (aref column-widths 3))
                                        t
                                        :right-align t)
                       `(""             1
                                        t)))
                (tabulated-list-init-header)
                (setq tabulated-list-entries
                      ;; expects list of:
                      ;;   (nil [key  flag  tag  keybind  info])
                      (mapcar (lambda (row)
                                (list nil (vconcat row)))
                              table-data))
                (tabulated-list-print)
                ;; alternative approach with vtable:
                ;;             (make-vtable
                ;;              :columns '(:name "Key" :align left)
                ;;              :objects rows
                ;;              ;:face ...
                ;;             )
                )
              ;; resize window
              (when mu4e-tagging-tag-info-window
                (fit-window-to-buffer mu4e-tagging-tag-info-window))
              ;; hide cursor
              (setq cursor-type nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key handling

;; We use two keymaps:
;; - `mu4e-tagging-mode-map'
;; - `mu4e-tagging--dyn-keymap' for keybindings generated from
;;   user customisation in `mu4e-tagging-tags'.

;; `mu4e-tagging-mode-map' overrides `mu4e-tagging--dyn-keymap'
;; to ensure that users don't accidentally shoot themselves
;; in the foot.  The latter  handles dynamically generated
;; key bindings.

;; `mu4e-tagging--dyn-keys' always maps to
;; `mu4e-tagging--key-dispatch'.  The actual action to perform is
;; stored in `mu4e-tagging--key-action-table'.
(defvar-keymap mu4e-tagging--dyn-keymap
  :doc "Auxiliary keymap for mu4e-tagging-minor mode:
this map is automatically set up to manage tagging and query
commands, but is overriden by `mu4e-tagging-mode-map'.")

(defvar-keymap mu4e-tagging-mode-map
  :doc "Primary keymap for mu4e-tagging-minor mode."
  :parent mu4e-tagging--dyn-keymap)

(defun mu4e-tagging-clear-tagging-dyn-keymap ()
  "Remove all key bindings from `mu4e-tagging--dyn-keymap'."
  (setcdr mu4e-tagging--dyn-keymap (cdr (make-sparse-keymap))))

(setq mu4e-tagging--key-action-table
      (make-hash-table :test 'equal))

;; An extension of of `mu4e-tagging-tags` with defaults generated
;; for missing fields.
(setq mu4e-tagging-flags-var nil)

(defun mu4e-tagging-categories-get ()
  "Return alist of all category tags.

The alist values are a plists that is guaranteed to contain
:short and :key, and may contain the optional properties."
  mu4e-tagging-categories-var)

(defun mu4e-tagging-flags-get ()
  "Retur an alist of all flag tags.

The value is a plist that is guaranteed to contain :short and
:key, and may contain the optional properties."
  mu4e-tagging-flags-var)

(defun mu4e-tagging--interceptor-tag ()
  "Handle callbacks for tagging from keymap.

Uses the most recent key sequence to identify the requested tag."
  (interactive)
  (-let* (((tag-name . taginfo) (gethash (this-command-keys-vector)
                                         mu4e-tagging--key-action-table))
          (is-flag (plist-get taginfo :flag))
          (msg (mu4e-message-at-point))
          (tag-add (concat "+" tag-name))
          (tag-remove (if (not is-flag)
                          (mapconcat (lambda (n) (concat "-" n))
                                     (remove tag-name
                                             (mu4e-tagging-known-category-tags))
                                     ",")
                        ;; no tags to remove for tag-add
                        ""))
          (tags-update  (concat
                         tag-add
                         (if (eq "" tag-remove)
                             ""
                           ",")
                         tag-remove)))
    (mu4e-action-retag-message msg tags-update)))


(defun mu4e-tagging--interceptor-untag ()
  "Handle callbacks for untagging from keymap.

Uses the most recent key sequence to identify the requested tag."
  (interactive)
  (-let* (((tag-name . taginfo)
             (gethash (this-command-keys-vector)
                      mu4e-tagging--key-action-table))
          (msg (mu4e-message-at-point)))
    (mu4e-action-retag-message msg (concat "-" tag-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration interface

(defcustom
  mu4e-tagging-untag-prefix
  "-"
  "Key or key combination to use as prefix for removing a tag."
  :type 'key
  :group 'mu4e-tagging)

(defcustom
  mu4e-tagging-query-prefix
  "C-w"
  "Key or key combination to use as prefix for tag search."
  :type 'key
  :group 'mu4e-tagging)

(defcustom
  mu4e-tagging-uncategorized-suffix
  "C-d"
  "Key to decategorise, and to query for uncategorised mails."
  :type 'key
  :group 'mu4e-tagging)

(define-minor-mode mu4e-tagging
        "A minor mode to quickly retag mails.

To use:
1.  Key bindings:
    Run `mu4e-tagging-setup-default-bindings' to install the
    mode into `mu4e-headers' mode with the default key settings
    (or roll your own)
2.  Tag visualisation:
    Use :short-tags as one of your `mu4e-header-fields' (to get
    propertized/shortened tag displays) or use mu4e's existing
    :tags field if you don't care for this feature
3.  Customise `mu4e-tagging-tags' for the tags you want to manage
    in this mode."
  :lighter " tagging"
  :keymap mu4e-tagging-mode-map

  (progn
    (if mu4e-tagging
        (progn
          (mu4e-tagging-enable-handler)
          (message "mu4e-tagging enabled"))
      (progn
        (mu4e-tagging-disable-handler)
        (message "mu4e-tagging disabled")))))

(defun mu4e-tagging-setup-default-bindings ()
  "Set default key bindings for `mu4e-tagging-mode'.

The function also adds hooks to activate `mu4e-tagging-mode' from
`mu4e-headers-mode' and advice to automatically run
`mu4e-tagging-mail-at-point-changed' after `mu4e-headers-prev'
and `mu4e-headers-next'."
  (let ((map mu4e-tagging-mode-map))
    (define-key map (kbd "C-t") #'mu4e-tagging-disable)
    (define-key map (kbd "C-w C-w") #'mu4e-tagging-query-clear)
    (define-key map (kbd "C-w C-d") #'mu4e-tagging-query-require-uncategorized)
    (define-key map (kbd "ESC ESC ESC") #'mu4e-tagging))
  (add-hook 'mu4e-headers-mode-hook
            (lambda ()
              (local-set-key (kbd "C-t") 'mu4e-tagging)))
  (advice-add 'mu4e-headers-next
              :after
              #'mu4e-tagging-mail-at-point-changed)
  (advice-add 'mu4e-headers-prev
              :after
              #'mu4e-tagging-mail-at-point-changed))

(defun mu4e-tagging-update-customize-reset (symbol value)
  "Initial setup for the default values of `mu4e-tagging-tags'.

SYMBOL and VALUE are passed directly to `custom-initialize-reset'."
  (custom-initialize-reset symbol value)
  (mu4e-tagging-update-tags))

(defcustom
  mu4e-tagging-tags
  '((todo :key "+" :short "+" :flag t :foreground "yellow" :background "black")
    (spam :key "s" :short "SPAM")
    (regular-mail :key "m" :short "M"))

  "List of all tags examined by mu4e-tagging-mode.

Tags take the form of alists whose values are plists, of the
form (TAGNAME :short SHORTNAME :key KEY :flag FLAG :foreground FG
:background BG :weight WEIGHT :box BOX).  mu4e-tagging-mode will
use KEY to select this tag and SHORTNAME to display it in the
mu4e-headers view.  Any occurrences of the tag will be rendered
in the specified FG and BG colours.

Tags without :flag or with :flag nil are categorical tags,
meaning that they are mutually exclusive.  Tags with :flag t
are flags, meaning that any number of them can be 'added' to
any category."
  :type '(alist :tag "Blah"
                :value-type
                (plist :options ((:tag string)
                    (:short string)
                    (:key   key)
                    (:flag  boolean
                            :doc "Non-exclusive, can be toggled")
                    (:foreground (choice (const :tag "none" nil)
                                         (color :tag "Foreground")))
                    (:background (choice (const :tag "none" nil)
                                         (color :tag "Background")))
                    (:weight (choice (const :tag "none" nil)
                                     (const ultrathin)
                                     (const light)
                                     (const semilight)
                                     (const normal)
                                     (const medium)
                                     (const semibold)
                                     (const bold)
                                     (const ultra-bold)))
                    (:box (choice (const :tag "none" nil)
                                  (const :tag "simple" t)
                                  (color :tag "color")
                                  (plist :options
                                         ((:line-width
                                           (choice (integer :tag "width+height (positive: extrude, negative: intrude)")
                                                   (cons :tag "width . height")))
                                          (:color color)
                                          (:style (choice
                                                   (const line)
                                                   (const wave))))))))))
  :initialize 'mu4e-tagging-update-customize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (mu4e-tagging-update-tags))
  :group 'mu4e-tagging)

(mu4e-tagging-update-tags)
(mu4e-tagging--on-load)

(add-hook 'mu4e-tagging-mode-unload-hook
          #'mu4e-tagging--on-unload)

(provide 'mu4e-tagging)

;;; mu4e-tagging.el ends here
