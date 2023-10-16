;;; mu4e-tagging-mode.el --- minor mode for quick tagging in mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2023 Christoph Reichenbach

;; Author: Christoph Reichenbach <creichen@creichen.net>
;; Maintainer: Christoph Reichenbach <creichen@creichen.net>
;; Keywords: email

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Terminology:
;; - tags: all the things managed by mu4e-tagging-mode, comprising
;;   - flags: toggles that are independent of each other
;;   - categories: selections that are mutually exclusive.
;;
;; Mails may violate the "categories" restriction and/or include unknown tags;
;; we handle these cases gracefully.
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
(setq creichen/mu4e-tagging--supports-vtable
      (require 'vtable nil 'optional))

(provide 'mu4e-tagging-mode)

(defgroup creichen/mu4e-tagging
  nil
  "Key bindings and faces for `creichen/mu4e-tagging-mode`.")

(defvar-keymap creichen/mu4e-tagging-minor-mode-auto-keymap
  :doc "Auxiliary keymap for creichen/mu4e-tagging-minor mode:
this map is automatically set up to manage tagging
commands, but is overriden by
creichen/mu4e-tagging-minor-mode-keymap."
  )

(defvar-keymap creichen/mu4e-tagging-minor-mode-keymap
  :doc "Primary keymap for creichen/mu4e-tagging-minor mode."
  :parent creichen/mu4e-tagging-minor-mode-auto-keymap
  )

(defun creichen/mu4e-tagging-minor-mode-clear-tagging-auto-keymap ()
  "Removes all key bindings from `creichen/mu4e-tagging-minor-mode-auto-keymap`"
  (setcdr creichen/mu4e-tagging-minor-mode-auto-keymap (cdr (make-sparse-keymap)))
  )


(defvar creichen/mu4e-tagging-known-tags
  (make-hash-table :test 'equal)
  "All tags managed by creichen/mu4e-tagging-mode.  Automatically constructed from
creichen/mu4e-tagging-tags.  Maps to plist with tag properties.")

(setq creichen/mu4e-tagging-reverse-key-table-tag
      (make-hash-table :test 'equal))
(setq creichen/mu4e-tagging-reverse-key-table-untag
      (make-hash-table :test 'equal))

(setq creichen/mu4e-tagging-categories-var nil)
(setq creichen/mu4e-tagging-flags-var nil)

(defun creichen/mu4e-tagging-categories-get ()
  "Returns an alist of all category tags.
The value is a plist that is guaranteed to contain :short and :key, and may contain the optional properties."
  creichen/mu4e-tagging-categories-var
  )

(defun creichen/mu4e-tagging-flags-get ()
  "Returns an alist of all flag tags.
The value is a plist that is guaranteed to contain :short and :key, and may contain the optional properties."
  creichen/mu4e-tagging-flags-var
  )

(defun creichen/mu4e-tagging-interceptor-tag ()
  "Handles callbacks for tagging and uses the most recent key sequence to identify the requested tag."
  (interactive)
  (-let* (((tag-name . taginfo) (gethash (this-command-keys-vector) creichen/mu4e-tagging-reverse-key-table-tag))
	  (is-flag (plist-get taginfo :flag))
	  (msg (mu4e-message-at-point))
	  (tag-add (concat "+" tag-name))
	  (tag-remove (if (not is-flag)
			  (mapconcat (lambda (n) (concat "-" n))
				     (remove tag-name (creichen/mu4e-tagging-known-category-tags))
				     ",")
			;; no tags to remove for tag-add
			"")
		      )
	  (tags-update  (concat
			 tag-add
			 (if (eq "" tag-remove)
			     ""
			   ",")
			 tag-remove
			 ))
	  )
    (mu4e-action-retag-message msg tags-update)
    )
  )

(defun creichen/mu4e-tagging-interceptor-untag ()
  "Handles callbacks for untagging and uses the most recent key sequence to identify the requested tag."
  (interactive)
  (-let* (((tag-name . taginfo) (gethash (this-command-keys-vector) creichen/mu4e-tagging-reverse-key-table-untag))
	  (msg (mu4e-message-at-point)))
    (mu4e-action-retag-message msg (concat "-" tag-name))
    )
  )

(defun creichen/mu4e-tagging-decategorize ()
  "Removes any category tags for the message at point."
  (interactive)
  (let ((tags-remove (mapconcat (lambda (n) (concat "-" n))
				(creichen/mu4e-tagging-known-category-tags)
			       ","))
	(msg (mu4e-message-at-point))
	)
    (mu4e-action-retag-message msg tags-remove)
    )
  )

(defun creichen/mu4e-tagging-keystring (key)
  "Translates a key binding (string or character or key vector) into a string that satisfies `key-valid-p`"
  (cond ((key-valid-p key)
	 key)
	((vectorp key)
	 (key-description key))
	((characterp key)
	 (key-description (vector key)))
	(t
	 (key-description (key-parse key)))
	)
  )

(defun creichen/mu4e-tagging-keyvec (key)
  "Translates a key binding (string or character or key vector) into a key vector"
  (let* ((key-vec-or-string key)
	 (key-tag (cond ((vectorp key-vec-or-string)
			 key-vec-or-string)
			((characterp key-vec-or-string)
			 (vector key-vec-or-string))
			(t
			 (key-parse key-vec-or-string)))))
    (if (eq 0 (length key-tag))
	(progn
	  (warn "creichen/mu4e-tagging: empty key binding (%s); misconfigured?" key-vec-or-string)
	  key-tag
	  )
	;; else all is well
      key-tag
    ))
  )

(defun creichen/mu4e-tagging-alloc-keys (tag-name tag-pbody)
  "Allocates and binds keys for tag-name, ensuring uniqueness.
Returns tag-pbody with :key possibly updated.  Throws
'keybind-impossible if
(1) :key in tag-pbody is already bound,
(2) :key is not in tag-pbody and the function couldn't guess an
unused binding, or
 (3) :key is somehow invalid."
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
					     (plist-get tag-pbody :short)
					     "1234567890")
			when (not (keymap-lookup creichen/mu4e-tagging-minor-mode-auto-keymap (kbd (string c))))
			return (key-description (vector c)))))
	 (prefix-key-vec (creichen/mu4e-tagging-keyvec  creichen/mu4e-tagging-untag-prefix))
	 (query-key-vec (creichen/mu4e-tagging-keyvec  creichen/mu4e-tagging-query-prefix))
	 )
    (when (or
	   (null key)
	   (equal (string-to-char key)
		  creichen/mu4e-tagging-untag-prefix)
	   (keymap-lookup creichen/mu4e-tagging-minor-mode-keymap (kbd key)))
      (throw 'keybind-impossible (list tag-name :key requested-key))
      )
    ;; Otherwise the keybind is available
    (let* ((key-tag (creichen/mu4e-tagging-keyvec key))
	   (key-untag (vconcat prefix-key-vec key-tag))
	   ;; keys for querying
	   (key-query-require (vconcat query-key-vec key-tag))
	   (key-query-block (vconcat query-key-vec prefix-key-vec key-tag))
	   ;; update :key accordingly
	   (tag-pbody-new (plist-put tag-pbody :key (creichen/mu4e-tagging-keystring key-tag)))
	   (taginfo (cons tag-name tag-pbody-new))
	   )
      ;; Bind the key to call the generic interceptor functions
      (define-key creichen/mu4e-tagging-minor-mode-auto-keymap
		  key-tag #'creichen/mu4e-tagging-interceptor-tag)
      (define-key creichen/mu4e-tagging-minor-mode-auto-keymap
		  key-untag #'creichen/mu4e-tagging-interceptor-untag)
      (define-key creichen/mu4e-tagging-minor-mode-auto-keymap
		  key-query-require #'creichen/mu4e-tagging-interceptor-query-require)
      (define-key creichen/mu4e-tagging-minor-mode-auto-keymap
		  key-query-block #'creichen/mu4e-tagging-interceptor-query-block)

      ;; Set up reverse lookup keymaps so the interceptors can figure out why they were called
      (puthash key-tag taginfo
	       creichen/mu4e-tagging-reverse-key-table-tag)
      (puthash key-untag taginfo
	       creichen/mu4e-tagging-reverse-key-table-untag)
      (puthash key-query-require taginfo
	       creichen/mu4e-tagging-reverse-key-table-tag)
      (puthash key-query-block taginfo
	       creichen/mu4e-tagging-reverse-key-table-tag)
      ;; Return updated tag-body plist
      tag-pbody-new
      )
    )
  )

(defun creichen/mu4e-tagging-alloc-default-keys ()
  "Installs default dynamic key bindings"
  (define-key creichen/mu4e-tagging-minor-mode-auto-keymap
	      (creichen/mu4e-tagging-keyvec creichen/mu4e-tagging-uncategorized-suffix)
	      'creichen/mu4e-tagging-decategorize)
  )

(defun creichen/mu4e-tagging-update-tags ()
  "Extracts all tag information from the user specification and plugs in defaults as needed"
  (clrhash creichen/mu4e-tagging-reverse-key-table-tag)
  (clrhash creichen/mu4e-tagging-reverse-key-table-untag)
  (clrhash creichen/mu4e-tagging-known-tags)
  ;(setq creichen/mu4e-tagging-known-category-tags nil)
  (creichen/mu4e-tagging-minor-mode-clear-tagging-auto-keymap)
  (let ((prefix-key-vec (if (vectorp creichen/mu4e-tagging-untag-prefix)
			    creichen/mu4e-tagging-untag-prefix
			  (vector creichen/mu4e-tagging-untag-prefix)))
	(tag-aspecs-list (if (boundp 'creichen/mu4e-tagging-tags)
			      creichen/mu4e-tagging-tags
			    nil))
	(tag-infos-categories nil)
	(tag-infos-flags nil)
	)
    (dolist (tag-aspec tag-aspecs-list)
      (-let* (((tag-name . tag-pbody-orig) tag-aspec)
	      (tag-short-orig (plist-get tag-pbody-orig :short))
	      (tag-short (if tag-short-orig
			     tag-short-orig
			   tag-name))
	      (tag-pbody-pre (plist-put tag-pbody-orig :short tag-short))
	      ;; alloc-keys ensures that tag-pbody contains the correct :key value
	      (tag-pbody (creichen/mu4e-tagging-alloc-keys tag-name
							   tag-pbody-pre))
	      (is-flag (plist-get tag-pbody :flag))
	      (tag-info (cons tag-name tag-pbody)))
	(puthash tag-name tag-pbody creichen/mu4e-tagging-known-tags)
	(if is-flag
	    ; flag:
	    (push tag-info tag-infos-flags)
	  ; category:
	  (progn
	    ;(push tag-name creichen/mu4e-tagging-known-category-tags)
	    (push tag-info tag-infos-categories)
	    ))
	)
      )
    ;; Add the remaining dynamic key bindings
    (creichen/mu4e-tagging-alloc-default-keys)
    ;; Store the results
    (setq creichen/mu4e-tagging-categories-var (reverse tag-infos-categories))
    (setq creichen/mu4e-tagging-flags-var (reverse tag-infos-flags))
  ))


(defun creichen/mu4e-tagging-known-category-tags ()
  "All tags that are category tags (list of strings)."
  (mapcar 'car creichen/mu4e-tagging-categories-var)
  )

(setq creichen/mu4e-tagging-tag-info-window nil)
(setq creichen/mu4e-tagging-mail-info-window nil)

(setq creichen/mu4e-tagging-tag-info-window-buf nil)
(setq creichen/mu4e-tagging-mail-info-window-buf nil)

(defun creichen/mu4e-tagging-mail-info-buf ()
  "Returns the buffer used for showing mail information.
Shows the buffer in the designated window, if it still exists."
  (when creichen/mu4e-tagging-mail-info-window
    (let ((buf (get-buffer-create " *mu4e-tagging-mail-info*")))
      (unless
	  creichen/mu4e-tagging-mail-info-window-buf
	(set-window-buffer creichen/mu4e-tagging-mail-info-window buf t)
	(setq creichen/mu4e-tagging-mail-info-window-buf buf)
	)
      buf))
  )


(defun creichen/mu4e-tagging-tag-info-buf ()
  "Returns the buffer used for showing tag information.
Shows the buffer in the designated window, if it still exists."
  (when creichen/mu4e-tagging-tag-info-window
    (let ((buf (get-buffer-create " *mu4e-tagging-tag-info*")))
      (unless
	  creichen/mu4e-tagging-tag-info-window-buf
	(set-window-buffer creichen/mu4e-tagging-tag-info-window buf t)
	(setq creichen/mu4e-tagging-tag-info-window-buf buf)
	)
      buf))
  )

(defun creichen/mu4e-tagging-minor-mode-enable-handler ()
  "Handles minor mode activation: creates info windows and adds hooks to mu4e."
  (let ((tag-info-window
	 (if (window-live-p creichen/mu4e-tagging-tag-info-window)
	     creichen/mu4e-tagging-tag-info-window
	   (split-window-below (- -2 (hash-table-count creichen/mu4e-tagging-known-tags)))))
        (mail-info-window
	 (if (window-live-p creichen/mu4e-tagging-mail-info-window)
	     creichen/mu4e-tagging-mail-info-window
	   (split-window-right -80)))
	)
    (setq creichen/mu4e-tagging-tag-info-window tag-info-window)
    (setq creichen/mu4e-tagging-mail-info-window mail-info-window)
    (creichen/mu4e-tagging-mail-info-buf)
    (creichen/mu4e-tagging-tag-info-buf)
    (creichen/mu4e-tagging-mail-at-point-changed)
    (add-hook 'mu4e-search-hook #'creichen/mu4e-tagging--query-submode-auto-disable)
    )
  )

(defun creichen/mu4e-tagging-minor-mode-disable-handler ()
  "Handles minor mode deactivation: closes info windows and removes hooks from mu4e."
  (remove-hook 'mu4e-search-hook #'creichen/mu4e-tagging--query-submode-auto-disable)
  (creichen/mu4e-tagging-query-submode-disable)
  (unwind-protect
      (when (eq creichen/mu4e-tagging-tag-info-window-buf
	      (window-buffer creichen/mu4e-tagging-tag-info-window))
	  (delete-window creichen/mu4e-tagging-tag-info-window)))
  (unwind-protect
      (when (eq creichen/mu4e-tagging-mail-info-window-buf
	      (window-buffer creichen/mu4e-tagging-mail-info-window))
	  (delete-window creichen/mu4e-tagging-mail-info-window)))
  (setq creichen/mu4e-tagging-tag-info-window nil)
  (setq creichen/mu4e-tagging-mail-info-window nil)
  (setq creichen/mu4e-tagging-tag-info-window-buf nil)
  (setq creichen/mu4e-tagging-mail-info-window-buf nil)
  )

(defun creichen/mu4e-tagging-minor-mode-disable ()
  "Ensure that creichen/mu4e-tagging-minor-mode is disabled."
  (interactive)
  (if creichen/mu4e-tagging-minor-mode
      ;; switch mode off, which automatically calls disbale-handler
      (creichen/mu4e-tagging-minor-mode 'toggle)
    ;; no need to switch mode off, but must still call disbale-handler
    (creichen/mu4e-tagging-minor-mode-disable-handler)
    )
  )

(defun creichen/mu4e-tagging-type (tagname)
  "Determines if tagname is a tag for a 'category, for a 'flag, or not a known tag (NIL)"
  (let ((plist (creichen/mu4e-tagging-info tagname)))
    (cond ((null plist)
	   nil)
	  ((plist-get plist :flag)
	   'flag)
	  (t
	   'category)
	  ))
  )

(defun creichen/mu4e-tagging-info (tagname)
  "Retrieves the tag plist information for a given tag."
  (gethash tagname creichen/mu4e-tagging-known-tags '())
  )

(defun creichen/mu4e-tagging-category-tag-p (tagname)
  "Checks if the given tag name designates a categorical tag."
  (eq 'category (creichen/mu4e-tagging-type tagname))
  )

(defun creichen/mu4e-tagging-flag-tag-p (tagname)
  "Checks if the given tag name designates a flag."
  (eq 'flag (creichen/mu4e-tagging-type tagname))
  )

(defun creichen/mu4e-tagging-no-tag-p (tagname)
  "Checks if the given tag name is not managed by `creichen/mu4e-tagging-mode`."
  (not (creichen/mu4e-tagging-type tagname))
  )

(defun creichen/mu4e-tagging-propertized-name (tag-name &rest short)
  "Retrieves the propertized tag string for the given tag name.  If SHORT is non-NIL, uses :short instead of :tag."
  (let* ((tinfo (creichen/mu4e-tagging-info tag-name))
	 (name (if (and short (car short))
		   (plist-get tinfo :short)
		 tag-name))
	 (fg (plist-get tinfo :foreground))
	 (bg (plist-get tinfo :background))
	 (box (plist-get tinfo :box))
	 (weight (plist-get tinfo :weight))
	 (face 		     (append
			      (if fg (list :foreground fg))
			      (if bg (list :background bg))
			      (cond ((null box) nil)
				    ((eq box t) '(:box (:line-width -1)))
				    (t (list :box box)))
			      (if weight (list :weigth weight))))
	 )
    (propertize name 'face (list face))
    )
  )

(add-to-list 'mu4e-header-info-custom
             '(:short-tags .
                           (:name "Short Tags"
				  :shortname "mTags" ; "milli-Tags"
				  :function creichen/mu4e-tagging-render-tags)))

(setq mu4e-headers-fields
      '((:human-date   .  10)
        (:flags        .  6)
        (:short-tags   .  10)
        (:mailing-list .  10)
        (:from         .  20)
        (:subject      .  nil)
	))

(defun creichen/mu4e-tagging-render-tags (msg)
  "Returns a propertized string that highlights the tags for the given message."
  (let* (
	 (tags (mu4e-message-field msg :tags))
	 (category-tags (-filter 'creichen/mu4e-tagging-category-tag-p tags))
	 (flag-tags (-filter 'creichen/mu4e-tagging-flag-tag-p tags))
	 (unknown-tags (-filter 'creichen/mu4e-tagging-no-tag-p tags))
	 (category-part (mapconcat (lambda (tag)
				     (creichen/mu4e-tagging-propertized-name tag t))
				   category-tags "_"))
	 (flag-part (mapconcat (lambda (tag)
				 (creichen/mu4e-tagging-propertized-name tag t))
			       flag-tags ""))
	 (unknown-part (mapconcat (lambda (tag)
				    tag)
				  unknown-tags ","))
	 (flag-and-unknown-part (concat flag-part (if unknown-tags
						      (concat " " unknown-part)
						    "")))
	 )
    (concat category-part
	    (if (and flag-tags (not (string-empty-p flag-and-unknown-part)))
		":"
	      "")
	    flag-and-unknown-part
	    )
    ))

(defun creichen/mu4e-tagging-mail-at-point-changed (&rest ignoreme)
  "Hook function that handles situations in which the mail at point changed and we need to update
the info buffers."
  (when (or creichen/mu4e-tagging-tag-info-window creichen/mu4e-tagging-mail-info-window)
    ;; only react if at least one of the creichen/mu4e-tagging info windows is open
    (creichen/mu4e-tagging-update-mail-info-buf)
    (creichen/mu4e-tagging-update-tag-info-buf)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query submode

;; alist (name . '+ or '-)
(setq creichen/mu4e-tagging-query-flags nil)
;; category tag (string) or 'uncategorized
(setq creichen/mu4e-tagging-query-category nil)
;; backup for  mu4e-query-rewrite-function
(setq creichen/mu4e-tagging-query-rewrite-function-backup nil)

(setq creichen/mu4e-tagging-query-rewrite-last-original-query nil)
(setq creichen/mu4e-tagging-query-rewrite-last-rewritten-query nil)

(defun creichen/mu4e-tagging--query-rewrite (initial-query)
  "Rewrites a mu4e query to account for mu4e-tagging filters.
Updates `creichen/mu4e-tagging-query-rewrite-last-original-query` with
INITIAL-QUERY and `creichen/mu4e-tagging-query-rewrite-last-rewritten-query`
with the function return value, cf. `creichen/mu4e-tagging--query-rewrite-mu4e`."
  (if (null creichen/mu4e-tagging-query-rewrite-function-backup)
      (progn
	;(message "creichen/mu4e-tagging--query-rewrite called outside of tagging-query submode")
	initial-query)
    ;; otherwise
    (let* ((query (if (eq #'creichen/mu4e-tagging--query-rewrite
			  creichen/mu4e-tagging-query-rewrite-function-backup) ;; might happen due to a bug or ill-timed package upgrade
		      initial-query
		    (apply creichen/mu4e-tagging-query-rewrite-function-backup (list initial-query))))
	   (cat-filter creichen/mu4e-tagging-query-category)
	   (predicates-for-category (cond
				     ((eq 'uncategorized cat-filter)
				      (mapcar (lambda (cat) (concat "(NOT x:" cat ")"))  (creichen/mu4e-tagging-known-category-tags)))
				     ((stringp cat-filter)
				      (list (concat "x:" cat-filter)))
				     (t
				      nil)))
	   (predicates-for-flags (mapcar (lambda (pair)
					   (let* ((tag (car pair))
						  (q (concat "x:" tag))
						  (dir (cdr pair))
						  )
					     (if (eq '+ dir)
						 q
					       (concat "(NOT " q ")"))
					     )
					   )
					 creichen/mu4e-tagging-query-flags))
	   (predicates (append predicates-for-category predicates-for-flags))
	   (result (string-join (cons
		    (concat "(" query ")")
		    predicates)
		   " AND "))

	   )
      (setq creichen/mu4e-tagging-query-rewrite-last-original-query initial-query)
      (setq creichen/mu4e-tagging-query-rewrite-last-rewritten-query result)
      ;; (message "Query rewritten to: %s" result)
      result
      ))
  )

(defun creichen/mu4e-tagging--query-rewrite-mu4e (initial-query)
  "Calls `creichen/mu4e-tagging--query-rewrite` as appropriate to rewrite the given query.
Since re-running a mu4e query will re-run the query returned by `creichen/mu4e-tagging--query-rewrite`,
we need to detect this situation and instead rewrite the original, un-rewritten query
(from `creichen/mu4e-tagging-query-rewrite-last-original-query`) to avoid generating
increasingly complex queries with redundant filters."
  ;; Ensure that we don't keep appending our rewrites to the same query
  (if (eq initial-query creichen/mu4e-tagging-query-rewrite-last-rewritten-query)
      (creichen/mu4e-tagging--query-rewrite creichen/mu4e-tagging-query-rewrite-last-original-query)
    (creichen/mu4e-tagging--query-rewrite initial-query)
    )
  )

(defun creichen/mu4e-tagging-query-submode-p ()
  "Checks wither query submode is enabled."
  (interactive)
  creichen/mu4e-tagging-query-rewrite-function-backup
  )

(defun creichen/mu4e-tagging-query-submode-enable ()
  "Enables query submode, if it is not already active.  This will automatically rewrite mu4e queries."
  (interactive)
  (when (not (creichen/mu4e-tagging-query-submode-p))
    ;; removes query-rewrite function if enabled
    (creichen/mu4e-tagging-query-submode-reset)
    ;; so now we should definitely be able to set it
    (setq creichen/mu4e-tagging-query-rewrite-function-backup mu4e-query-rewrite-function)
    (setq mu4e-query-rewrite-function #'creichen/mu4e-tagging--query-rewrite-mu4e)
    )
  )

(defun creichen/mu4e-tagging-query-submode-disable ()
  "Deactivates query submode, if it is active.  This will stop rewrites to mu4e queries."
  (interactive)
  ;; removes the query-rewrite function
  (when (creichen/mu4e-tagging-query-submode-p)
    (creichen/mu4e-tagging-query-submode-reset)
    )
  )

(defun creichen/mu4e-tagging--query-submode-auto-disable (&rest any)
  "Disables query-submode if either `creichen/mu4e-tagging-mode` itself has been
 disabled or the query hook processing hook is disabled."
  (interactive)
  (unless (and creichen/mu4e-tagging-minor-mode
	       creichen/mu4e-tagging-query-rewrite-function-backup)
    (if (and (eq major-mode 'mu4e-headers-mode)
	     (not creichen/mu4e-tagging-minor-mode))
	;; We got disabled by accident?
	(progn
	  (creichen/mu4e-tagging-minor-mode t)
	  )
      ;; We really aren't needed any more
      (creichen/mu4e-tagging-query-submode-disable)
      )
    )
  )

(defun creichen/mu4e-tagging-query-submode-reset (&rest any)
  "Resets all filters in query-submode and uninstalls the mu4e-query rewriter."
  (interactive)
  (when creichen/mu4e-tagging-query-rewrite-function-backup
    (setq mu4e-query-rewrite-function creichen/mu4e-tagging-query-rewrite-function-backup)
    (setq creichen/mu4e-tagging-query-rewrite-function-backup nil))
  (setq creichen/mu4e-tagging-query-flags nil)
  (setq creichen/mu4e-tagging-query-category nil)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query submode M-x UI

(defvar creichen/mu4e-tagging-query-separator ","
  "String that separates lists of items.  Must match the regex in `crm-separator`."
  )

(setq creichen/mu4e-tagging-query-category-history nil)
(setq creichen/mu4e-tagging-query-tags-history nil)

(defun creichen/mu4e-tagging--query-current-category-string ()
  "String representation of the currently selected category in
`creichen/mu4e-tagging-query-category`, as parsed by
`creichen/mu4e-tagging--query-category-parse`."
  (cond
   ((null creichen/mu4e-tagging-query-category)
    "")
   ((eq 'uncategorized creichen/mu4e-tagging-query-category)
    "-")
   (t
    creichen/mu4e-tagging-query-category)
  ))

(defun creichen/mu4e-tagging--query-category-parse (name)
  "Parses query category string into internal query category representation."
  (cond
   ((equal "" name)
    nil)
   ((equal "-" name)
    'uncategorized)
   (t
    name)))

(defun creichen/mu4e-tagging--query-current-flags-string ()
  "String representation of all currently selected flags in
`creichen/mu4e-tagging-query-flags`,
separated by `creichen/mu4e-tagging-query-separator`."
  (mapconcat
   (lambda (pair)
     (let ((tag (car pair))
	   (dir (cdr pair)))
       (concat (symbol-name dir) tag)))
   creichen/mu4e-tagging-query-flags
   creichen/mu4e-tagging-query-separator
   )
  )

(defun creichen/mu4e-tagging--query-flag-parse (pmtag)
  "Parses a single flag specification of the form \"+foo\" or \"-foo\" into
a pair (\"foo\" . '+) or (\"foo\" . '-), suitable for alist storage, or nil on error."
  (if (< (length pmtag) 2)
      nil
    (let ((dir-string (substring pmtag 0 1))
	  (tag (substring pmtag 1)))
      (if (or
	   (equal "-" dir-string)
	   (equal "+" dir-string))
	  ;; well-formed:
	  (cons tag (intern dir-string))
	)
      ))
  )

(defun creichen/mu4e-tagging--query-flags-parse (pmtags)
  "Parses either a list or a string of query flag markers (e.g., \"+tag\" or \"-tag\").
If PMTAGS is a string, the markers must be separated by
`creichen/mu4e-tagging-query-separator`.
The result is an alist of tags and + and - symbols, as used in
`creichen/mu4e-tagging-query-flags`."
  (-filter #'identity ; remove any nil
	   (mapcar #'creichen/mu4e-tagging--query-flag-parse
		   (if (listp pmtags)
		       pmtags
		     (split-string pmtags creichen/mu4e-tagging-query-separator))
		   )
	   )
  )


(defun creichen/mu4e-tagging-query-category (category)
  "Selects the category to filter for query-submode and enables query-submode, if needed."
  (interactive
   (list
    (completing-read "Tag category (\"-\" for uncategorised): " (cons "" (cons "-" (creichen/mu4e-tagging-categories-get)))
		     nil
		     t
		     (creichen/mu4e-tagging--query-current-category-string)
		     'creichen/mu4e-tagging-query-category-history)
    ))
  (creichen/mu4e-tagging-query-submode-enable)
  (setq creichen/mu4e-tagging-query-category
	(creichen/mu4e-tagging--query-category-parse category))
  )

(defun creichen/mu4e-tagging-query-flags (tags)
  "Selects the tags to filter for query-submode and enables query-submode, if needed.
The parameter follows the same format as `creichen/mu4e-tagging--query-flags-parse`."
  (interactive
   (list
    (completing-read-multiple "Flags to require / disallow: "
			      (let ((flags (creichen/mu4e-tagging-flags-get)))
				(append (mapcar (lambda (flag) (concat "+" (car flag))) flags)
					(mapcar (lambda (flag) (concat "-" (car flag))) flags)))
			      nil
			      nil
			      (creichen/mu4e-tagging--query-current-flags-string)
			      'creichen/mu4e-tagging-query-flags-history))
   )
  (creichen/mu4e-tagging-query-submode-enable)
  (setq creichen/mu4e-tagging-query-flags
	(creichen/mu4e-tagging--query-flags-parse tags))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query submode quick UI

(defun creichen/mu4e-tagging-query-rerun ()
  "Called after changing the query filter configuration.
Currently hardwired to run `mu4e-search-retun`."
  ;; (message "filtering: [%s] %s"
  ;; 	   (creichen/mu4e-tagging--query-current-flags-string)
  ;; 	   (creichen/mu4e-tagging--query-current-category-string))
  (mu4e-search-rerun)
  )

(defun creichen/mu4e-tagging-query-clear ()
  "Clears the category filter, i.e., stops filtering by category."
  (interactive)
  (creichen/mu4e-tagging-query-category "")
  (setq creichen/mu4e-tagging-query-flags nil)
  (creichen/mu4e-tagging-query-rerun)
  )

(defun creichen/mu4e-tagging-query-require-uncategorized ()
  "Sets the category filter to only show uncategorized mails."
  (interactive)
  (creichen/mu4e-tagging-query-category "-")
  (creichen/mu4e-tagging-query-rerun)
  )

(defun creichen/mu4e-tagging-interceptor-query-require ()
  "Sets the category filter to only show one specific category, or to require a given flag.
Decodes the category or flag by looking at the most recent command.
For flags, if the flag is currently blocked, it will be ignored instead of being required."
  (interactive)
  (-let* (((tag-name . taginfo) (gethash (this-command-keys-vector) creichen/mu4e-tagging-reverse-key-table-tag))
	  (is-flag (plist-get taginfo :flag))
	  )
    (if (not is-flag)
	;; category?
	(creichen/mu4e-tagging-query-category tag-name)
      ;; flag?
      (progn
	(creichen/mu4e-tagging-query-submode-enable)
	(setq creichen/mu4e-tagging-query-flags
	      (let* ((last-bind (alist-get tag-name creichen/mu4e-tagging-query-flags))
		     (filtered-flags (assq-delete-all tag-name creichen/mu4e-tagging-query-flags)))
		(if (eq '- last-bind)
		    ;; From -flag to ignoring the flag
		    filtered-flags
		  ;; otherwise +flag
		  (cons (cons tag-name '+) filtered-flags)
		  )))
	)))
  (creichen/mu4e-tagging-query-rerun)
  )

(defun creichen/mu4e-tagging-interceptor-query-block ()
  "Sets the category filter to stop filtering categories, or to block out a given flag.
Decodes the category or flag by looking at the most recent command.
For flags, if the flag is currently required, it will be ignored instead of being blocked."
  (interactive)
  (-let* (((tag-name . taginfo) (gethash (this-command-keys-vector) creichen/mu4e-tagging-reverse-key-table-tag))
	  (is-flag (plist-get taginfo :flag))
	  )
    (if (not is-flag)
	;; category?
	(creichen/mu4e-tagging-query-category "")
      ;; flag?
      (progn
	(creichen/mu4e-tagging-query-submode-enable)
	(setq creichen/mu4e-tagging-query-flags
	      (let* ((last-bind (alist-get tag-name creichen/mu4e-tagging-query-flags))
		     (filtered-flags (assq-delete-all tag-name creichen/mu4e-tagging-query-flags)))
		(if (eq '+ last-bind)
		    ;; From +flag to ignoring the flag
		    filtered-flags
		  ;; otherwise -flag
		  (cons (cons tag-name '-) filtered-flags)
		  )))
	)))
  (creichen/mu4e-tagging-query-rerun)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mail-info buffer

(defun creichen/mu4e-tagging-update-mail-info-buf ()
  "Update the tagging-related mail-info buffer displayed to the user"
  (let ((buf (creichen/mu4e-tagging-mail-info-buf)))
    (when buf
      (let ((msg (mu4e-message-at-point)))
	(save-excursion
	  (with-current-buffer buf
	    (read-only-mode)
	    (let ((inhibit-read-only t))
	      (erase-buffer)
	      (if msg
		  (progn
		    (insert "Mailinfo\n")
		    (insert (format "%s" msg))
		    )
		;; if msg is NIL:
		(insert "Nomail")
		)))
	  )
	)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag-info buffer

(defface creichen/mu4e-tagging-separator
  '((t (:inherit shadow)))
  "Low-intensity separator characters for `creichen/mu4e-tagging-mode`."
  :group 'creichen/mu4e-tagging
  )

(defface creichen/mu4e-tagging-filter-disabled
  '((t (:inherit widget-inactive)))
  "Indicates that a filter is disabled in `creichen/mu4e-tagging-mode`."
  :group 'my-faces)

(defface creichen/mu4e-tagging-filter-require
  '((((class color) (background light))
     :background "dark green" :foreground "greenyellow" :weight ultra-bold)
    (((class color) (background dark))
     :background "greenyellow" :foreground "black" :weight ultra-bold)
    (t :weight ultra-bold))
  "Indicates that a filter is set to require a tag in `creichen/mu4e-tagging-mode`."
  :group 'creichen/mu4e-tagging
  )

(defface creichen/mu4e-tagging-filter-block
  '((((class color) (background light))
     :background "LightSalmon3" :foreground "yellow" :weight ultra-bold)
    (((class color) (background dark))
     :background "LightSalmon1" :foreground "black" :weight ultra-bold)
    (t :weight ultra-bold :inverse-video t))
  "Indicates that a filter is set to block a tag in `creichen/mu4e-tagging-mode`."
  :group 'creichen/mu4e-tagging
  )

(define-derived-mode creichen/mu4e-tagging-tags-info-mode tabulated-list-mode "mu4e-tag info"
  "Major mode to support creichen/mu4e-tagging-minor-mode.  Used to display
active tags and key bindings."
  )

(defun creichen/mu4e-tagging--show-keybind (keybind)
  "Visualises KEYBIND, which must satisfy `key-valid-p`, as a propertized string."
  (concat
   (propertize "[" 'face 'creichen/mu4e-tagging-separator)
   (propertize (key-description (creichen/mu4e-tagging-keyvec keybind)) 'face 'help-key-binding)
   (propertize "]" 'face 'creichen/mu4e-tagging-separator)
   )
  )

(defun creichen/mu4e-tagging--column-widths (rows num-columns)
  "Computes the maximum widths for a table of strings.

ROWS must be a list of lists that each represent one row.
The rows in turn must be a list of strings, where each string represents
data to be shown in that specific column of the row.
No row may contain more than NUM-COLUMNS strings.

Returns a vector of length num-columns with the largest string length observed for each column."
  (let ((widths (make-vector num-columns 0)))
    (dolist (row rows)
      (seq-do-indexed (lambda (elt index)
			(aset widths index
			      (max (length elt)
				   (aref widths index))))
		      row)
      )
    widths))

(defun creichen/mu4e-tagging--list-tag-info (taginfo)
  "For a TAGINFO of (NAME . PLIST), this returns a list of five propertized strings
that show the information to be displayed in the tag info table."
  (-let* (((tag-name . tag-pinfo) taginfo)
	  (keybind (plist-get tag-pinfo :key))
	  (is-flag (plist-get tag-pinfo :flag))
	  ;(tagname (plist-get tag-pinfo :tag))
	  (short-tagstring (creichen/mu4e-tagging-propertized-name tag-name t))
	  ;;(tagstring (creichen/mu4e-tagging-propertized-name tag-name nil))
	  (filter-status (if is-flag
			     (creichen/mu4e-tagging--propertized-info
			      (alist-get tag-name creichen/mu4e-tagging-query-flags))
			   ;; categorical
			   (creichen/mu4e-tagging--propertized-info (eq creichen/mu4e-tagging-query-category tag-name))
			   ))
	  )
    (list
     (creichen/mu4e-tagging--show-keybind keybind)
     (if is-flag "f" "")
     short-tagstring
     filter-status
     tag-name
     )
    )
  )

(defun creichen/mu4e-tagging--propertized-info (mode)
  "Translates MODE, which must be t, nil, '+, or '-, into a suitable propertized string
to describe whether the given tag is required, blocked, or ignored by the filter."
  (let ((string (cond ((eq mode t)
		       "required")
		      ((eq mode '+)
		       "required")
		      ((eq mode '-)
		       "blocked")
		      (t
		       "shown")
		      ))
	)
    (propertize string 'face
		(cond ((eq mode t)
		       'creichen/mu4e-tagging-filter-require)
		      ((eq mode '+)
		       'creichen/mu4e-tagging-filter-require)
		      ((eq mode '-)
		       'creichen/mu4e-tagging-filter-block)
		      (t
		       'creichen/mu4e-tagging-filter-disabled)))
    )
  )

(defun creichen/mu4e-tagging-update-tag-info-buf ()
  "Update the tagging-related tag-info buffer displayed to the user."
  (let ((buf (creichen/mu4e-tagging-tag-info-buf)))
    (when buf
      (let ((msg (mu4e-message-at-point)))
	(save-excursion
	  (with-current-buffer buf
	    (read-only-mode)
	    (let ((inhibit-read-only t))
	      (erase-buffer)
	      (creichen/mu4e-tagging-tags-info-mode)
	      (let* ((table-data-main
		      (mapcar #'creichen/mu4e-tagging--list-tag-info
			      (append
			       (creichen/mu4e-tagging-categories-get)
			       (creichen/mu4e-tagging-flags-get))))
		     ;; prepend "uncategorised" row
		     (table-data (cons
				  (list
				   (creichen/mu4e-tagging--show-keybind creichen/mu4e-tagging-uncategorized-suffix)
				   ""
				   ""
				   (creichen/mu4e-tagging--propertized-info (eq creichen/mu4e-tagging-query-category 'uncategorized))
				   "(uncategorised)"
				   )
				  table-data-main))
		     (column-widths (creichen/mu4e-tagging--column-widths table-data 5))
		     (query-keybind (creichen/mu4e-tagging--show-keybind creichen/mu4e-tagging-query-prefix))
		     )
		(setq tabulated-list-format (vector
					     `("Key"          ,(max 3 (aref column-widths 0)) t)
					     `("Flag"         ,(max 4 (aref column-widths 1)) t)
					     `("Tag"          ,(max 3 (aref column-widths 2)) t :right-align t)
					     `(,query-keybind ,(max 8 (length query-keybind) (aref column-widths 3)) t :right-align t)
					     `(""            1 t)
					     ))
		(tabulated-list-init-header)
		(setq tabulated-list-entries
		      ;; expects list of (nil [key  flag  tag  keybind  info])
		      (mapcar (lambda (row)
				(list nil (vconcat row)))
			      table-data))
		(tabulated-list-print)
		;; alternative approach with vtable:
		;; 	    (make-vtable
		;; 	     :columns '(:name "Key" :align left)
		;; 	     :objects rows
		;; 	     ;:face ...
		;; 	    )
		)
	      ;; resize window
	      (when creichen/mu4e-tagging-tag-info-window
		(fit-window-to-buffer creichen/mu4e-tagging-tag-info-window))
	      ;; hide cursor
	      (setq cursor-type nil)
	      )))
	))
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration interface

(defcustom
  creichen/mu4e-tagging-untag-prefix
  "-"
  "Key or key combination to use as prefix for removing a tag."
  :type 'key
  :group 'creichen/mu4e-tagging
  )

(defcustom
  creichen/mu4e-tagging-query-prefix
  "C-w"
  "Key or key combination to use as prefix for tag search."
  :type 'key
  :group 'creichen/mu4e-tagging
  )

(defcustom
  creichen/mu4e-tagging-uncategorized-suffix
  "C-d"
  "Key or key combination to decategorise, and to query for uncategorised mails."
  :type 'key
  :group 'creichen/mu4e-tagging
  )

(define-minor-mode creichen/mu4e-tagging-minor-mode
  "A minor mode to quickly retag mails.
Use `creichen/mu4e-tagging-minor-mode-setup-default-bindings` to install the mode
into `mu4e-headers` mode with the default settins, and check `creichen/mu4e-tagging-tags`
for the tags you wish to manage in this mode."
  :lighter " tag-mu4e"
  :keymap creichen/mu4e-tagging-minor-mode-keymap

  (progn
    (if creichen/mu4e-tagging-minor-mode
	(progn
	  (creichen/mu4e-tagging-minor-mode-enable-handler)
	  (message "mu4e-tagging-minor-mode enabled")
	  )
      (progn
	(creichen/mu4e-tagging-minor-mode-disable-handler)
	(message "mu4e-tagging-minor-mode disabled")
	))
    ))

(defun creichen/mu4e-tagging-minor-mode-setup-default-bindings ()
  "Sets the default key bindings for `creichen/mu4e-tagging` minor mode, including
a hook to start it from `mu4e-headers` mode."
  (let ((map creichen/mu4e-tagging-minor-mode-keymap))
    (define-key map (kbd "C-t") #'creichen/mu4e-tagging-minor-mode-disable) ; switch off again
    (define-key map (kbd "q") #'creichen/mu4e-tagging-query-require-uncategorized)
    (define-key map (kbd "C-w C-w") #'creichen/mu4e-tagging-query-clear)
    (define-key map (kbd "C-w C-d") #'creichen/mu4e-tagging-query-require-uncategorized)
    (define-key map (kbd "ESC ESC ESC") #'creichen/mu4e-tagging-minor-mode)
    )
  (add-hook 'mu4e-headers-mode-hook
            (lambda ()
              (local-set-key (kbd "C-t") 'creichen/mu4e-tagging-minor-mode)))
  (add-hook 'mu4e-main-mode-hook #'creichen/mu4e-tagging-minor-mode-disable)
  (add-hook 'mu4e-message-changed-hook #'creichen/mu4e-tagging-mail-at-point-changed)
  (advice-add 'mu4e-headers-next :after #'creichen/mu4e-tagging-mail-at-point-changed)
  (advice-add 'mu4e-headers-prev :after #'creichen/mu4e-tagging-mail-at-point-changed)
  ;; maybe override mu4e-found-func instead?
  )

(defun creichen/mu4e-tagging-update-customize-reset (symbol value)
  "Initial setup for the default values of `creichen/mu4e-tagging-tags`."
  (custom-initialize-reset symbol value)
  (creichen/mu4e-tagging-update-tags))

(defcustom
  creichen/mu4e-tagging-tags
  '(("todo" :key "+" :short "+" :flag t :foreground "yellow" :background "black")
    ("spam" :key "s" :short "SPAM")
    ("regular-mail" :key "m" :short "M"))

  "List of all tags examined by mu4e-tagging-mode.  Tags take the form of alists whose values
are plists, of the form (TAGNAME :short SHORTNAME :key KEY :flag FLAG :foreground FG
:background BG :weight WEIGHT :box BOX).  creichen/mu4e-tagging-minor-mode will use
KEY to select this tag and SHORTNAME to display it in the
mu4e-headers view.  Any occurrences of the tag will be
rendered in the specified FG and BG colours.

Tags without :flag or with :flag nil are categorical tags,
meaning that they are mutually exclusive.  Tags with :flag t
are flags, meaning that any number of them can be 'added' to
any category."
  :type '(alist :tag "Blah"
		:value-type
		(plist :options ((:tag   string)
		    (:short string)
		    (:key   key)
		    (:flag  boolean :doc "Non-exclusive, can be toggled on and off")
		    (:foreground (choice (const :tag "none" nil)
					 (color :tag "Foreground color")))
		    (:background (choice (const :tag "none" nil)
					 (color :tag "Background color")))
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
				  (plist :options ((:line-width (choice (integer :tag "width+height (positive: extrude, negative: intrude)")
									(cons :tag "width . height")))
						   (:color color)
						   (:style (choice (const line)
								   (const wave)))))))
		    )))
  :initialize 'creichen/mu4e-tagging-update-customize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (creichen/mu4e-tagging-update-tags))
  :group 'creichen/mu4e-tagging
  )

(creichen/mu4e-tagging-update-tags)
