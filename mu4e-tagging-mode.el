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

;;; Code:

(require 'dash)
(require 'mu4e)
(require 'compat-29)

(provide 'mu4e-tagging-mode)

(defvar-keymap creichen/mu4e-tagging-minor-mode-keymap
  :doc "Primary keymap for creichen/mu4e-tagging-minor mode."
  )

(defvar-keymap creichen/mu4e-tagging-minor-mode-auto-keymap
  :doc "Auxiliary keymap for creichen/mu4e-tagging-minor mode:
        this map is automatically set up to manage tagging
        commands, but is overriden by
        creichen/mu4e-tagging-minor-mode-keymap."
  :parent creichen/mu4e-tagging-minor-mode-keymap
  )

(defun creichen/mu4e-tagging-minor-mode-clear-tagging-auto-keymap ()
  (setcdr creichen/mu4e-tagging-minor-mode-auto-keymap (cdr (make-sparse-keymap)))
  )


(defvar creichen/mu4e-tagging-known-tags
  (make-hash-table :test 'equal)
  "All tags managed by creichen/mu4e-tagging-mode.  Automatically constructed from
   creichen/mu4e-tagging-tags.")

;; ;; alist of normalised tags, with :short and :key guaranteed:
;; (setq creichen/mu4e-tagging-tags-normalized nil)

;; (defun creichen/mu4e-tagging-normalize-tag (taginfo)
;;   "Normalizes taginfo, which must be of the form (tagname . plist),
;;    with plist being the permitted properties."


(setq creichen/mu4e-tagging-reverse-key-table-tag
      (make-hash-table :test 'equal))
(setq creichen/mu4e-tagging-reverse-key-table-untag
      (make-hash-table :test 'equal))

;; (defun creichen/mu4e-tagging-tags-get ()
;;   (if (boundp 'creichen/mu4e-tagging-tags)
;;       creichen/mu4e-tagging-tags
;;     nil)
;;   )

(setq creichen/mu4e-tagging-categories-var nil)
(setq creichen/mu4e-tagging-flags-var nil)

(defun creichen/mu4e-tagging-categories-get ()
  creichen/mu4e-tagging-categories-var
  ;; (-filter (lambda (tag) (not (plist-get tag :flag)))
  ;; 	   (creichen/mu4e-tagging-tags-get))
  )

(defun creichen/mu4e-tagging-flags-get ()
  creichen/mu4e-tagging-flags-var
  ;; (-filter (lambda (tag) (plist-get tag :flag))
  ;; 	   (creichen/mu4e-tagging-tags-get))
  )

(defmacro creichen/mu4e-tagging-dotags (VARS &rest BODY)
  "Iterates over all tags.  (creichen/mu4e-tagging-dotags (TAGINFO TY) BODY)
   will execute BODY for all tags, bound to TAGINFO.  First, it iterates over all
   categorical tags (with TY = 'category), then execute BODY once with TAGINFO=TY=nil,
   then iterate over all flags (with TY = 'flag).
  "
  `(dolist (tags-ty (list
		     (list (creichen/mu4e-tagging-categories-get) 'category)
		     (list '(separator) nil)
		     (list (creichen/mu4e-tagging-flags-get) 'flag)
		     ))
    (-let (((tags-list ,(cadr VARS)) tags-ty))
      (dolist (,(car VARS) tags-list)
	(when ,(car VARS)
	  ,@BODY
	  )
	)))
  )

(defun creichen/mu4e-tagging-interceptor-tag ()
  (interactive)
  ;; (-let* (((taginfo ty) (gethash (this-command-keys-vector) creichen/mu4e-tagging-reverse-key-table-tag))
  ;; 	  (tagname (plist-get taginfo :tag))
  ;; 	  (msg (mu4e-message-at-point)))
  ;;   (mu4e-action-retag-message msg (concat "+" tagname))
  ;;   )
  (-let* (((tag-name . taginfo) (gethash (this-command-keys-vector) creichen/mu4e-tagging-reverse-key-table-tag))
	  (msg (mu4e-message-at-point))
	  (tag-add (concat "+" tag-name))
	  (tag-remove (if (eq 'category ty)
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
  (interactive)
  (-let* (((taginfo ty) (gethash (this-command-keys-vector) creichen/mu4e-tagging-reverse-key-table-untag))
	  (tag-name (plist-get taginfo :tag))
	  (msg (mu4e-message-at-point)))
    (mu4e-action-retag-message msg (concat "-" tag-name))
    )
  )

(defun creichen/mu4e-tagging-keyvec (tag-name key)
  (setq debug-on-error t)
  (let* ((key-vec-or-string key)
	 (key-tag (cond ((vectorp key-vec-or-string)
			 key-vec-or-string)
			((characterp key-vec-or-string)
			 (vector key-vec-or-string))
			(t
			 (key-parse key-vec-or-string)))))
    (if (eq 0 (length key-tag))
	(progn
	  (message "Empty key binding (%s) for tag %s" key-vec-or-string tag-name)
	  key-tag
	  )
	;; else all is well
      key-tag
    ))
  )

(defcustom
  creichen/mu4e-tagging-untag-prefix
  ?-
  "Key or key combination to use as prefix for removing a tag."
  :type '(choice (vector :tag "Key sequence") (character :tag "Single key")))

(defun creichen/mu4e-tagging-alloc-keys (tag-name tag-pbody)
  "Allocates and binds keys for tag-name, ensuring uniqueness.
  Returns tag-pbody with :key possibly updated.  Throws
  'keybind-impossible if (1) :key in tag-pbody is already bound,
  (2) :key is not in tag-pbody and the function couldn't guess an
  unused binding, or (3) :key is somehow invalid."
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
	 (prefix-key-vec (if (vectorp creichen/mu4e-tagging-untag-prefix)
			    creichen/mu4e-tagging-untag-prefix
			   (vector creichen/mu4e-tagging-untag-prefix))))
    (when (or
	   (null key)
	   (equal (string-to-char key)
		  creichen/mu4e-tagging-untag-prefix)
	   (keymap-lookup creichen/mu4e-tagging-minor-mode-keymap (kbd key)))
      (throw 'keybind-impossible (list tag-name :key requested-key))
      )
    ;; Otherwise the keybind is available
    (let* ((key-tag (creichen/mu4e-tagging-keyvec tag-name key))
	   (key-untag (vconcat prefix-key-vec key-tag))
	   ;; update :key accordingly
	   (tag-pbody-new (plist-put tag-pbody :key key-tag))
	   (taginfo (cons tag-name tag-pbody-new))
	   )
      ;; Bind the key to call the generic interceptor functions
      (define-key creichen/mu4e-tagging-minor-mode-auto-keymap
		  key-tag 'creichen/mu4e-tagging-interceptor-tag)
      (define-key creichen/mu4e-tagging-minor-mode-auto-keymap
		  key-untag 'creichen/mu4e-tagging-interceptor-untag)

      ;; Set up reverse lookup keymaps so the interceptors can figure out why they were called
      (puthash key-tag taginfo
	       creichen/mu4e-tagging-reverse-key-table-tag)
      (puthash key-untag taginfo
	       creichen/mu4e-tagging-reverse-key-table-untag)
      ;; Return updated tag-body plist
      tag-pbody-new
      )
    )
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
    ;; Store the results
    (setq creichen/mu4e-tagging-categories-var (reverse tag-infos-categories))
    (setq creichen/mu4e-tagging-flags-var (reverse tag-infos-flags))
  ))


(defun creichen/mu4e-tagging-known-category-tags ()
  "All tags that are category tags (list of strings)."
  (mapcar 'car creichen/mu4e-tagging-categories-var)
  )

(add-hook 'mu4e-headers-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t") 'creichen/mu4e-tagging-minor-mode)))

(setq creichen/mu4e-tagging-tag-info-window nil)
(setq creichen/mu4e-tagging-mail-info-window nil)

(setq creichen/mu4e-tagging-tag-info-window-buf nil)
(setq creichen/mu4e-tagging-mail-info-window-buf nil)

(defun creichen/mu4e-tagging-mail-info-buf ()
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
  (when creichen/mu4e-tagging-tag-info-window
    (let ((buf (get-buffer-create " *mu4e-tagging-tag-info*")))
      (unless
	  creichen/mu4e-tagging-tag-info-window-buf
	(set-window-buffer creichen/mu4e-tagging-tag-info-window buf t)
	(setq creichen/mu4e-tagging-tag-info-window-buf buf)
	)
      buf))
  )

(defun creichen/mu4e-tagging-minor-mode-enable ()
  (let ((tag-info-window (split-window-below (- -1 (hash-table-count creichen/mu4e-tagging-known-tags))))
        (mail-info-window (split-window-right -80)))
    (setq creichen/mu4e-tagging-tag-info-window tag-info-window)
    (setq creichen/mu4e-tagging-mail-info-window mail-info-window)
    (creichen/mu4e-tagging-mail-info-buf)
    (creichen/mu4e-tagging-tag-info-buf)
    (creichen/mu4e-tagging-mail-at-point-changed)
    )
  )

(defun creichen/mu4e-tagging-minor-mode-disable ()
  (delete-window creichen/mu4e-tagging-tag-info-window)
  (delete-window creichen/mu4e-tagging-mail-info-window)
  (setq creichen/mu4e-tagging-tag-info-window nil)
  (setq creichen/mu4e-tagging-mail-info-window nil)
  (setq creichen/mu4e-tagging-tag-info-window-buf nil)
  (setq creichen/mu4e-tagging-mail-info-window-buf nil)
  )

(defun creichen/mu4e-tagging-type (tagname)
  "Determines if tagname is a tag for a 'category, for a 'flag, or not a known tag (NIL)"
  (if (plist-get (creichen/mu4e-tagging-info tagname) :flag)
      'flag
    'category)
  )

(defun creichen/mu4e-tagging-info (tagname)
  "Retrieves the tag plist information for a given tag."
  (gethash tagname creichen/mu4e-tagging-known-tags '())
  )

(defun creichen/mu4e-tagging-category-tag-p (tagname)
  (eq 'category (creichen/mu4e-tagging-type tagname))
  )

(defun creichen/mu4e-tagging-flag-tag-p (tagname)
  (eq 'flag (creichen/mu4e-tagging-type tagname))
  )

(defun creichen/mu4e-tagging-no-tag-p (tagname)
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
      '((:human-date   .  9)
        (:flags        .  6)
        (:short-tags   .  10)
        (:from         .  20)
        (:mailing-list .  10)
        (:subject      .  nil)
	))

(defun creichen/mu4e-tagging-render-tags (msg)
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

(defun creichen/mu4e-tagging-update-mail-info-buf ()
  "Update the tagging-related mail-info buffer displayed to the user"
  (let ((buf (creichen/mu4e-tagging-mail-info-buf)))
    (when buf
      (let ((msg (mu4e-message-at-point)))
	(save-excursion
	  (with-current-buffer buf
	    (erase-buffer)
	    (if msg
		(progn
		  (insert "Mailinfo\n")
		  (insert (format "%s" msg))
		  )
	      ;; if msg is NIL:
	      (insert "Nomail")
	      )))
	)))
  )

(defun creichen/mu4e-tagging-format-tag-info (taginfo tagty)
  (setq debug-on-error t)
  (-let* (((tag-name . tag-pinfo) taginfo)
	  (key-tag (creichen/mu4e-tagging-keyvec tag-name (plist-get tag-pinfo :key)))
	  (tagname (plist-get tag-pinfo :tag))
	  (short-tagstring (creichen/mu4e-tagging-propertized-name tag-name t))
	  (tagstring (creichen/mu4e-tagging-propertized-name tag-name nil))
	  )
    (format "%-8s %5s %s\n"
 	    (format "[%s]" (key-description key-tag))
 	    short-tagstring
 	    tagstring)
    )
  )

(defun creichen/mu4e-tagging-update-tag-info-buf ()
  "Update the tagging-related tag-info buffer displayed to the user"
  (let ((buf (creichen/mu4e-tagging-tag-info-buf)))
    (when buf
      (let ((msg t ));(mu4e-message-at-point)))
	(save-excursion
	  (with-current-buffer buf
	    (erase-buffer)
	    (if msg
		(progn
		  (creichen/mu4e-tagging-dotags (taginfo tagty)
						(if tagty
						    (insert (creichen/mu4e-tagging-format-tag-info taginfo tagty))
						  ;; separator line
						  (insert "-----\n")
						))
		  )
	      ;; if msg is NIL:
	      (insert "Nothing")
	      )))
	))
    )
  )

(defun creichen/mu4e-tagging-mail-at-point-changed (&rest ignoreme)
  (when (or creichen/mu4e-tagging-tag-info-window creichen/mu4e-tagging-mail-info-window)
    ;; only react if at least one of the creichen/mu4e-tagging info windows is open
    (creichen/mu4e-tagging-update-mail-info-buf)
    (creichen/mu4e-tagging-update-tag-info-buf)
    ))

(define-minor-mode creichen/mu4e-tagging-minor-mode
  "A minor mode to quickly retag mails."
  :lighter " tag-mu4e"
  :keymap creichen/mu4e-tagging-minor-mode-auto-keymap

  (if creichen/mu4e-tagging-minor-mode
      (progn
	(creichen/mu4e-tagging-minor-mode-enable)
	(message "mu4e-tagging-minor-mode enabled")
	)
    (progn
      (creichen/mu4e-tagging-minor-mode-disable)
      (message "mu4e-tagging-minor-mode disabled")
      ))
  )

(defun creichen/mu4e-tagging-minor-mode-setup-default-bindings ()
  (let ((map creichen/mu4e-tagging-minor-mode-keymap))
    (define-key map (kbd "C-c t") 'creichen/mu4e-tagging-minor-mode) ; switch off again
    (define-key map (kbd "q") 'creichen/mu4e-tagging-minor-mode)
    )
  )

(defun creichen/mu4e-tagging-update-customize-reset (symbol value)
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
				     (const "ultra-thin")
				     (const "light")
				     (const "semi-light")
				     (const "normal")
				     (const "medium")
				     (const "semi-bold")
				     (const "bold")
				     (const "ultra-bold")))
		    (:box (choice (const :tag "none" nil)
				  (const :tag "simple" t)
				  (color :tag "color")
				  (plist :options ((:line-width (choice (integer :tag "width+height (positive: extrude, negative: intrude)")
									(cons :tag "width . height")))
						   (:color color)
						   (:style (choice (const 'line)
								   (const 'wave)))))))
		    )))
  :initialize 'creichen/mu4e-tagging-update-customize-reset
  :set (lambda (symbol value)
         (set-default symbol value)
         (creichen/mu4e-tagging-update-tags))

  )

(add-hook 'mu4e-message-changed-hook 'creichen/mu4e-tagging-mail-at-point-changed)

(advice-add 'mu4e-headers-next :after #'creichen/mu4e-tagging-mail-at-point-changed)
(advice-add 'mu4e-headers-prev :after #'creichen/mu4e-tagging-mail-at-point-changed)
;; maybe override mu4e-found-func instead?

(creichen/mu4e-tagging-update-tags)
(creichen/mu4e-tagging-minor-mode-setup-default-bindings)

