;;; ox-hub.el --- Export Org articles to Zenn and Qiita -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ox-hub maintainers

;; Author: ox-hub maintainers
;; Keywords: org, markdown

;;; Commentary:

;; ox-hub exports one Org-mode source article to Markdown for Zenn and Qiita.

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defconst ox-hub--metadata-key-map
  '(("OXHUB_TITLE" . :title)
    ("OXHUB_TAGS" . :tags)
    ("OXHUB_STATUS" . :status)
    ("OXHUB_ZENN_EMOJI" . :zenn-emoji)
    ("OXHUB_ZENN_TYPE" . :zenn-type)
    ("OXHUB_QIITA_PRIVATE" . :qiita-private)
    ("OXHUB_QIITA_SLIDE" . :qiita-slide))
  "Mapping from Org keyword names to internal metadata keys.")

(defconst ox-hub--required-metadata
  '(:title :tags :status :zenn-emoji :zenn-type :qiita-private :qiita-slide)
  "Metadata keys required for export.")

(defun ox-hub--new-article-template ()
  "Return the default Org metadata template for a new article."
  (concat "#+OXHUB_TITLE:\n"
          "#+OXHUB_TAGS:\n"
          "#+OXHUB_STATUS: draft\n"
          "#+OXHUB_ZENN_EMOJI: 📝\n"
          "#+OXHUB_ZENN_TYPE: tech\n"
          "#+OXHUB_QIITA_PRIVATE: false\n"
          "#+OXHUB_QIITA_SLIDE: false\n"
          "\n"))

(defun ox-hub--valid-slug-p (slug)
  "Return non-nil when SLUG is valid for an ox-hub article."
  (and (stringp slug)
       (<= 12 (length slug))
       (<= (length slug) 50)
       (let ((case-fold-search nil))
         (string-match-p "\\`[a-z0-9_-]+\\'" slug))))

(defun ox-hub--git-root (&optional file)
  "Return the Git root for FILE or the current buffer.
Signal `user-error' when there is no file or no Git root."
  (let ((path (or file buffer-file-name)))
    (unless path
      (user-error "Current buffer is not visiting a file"))
    (let ((root (locate-dominating-file path ".git")))
      (unless root
        (user-error "Git root not found"))
      (directory-file-name (expand-file-name root)))))

(defun ox-hub--parse-buffer ()
  "Parse the current Org buffer and return its Org AST."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (save-excursion
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-element-parse-buffer))))

(defun ox-hub--metadata-key (org-key)
  "Return the internal metadata key for ORG-KEY, or nil."
  (cdr (assoc (upcase org-key) ox-hub--metadata-key-map)))

(defun ox-hub--extract-metadata (ast)
  "Extract ox-hub metadata from Org AST.
Duplicate metadata keys are resolved by keeping the last value."
  (let (metadata)
    (org-element-map ast 'keyword
      (lambda (node)
        (let* ((org-key (org-element-property :key node))
               (metadata-key (and org-key (ox-hub--metadata-key org-key))))
          (when metadata-key
            (setq metadata
                  (plist-put metadata
                             metadata-key
                             (string-trim
                              (or (org-element-property :value node) ""))))))))
    metadata))

(defun ox-hub--parse-boolean (value)
  "Parse VALUE as an ox-hub boolean.
Accepted values are true, false, t, and nil.  Signal an error otherwise."
  (pcase (downcase (string-trim (or value "")))
    ((or "true" "t") t)
    ((or "false" "nil") nil)
    (_ (error "Invalid boolean value: %s" value))))

(defun ox-hub--require-metadata (metadata key)
  "Return required KEY from METADATA, or signal an error."
  (let ((value (plist-get metadata key)))
    (unless (and value (not (string-empty-p (string-trim value))))
      (error "Missing required metadata: %s" key))
    (string-trim value)))

(defun ox-hub--validate-enum (value allowed label)
  "Validate VALUE against ALLOWED values for LABEL and return normalized value."
  (let ((normalized (downcase (string-trim value))))
    (unless (member normalized allowed)
      (error "Invalid %s: %s" label value))
    normalized))

(defun ox-hub--parse-tags (value)
  "Parse comma-separated tag VALUE into a non-empty list of strings."
  (let ((tags (seq-filter (lambda (tag)
                            (not (string-empty-p tag)))
                          (mapcar #'string-trim
                                  (split-string (or value "") ",")))))
    (unless tags
      (error "At least one OXHUB_TAGS value is required"))
    tags))

(defun ox-hub--yaml-escape-string (value)
  "Escape VALUE for use in a YAML double-quoted string."
  (mapconcat (lambda (char)
               (pcase char
                 (?\\ "\\\\")
                 (?\" "\\\"")
                 (?\n "\\n")
                 (_ (char-to-string char))))
             value
             ""))

(defun ox-hub--yaml-boolean (value)
  "Return VALUE as a YAML boolean string."
  (if value "true" "false"))

(defun ox-hub--yaml-quoted (value)
  "Return VALUE as a YAML double-quoted string."
  (format "\"%s\"" (ox-hub--yaml-escape-string value)))

(defun ox-hub--published-p (metadata)
  "Return non-nil when METADATA should be published."
  (equal (plist-get metadata :status) "published"))

(defun ox-hub--validate-metadata (metadata)
  "Validate and normalize ox-hub METADATA."
  (dolist (key ox-hub--required-metadata)
    (ox-hub--require-metadata metadata key))
  (let ((title (ox-hub--require-metadata metadata :title))
        (tags (ox-hub--parse-tags (ox-hub--require-metadata metadata :tags)))
        (status (ox-hub--validate-enum
                 (ox-hub--require-metadata metadata :status)
                 '("draft" "published")
                 "OXHUB_STATUS"))
        (zenn-emoji (ox-hub--require-metadata metadata :zenn-emoji))
        (zenn-type (ox-hub--validate-enum
                    (ox-hub--require-metadata metadata :zenn-type)
                    '("tech" "idea")
                    "OXHUB_ZENN_TYPE"))
        (qiita-private (ox-hub--parse-boolean
                        (ox-hub--require-metadata metadata :qiita-private)))
        (qiita-slide (ox-hub--parse-boolean
                      (ox-hub--require-metadata metadata :qiita-slide))))
    (list :title title
          :tags tags
          :status status
          :zenn-emoji zenn-emoji
          :zenn-type zenn-type
          :qiita-private qiita-private
          :qiita-slide qiita-slide)))

(defun ox-hub--render-zenn-front-matter (metadata)
  "Render Zenn front matter from normalized METADATA."
  (let ((topics (mapconcat #'ox-hub--yaml-quoted
                           (plist-get metadata :tags)
                           ", ")))
    (format "---\ntitle: %s\nemoji: %s\ntype: %s\ntopics: [%s]\npublished: %s\n---\n"
            (ox-hub--yaml-quoted (plist-get metadata :title))
            (ox-hub--yaml-quoted (plist-get metadata :zenn-emoji))
            (ox-hub--yaml-quoted (plist-get metadata :zenn-type))
            topics
            (ox-hub--yaml-boolean (ox-hub--published-p metadata)))))

(defun ox-hub--render-qiita-front-matter (metadata)
  "Render Qiita front matter from normalized METADATA."
  (let ((tags (mapconcat (lambda (tag)
                           (format "  - %s" (ox-hub--yaml-quoted tag)))
                         (plist-get metadata :tags)
                         "\n")))
    (format "---\ntitle: %s\ntags:\n%s\nprivate: %s\nslide: %s\nignorePublish: %s\n---\n"
            (ox-hub--yaml-quoted (plist-get metadata :title))
            tags
            (ox-hub--yaml-boolean (plist-get metadata :qiita-private))
            (ox-hub--yaml-boolean (plist-get metadata :qiita-slide))
            (ox-hub--yaml-boolean (not (ox-hub--published-p metadata))))))

;;;###autoload
(defun ox-hub-new-article (slug)
  "Create a new Org article for SLUG under the Git root."
  (interactive "sArticle slug: ")
  (unless (ox-hub--valid-slug-p slug)
    (user-error "Invalid article slug: %s" slug))
  (let* ((root (ox-hub--git-root))
         (org-dir (expand-file-name "org" root))
         (article-file (expand-file-name (concat slug ".org") org-dir)))
    (when (file-exists-p article-file)
      (user-error "Article already exists: %s" article-file))
    (make-directory org-dir t)
    (write-region (ox-hub--new-article-template) nil article-file nil 'silent)
    (find-file article-file)
    (message "Created article: %s" article-file)
    article-file))

(provide 'ox-hub)

;;; ox-hub.el ends here
