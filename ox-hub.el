;;; ox-hub.el --- Export Org articles to Zenn and Qiita -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ox-hub maintainers

;; Author: miyatarutyyy <study.miyata026@gmail.com>
;; Keywords: org, markdown
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.5"))

;;; Commentary:

;; ox-hub exports a single Org-mode source article into Markdown files for publishing platforms such as Zenn and Qiita
;;
;; It reads OXHUB_* metadata from Org keywords and converts the articles body into platform-specific Markdown

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

;;; Constants

;; Shared tables that define supported metadata, paths, and target-specific
;; fields.

(defconst ox-hub--metadata-key-map
  '(("OXHUB_TITLE" . :title)
    ("OXHUB_TAGS" . :tags)
    ("OXHUB_STATUS" . :status)
    ("OXHUB_ZENN_EMOJI" . :zenn-emoji)
    ("OXHUB_ZENN_TYPE" . :zenn-type)
    ("OXHUB_QIITA_PRIVATE" . :qiita-private)
    ("OXHUB_QIITA_SLIDE" . :qiita-slide))
  "Mapping from Org keyword names to internal metadata keys.")

;; Currently, ox-hub treats all metadata keys as required because the MVP
;; assumes exporting a single Org source to both Zenn and Qiita. If future
;; versions support exporting to only one target, split this list into
;; target-specific required metadata, e.g. common keys plus Zenn-only and
;; Qiita-only keys.
;;
;; (defconst ox-hub--required-metadata
;;   '(:title :tags :status :zenn-emoji :zenn-type :qiita-private :qiita-slide)
;;  "Metadata keys required for export.")

(defconst ox-hub--required-metadata
  '(:title :tags :status :zenn-emoji :zenn-type :qiita-private :qiita-slide)
  "Metadata keys required for export.")

(defconst ox-hub--image-file-extensions
  '("avif" "gif" "jpeg" "jpg" "png" "svg" "webp")
  "File extensions treated as images in Markdown links.")

(defconst ox-hub--target-output-directories
  '((zenn . "articles")
    (qiita . "public"))
  "Git-root relative output directories for each export target.")

(defconst ox-hub--qiita-cli-managed-fields
  '(("updated_at" . :qiita-updated-at)
    ("id" . :qiita-id)
    ("organization_url_name" . :qiita-organization-url-name))
  "Qiita CLI managed front matter fields preserved across exports.")

;;; Article and Buffer Context

;; Helpers for creating article skeletons and resolving the current project,
;; source buffer, and article identity.

(defconst ox-hub--slug-min-length 12
  "Minimum length of an ox-hub article slug.")

(defconst ox-hub--slug-max-length 50
  "Maximum length of an ox-hub article slug.")

(defconst ox-hub--slug-regexp "\\`[a-z0-9_-]+\\'"
  "Regular expression for a valid ox-hub article slug.")

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
       (<= ox-hub--slug-min-length (length slug))
       (<= (length slug) ox-hub--slug-max-length)
       (let ((case-fold-search nil))
         (string-match-p ox-hub--slug-regexp slug))))

(defun ox-hub--git-root (&optional start-path)
  "Return the Git root for START-PATH or the current buffer context.
When START-PATH is nil and the current buffer is not visiting a file, use
`default-directory'.  Signal `user-error' when there is no Git root."
  (let ((path (or start-path buffer-file-name default-directory)))
    (unless path
      (user-error "Current buffer has no directory context"))
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

;;; Metadata Parsing and Validation

;; Convert #+OXHUB_* keywords into normalized metadata used by all export
;; targets.

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
  "Parse VALUE as a comma-separated list of tags. Signal an error when VALUE does not contain at least one non-empty tag."
  (let ((tags (seq-remove #'string-empty-p
                          (mapcar #'string-trim
                                  (split-string value ",")))))
    (unless tags
      (user-error "OXHUB_TAGS must contain at least one tag"))
    tags))

;;; Scalar Formatting

;; These helpers assume values have already been parsed and validated by the metadata layer.  They only handle presentation-level escaping.

(defun ox-hub--yaml-escape-string (value)
  "Escape VALUE for use in a YAML double-quoted string."
  (mapconcat (lambda (char)
               (pcase char
                 (?\\ "\\\\")
                 (?\" "\\\"")
                 (?\n "\\n")
                 (?\t "\\t")
                 (?\r "\\r")
                 (_ (char-to-string char))))
             value
             ""))

(defun ox-hub--yaml-boolean (value)
  "Return VALUE as a YAML boolean string. Non-nil values are rendered as \"true\" and nil as \"false\"."
  (if value "true" "false"))

(defun ox-hub--yaml-quoted (value)
  "Return VALUE as a YAML double-quoted string."
  (format "\"%s\"" (ox-hub--yaml-escape-string value)))

(defun ox-hub--html-escape-string (value)
  "Escape VALUE for use as HTML text."
  (mapconcat (lambda (char)
               (pcase char
                 (?& "&amp;")
                 (?< "&lt;")
                 (?> "&gt;")
                 (?\" "&quot;")
                 (_ (char-to-string char))))
             value
             ""))

;;; Metadata Normalization

;; Build the canonical metadata plist consumed by front matter renderers.

(defun ox-hub--published-p (metadata)
  "Return non-nil when METADATA should be published."
  (equal (plist-get metadata :status) "published"))

(defun ox-hub--validate-metadata (metadata)
  "Validate and normalize ox-hub METADATA."
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

;;; Front Matter Rendering

;; Render normalized metadata into Zenn and Qiita front matter.

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

(defun ox-hub--render-qiita-front-matter (metadata &optional cli-metadata)
  "Render Qiita front matter from normalized METADATA.
CLI-METADATA is a plist of Qiita CLI managed field values."
  (let ((tags (mapconcat (lambda (tag)
                           (format "  - %s" (ox-hub--yaml-quoted tag)))
                         (plist-get metadata :tags)
                         "\n")))
    (format "---\ntitle: %s\ntags:\n%s\nprivate: %s\nupdated_at: %s\nid: %s\norganization_url_name: %s\nslide: %s\nignorePublish: %s\n---\n"
            (ox-hub--yaml-quoted (plist-get metadata :title))
            tags
            (ox-hub--yaml-boolean (plist-get metadata :qiita-private))
            (ox-hub--yaml-quoted
             (ox-hub--qiita-cli-metadata-value cli-metadata :qiita-updated-at))
            (ox-hub--yaml-quoted
             (ox-hub--qiita-cli-metadata-value cli-metadata :qiita-id))
            (ox-hub--yaml-quoted
             (ox-hub--qiita-cli-metadata-value cli-metadata
                                               :qiita-organization-url-name))
            (ox-hub--yaml-boolean (plist-get metadata :qiita-slide))
            (ox-hub--yaml-boolean (not (ox-hub--published-p metadata))))))

(defun ox-hub--qiita-cli-metadata-value (metadata key)
  "Return Qiita CLI METADATA value for KEY, defaulting to an empty string."
  (let ((value (plist-get metadata key)))
    (if (stringp value) value "")))

;;; Markdown Rendering Dispatcher

;; Dispatch Org AST nodes to the Markdown renderer for each supported element.

(defun ox-hub--render-body (ast &optional target)
  "Render Org AST body as Markdown for TARGET."
  (let ((body (string-trim-right (ox-hub--render-node ast target))))
    (if (string-empty-p body)
        ""
      (concat body "\n"))))

(defun ox-hub--render-node (node target)
  "Render Org NODE as Markdown for TARGET."
  (cond
   ((stringp node) node)
   ((not node) "")
   (t
    (pcase (org-element-type node)
      ('org-data (ox-hub--render-contents node target))
      ('section (ox-hub--render-contents node target))
      ;; Keywords are handled as metadata or target-specific attributes elsewhere.
      ;; The body renderer intentionally omits them for now.
      ('keyword "")
      ('headline (ox-hub--render-headline node target))
      ('paragraph (concat (string-trim-right
                           (ox-hub--render-contents node target))
                          "\n\n"))
      ('bold (concat (format "**%s**" (ox-hub--render-contents node target))
                     (ox-hub--render-post-blank node)))
      ('italic (concat (format "*%s*" (ox-hub--render-contents node target))
                       (ox-hub--render-post-blank node)))
      ('strike-through
       (concat (format "~~%s~~" (ox-hub--render-contents node target))
               (ox-hub--render-post-blank node)))
      ((or 'code 'verbatim)
       (concat (format "`%s`" (org-element-property :value node))
               (ox-hub--render-post-blank node)))
      ('footnote-reference (ox-hub--render-footnote-reference node))
      ('footnote-definition (ox-hub--render-footnote-definition node target))
      ('link (ox-hub--render-link node target))
      ('src-block (ox-hub--render-src-block node))
      ('example-block (ox-hub--render-example-block node))
      ('plain-list (ox-hub--render-plain-list node target))
      ('quote-block (ox-hub--render-quote-block node target))
      ('table (ox-hub--render-table node target))
      ('horizontal-rule "-----\n\n")
      ('special-block (ox-hub--render-special-block node target))
      (_ (error "Unsupported Org element: %s" (org-element-type node)))))))

(defun ox-hub--render-post-blank (node)
  "Render NODE post-blank spaces."
  (make-string (or (org-element-property :post-blank node) 0) ? ))

(defun ox-hub--render-contents (node target)
  "Render NODE contents as Markdown for TARGET."
  (mapconcat (lambda (child)
               (ox-hub--render-node child target))
             (org-element-contents node)
             ""))

(defun ox-hub--render-headline (node target)
  "Render headline NODE as Markdown for TARGET."
  (let ((level (org-element-property :level node))
        (title (mapconcat (lambda (child)
                            (ox-hub--render-node child target))
                          (org-element-property :title node)
                          ""))
        (contents (ox-hub--render-contents node target)))
    (concat (make-string level ?#)
            " "
            title
            "\n\n"
            contents)))

;;; Code Blocks

;; Render Org source and example blocks with fences that safely contain nested
;; backticks.

(defun ox-hub--markdown-code-fence (content)
  "Return a Markdown code fence longer than any backtick run in CONTENT."
  (let ((max-run 0)
        (start 0)
        match-length)
    (while (string-match "`+" content start)
      (setq match-length (- (match-end 0) (match-beginning 0)))
      (setq max-run (max max-run match-length))
      (setq start (match-end 0)))
    (make-string (max 3 (1+ max-run)) ?`)))

(defun ox-hub--render-fenced-code-block (info content)
  "Render CONTENT as a fenced Markdown code block with INFO."
  (let* ((value (string-trim-right (or content "")))
         (fence (ox-hub--markdown-code-fence value)))
    (format "%s%s\n%s\n%s\n\n" fence (or info "") value fence)))

(defun ox-hub--render-src-block (node)
  "Render src-block NODE as a fenced Markdown code block."
  (ox-hub--render-fenced-code-block
   (or (org-element-property :language node) "")
   (or (org-element-property :value node) "")))

(defun ox-hub--render-example-block (node)
  "Render example-block NODE as a fenced Markdown code block."
  (ox-hub--render-fenced-code-block
   ""
   (or (org-element-property :value node) "")))

;;; Lists

;; Preserve ordered and unordered list markers while normalizing indentation for
;; nested list content.

(defun ox-hub--render-plain-list (node target)
  "Render plain-list NODE as a Markdown list for TARGET."
  (let (previous-ordered
        seen-item
        rendered)
    (dolist (item (org-element-contents node))
      (let* ((marker (ox-hub--list-item-marker item))
             (ordered (ox-hub--ordered-list-marker-p marker))
             (item-text (ox-hub--render-list-item item target marker)))
        (setq rendered
              (concat rendered
                      (cond
                       ((not seen-item) "")
                       ((eq previous-ordered ordered) "\n")
                       (t "\n\n"))
                      item-text))
        (setq seen-item t)
        (setq previous-ordered ordered)))
    (concat rendered "\n\n")))

(defun ox-hub--ordered-list-marker-p (marker)
  "Return non-nil when MARKER is an ordered Markdown list marker."
  (string-match-p "\\`[0-9]+\\. " marker))

(defun ox-hub--list-item-marker (node)
  "Return Markdown list marker for item NODE."
  (let ((bullet (or (org-element-property :bullet node) "")))
    (if (string-match "\\`\\([0-9]+\\)[.)][ \t]*\\'" bullet)
        (format "%s. " (match-string 1 bullet))
      "- ")))

(defun ox-hub--render-list-item (node target marker)
  "Render list item NODE with Markdown MARKER for TARGET."
  (let* ((content (ox-hub--join-list-item-parts
                   (ox-hub--render-list-item-parts node target)))
         (lines (split-string content "\n"))
         (first-line t)
         (indent (make-string (length marker) ? )))
    (mapconcat (lambda (line)
                 (prog1
                     (cond
                      (first-line (concat marker line))
                      ((string-empty-p line) "")
                      (t (concat indent line)))
                   (setq first-line nil)))
               lines
               "\n")))

(defun ox-hub--render-list-item-parts (node target)
  "Render direct child parts of list item NODE for TARGET."
  (let (parts)
    (dolist (child (org-element-contents node))
      (let ((rendered (string-trim-right
                       (ox-hub--render-node child target))))
        (unless (string-empty-p rendered)
          (setq parts
                (cons (cons (org-element-type child) rendered)
                      parts)))))
    (nreverse parts)))

(defun ox-hub--join-list-item-parts (parts)
  "Join rendered list item PARTS without extra blanks around nested lists."
  (let (rendered
        previous-type)
    (dolist (part parts)
      (let ((type (car part))
            (text (cdr part)))
        (setq rendered
              (if rendered
                  (concat rendered
                          (if (or (eq previous-type 'plain-list)
                                  (eq type 'plain-list))
                              "\n"
                            "\n\n")
                          text)
                text))
        (setq previous-type type)))
    (or rendered "")))

;;; Links and Images

;; Render regular links directly and rewrite image file links when a target has
;; stricter path requirements.

(defun ox-hub--render-link (node target)
  "Render link NODE as Markdown for TARGET."
  (let* ((type (org-element-property :type node))
         (path (org-element-property :path node))
         (raw-link (or (org-element-property :raw-link node) path))
         (url (if (equal type "file") path raw-link))
         (description (if (org-element-contents node)
                          (ox-hub--render-contents node target)
                        nil)))
    (if (and (equal type "file") (ox-hub--image-path-p path))
        (concat (format "![%s](%s)"
                        (or description "")
                        (ox-hub--render-image-url url target))
                (ox-hub--render-post-blank node))
      (concat (format "[%s](%s)" (or description url) url)
              (ox-hub--render-post-blank node)))))

(defun ox-hub--render-image-url (url target)
  "Render image URL for TARGET."
  (pcase target
    ('zenn (ox-hub--zenn-image-url url))
    (_ url)))

(defun ox-hub--zenn-image-url (url)
  "Return URL as a Zenn project-root image path."
  (let ((normalized (replace-regexp-in-string "\\`\\./+" "" (or url ""))))
    (if (string-prefix-p "/" normalized)
        normalized
      (concat "/" normalized))))

(defun ox-hub--image-path-p (path)
  "Return non-nil when PATH has a known image file extension."
  (let ((extension (and path (file-name-extension path))))
    (and extension
         (member (downcase extension) ox-hub--image-file-extensions))))

;;; Block Elements

;; Render Markdown blocks that are not handled by the main dispatcher directly.

(defun ox-hub--render-quote-block (node target)
  "Render quote-block NODE as Markdown for TARGET."
  (let ((content (string-trim-right (ox-hub--render-contents node target))))
    (concat (mapconcat (lambda (line)
                         (concat "> " line))
                       (split-string content "\n")
                       "\n")
            "\n\n")))

(defun ox-hub--render-table (node target)
  "Render table NODE as a Markdown table for TARGET."
  (let ((rows (seq-filter (lambda (row)
                            (eq (org-element-property :type row) 'standard))
                          (org-element-contents node))))
    (unless rows
      (error "Table must contain at least one standard row"))
    (let* ((rendered-rows (mapcar (lambda (row)
                                    (ox-hub--render-table-row row target))
                                  rows))
           (header (car rendered-rows))
           (separator (mapcar (lambda (_cell) "---") header))
           (body (cdr rendered-rows)))
      (concat (ox-hub--format-markdown-table-row header)
              "\n"
              (ox-hub--format-markdown-table-row separator)
              (if body
                  (concat "\n"
                          (mapconcat #'ox-hub--format-markdown-table-row
                                     body
                                     "\n"))
                "")
              "\n\n"))))

(defun ox-hub--render-table-row (node target)
  "Render table-row NODE as a list of Markdown cell strings for TARGET."
  (mapcar (lambda (cell)
            (string-trim (ox-hub--render-contents cell target)))
          (org-element-contents node)))

(defun ox-hub--format-markdown-table-row (cells)
  "Format CELLS as one Markdown table row."
  (concat "| " (mapconcat #'identity cells " | ") " |"))

;;; Footnotes

;; Convert Org footnote references and definitions to Markdown footnote syntax.

(defun ox-hub--render-footnote-reference (node)
  "Render footnote-reference NODE as Markdown."
  (concat (format "[^%s]" (org-element-property :label node))
          (ox-hub--render-post-blank node)))

(defun ox-hub--render-footnote-definition (node target)
  "Render footnote-definition NODE as Markdown for TARGET."
  (let ((label (org-element-property :label node))
        (content (string-trim-right (ox-hub--render-contents node target))))
    (format "[^%s]: %s\n\n"
            label
            (replace-regexp-in-string "\n" "\n    " content nil t))))

;;; oxhub Special Directives

;; Target-specific extensions encoded as Org special blocks named "oxhub".

(defun ox-hub--parse-directive-parameters (parameters)
  "Parse oxhub directive PARAMETERS into a plist.
The first plist value is stored as :directive."
  (let ((tokens (split-string-and-unquote (or parameters ""))))
    (unless tokens
      (error "Missing oxhub directive"))
    (let ((plist (list :directive (car tokens)))
          (rest (cdr tokens)))
      (while rest
        (let ((key (car rest))
              (value (cadr rest)))
          (unless (and key value (string-prefix-p ":" key))
            (error "Invalid oxhub directive parameters: %s" parameters))
          (setq plist (plist-put plist (intern key) value))
          (setq rest (cddr rest))))
      plist)))

(defun ox-hub--require-directive-parameter (plist key directive)
  "Return required KEY from PLIST for DIRECTIVE, or signal an error."
  (let ((value (plist-get plist key)))
    (unless (and value (not (string-empty-p value)))
      (error "Missing %s parameter for oxhub %s" key directive))
    value))

(defun ox-hub--render-special-block (node target)
  "Render special-block NODE for TARGET."
  (unless (equal (org-element-property :type node) "oxhub")
    (error "Unsupported Org special block: %s"
           (org-element-property :type node)))
  (unless target
    (error "oxhub directives require an export target"))
  (let* ((parameters (ox-hub--parse-directive-parameters
                      (org-element-property :parameters node)))
         (directive (plist-get parameters :directive)))
    (pcase directive
      ("message" (ox-hub--render-message-directive node target parameters))
      ("details" (ox-hub--render-details-directive node target parameters))
      ("codefile" (ox-hub--render-codefile-directive node target parameters))
      (_ (error "Unsupported oxhub directive: %s" directive)))))

(defun ox-hub--render-message-directive (node target parameters)
  "Render oxhub message directive NODE for TARGET using PARAMETERS."
  (let* ((type (or (plist-get parameters :type) "info"))
         (content (string-trim-right (ox-hub--render-contents node target))))
    (unless (member type '("info" "alert"))
      (error "Invalid oxhub message type: %s" type))
    (pcase target
      ('zenn
       (format ":::message%s\n%s\n:::\n\n"
               (if (equal type "alert") " alert" "")
               content))
      ('qiita
       (ox-hub--render-blockquote
        (if (equal type "alert")
            (concat "**Warning:**\n\n" content)
          content)))
      (_ (error "Unsupported export target: %s" target)))))

(defun ox-hub--render-details-directive (node target parameters)
  "Render oxhub details directive NODE for TARGET using PARAMETERS."
  (let ((summary (ox-hub--require-directive-parameter
                  parameters :summary "details"))
        (content (string-trim-right (ox-hub--render-contents node target))))
    (pcase target
      ('zenn
       (format ":::details %s\n%s\n:::\n\n" summary content))
      ('qiita
       (format "<details><summary>%s</summary>\n\n%s\n\n</details>\n\n"
               (ox-hub--html-escape-string summary)
               content))
      (_ (error "Unsupported export target: %s" target)))))

(defun ox-hub--render-codefile-directive (node target parameters)
  "Render oxhub codefile directive NODE for TARGET using PARAMETERS."
  (let ((language (ox-hub--require-directive-parameter
                   parameters :lang "codefile"))
        (filename (ox-hub--require-directive-parameter
                   parameters :filename "codefile"))
        (content (ox-hub--raw-contents node target)))
    (pcase target
      ((or 'zenn 'qiita)
       (ox-hub--render-fenced-code-block
        (format "%s:%s" language filename)
        content))
      (_ (error "Unsupported export target: %s" target)))))

(defun ox-hub--raw-contents (node target)
  "Return raw contents of NODE, falling back to rendered contents for TARGET."
  (let ((begin (org-element-property :contents-begin node))
        (end (org-element-property :contents-end node))
        (buffer (org-element-property :buffer node)))
    (if (and begin end (buffer-live-p buffer))
        (with-current-buffer buffer
          (buffer-substring-no-properties begin end))
      (ox-hub--render-contents node target))))

(defun ox-hub--render-blockquote (content)
  "Render CONTENT as a Markdown blockquote."
  (concat (mapconcat (lambda (line)
                       (if (string-empty-p line)
                           ">"
                         (concat "> " line)))
                     (split-string (string-trim-right content) "\n")
                     "\n")
          "\n\n"))

;;; Export Paths and Existing Output Metadata

;; Resolve output locations and preserve fields owned by external publishing
;; tools when regenerating Markdown files.

(defun ox-hub--current-article-slug ()
  "Return the current Org article slug from `buffer-file-name'."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((slug (file-name-base buffer-file-name)))
    (unless (ox-hub--valid-slug-p slug)
      (user-error "Invalid article slug: %s" slug))
    slug))

(defun ox-hub--target-output-file (root slug target)
  "Return output path under ROOT for article SLUG and TARGET."
  (let ((directory (alist-get target ox-hub--target-output-directories)))
    (unless directory
      (error "Unsupported export target: %s" target))
    (expand-file-name (concat directory "/" slug ".md") root)))

(defun ox-hub--read-qiita-cli-metadata (file)
  "Read Qiita CLI managed metadata from Markdown FILE."
  (if (file-exists-p file)
      (let ((front-matter (ox-hub--read-markdown-front-matter file))
            metadata)
        (dolist (field ox-hub--qiita-cli-managed-fields)
          (let ((value (and front-matter
                            (ox-hub--front-matter-string-value
                             front-matter
                             (car field)))))
            (when value
              (setq metadata (plist-put metadata (cdr field) value)))))
        metadata)
    nil))

(defun ox-hub--read-markdown-front-matter (file)
  "Return Markdown front matter from FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (looking-at-p "---[ \t]*\n")
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^---[ \t]*$" nil t)
          (buffer-substring-no-properties start (line-beginning-position)))))))

(defun ox-hub--front-matter-string-value (front-matter field)
  "Return string value for FIELD from FRONT-MATTER."
  (with-temp-buffer
    (insert front-matter)
    (goto-char (point-min))
    (when (re-search-forward
           (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote field))
           nil
           t)
      (ox-hub--parse-yaml-string-scalar (match-string 1)))))

(defun ox-hub--parse-yaml-string-scalar (value)
  "Parse simple YAML string scalar VALUE."
  (let ((trimmed (string-trim (or value ""))))
    (cond
     ((or (string-empty-p trimmed)
          (member (downcase trimmed) '("null" "~")))
      "")
     ((and (>= (length trimmed) 2)
           (eq (aref trimmed 0) ?\")
           (eq (aref trimmed (1- (length trimmed))) ?\"))
      (ox-hub--unescape-yaml-double-quoted
       (substring trimmed 1 (1- (length trimmed)))))
     ((and (>= (length trimmed) 2)
           (eq (aref trimmed 0) ?')
           (eq (aref trimmed (1- (length trimmed))) ?'))
      (replace-regexp-in-string "''" "'" (substring trimmed 1 (1- (length trimmed))) t t))
     (t trimmed))))

(defun ox-hub--unescape-yaml-double-quoted (value)
  "Unescape the YAML double-quoted string VALUE subset ox-hub emits."
  (let ((result "")
        (index 0)
        char)
    (while (< index (length value))
      (setq char (aref value index))
      (if (and (eq char ?\\)
               (< (1+ index) (length value)))
          (let ((escaped (aref value (1+ index))))
            (setq result
                  (concat result
                          (pcase escaped
                            (?n "\n")
                            (?\" "\"")
                            (?\\ "\\")
                            (_ (char-to-string escaped)))))
            (setq index (+ index 2)))
        (setq result (concat result (char-to-string char)))
        (setq index (1+ index))))
    result))

;;; Document Assembly and Writing

;; Combine metadata and rendered body content, then write target Markdown files.

(defun ox-hub--render-front-matter (metadata target &optional output-file)
  "Render front matter from METADATA for TARGET.
OUTPUT-FILE is used when target-specific metadata should be preserved."
  (pcase target
    ('zenn (ox-hub--render-zenn-front-matter metadata))
    ('qiita (ox-hub--render-qiita-front-matter
             metadata
             (and output-file
                  (ox-hub--read-qiita-cli-metadata output-file))))
    (_ (error "Unsupported export target: %s" target))))

(defun ox-hub--render-document (ast target &optional output-file)
  "Render full Markdown document from Org AST for TARGET.
OUTPUT-FILE is used when target-specific metadata should be preserved."
  (let* ((metadata (ox-hub--validate-metadata
                    (ox-hub--extract-metadata ast)))
         (front-matter (ox-hub--render-front-matter metadata target output-file))
         (body (ox-hub--render-body ast target)))
    (concat front-matter "\n" body)))

(defun ox-hub--write-file (file content)
  "Write CONTENT to FILE, creating parent directories."
  (make-directory (file-name-directory file) t)
  (write-region content nil file nil 'silent))

(defun ox-hub--export-current-buffer-to-target (target)
  "Export current Org buffer to TARGET and return the output file."
  (let* ((slug (ox-hub--current-article-slug))
         (root (ox-hub--git-root))
         (ast (ox-hub--parse-buffer))
         (output-file (ox-hub--target-output-file root slug target))
         (content (ox-hub--render-document ast target output-file)))
    (ox-hub--write-file output-file content)
    (message "Exported %s: %s" target output-file)
    output-file))

;;; Public Commands

;; Interactive entry points used by package users.

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

;;;###autoload
(defun ox-hub-export-current-buffer ()
  "Export the current Org buffer to Zenn and Qiita Markdown."
  (interactive)
  (list (ox-hub--export-current-buffer-to-target 'zenn)
        (ox-hub--export-current-buffer-to-target 'qiita)))

;;;###autoload
(defun ox-hub-export-current-buffer-to-zenn ()
  "Export the current Org buffer to Zenn Markdown."
  (interactive)
  (ox-hub--export-current-buffer-to-target 'zenn))

;;;###autoload
(defun ox-hub-export-current-buffer-to-qiita ()
  "Export the current Org buffer to Qiita Markdown."
  (interactive)
  (ox-hub--export-current-buffer-to-target 'qiita))

(provide 'ox-hub)

;;; ox-hub.el ends here
