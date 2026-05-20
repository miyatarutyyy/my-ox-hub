;;; ox-hub-test.el --- Tests for ox-hub -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'org-element)
(require 'ox-hub)

(defun ox-hub-test--parse-string (content)
  "Parse CONTENT as Org and return its AST."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (org-element-parse-buffer)))

(defun ox-hub-test--valid-metadata ()
  "Return valid raw metadata for tests."
  '(:title "Example Title"
    :tags "emacs, org-mode"
    :status "draft"
    :zenn-emoji "memo"
    :zenn-type "tech"
    :qiita-private "false"
    :qiita-slide "nil"))

(defmacro ox-hub-test--with-temp-git-root (bindings &rest body)
  "Create a temporary Git ROOT and evaluate BODY from SOURCE-FILE."
  (declare (indent 1))
  (let ((root (car bindings))
        (source-file (cadr bindings)))
    `(let* ((,root (make-temp-file "ox-hub-test-" t))
            (,source-file (expand-file-name "current.org" ,root)))
       (unwind-protect
           (progn
             (make-directory (expand-file-name ".git" ,root))
             (with-temp-file ,source-file)
             (with-current-buffer (find-file-noselect ,source-file)
               ,@body))
         (dolist (buffer (buffer-list))
           (let ((file (buffer-file-name buffer)))
             (when (and file (file-in-directory-p file ,root))
               (kill-buffer buffer))))
         (when (file-directory-p ,root)
           (delete-directory ,root t))))))

(defun ox-hub-test--read-file-string (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest ox-hub-new-article-template-returns-default-metadata ()
  (should (equal (ox-hub--new-article-template)
                 "#+OXHUB_TITLE:\n#+OXHUB_TAGS:\n#+OXHUB_STATUS: draft\n#+OXHUB_ZENN_EMOJI: 📝\n#+OXHUB_ZENN_TYPE: tech\n#+OXHUB_QIITA_PRIVATE: false\n#+OXHUB_QIITA_SLIDE: false\n\n")))

(ert-deftest ox-hub-valid-slug-p-accepts-valid-slugs ()
  (should (ox-hub--valid-slug-p "valid_slug-12"))
  (should (ox-hub--valid-slug-p (make-string 50 ?a))))

(ert-deftest ox-hub-valid-slug-p-rejects-invalid-slugs ()
  (should-not (ox-hub--valid-slug-p "short_slug1"))
  (should-not (ox-hub--valid-slug-p (make-string 51 ?a)))
  (should-not (ox-hub--valid-slug-p "Invalid_slug1"))
  (should-not (ox-hub--valid-slug-p "invalid slug1"))
  (should-not (ox-hub--valid-slug-p "invalid.slug1"))
  (should-not (ox-hub--valid-slug-p "invalid/slug1")))

(ert-deftest ox-hub-new-article-creates-org-file-and-opens-it ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let* ((slug "valid_slug-12")
           (article-file (expand-file-name "org/valid_slug-12.org" root))
           (created-file (ox-hub-new-article slug)))
      (should (equal created-file article-file))
      (should (file-exists-p article-file))
      (should (equal (buffer-file-name) article-file))
      (should (equal (ox-hub-test--read-file-string article-file)
                     (ox-hub--new-article-template))))))

(ert-deftest ox-hub-new-article-creates-org-directory ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((org-dir (expand-file-name "org" root)))
      (should-not (file-directory-p org-dir))
      (ox-hub-new-article "valid_slug-12")
      (should (file-directory-p org-dir)))))

(ert-deftest ox-hub-new-article-rejects-invalid-slug ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (should-error (ox-hub-new-article "Invalid slug") :type 'user-error)
    (should-not (file-directory-p (expand-file-name "org" root)))))

(ert-deftest ox-hub-new-article-rejects-existing-file ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file
        (insert "existing content\n"))
      (should-error (ox-hub-new-article "valid_slug-12") :type 'user-error)
      (should (equal (ox-hub-test--read-file-string article-file)
                     "existing content\n")))))

(ert-deftest ox-hub-new-article-rejects-non-file-buffer ()
  (with-temp-buffer
    (should-error (ox-hub-new-article "valid_slug-12") :type 'user-error)))

(ert-deftest ox-hub-parse-boolean-accepts-valid-values ()
  (should (eq (ox-hub--parse-boolean "true") t))
  (should (eq (ox-hub--parse-boolean "t") t))
  (should (eq (ox-hub--parse-boolean "TRUE") t))
  (should (eq (ox-hub--parse-boolean " false ") nil))
  (should (eq (ox-hub--parse-boolean "nil") nil))
  (should (eq (ox-hub--parse-boolean "NIL") nil)))

(ert-deftest ox-hub-parse-boolean-rejects-invalid-values ()
  (should-error (ox-hub--parse-boolean "yes")))

(ert-deftest ox-hub-parse-tags-normalizes-comma-separated-tags ()
  (should (equal (ox-hub--parse-tags " emacs, org-mode ,, lisp ")
                 '("emacs" "org-mode" "lisp"))))

(ert-deftest ox-hub-parse-tags-rejects-empty-tags ()
  (should-error (ox-hub--parse-tags " , , ")))

(ert-deftest ox-hub-yaml-escape-string-escapes-special-characters ()
  (should (equal (ox-hub--yaml-escape-string "a\\b\"c\nd")
                 "a\\\\b\\\"c\\nd")))

(ert-deftest ox-hub-extract-metadata-reads-oxhub-keywords ()
  (let* ((ast (ox-hub-test--parse-string
               "#+OXHUB_TITLE: Example\n#+OXHUB_STATUS: draft\n#+AUTHOR: Someone\n"))
         (metadata (ox-hub--extract-metadata ast)))
    (should (equal (plist-get metadata :title) "Example"))
    (should (equal (plist-get metadata :status) "draft"))
    (should-not (plist-member metadata :author))))

(ert-deftest ox-hub-extract-metadata-normalizes-lowercase-keywords ()
  (let* ((ast (ox-hub-test--parse-string "#+oxhub_title: Example\n"))
         (metadata (ox-hub--extract-metadata ast)))
    (should (equal (plist-get metadata :title) "Example"))))

(ert-deftest ox-hub-extract-metadata-keeps-last-duplicate-value ()
  (let* ((ast (ox-hub-test--parse-string
               "#+OXHUB_TITLE: First\n#+OXHUB_TITLE: Second\n"))
         (metadata (ox-hub--extract-metadata ast)))
    (should (equal (plist-get metadata :title) "Second"))))

(ert-deftest ox-hub-validate-metadata-normalizes-valid-metadata ()
  (let ((metadata (ox-hub--validate-metadata
                   '(:title " Example Title "
                     :tags " emacs, org-mode "
                     :status "DRAFT"
                     :zenn-emoji " memo "
                     :zenn-type "TECH"
                     :qiita-private "FALSE"
                     :qiita-slide "t"))))
    (should (equal (plist-get metadata :title) "Example Title"))
    (should (equal (plist-get metadata :tags) '("emacs" "org-mode")))
    (should (equal (plist-get metadata :status) "draft"))
    (should (equal (plist-get metadata :zenn-emoji) "memo"))
    (should (equal (plist-get metadata :zenn-type) "tech"))
    (should (eq (plist-get metadata :qiita-private) nil))
    (should (eq (plist-get metadata :qiita-slide) t))))

(ert-deftest ox-hub-validate-metadata-rejects-missing-required-key ()
  (let ((metadata (copy-sequence (ox-hub-test--valid-metadata))))
    (setq metadata (plist-put metadata :title nil))
    (should-error (ox-hub--validate-metadata metadata))))

(ert-deftest ox-hub-validate-metadata-rejects-empty-required-value ()
  (let ((metadata (copy-sequence (ox-hub-test--valid-metadata))))
    (setq metadata (plist-put metadata :title " "))
    (should-error (ox-hub--validate-metadata metadata))))

(ert-deftest ox-hub-validate-metadata-rejects-invalid-status ()
  (let ((metadata (copy-sequence (ox-hub-test--valid-metadata))))
    (setq metadata (plist-put metadata :status "ready"))
    (should-error (ox-hub--validate-metadata metadata))))

(ert-deftest ox-hub-validate-metadata-rejects-invalid-zenn-type ()
  (let ((metadata (copy-sequence (ox-hub-test--valid-metadata))))
    (setq metadata (plist-put metadata :zenn-type "memo"))
    (should-error (ox-hub--validate-metadata metadata))))

(ert-deftest ox-hub-validate-metadata-rejects-invalid-qiita-boolean ()
  (let ((metadata (copy-sequence (ox-hub-test--valid-metadata))))
    (setq metadata (plist-put metadata :qiita-private "no"))
    (should-error (ox-hub--validate-metadata metadata))))

(ert-deftest ox-hub-validate-metadata-rejects-empty-tags ()
  (let ((metadata (copy-sequence (ox-hub-test--valid-metadata))))
    (setq metadata (plist-put metadata :tags " , , "))
    (should-error (ox-hub--validate-metadata metadata))))

(ert-deftest ox-hub-render-zenn-front-matter-renders-draft ()
  (let ((metadata (ox-hub--validate-metadata
                   '(:title "Example \"Title\""
                     :tags "emacs, org-mode"
                     :status "draft"
                     :zenn-emoji "memo"
                     :zenn-type "tech"
                     :qiita-private "false"
                     :qiita-slide "false"))))
    (should (equal (ox-hub--render-zenn-front-matter metadata)
                   "---\ntitle: \"Example \\\"Title\\\"\"\nemoji: \"memo\"\ntype: \"tech\"\ntopics: [\"emacs\", \"org-mode\"]\npublished: false\n---\n"))))

(ert-deftest ox-hub-render-zenn-front-matter-renders-published ()
  (let ((metadata (ox-hub--validate-metadata
                   '(:title "Example Title"
                     :tags "emacs"
                     :status "published"
                     :zenn-emoji "memo"
                     :zenn-type "idea"
                     :qiita-private "false"
                     :qiita-slide "false"))))
    (should (string-match-p "^published: true$"
                            (ox-hub--render-zenn-front-matter metadata)))))

(ert-deftest ox-hub-render-qiita-front-matter-renders-draft ()
  (let ((metadata (ox-hub--validate-metadata
                   '(:title "Example Title"
                     :tags "emacs, org-mode"
                     :status "draft"
                     :zenn-emoji "memo"
                     :zenn-type "tech"
                     :qiita-private "true"
                     :qiita-slide "false"))))
    (should (equal (ox-hub--render-qiita-front-matter metadata)
                   "---\ntitle: \"Example Title\"\ntags:\n  - \"emacs\"\n  - \"org-mode\"\nprivate: true\nslide: false\nignorePublish: true\n---\n"))))

(ert-deftest ox-hub-render-qiita-front-matter-renders-published ()
  (let ((metadata (ox-hub--validate-metadata
                   '(:title "Example Title"
                     :tags "emacs"
                     :status "published"
                     :zenn-emoji "memo"
                     :zenn-type "tech"
                     :qiita-private "false"
                     :qiita-slide "true"))))
    (should (equal (ox-hub--render-qiita-front-matter metadata)
                   "---\ntitle: \"Example Title\"\ntags:\n  - \"emacs\"\nprivate: false\nslide: true\nignorePublish: false\n---\n"))))

(ert-deftest ox-hub-render-body-skips-keywords ()
  (let ((ast (ox-hub-test--parse-string
              "#+OXHUB_TITLE: Example\n#+TITLE: Ignored\n\nBody text\n")))
    (should (equal (ox-hub--render-body ast)
                   "Body text\n"))))

(ert-deftest ox-hub-render-body-renders-headings-and-paragraphs ()
  (let ((ast (ox-hub-test--parse-string
              "* Heading\nParagraph text\n\n** Child\nMore text\n")))
    (should (equal (ox-hub--render-body ast)
                   "# Heading\n\nParagraph text\n\n## Child\n\nMore text\n"))))

(ert-deftest ox-hub-render-body-renders-inline-markup-and-links ()
  (let ((ast (ox-hub-test--parse-string
              "Text *bold* /italic/ =code= ~verbatim~ [[https://example.com][Example]] [[file:notes.org][Notes]] [[file:image.png]]\n")))
    (should (equal (ox-hub--render-body ast)
                   "Text **bold** *italic* `code` `verbatim` [Example](https://example.com) [Notes](notes.org) ![](image.png)\n"))))

(ert-deftest ox-hub-render-body-renders-code-blocks ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n\n#+begin_example\nplain text\n#+end_example\n")))
    (should (equal (ox-hub--render-body ast)
                   "```emacs-lisp\n(message \"hi\")\n```\n\n```\nplain text\n```\n"))))

(ert-deftest ox-hub-render-body-renders-lists ()
  (let ((ast (ox-hub-test--parse-string
              "- one\n- two\n\n1. first\n2. second\n")))
    (should (equal (ox-hub--render-body ast)
                   "- one\n- two\n\n1. first\n2. second\n"))))

(ert-deftest ox-hub-render-body-renders-quote-block ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_quote\nquoted *text*\nsecond line\n#+end_quote\n")))
    (should (equal (ox-hub--render-body ast)
                   "> quoted **text**\n> second line\n"))))

(ert-deftest ox-hub-render-body-renders-table ()
  (let ((ast (ox-hub-test--parse-string
              "| Name | Value |\n|------+-------|\n| one  |     1 |\n| two  |     2 |\n")))
    (should (equal (ox-hub--render-body ast)
                   "| Name | Value |\n| --- | --- |\n| one | 1 |\n| two | 2 |\n"))))

(ert-deftest ox-hub-render-body-rejects-unsupported-elements ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_note\nUnsupported\n#+end_note\n")))
    (should-error (ox-hub--render-body ast))))

(provide 'ox-hub-test)

;;; ox-hub-test.el ends here
