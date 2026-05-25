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

(defun ox-hub-test--diagnostics (content)
  "Return ox-hub compatibility diagnostics for CONTENT."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (let ((ast (org-element-parse-buffer)))
      (ox-hub--compatibility-diagnostics ast))))

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

(defun ox-hub-test--valid-article-content ()
  "Return valid Org article content for export tests."
  (concat "#+OXHUB_TITLE: Example Title\n"
          "#+OXHUB_TAGS: emacs, org-mode\n"
          "#+OXHUB_STATUS: draft\n"
          "#+OXHUB_ZENN_EMOJI: memo\n"
          "#+OXHUB_ZENN_TYPE: tech\n"
          "#+OXHUB_QIITA_PRIVATE: false\n"
          "#+OXHUB_QIITA_SLIDE: false\n"
          "\n"
          "Hello *world*\n"))

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

(ert-deftest ox-hub-new-article-creates-from-non-file-buffer-default-directory ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root)))
      (with-temp-buffer
        (let ((default-directory root))
          (should (equal (ox-hub-new-article "valid_slug-12") article-file))))
      (should (file-exists-p article-file)))))

(ert-deftest ox-hub-new-article-rejects-non-git-default-directory ()
  (with-temp-buffer
    (let ((default-directory "/"))
      (should-error (ox-hub-new-article "valid_slug-12") :type 'user-error))))

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

(ert-deftest ox-hub-html-escape-string-escapes-special-characters ()
  (should (equal (ox-hub--html-escape-string "A < B & C > \"D\"")
                 "A &lt; B &amp; C &gt; &quot;D&quot;")))

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

(ert-deftest ox-hub-compatibility-diagnostics-warn-for-japanese-punctuation ()
  (let ((diagnostics
         (ox-hub-test--diagnostics
          "*bold*、/italic/、=code=、~verbatim~、+strike+\n")))
    (should (= (length diagnostics) 5))
    (dolist (diagnostic diagnostics)
      (should (eq (plist-get diagnostic :severity) 'warning))
      (should (equal (plist-get diagnostic :code) "OXHUB001"))
      (should (plist-get diagnostic :begin))
      (should (plist-get diagnostic :end))
      (should (plist-get diagnostic :line))
      (should (plist-get diagnostic :column))
      (should (equal (plist-get diagnostic :message)
                     ox-hub--compatibility-warning-message)))))

(ert-deftest ox-hub-compatibility-diagnostics-ignore-ascii-punctuation ()
  (should-not
   (ox-hub-test--diagnostics
    "*bold*, /italic/, =code=, ~verbatim~, +strike+\n")))

(ert-deftest ox-hub-compatibility-diagnostics-ignore-parsed-inline-elements ()
  (should-not
   (ox-hub-test--diagnostics
    "Text *bold* /italic/ =code= ~verbatim~ +strike+ text\n")))

(ert-deftest ox-hub-compatibility-diagnostics-ignore-code-like-blocks ()
  (should-not
   (ox-hub-test--diagnostics
    "#+begin_src text\n、*bold*\n#+end_src\n\n#+begin_example\n、/italic/\n#+end_example\n")))

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
                   "---\ntitle: \"Example Title\"\ntags:\n  - \"emacs\"\n  - \"org-mode\"\nprivate: true\nupdated_at: \"\"\nid: \"\"\norganization_url_name: \"\"\nslide: false\nignorePublish: true\n---\n"))))

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
                   "---\ntitle: \"Example Title\"\ntags:\n  - \"emacs\"\nprivate: false\nupdated_at: \"\"\nid: \"\"\norganization_url_name: \"\"\nslide: true\nignorePublish: false\n---\n"))))

(ert-deftest ox-hub-render-qiita-front-matter-preserves-cli-metadata ()
  (let ((metadata (ox-hub--validate-metadata
                   '(:title "Example Title"
                     :tags "emacs"
                     :status "draft"
                     :zenn-emoji "memo"
                     :zenn-type "tech"
                     :qiita-private "false"
                     :qiita-slide "false"))))
    (should (equal (ox-hub--render-qiita-front-matter
                    metadata
                    '(:qiita-updated-at "2026-05-22T00:00:00+09:00"
                      :qiita-id "abc123"
                      :qiita-organization-url-name "org-name"))
                   "---\ntitle: \"Example Title\"\ntags:\n  - \"emacs\"\nprivate: false\nupdated_at: \"2026-05-22T00:00:00+09:00\"\nid: \"abc123\"\norganization_url_name: \"org-name\"\nslide: false\nignorePublish: true\n---\n"))))

(ert-deftest ox-hub-read-qiita-cli-metadata-reads-existing-front-matter ()
  (let ((file (make-temp-file "ox-hub-qiita-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "---\n"
                    "title: \"Old\"\n"
                    "updated_at: \"2026-05-22T00:00:00+09:00\"\n"
                    "id: \"abc123\"\n"
                    "organization_url_name: \"org-name\"\n"
                    "---\n"
                    "\n"
                    "Body\n"))
          (should (equal (ox-hub--read-qiita-cli-metadata file)
                         '(:qiita-updated-at "2026-05-22T00:00:00+09:00"
                           :qiita-id "abc123"
                           :qiita-organization-url-name "org-name"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest ox-hub-read-qiita-cli-metadata-normalizes-null-values ()
  (let ((file (make-temp-file "ox-hub-qiita-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "---\n"
                    "updated_at: null\n"
                    "id:\n"
                    "organization_url_name: ~\n"
                    "---\n"))
          (should (equal (ox-hub--read-qiita-cli-metadata file)
                         '(:qiita-updated-at ""
                           :qiita-id ""
                           :qiita-organization-url-name ""))))
      (when (file-exists-p file)
        (delete-file file)))))

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

(ert-deftest ox-hub-render-body-renders-zenn-image-links-from-root ()
  (let ((ast (ox-hub-test--parse-string
              "Image [[file:images/sample.png]] and [[file:/images/root.png]]\n")))
    (should (equal (ox-hub--render-body ast 'zenn)
                   "Image ![](/images/sample.png) and ![](/images/root.png)\n"))))

(ert-deftest ox-hub-render-body-keeps-qiita-image-links-relative ()
  (let ((ast (ox-hub-test--parse-string
              "Image [[file:images/sample.png]]\n")))
    (should (equal (ox-hub--render-body ast 'qiita)
                   "Image ![](images/sample.png)\n"))))

(ert-deftest ox-hub-render-body-keeps-plain-file-links-relative-for-zenn ()
  (let ((ast (ox-hub-test--parse-string
              "Notes [[file:notes.org][Notes]]\n")))
    (should (equal (ox-hub--render-body ast 'zenn)
                   "Notes [Notes](notes.org)\n"))))

(ert-deftest ox-hub-render-body-renders-strike-through ()
  (let ((ast (ox-hub-test--parse-string
              "Text +deleted+ text\n")))
    (should (equal (ox-hub--render-body ast)
                   "Text ~~deleted~~ text\n"))))

(ert-deftest ox-hub-render-body-renders-horizontal-rule ()
  (let ((ast (ox-hub-test--parse-string
              "Before\n\n-----\n\nAfter\n")))
    (should (equal (ox-hub--render-body ast)
                   "Before\n\n-----\n\nAfter\n"))))

(ert-deftest ox-hub-render-body-renders-footnotes ()
  (let ((ast (ox-hub-test--parse-string
              "Footnote ref[fn:one].\n\n[fn:one] Footnote body\n")))
    (should (equal (ox-hub--render-body ast)
                   "Footnote ref[^one].\n\n[^one]: Footnote body\n"))))

(ert-deftest ox-hub-render-body-renders-code-blocks ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n\n#+begin_example\nplain text\n#+end_example\n")))
    (should (equal (ox-hub--render-body ast)
                   "```emacs-lisp\n(message \"hi\")\n```\n\n```\nplain text\n```\n"))))

(ert-deftest ox-hub-render-body-extends-code-fence-for-src-block ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_src text\n```\n#+end_src\n")))
    (should (equal (ox-hub--render-body ast)
                   "````text\n```\n````\n"))))

(ert-deftest ox-hub-render-body-extends-code-fence-for-example-block ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_example\n```\n#+end_example\n")))
    (should (equal (ox-hub--render-body ast)
                   "````\n```\n````\n"))))

(ert-deftest ox-hub-render-body-renders-mermaid-src-block ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_src mermaid\ngraph TD\n  A-->B\n#+end_src\n")))
    (should (equal (ox-hub--render-body ast)
                   "```mermaid\ngraph TD\n  A-->B\n```\n"))))

(ert-deftest ox-hub-render-body-renders-lists ()
  (let ((ast (ox-hub-test--parse-string
              "- one\n- two\n\n1. first\n2. second\n")))
    (should (equal (ox-hub--render-body ast)
                   "- one\n- two\n\n1. first\n2. second\n"))))

(ert-deftest ox-hub-render-body-renders-multi-paragraph-unordered-list-items ()
  (let ((ast (ox-hub-test--parse-string
              "- first paragraph\n\n  second paragraph\n")))
    (should (equal (ox-hub--render-body ast)
                   "- first paragraph\n\n    second paragraph\n"))))

(ert-deftest ox-hub-render-body-renders-multi-paragraph-ordered-list-items ()
  (let ((ast (ox-hub-test--parse-string
              "1. first paragraph\n\n   second paragraph\n")))
    (should (equal (ox-hub--render-body ast)
                   "1. first paragraph\n\n      second paragraph\n"))))

(ert-deftest ox-hub-render-body-renders-nested-unordered-lists ()
  (let ((ast (ox-hub-test--parse-string
              "- parent\n  - child\n")))
    (should (equal (ox-hub--render-body ast)
                   "- parent\n  - child\n"))))

(ert-deftest ox-hub-render-body-renders-nested-ordered-lists ()
  (let ((ast (ox-hub-test--parse-string
              "1. parent\n   1. child\n")))
    (should (equal (ox-hub--render-body ast)
                   "1. parent\n   1. child\n"))))

(ert-deftest ox-hub-render-body-renders-mixed-nested-lists ()
  (let ((ast (ox-hub-test--parse-string
              "- parent\n  1. child\n")))
    (should (equal (ox-hub--render-body ast)
                   "- parent\n  1. child\n"))))

(ert-deftest ox-hub-render-body-renders-deeply-nested-lists ()
  (let ((ast (ox-hub-test--parse-string
              "- grandparent\n  - parent\n    - child\n")))
    (should (equal (ox-hub--render-body ast)
                   "- grandparent\n  - parent\n    - child\n"))))

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

(ert-deftest ox-hub-render-body-renders-zenn-message-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub message\nInfo *body*\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'zenn)
                   ":::message\nInfo **body**\n:::\n"))))

(ert-deftest ox-hub-render-body-renders-qiita-message-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub message\nInfo *body*\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'qiita)
                   "> Info **body**\n"))))

(ert-deftest ox-hub-render-body-renders-zenn-alert-message-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub message :type alert\nAlert *body*\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'zenn)
                   ":::message alert\nAlert **body**\n:::\n"))))

(ert-deftest ox-hub-render-body-renders-qiita-alert-message-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub message :type alert\nAlert *body*\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'qiita)
                   "> **Warning:**\n>\n> Alert **body**\n"))))

(ert-deftest ox-hub-render-body-renders-zenn-details-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub details :summary \"Summary text\"\nDetails *body*\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'zenn)
                   ":::details Summary text\nDetails **body**\n:::\n"))))

(ert-deftest ox-hub-render-body-renders-qiita-details-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub details :summary \"Summary text\"\nDetails *body*\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'qiita)
                   "<details><summary>Summary text</summary>\n\nDetails **body**\n\n</details>\n"))))

(ert-deftest ox-hub-render-body-escapes-qiita-details-summary ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub details :summary \"A < B & C > D\"\nDetails body\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'qiita)
                   "<details><summary>A &lt; B &amp; C &gt; D</summary>\n\nDetails body\n\n</details>\n"))))

(ert-deftest ox-hub-render-body-renders-codefile-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub codefile :lang emacs-lisp :filename init.el\n(message \"hi\")\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'zenn)
                   "```emacs-lisp:init.el\n(message \"hi\")\n```\n"))
    (should (equal (ox-hub--render-body ast 'qiita)
                   "```emacs-lisp:init.el\n(message \"hi\")\n```\n"))))

(ert-deftest ox-hub-render-body-extends-code-fence-for-codefile-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub codefile :lang text :filename f.txt\n```\n#+end_oxhub\n")))
    (should (equal (ox-hub--render-body ast 'zenn)
                   "````text:f.txt\n```\n````\n"))
    (should (equal (ox-hub--render-body ast 'qiita)
                   "````text:f.txt\n```\n````\n"))))

(ert-deftest ox-hub-render-body-rejects-oxhub-directive-without-target ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub message\nInfo\n#+end_oxhub\n")))
    (should-error (ox-hub--render-body ast))))

(ert-deftest ox-hub-render-body-rejects-unsupported-oxhub-directive ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub unknown\nInfo\n#+end_oxhub\n")))
    (should-error (ox-hub--render-body ast 'zenn))))

(ert-deftest ox-hub-render-body-rejects-invalid-message-type ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub message :type warning\nInfo\n#+end_oxhub\n")))
    (should-error (ox-hub--render-body ast 'zenn))))

(ert-deftest ox-hub-render-body-rejects-missing-details-summary ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub details\nInfo\n#+end_oxhub\n")))
    (should-error (ox-hub--render-body ast 'zenn))))

(ert-deftest ox-hub-render-body-rejects-missing-codefile-parameter ()
  (let ((ast (ox-hub-test--parse-string
              "#+begin_oxhub codefile :lang emacs-lisp\n(message \"hi\")\n#+end_oxhub\n")))
    (should-error (ox-hub--render-body ast 'zenn))))

(ert-deftest ox-hub-current-article-slug-uses-file-basename ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file)
      (with-current-buffer (find-file-noselect article-file)
        (should (equal (ox-hub--current-article-slug) "valid_slug-12"))))))

(ert-deftest ox-hub-target-output-file-resolves-target-paths ()
  (should (equal (ox-hub--target-output-file "/repo" "valid_slug-12" 'zenn)
                 "/repo/articles/valid_slug-12.md"))
  (should (equal (ox-hub--target-output-file "/repo" "valid_slug-12" 'qiita)
                 "/repo/public/valid_slug-12.md"))
  (should-error (ox-hub--target-output-file "/repo" "valid_slug-12" 'other)))

(ert-deftest ox-hub-target-output-directories-match-cli-root-layout ()
  (should (equal (alist-get 'zenn ox-hub--target-output-directories)
                 "articles"))
  (should (equal (alist-get 'qiita ox-hub--target-output-directories)
                 "public")))

(ert-deftest ox-hub-export-current-buffer-to-zenn-writes-markdown ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root))
          (output-file (expand-file-name "articles/valid_slug-12.md" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content)))
      (with-current-buffer (find-file-noselect article-file)
        (should (equal (ox-hub-export-current-buffer-to-zenn) output-file)))
      (should (equal (ox-hub-test--read-file-string output-file)
                     "---\ntitle: \"Example Title\"\nemoji: \"memo\"\ntype: \"tech\"\ntopics: [\"emacs\", \"org-mode\"]\npublished: false\n---\n\nHello **world**\n")))))

(ert-deftest ox-hub-export-current-buffer-to-qiita-writes-markdown ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root))
          (output-file (expand-file-name "public/valid_slug-12.md" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content)))
      (with-current-buffer (find-file-noselect article-file)
        (should (equal (ox-hub-export-current-buffer-to-qiita) output-file)))
      (should (equal (ox-hub-test--read-file-string output-file)
                     "---\ntitle: \"Example Title\"\ntags:\n  - \"emacs\"\n  - \"org-mode\"\nprivate: false\nupdated_at: \"\"\nid: \"\"\norganization_url_name: \"\"\nslide: false\nignorePublish: true\n---\n\nHello **world**\n")))))

(ert-deftest ox-hub-export-current-buffer-to-qiita-preserves-cli-metadata ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root))
          (output-file (expand-file-name "public/valid_slug-12.md" root)))
      (make-directory (file-name-directory article-file) t)
      (make-directory (file-name-directory output-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content)))
      (with-temp-file output-file
        (insert "---\n"
                "title: \"Old Title\"\n"
                "updated_at: \"2026-05-22T00:00:00+09:00\"\n"
                "id: \"abc123\"\n"
                "organization_url_name: \"org-name\"\n"
                "---\n"
                "\n"
                "Old body\n"))
      (with-current-buffer (find-file-noselect article-file)
        (should (equal (ox-hub-export-current-buffer-to-qiita) output-file)))
      (should (string-match-p
               (regexp-quote
                "updated_at: \"2026-05-22T00:00:00+09:00\"\nid: \"abc123\"\norganization_url_name: \"org-name\"")
               (ox-hub-test--read-file-string output-file))))))

(ert-deftest ox-hub-export-current-buffer-writes-both-targets ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root))
          (zenn-file (expand-file-name "articles/valid_slug-12.md" root))
          (qiita-file (expand-file-name "public/valid_slug-12.md" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content)))
      (with-current-buffer (find-file-noselect article-file)
        (should (equal (ox-hub-export-current-buffer)
                       (list zenn-file qiita-file))))
      (should (file-exists-p zenn-file))
      (should (file-exists-p qiita-file)))))

(ert-deftest ox-hub-export-current-buffer-rejects-lint-warnings ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root))
          (zenn-file (expand-file-name "articles/valid_slug-12.md" root))
          (qiita-file (expand-file-name "public/valid_slug-12.md" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content))
        (insert "\n、*raw-bold*\n"))
      (with-current-buffer (find-file-noselect article-file)
        (should (ox-hub--compatibility-diagnostics))
        (should-error (ox-hub-export-current-buffer) :type 'user-error))
      (should-not (file-exists-p zenn-file))
      (should-not (file-exists-p qiita-file)))))

(ert-deftest ox-hub-export-current-buffer-overwrites-existing-output ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root))
          (output-file (expand-file-name "articles/valid_slug-12.md" root)))
      (make-directory (file-name-directory article-file) t)
      (make-directory (file-name-directory output-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content)))
      (with-temp-file output-file
        (insert "old content\n"))
      (with-current-buffer (find-file-noselect article-file)
        (ox-hub-export-current-buffer-to-zenn))
      (should-not (equal (ox-hub-test--read-file-string output-file)
                         "old content\n")))))

(ert-deftest ox-hub-export-current-buffer-renders-target-specific-directives ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/valid_slug-12.org" root))
          (zenn-file (expand-file-name "articles/valid_slug-12.md" root))
          (qiita-file (expand-file-name "public/valid_slug-12.md" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content))
        (insert "\n#+begin_oxhub message :type alert\nCareful\n#+end_oxhub\n"))
      (with-current-buffer (find-file-noselect article-file)
        (ox-hub-export-current-buffer))
      (should (string-match-p ":::message alert\nCareful\n:::"
                              (ox-hub-test--read-file-string zenn-file)))
      (should (string-match-p "> \\*\\*Warning:\\*\\*\n>\n> Careful"
                              (ox-hub-test--read-file-string qiita-file))))))

(ert-deftest ox-hub-export-current-buffer-rejects-invalid-slug ()
  (ox-hub-test--with-temp-git-root (root source-file)
    (let ((article-file (expand-file-name "org/Invalid_slug1.org" root)))
      (make-directory (file-name-directory article-file) t)
      (with-temp-file article-file
        (insert (ox-hub-test--valid-article-content)))
      (with-current-buffer (find-file-noselect article-file)
        (should-error (ox-hub-export-current-buffer-to-zenn) :type 'user-error)))))

(provide 'ox-hub-test)

;;; ox-hub-test.el ends here
