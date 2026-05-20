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

(ert-deftest ox-hub-parse-boolean-accepts-valid-values ()
  (should (eq (ox-hub--parse-boolean "true") t))
  (should (eq (ox-hub--parse-boolean "t") t))
  (should (eq (ox-hub--parse-boolean "TRUE") t))
  (should (eq (ox-hub--parse-boolean " false ") nil))
  (should (eq (ox-hub--parse-boolean "nil") nil))
  (should (eq (ox-hub--parse-boolean "NIL") nil)))

(ert-deftest ox-hub-parse-boolean-rejects-invalid-values ()
  (should-error (ox-hub--parse-boolean "yes")))

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
    (should (equal (plist-get metadata :tags) "emacs, org-mode"))
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

(provide 'ox-hub-test)

;;; ox-hub-test.el ends here
