;;; discord-emoji.el --- Discord emoji insertion -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Sridaran Thoniyil
;;
;; Author: Sridaran Thoniyil <sri7thon@gmail.com>
;; Maintainer: Sridaran Thoniyil <sri7thon@gmail.com>
;; Created: December 18, 2023
;; Modified: December 18, 2023
;; Version: 0.0.1
;; Keywords: multimedia
;; Homepage: https://github.com/sridaran/discord-emoji.el
;; Package-Requires: ((emacs "26.1") (f "0.20.0") (json "1.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The main function, `discord-emoji-insert', prompts the user using
;; `completing-read', allowing them to select Emojis using Discord's shortcodes,
;; and inserts the selected emoji into the document.
;;
;;; Code:

(defface discord-emoji-category
  `((default . (:inherit 'font-lock-keyword-face)))
  "Face used to render the `completing-read' /category/ annotation.")

(defvar discord-emoji--repository-path
  (or load-file-name buffer-file-name)
  "Base path for the repository, used to find the JSON file path.")

(defvar discord-emoji--definitions
  nil
  "List of mappings from emoji shortcodes to raw characters and metadata.")

(defvar discord-emoji--annotation-start-column
  60
  "Start column for annotations for the `completing-read' interface
in `discord-emoji-insert'.")

(defun discord-emoji--load-data ()
  (let* ((script-path discord-emoji--repository-path)
         (json-path (f-join (file-name-directory script-path) "emojis.json"))
         (json (json-read-file json-path))
         (emoji-defs (alist-get 'emojiDefinitions json))
         (parse-emoji-def (lambda (obj)
                            (let ((primaryName (alist-get 'primaryName obj))
                                  (surrogates (alist-get 'surrogates obj))
                                  (category (alist-get 'category obj)))
                              `(,(format "%s (%s)" primaryName surrogates)
                                (emoji . ,surrogates)
                                (category . ,category)))))
         (preprocessed-defs (seq-map parse-emoji-def emoji-defs)))
    (setq discord-emoji--definitions preprocessed-defs)
    nil))

(defun discord-emoji--completing-read-annotate (candidate)
  (when-let ((candidate-width (string-width candidate))
             (annotation-start-column discord-emoji--annotation-start-column)
             (spaces-to-fill (- annotation-start-column candidate-width))
             (category (alist-get 'category (alist-get candidate discord-emoji--definitions nil nil #'string-equal)))
             (annotation-string (propertize (format "%s%s" (make-string spaces-to-fill ?\s) category)
                                            'face
                                            ;; see `list-faces-display'
                                            'discord-emoji-category)))
    annotation-string))

(defun discord-emoji--completing-read ()
  (completing-read "Insert Emoji: "
                   (lambda (str filter-function flag)
                     (cl-case flag
                       (metadata
                        `(metadata .
                          ((annotation-function . discord-emoji--completing-read-annotate))))
                       (t (all-completions str discord-emoji--definitions filter-function))))))

(defun discord-emoji-insert ()
  (interactive)
  (unless (boundp 'discord-emoji--definitions)
    (discord-emoji--load-data))
  (when-let* ((selection (discord-emoji--completing-read))
              (value (alist-get selection discord-emoji--definitions nil nil #'string-equal))
              (emoji (alist-get 'emoji value)))
    (insert emoji)))

(provide 'discord-emoji)
;;; discord-emoji.el ends here
