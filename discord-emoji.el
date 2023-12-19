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
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The main function, `discord-emoji-insert', inserts an emoji into the
;; document.
;;
;;; Code:

(defun discord-emoji--load-data ()
  (let* ((script-path (or load-true-file-name buffer-file-name))
         (json-path (f-join (file-name-directory script-path) "discordEmojiMap-canary.min.json"))
         (json (json-read-file json-path))
         (emoji-defs (alist-get 'emojiDefinitions json))
         (parse-emoji-def (lambda (obj)
                            (map-let (primaryName surrogates category) obj
                              `(,(format "%s (%s)" primaryName surrogates)
                                (primary-name . ,primaryName)
                                (emoji . ,surrogates)
                                (category . ,category)))))
         (preprocessed-defs (seq-map parse-emoji-def emoji-defs)))
    (setq discord-emoji--definitions preprocessed-defs)
    nil))

(defun discord-emoji-insert ()
  (interactive)
  (unless (boundp 'discord-emoji--definitions)
    (discord-emoji--load-data))
  (when-let* ((selection (completing-read "Insert Emoji:" discord-emoji--definitions))
              (value (alist-get selection discord-emoji--definitions nil nil #'string-equal))
              (emoji (alist-get 'emoji value)))
    (insert emoji)))

(provide 'discord-emoji)
;;; discord-emoji.el ends here
