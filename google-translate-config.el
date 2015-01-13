(require 'popup)
(require 'chinese-word-at-point)
(require 'google-translate)

(setq google-translate-base-url
      "http://translate.google.cn/translate_a/t")

(setq google-translate-listen-url
      "http://translate.google.cn/translate_tts")

(defun google-translate-chinese--region-or-word ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'chinese-or-other-word t)))

(defun google-translate-chinese--translate (word)
  (google-translate-json-translation
   (if (chinese-word-cjk-string-p word)
       (google-translate-request "zh-CN" "en" word)
     (google-translate-request "en" "zh-CN" word))))

(defun google-translate-chinese--detailed-translate (word)
  (google-translate-json-detailed-translation
   (if (chinese-word-cjk-string-p word)
       (google-translate-request "zh-CN" "en" word)
     (google-translate-request "en" "zh-CN" word))))

(defun concat-array-as-string (v)
  "Concat strings in v to a whole string."
  (let ((index 0) (str ""))
    (while (< index (length v))
      (setq str (concat str (elt v index) " "))
      (cl-incf index))
    (substring str 0 (1- (length str)))))

(defun google-translate-chinese-at-point ()
  "Translate at point and show full result with buffer."
  (interactive)
  (let ((word (google-translate-chinese--region-or-word)))
    (if word
        (if (chinese-word-cjk-string-p word)
            (google-translate-translate "zh-CN" "en" word)
          (google-translate-translate "en" "zh-CN" word))
      (message "Nothing to translate"))))

(defun google-translate-chinese-at-point-echo-area ()
  "Translate at point and show only translation in echo area. "
  (interactive)
  (let ((word (google-translate-chinese--region-or-word)))
    (if word
        (message (google-translate-chinese--translate word))
      (message "Nothing to translate"))))

(defun google-translate-chinese-query ()
  "Translate input and show full result with buffer."
  (interactive)
  (let* ((word (google-translate-chinese--region-or-word))
         (word (read-string (format "Translate (%s): "
                                    (or word ""))
                            nil nil
                            word)))
    (if word
        (if (chinese-word-cjk-string-p word)
            (google-translate-translate "zh-CN" "en" word)
          (google-translate-translate "en" "zh-CN" word))
      (message "Nothing to translate"))))

(defun google-translate-chinese--detailed-translate (word)
  (google-translate-json-detailed-translation
   (if (chinese-word-cjk-string-p word)
       (google-translate-request "zh-CN" "en" word)
     (google-translate-request "en" "zh-CN" word))))

(defun google-translate-search-at-point-and-replace ()
  "可以不写 DocString."
  (interactive)
  (let ((word (google-translate-chinese--region-or-word)))
    (if word
        (let (explains
              popup-list
              (detailed-translate (google-translate-chinese--detailed-translate word)))
          (loop for item across detailed-translate do
                (let ((index 0))
                  (unless (string-equal (aref item 0) "")
                    (loop for translation across (aref item 1) do
                          (push (format "%d. %s" (incf index) translation) popup-list)
                          (push (concat "<" (aref item 0) "> "
                                        (concat-array-as-string (elt (elt (aref item 2) (1- index)) 1)))
                                popup-list)))))

          (setq popup-list (reverse popup-list))

          (let ((index 0) (a-popup-menu nil) selected-item)
            (while (< index (length popup-list))
              (push (popup-make-item (nth index popup-list)
                                     :summary (nth (1+ index) popup-list))
                    a-popup-menu)
              (setq index (+ index 2)))
            (setq selected-item (popup-menu* (reverse a-popup-menu)))
            (delete-char (- 0 (length word)))
            (insert (substring selected-item 3))))
      (message "Nothing to translate"))))
