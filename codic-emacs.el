;;; codic --- ja->en dictionary for naming functions/variables

;; Author: tsukimizake <shomasd_at_gmail.com>
;; Version: 0.1
;; Package-Requires: ((request "0.2.0") (cl-lib "0.5"))
;; Keywords: languages
;; URL: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; make english names using codic.jp

;;; 使い方
;; 1: https://codic.jp/signup の指示に従って codic.jp にサインアップします。
;; 2: https://codic.jp/my/api_status にアクセスし、アクセストークンをコピーします。
;; 3: Emacsで M-x codic-set-access-token を呼び、アクセストークンをセットします。
;; 4: M-x codic-translate M-x codic-translate-dwim などが使えるようになります。


;;; Code:
(require 'request)
(require 'json)
(require 'cl-lib)


;; access token
(defvar codic--access-token nil)

(defun codic-show-access-token ()
  (interactive)
  (message (format "Access Token is %S ." codic--access-token)))

(defun codic-set-access-token (str)
  (interactive "sAccessToken:")
  (setq codic--access-token str)
  (codic-show-access-token))


;; request
(cl-defun codic-request (&key access_token text project_id casing acronym_style on_success)
  (let ((auth (format "Bearer %s" access_token)))
    (request "https://api.codic.jp/v1/engine/translate.json"
	     :type "GET"
	     :parser 'json-read
	     :headers `(("Authorization" . ,auth))
	     :params `(("text" . ,text)
		       ;; (project_id . ,project_id)
		       ;; (casing . ,casing)
		       ;; (acronym_style . ,acronym_style)
		       )
	     :error (cl-function
		     (lambda (&key data &allow-other-keys)
		       (let ((res (format "%S" data)))
			 (codic-parse-error res))))
	     
	     :success (cl-function
		       (lambda (&key data &allow-other-keys)
			 (let ((res (format "%S" data)))
			   (funcall on_success (format "%s" (codic-parse-succeeded res))))))
	     )))

;; example
;; (codic-request :access_token codic--access-token :text "こんにちわ世界")


;;; parse

;; parse succeeded results
(defun codic--read-res-succeeded (data)
  (aref (read data) 0))

(defun codic-parse-succeeded (data)
  (let* ((res (codic--read-res-succeeded data))
	 (condition (assoc 'successful res))
	 (text (cdr (assoc 'translated_text res))))
    text
    ))

;;parse error codes
(defun codic--get-code-from-err (err)
  (cdr (assoc 'code err)))
(defun codic--get-message-from-err (err)
  (cdr (assoc 'message err)))
(defun codic--get-context-from-err (err)
  (cdr (assoc 'context err)))

(defun codic--format-err (err)
  (format "ERROR! codic failed to translate. code: %S message: %S context: %S" (codic--get-context-from-err err) (codic--get-message-from-err err) (codic--get-context-from-err err)))

(defun codic-parse-error (data)
  (let* ((res data)
	 (errs (cdr (assoc 'errors res))))
    (cl-loop for err across errs
	     collect (codic--format-err err))
    ))



;; interface
(defun codic-translate (str)
  "入力したクエリを翻訳します。"
  (interactive "sQuery:")
  (codic-request :access_token codic--access-token :text str :on_success 'insert))

(defun codic-translate-dwim ()
  "リージョンがアクティブならその文章を、そうでなければ入力した文を翻訳します。"
  (interactive)
  (if (region-active-p)
      (codic-translate (buffer-substring (region-beginning) (region-end)))
    (call-interactively 'codic-translate))
  )




(provide 'codic)
;;; codic.el ends here
