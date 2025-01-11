;;; m4a-browse-name.el -- m4a file name browse title.
;; Copyright (C) 2023, 2024, 2025 fubuki

;; Author: fubuki at frill.org
;; Version: @(#)$Revision: 1.22 $$Name:  $
;; Keywords: multimedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Synchronize m4a or flac file with dired to list in buffer.

;;; Installation:

;; Need the fm.el(github.com/vapniks/fm) and wtag.el(github.com/s-fubuki/wtag) packages.

;; (require 'm4a-browse-name)

;;; Code:
(require 'tabulated-list)
(require 'dired)
(require 'wtag)
(require 'fm)

(defgroup m4a-browse-name nil
  "m4a browse name."
  :group   'music-file
  :version "30.0.50"
  :prefix  "m4a-")

(defcustom m4a-entries
  '((mark "M" 1 t) (file "File" 13 t) (time-stamp "TimeStamp" 16 t)
    (artist "Artist" 16 t) (title "Title" 32 t)
    (time "Time" 7 tabulated-list-entry-time>)
    (bitrate "BR" 4 tabulated-list-entry-bitrate>))
  "((list symbol label width sort-mode) ...)"
  :type '(repeat
          (list (choice :tag "Entry" (const mark)
                        (const file)
                        (const time-stamp)
                        (const artist)
                        (const title)
                        (const time)
                        (const bitrate))
                (string :tag "Label")
                (number :tag "Width")
                (choice :tag "Sort Function" (const :tag "Default" t) function)))
  :group 'm4a-browse-name)

(defun m4a-entries-column-number ()
  (let ((i 0))
    (mapcar (lambda (a) 
              (prog1
                  (append a (list i))
                (setq i (1+ i))))
            m4a-entries)))

(defvar m4a-entries-column-number (m4a-entries-column-number))

(defcustom m4a-sort '("TimeStamp" . flip)
  "Default sort field."
  :type  '(choice (const :tag "Mark" ("M"))
                  (const :tag "Mark /Flip" ("M" . flip))
                  (const :tag "File" ("File"))
                  (const :tag "File /Flip" ("File" . flip))
                  (const :tag "Time Stamp" ("TimeStamp"))
                  (const :tag "Time Stamp /Flip" ("TimeStamp" . flip))
                  (const :tag "Artist" ("Artist"))
                  (const :tag "Artist /Flip"  ("Artist" . flip))
                  (const :tag "Title"  ("Title"))
                  (const :tag "Title /Flip" ("Title" . flip))
                  (const :tag "Time" ("Time"))
                  (const :tag "Time /Flip" ("Time" . flip))
                  (const :tag "Bitrate" ("BR"))
                  (const :tag "Bitrate /Flip" ("BR" . flip))
                  (const :tag "No Sort" nil))
  :group 'm4a-browse-name)

(defcustom m4a-browse-name-regexp (rx (and "." (or "m4a" "flac") eos))
  "Target file name."
  :type 'regexp
  :group 'm4a-browse-name)

(defvar m4a-buff-name "*m4a browse name*" "Buffer name.")
(defvar m4a-dired-buffer nil "Work.")

(unless (featurep 'wtag)
  (defvar wtag-process-name "*wtag process*")
  (defvar wtag-process nil))

(defcustom m4a-music-players
  (or wtag-music-players
  `((,(rx "." (or "mp4" "m4a" "flac" "wav") eos)
     ,(executable-find "wmplayer.exe") "/play" "/close")
    (,(rx "." (or "mp3") eos)
     ,(executable-find "mpg123"))))
  "Music Player and Opt."
  :type '(repeat
          (list regexp
                (file :tag "Player" :must-match t)
                (repeat :inline t :tag "Option" string)))
  :group 'm4a-browse-name)

(defgroup m4a-faces nil
  "Faces for m4a browse name."
  :group 'm4a-browse-name
  :group 'faces)

(defface m4a-file
  '((t :inherit bold))
  "m4a mode file name face."
  :group 'm4a-faces)

(defface m4a-time
  '((t :inherit nil))
  "m4a mode time stamp face."
  :group 'm4a-faces)

(defface m4a-artist
  '((t :inherit font-lock-keyword-face))
  "m4a mode artist name face."
  :group 'm4a-faces)

(defface m4a-title
  '((t :inherit font-lock-constant-face))
  "m4a mode title name face."
  :group 'm4a-faces)

(defface m4a-bitrate
  '((t :inherit font-lock-variable-name-face))
  "m4a mode bitrate and time face."
  :group 'm4a-faces)

(defface m4a-mark
  '((t :inherit font-lock-constant-face))
  "m4a mode mark face."
  :group 'm4a-faces)

(defface m4a-marked
  '((t :inherit warning))
  "m4a mode marked face."
  :group 'm4a-faces)

(defsubst m4a-column-mark ()
  (nth 4 (assq 'mark m4a-entries-column-number)))

(defsubst m4a-column-file ()
  (nth 4 (assq 'file m4a-entries-column-number)))

(defsubst m4a-column-time-stamp ()
  (nth 4 (assq 'time-stamp m4a-entries-column-number)))

(defsubst m4a-column-artist ()
  (nth 4 (assq 'artist m4a-entries-column-number)))

(defsubst m4a-column-title ()
  (nth 4 (assq 'title m4a-entries-column-number)))

(defsubst m4a-column-time ()
  (nth 4 (assq 'time m4a-entries-column-number)))

(defsubst m4a-column-bitrate ()
  (nth 4 (assq 'bitrate m4a-entries-column-number)))

(defun m4a-browse-name-mode-goto ()
  "next-next / previous-line のバインドでシンクロして動く."
  (interactive)
  (let* ((id (tabulated-list-get-id (line-beginning-position)))
         (pos (plist-get id '*pos)))
    (pop-to-buffer m4a-dired-buffer)
    (goto-char pos)))

(defun m4a-file-pos (buff name)
  (with-current-buffer buff
    (dired-goto-file name)))

(defun m4a-music-play ()
  (interactive)
  (let* ((id (tabulated-list-get-id (line-beginning-position)))
         (file (plist-get id '*file))
         (buff wtag-process-name)
         cmd)
    (m4a-music-kill)
    ;; (and (fboundp 'shuffle-all-minor-mode) (shuffle-all-minor-mode 1))
    (when (setq cmd (assoc-default file m4a-music-players #'string-match))
      (setq wtag-process
            (apply #'start-process (append (list buff buff) cmd (list file)))))))

(defun m4a-music-kill ()
  (interactive)
  (and wtag-process (delete-process wtag-process))
  (setq wtag-process nil))

(defun m4a-mark (&optional arg)
  (interactive "P")
  (let* ((pos (line-beginning-position))
         (id (tabulated-list-get-id pos))
         (ent (tabulated-list-get-entry pos))
         (file (plist-get id '*file)))
    (tabulated-list-set-col (m4a-column-mark) (propertize "*" 'face 'm4a-mark) t)
    (tabulated-list-set-col
     (m4a-column-file) (propertize (aref ent (m4a-column-file)) 'face 'm4a-marked) t)
    (and arg (forward-line arg))
    (with-current-buffer m4a-dired-buffer
      (dired-goto-file file)
      (dired-mark 1))))

(defun m4a-mark-forward ()
  (interactive)
  (m4a-mark 1))

(defun m4a-unmark (&optional arg)
  (interactive "p")
  (let* ((pos (line-beginning-position))
         (id (tabulated-list-get-id pos))
         (ent (tabulated-list-get-entry pos))
         (file (plist-get id '*file)))
    (tabulated-list-set-col (m4a-column-mark) " " t)
    (tabulated-list-set-col
     (m4a-column-file) (propertize (aref ent (m4a-column-file)) 'face 'm4a-file) t)
    (and arg (forward-line arg))
    (with-current-buffer m4a-dired-buffer
      (dired-goto-file file)
      (dired-unmark 1))))

(defun m4a-unmark-forward (arg)
  (interactive "p")
  (m4a-unmark arg))

(defun m4a-unmark-backword (arg)
  (interactive "p")
  (forward-line (- arg))
  (m4a-unmark))
  
(defun m4a-unmark-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (m4a-unmark-forward 1))))

(defun m4a-next-marked ()
  (interactive)
  (let ((pos (point)))
    (ignore-errors (forward-line))
    (unless (re-search-forward "^\\*" nil t)
      (ding)
      (message "No Match")
      (goto-char pos))))

(defun m4a-previous-marked ()
  (interactive)
  (let ((pos (point)))
    (ignore-errors (forward-line -1))
    (unless (re-search-backward "^\\*" nil t)
      (ding)
      (message "No Match")
      (goto-char pos))))

(defun m4a-match-mark (regexp col)
  (let ((i 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (and (string-match regexp (aref (tabulated-list-get-entry (point)) col))
             (setq i (1+ i))
             (m4a-mark))
        (forward-line)))
    (if (zerop i)
        (message "No Match")
      (message "Matched %d file(s)." i))))

(defun m4a-mark-artist (regexp)
  (interactive "sArtist Regexp: ")
  (m4a-match-mark regexp (m4a-column-artist)))

(defun m4a-mark-title (regexp)
  (interactive "sTitle Regexp: ")
  (m4a-match-mark regexp (m4a-column-title)))

(defun m4a-mark-bitrate (regexp)
  (interactive "sBitRate Regexp: ")
  (m4a-match-mark regexp (m4a-column-bitrate)))

(defun m4a-dired-mode-directories ()
  (let (result)
    (dolist (buff (buffer-list) (reverse result))
      (with-current-buffer buff
        (if (eq major-mode 'dired-mode)
            (push default-directory result))))))

(defun m4a-copy-title (prefix)
  (interactive "P")
  (let* ((id (tabulated-list-get-id (line-beginning-position)))
         name)
    (setq name
          (if prefix
              (plist-get id '*file)
            (format
             "%s - %s (%s)"
             (plist-get id 'artist) (plist-get id 'title)
             (file-name-nondirectory (plist-get id '*file)))))
    (message name)
    (kill-new name)))

;;;###autoload
(defun m4a-browse-name (dir)
  "DIR 下にある \".m4a\" (or \".flac\")のタイトルの一覧を表示."
  (interactive "DDir: ")
  (and (get-buffer m4a-buff-name) (kill-buffer m4a-buff-name))
  (dired dir)
  (setq m4a-dired-buffer (current-buffer))
  (with-current-buffer (get-buffer-create m4a-buff-name)
    (m4a-browse-name-mode)
    (m4a-browse-name-refresh dir)
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

(defun m4a-bitrate (id)
  (let ((tmp (nth 1 (plist-get id '*time))))
    (if (consp tmp) (car tmp) tmp)))

(defun tabulated-list-entry-bitrate> (ent1 ent2)
  "See `tabulated-list-entry-time>'."
  (> (m4a-bitrate (car ent1)) (m4a-bitrate (car ent2))))

(defun tabulated-list-entry-time> (ent1 ent2)
  "ENT1 ENT2 には其々の行の `tabulated-list-entries' がまとめて渡されてくる.
CAR には ID が CDR には ENTRY が入っている.
ペアリストではないので ENTRY は実質 CADR として得る."
  (>  (car (plist-get (car ent1) '*time))
      (car (plist-get (car ent2) '*time))))

(defun m4a-browse-name-refresh (&optional dir)
  (let* ((buff m4a-dired-buffer)
         (dir (or dir (with-current-buffer buff default-directory)))
         (files (directory-files-and-attributes dir t m4a-browse-name-regexp))
         result entries)
    (m4a-entries-column-number)
    (unless tabulated-list-entries
      ;; Get tag data.
      (dolist (f files (message "done"))
        (let ((name (car f))
              (size (round (* (/ (file-attribute-size (cdr f)) 100.0) 20)))
              (time (file-attribute-modification-time (cdr f)))
              message-log-max)
          (message "%s..." name)
          (push
           (append (list '*pos (m4a-file-pos buff name) '*file name '*mt time)
                   (mf-tag-read-plist name size t))
           result)))
      ;; Set tabulated list.
      (dolist (rec result)
        (push
         (list rec
               (vector
                " "
                (propertize
                 (file-name-nondirectory (plist-get rec '*file)) 'face 'm4a-file)
                (propertize
                 (format-time-string "%F %R" (plist-get rec '*mt)) 'face 'm4a-time)
                (propertize (or (plist-get rec 'artist) "") 'face 'm4a-artist)
                (propertize (or (plist-get rec 'title) "") 'face 'm4a-title)
                (propertize
                 (format-seconds "%3m'%02s\"" (nth 0 (plist-get rec '*time)))
                 'face 'm4a-bitrate)
                (propertize (number-to-string (m4a-bitrate rec)) 'face 'm4a-bitrate)))
         entries)))
    (setq tabulated-list-format (apply #'vector (mapcar #'cdr m4a-entries))
          tabulated-list-entries entries
          tabulated-list-use-header-line t)
    (and m4a-sort (setq tabulated-list-sort-key m4a-sort))
    (tabulated-list-init-header)))

;; TextProperty: (tabulated-list-entry [194160527.m4a 2021-04-15 14:08 リーガルリリー 東京 4'17" 320] tabulated-list-id (*pos 895 *file c:/Users/foo/Desktop/tmp/music/194160527.m4a *mt (24695 51783 0 0) *time (257 320 44100.0 2 16) *type mp4 title 東京 artist リーガルリリー a-artist リーガルリリー album the World track 1 disk 1 writer たかはしほのか cover nil s-title トウキョウ s-artist リーガルリリー s-a-artist リーガルリリー copy (P) 2021 Sony Music Labels Inc.) tabulated-list-column-name File help-echo File: 194160527.m4a face m4a-file) Overlay: nil

(defvar-keymap m4a-browse-name-mode-map
  :doc "m4a browse name mode map.
\"f\" is not available as it is follow mode(fm.el) toggle."
  "P"           #'m4a-music-play
  "C-c C-c"     #'m4a-music-kill
  "n"           #'next-line
  "SPC"         #'next-line
  "p"           #'previous-line
  "S-SPC"       #'previous-line
  "<backspace>" #'m4a-unmark-backword
  "<tab>"       #'tabulated-list-next-column
  "S-<tab>"     #'tabulated-list-previous-column
  "s"           #'tabulated-list-sort
  "m"           #'m4a-mark-forward
  "u"           #'m4a-unmark-forward
  "U"           #'m4a-unmark-all
  "% a"         #'m4a-mark-artist
  "% t"         #'m4a-mark-title
  "% b"         #'m4a-mark-bitrate
  "w"           #'m4a-copy-title
  "M-{"         #'m4a-previous-marked
  "M-}"         #'m4a-next-marked)

(easy-menu-define m4a-browse-name-mode-map-menu m4a-browse-name-mode-map
  "Menu for `Buffer-menu-mode'."
  '("m4a name"
    ["Mark"        m4a-mark-forward]
    ["Unmark"      m4a-unmark-forward]
    ["Unmark All"  m4a-unmark-all]
    ["Mark Artist" m4a-mark-artist]
    ["Mark Title"  m4a-mark-title]
    "---"
    ["Play" m4a-music-play]
    ["Stop" m4a-music-kill]))

(defvar m4a-ellipsis "...")

(define-derived-mode m4a-browse-name-mode tabulated-list-mode "m4a"
  "m4a browse name mode"
  (setq-local truncate-string-ellipsis m4a-ellipsis)
  (add-to-list 'fm-modes '(m4a-browse-name-mode m4a-browse-name-mode-goto))
  (fm-start)
  (add-hook 'tabulated-list-revert-hook #'m4a-browse-name-refresh))

(defun fm-post-command-hook-silence (org &optional lines)
  "End of Buffer にポイントがくるとメッセージが出て
バッファ走査するときにも出てしまうので殺しておく."
  (let ((inhibit-message t))
    (funcall org lines)))

(advice-add 'fm-post-command-hook :around #'fm-post-command-hook-silence)

(provide 'm4a-browse-name)
