;;; ytr.el --- YouTube radio  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/ytr
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

(defconst ytr--version "0.1.0")

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `ytr' (YouTube radio) plays audio from YouTube channels and
;; playlists, fetching their listings via `yt-dlp'.
;;
;; Report issues at https://github.com/xenodium/ytr/issues

;;; Code:

(require 'json)
(require 'map)
(require 'seq)
(require 'url)
(require 'url-parse)
(require 'url-util)
(eval-when-compile
  (require 'cl-lib))

;;; State

(cl-defun ytr--make-channel (&key id name url (kind 'channel) tracks
                                  uploader title)
  "Build a channel alist from ID, NAME, URL, KIND and TRACKS.

KIND is `channel' or `playlist'.  TRACKS is an ordered alist mapping
a track id to its track alist, in listing order.  NAME is the channel
name (absent for mixes and playlists); UPLOADER is the owning account
and TITLE the playlist title, kept so a display name can fall back to
them (see `ytr--channel-name').

For example:

  (ytr--make-channel :id \"@TOKYOECHOSOUNDS/videos\"
                     :name \"TOKYO ECHO SOUNDS\"
                     :url \"https://www.youtube.com/@TOKYOECHOSOUNDS/videos\")

  =>
  ((:id . \"@TOKYOECHOSOUNDS/videos\")
   (:name . \"TOKYO ECHO SOUNDS\")
   (:url . \"https://www.youtube.com/@TOKYOECHOSOUNDS/videos\")
   (:kind . channel)
   (:tracks) (:uploader) (:title))"
  (list (cons :id id)
        (cons :name name)
        (cons :url url)
        (cons :kind kind)
        (cons :tracks tracks)
        (cons :uploader uploader)
        (cons :title title)))

(cl-defun ytr--make-track (&key id title duration url channel-id
                                (music-p 'unknown)
                                artist track-name album)
  "Build a track alist from ID, TITLE, DURATION, URL and CHANNEL-ID.

ID is the YouTube video id.  DURATION is in seconds.  MUSIC-P is t,
nil, or `unknown' when not yet classified.  CHANNEL-ID references the
owning channel.  ARTIST, TRACK-NAME and ALBUM are optional metadata,
often absent until the track is enriched.  The thumbnail is derived
from ID on demand (see `ytr--thumbnail-file').

For example:

  (ytr--make-track :id \"8rUx3tYqg9Q\"
                   :title \"Electronica Echoing Through Emptiness\"
                   :duration 3651
                   :channel-id \"@TOKYOECHOSOUNDS/videos\")

  =>
  ((:id . \"8rUx3tYqg9Q\")
   (:title . \"Electronica Echoing Through Emptiness\")
   (:duration . 3651)
   (:url)
   (:channel-id . \"@TOKYOECHOSOUNDS/videos\")
   (:music-p . unknown)
   (:artist) (:track-name) (:album))"
  (list (cons :id id)
        (cons :title title)
        (cons :duration duration)
        (cons :url url)
        (cons :channel-id channel-id)
        (cons :music-p music-p)
        (cons :artist artist)
        (cons :track-name track-name)
        (cons :album album)))

(cl-defun ytr--make-player (&key (status 'idle) current
                                 queue history process (position 0) socket
                                 ipc-process loading-timer analyzer-timer
                                 marquee-timer position-timer)
  "Build the player alist.

STATUS is `idle', `loading', `playing', `paused' or `stopped'.
CURRENT is the track alist now playing.  QUEUE and HISTORY are lists
of track alists.  PROCESS is the mpv process.  POSITION is the elapsed
seconds of the current track.  SOCKET is the mpv IPC socket path.
IPC-PROCESS is the persistent network connection to that socket.
LOADING-TIMER animates the loading message in the echo area.
ANALYZER-TIMER animates the equalizer bars while playing.
MARQUEE-TIMER scrolls the title when it is wider than the thumbnail.
POSITION-TIMER polls mpv for the elapsed time while playing.

For example:

  (ytr--make-player)

  =>
  ((:status . idle) (:current) (:queue) (:history)
   (:process) (:position . 0) (:socket) (:ipc-process) (:loading-timer)
   (:analyzer-timer) (:marquee-timer) (:position-timer))"
  (list (cons :status status)
        (cons :current current)
        (cons :queue queue)
        (cons :history history)
        (cons :process process)
        (cons :position position)
        (cons :socket socket)
        (cons :ipc-process ipc-process)
        (cons :loading-timer loading-timer)
        (cons :analyzer-timer analyzer-timer)
        (cons :marquee-timer marquee-timer)
        (cons :position-timer position-timer)))

(cl-defun ytr--make-state (&key channels last-played
                                (player (ytr--make-player)))
  "Build the aggregate app state.

CHANNELS is an alist mapping a string id to a channel alist; each
channel holds its own `:tracks'.  PLAYER is a player alist.
LAST-PLAYED is the id of the track played most recently, kept across
sessions so it can be resumed.

For example:

  (ytr--make-state)

  =>
  ((:channels) (:last-played) (:player . ((:status . idle) ...)))"
  (list (cons :channels channels)
        (cons :last-played last-played)
        (cons :player player)))

(defvar ytr--state (ytr--make-state)
  "The one piece of mutable state for ytr.
All fields are declared in `ytr--make-state'.")

(defun ytr--tracks ()
  "Return all tracks across channels, in channel then listing order."
  (seq-mapcat (lambda (channel)
                (map-values (map-elt channel :tracks)))
              (map-values (map-elt ytr--state :channels))
              'list))

(defun ytr--track (id)
  "Return the track with ID across all channels, or nil.

For example, (ytr--track \"8rUx3tYqg9Q\") => ((:id . \"8rUx3tYqg9Q\") ...)."
  (seq-find (lambda (track) (equal (map-elt track :id) id))
            (ytr--tracks)))

(defun ytr--channel-name (channel)
  "Return CHANNEL's display name.

For a playlist or mix, prefer its title (the playlist name itself)
over the owning account.  For a channel, prefer the channel name,
falling back to the owning account and then the title.

For example, a `:kind' \\='channel with :name \"TOKYO ECHO SOUNDS\"
returns \"TOKYO ECHO SOUNDS\", while a `:kind' \\='playlist with
:title \"Bending Emacs\" returns \"Bending Emacs\"."
  (if (eq (map-elt channel :kind) 'playlist)
      (or (map-elt channel :title)
          (map-elt channel :name)
          (map-elt channel :uploader)
          "")
    (or (map-elt channel :name)
        (map-elt channel :uploader)
        (map-elt channel :title)
        "")))

;;; YouTube

(defun ytr--youtube-music-p (json)
  "Return t, nil, or `unknown' whether yt-dlp JSON is music.

`categories' is absent in the flat feed, so return `unknown' then.
For example, JSON with `categories' \\='(\"Music\") => t."
  (cond ((not (map-contains-key json 'categories)) 'unknown)
        ((seq-contains-p (map-elt json 'categories) "Music") t)
        (t nil)))

(cl-defun ytr--youtube-track-from-json (&key json channel-id)
  "Build a track alist from a yt-dlp entry JSON and CHANNEL-ID.

JSON is an alist parsed with `:object-type \\='alist' (snake-case
keys).  For example, given JSON with `id', `title' and `categories'
set to \\='(\"Music\"), returns a track alist whose `:music-p' is t."
  (ytr--make-track
   :id         (map-elt json 'id)
   :title      (map-elt json 'title)
   :duration   (map-elt json 'duration)
   :url        (map-elt json 'url)
   :channel-id channel-id
   :music-p    (ytr--youtube-music-p json)
   :artist     (map-elt json 'artist)
   :track-name (map-elt json 'track)
   :album      (map-elt json 'album)))

(defun ytr--youtube-playlist-p (url)
  "Return non-nil when URL is a playlist or mix.

Parses URL's query string and checks for a `list' key, rather than
matching the raw string, so it is not fooled by, say, a `list' inside
another value.

For example, a \"...playlist?list=PL123\" or \"...watch?v=x&list=RD\"
URL returns non-nil, while \"https://www.youtube.com/@chan\" returns
nil."
  (map-contains-key
   (url-parse-query-string
    (or (cdr (url-path-and-query (url-generic-parse-url url))) ""))
   "list"))

(defun ytr--youtube-listing-url (url)
  "Return the yt-dlp listing URL for URL.

Channel URLs target their Videos tab, since the default tab need not
list every upload.  Playlist and mix URLs are used as-is, since
appending a path would corrupt the query (see `ytr--youtube-playlist-p').

For example:

  (ytr--youtube-listing-url \"https://www.youtube.com/@TOKYOECHOSOUNDS\")
  => \"https://www.youtube.com/@TOKYOECHOSOUNDS/videos\"

  (ytr--youtube-listing-url \"https://www.youtube.com/playlist?list=PL123\")
  => \"https://www.youtube.com/playlist?list=PL123\""
  (cond ((ytr--youtube-playlist-p url) url)
        ((string-suffix-p "/videos" url) url)
        (t (concat url "/videos"))))

(cl-defun ytr--youtube-fetch-channel (&key url)
  "Fetch the channel at URL via yt-dlp and return a channel alist.

The channel's `:tracks' holds its video listing (flat, cheap fields
only) as an ordered id->track alist.  The channel name, owning account
and playlist title are all kept (mixes and playlists lack some), and
`ytr--channel-name' chooses among them at render time.  The id falls
back to the playlist id, which keys the channel.  Signals an error when
yt-dlp is missing or fails."
  (unless (executable-find "yt-dlp")
    (error "Cannot find yt-dlp in `exec-path'"))
  (with-temp-buffer
    (unless (zerop (call-process "yt-dlp" nil (list t nil) nil
                                 "--flat-playlist" "--dump-single-json"
                                 (ytr--youtube-listing-url url)))
      (error "Failed to fetch %s with yt-dlp" url))
    (goto-char (point-min))
    (let* ((json (json-parse-buffer :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil))
           (playlist (ytr--youtube-playlist-p url))
           ;; Key a playlist or mix by its own listing id, not the owner's
           ;; channel id: two playlists from one creator (or a creator's
           ;; channel and one of their playlists) would otherwise collide.
           (id (if playlist
                   (map-elt json 'id)
                 (or (map-elt json 'channel_id) (map-elt json 'id)))))
      (ytr--make-channel
       :id id
       :name (map-elt json 'channel)
       :uploader (or (map-elt json 'uploader) (map-elt json 'playlist_uploader))
       :title (map-elt json 'title)
       :url url
       :kind (if playlist 'playlist 'channel)
       :tracks (seq-map (lambda (entry)
                          (let ((track (ytr--youtube-track-from-json
                                        :json entry :channel-id id)))
                            (cons (map-elt track :id) track)))
                        (map-elt json 'entries))))))

;;; Persistence

(defvar ytr--state-file (locate-user-emacs-file "ytr/state.eld")
  "File where `ytr--state' is persisted between sessions.")

(defun ytr--save (&optional file)
  "Persist `ytr--state' to FILE, omitting the ephemeral player.

FILE defaults to `ytr--state-file'.  Durable fields are saved by name
so a future ephemeral field is not persisted by accident.  The
`:version' key allows migrating the on-disk format later."
  (setq file (or file ytr--state-file))
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (prin1 (list (cons :version 3)
                 (cons :channels (map-elt ytr--state :channels))
                 (cons :last-played (map-elt ytr--state :last-played)))
           (current-buffer))))

(defun ytr--migrate-channels (channels)
  "Backfill each track's `:channel-id' from a legacy `:station-id'.

Older state files keyed channels under `:stations' and tracks under
`:station-id'.  Mutates and returns CHANNELS so such files keep
working."
  (seq-do
   (lambda (channel-entry)
     (seq-do
      (lambda (track-entry)
        (let ((track (cdr track-entry)))
          (unless (map-elt track :channel-id)
            (nconc track (list (cons :channel-id (map-elt track :station-id)))))))
      (map-elt (cdr channel-entry) :tracks)))
   channels)
  channels)

(defun ytr--load (&optional file)
  "Restore `ytr--state' from FILE, with a fresh player.

FILE defaults to `ytr--state-file'.  Reads the legacy `:stations' key
when `:channels' is absent.  Does nothing when FILE does not exist."
  (setq file (or file ytr--state-file))
  (when (file-exists-p file)
    (let ((data (with-temp-buffer
                  (insert-file-contents file)
                  (read (current-buffer)))))
      (setq ytr--state
            (ytr--make-state
             :channels (or (map-elt data :channels)
                           (ytr--migrate-channels (map-elt data :stations)))
             :last-played (map-elt data :last-played))))))

;;; UI

(defvar ytr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") #'ytr-add-channel)
    (define-key map (kbd "-") #'ytr-remove-channel)
    (define-key map (kbd "/") #'ytr-play-track)
    (define-key map (kbd "c") #'ytr-play-channel)
    (define-key map (kbd "SPC") #'ytr-toggle-play)
    (define-key map (kbd "n") #'ytr-next)
    (define-key map (kbd "p") #'ytr-previous)
    (define-key map (kbd "f") #'ytr-seek-forward)
    (define-key map (kbd "b") #'ytr-seek-backward)
    (define-key map (kbd "o") #'ytr-open-in-browser)
    (define-key map (kbd "TAB") #'ytr-forward-button)
    (define-key map (kbd "<backtab>") #'ytr-backward-button)
    (define-key map (kbd "q") #'ytr-quit)
    ;; The player fits its frame, so disable scrolling entirely.  Remap
    ;; the commands (not the raw wheel events) so the bindings survive
    ;; minor modes such as `pixel-scroll-precision-mode'.
    (dolist (command '(scroll-up-command scroll-down-command
                       scroll-up scroll-down scroll-left scroll-right
                       mwheel-scroll pixel-scroll-precision))
      (define-key map (vector 'remap command) #'ignore))
    map)
  "Keymap for `ytr-mode'.")

(define-derived-mode ytr-mode special-mode "ytr"
  "Major mode for the YouTube radio.

\\{ytr-mode-map}"
  (setq-local mode-line-format nil)
  ;; Hidden by default, whether or not the player frame is selected; the
  ;; cursor only appears briefly when point moves (see
  ;; `ytr--flash-cursor-on-move').
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil)
  (add-hook 'post-command-hook #'ytr--flash-cursor-on-move nil t)
  ;; Defeat scrolling by any means (wheel, M-x, commands) at the display
  ;; level: keep the window pinned to the top.
  (add-hook 'window-scroll-functions #'ytr--prevent-scroll nil t)
  (add-hook 'kill-buffer-hook #'ytr--stop nil t))

(defun ytr--prevent-scroll (window start)
  "Pin WINDOW to the top, undoing any scroll away from `point-min'.
Added to `window-scroll-functions'; START is the position redisplay was
about to scroll to."
  (when (> start (point-min))
    (set-window-start window (point-min) t)))

(defvar-local ytr--cursor-timer nil
  "Timer that hides the cursor again after it briefly appears.")

(defvar-local ytr--cursor-point nil
  "Point at the last `ytr--flash-cursor-on-move' check.")

(defvar-local ytr--cursor-synced nil
  "Non-nil once the cursor tracker has recorded its starting point.")

(defun ytr--flash-cursor-on-move ()
  "Show the cursor for three seconds when point moves, then hide it.

Run from `post-command-hook', so the cursor stays hidden while idle.
The first call only records the starting point, so opening the player
does not flash the cursor."
  (cond ((not ytr--cursor-synced)
         (setq ytr--cursor-synced t
               ytr--cursor-point (point)))
        ((not (eq (point) ytr--cursor-point))
         (setq ytr--cursor-point (point)
               cursor-type t)
         (when (timerp ytr--cursor-timer)
           (cancel-timer ytr--cursor-timer))
         (setq ytr--cursor-timer
               (run-at-time 3 nil #'ytr--hide-cursor (current-buffer))))))

(defun ytr--hide-cursor (buffer)
  "Hide the cursor in BUFFER and clear its hide timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq cursor-type nil
            ytr--cursor-timer nil)
      (when-let* ((window (get-buffer-window buffer t)))
        (force-window-update window)))))

(defvar ytr--thumbnail-max-height 180
  "Maximum thumbnail height in pixels.")

(defvar ytr-previous-icon "◁◁" "Previous button icon.")
(defvar ytr-play-icon "▶" "Play button icon.")
(defvar ytr-stop-icon "■" "Icon shown on the play button while active.")
(defvar ytr-next-icon "▷▷" "Next button icon.")
(defvar ytr-open-icon "➦" "Open-in-browser button icon.")
(defvar ytr-search-icon "♪" "Search/pick-a-track button icon.")
(defvar ytr-channels-icon "⦿" "Switch-channel button icon.")
(defvar ytr-add-icon "+" "Add-channel button icon.")

(defun ytr-macos-use-sf-symbols ()
  "Set the player's button icons to macOS SF Symbols.

Requires a font with SF Symbols glyphs (for example \"SF Pro\").
Redraws the player so the new icons take effect immediately."
  (interactive)
  (setq ytr-previous-icon "􀊉"
        ytr-play-icon "􀊄"
        ytr-stop-icon "􀊆"
        ytr-next-icon "􀊋"
        ytr-open-icon "􀉐"
        ytr-search-icon "􀑪"
        ytr-channels-icon "􀪔"
        ytr-add-icon "􀁌")
  (ytr--render))

(defun ytr--track-label (track)
  "Return a display label for TRACK, prefixed with its channel name.

For example, \"TOKYO ECHO SOUNDS - Electronica\"."
  (format "%s - %s"
          (ytr--channel-name
           (map-nested-elt ytr--state
                           `(:channels ,(map-elt track :channel-id))))
          (or (map-elt track :title) "")))

(defun ytr--format-duration (seconds)
  "Format SECONDS as H:MM:SS.

For example, (ytr--format-duration 418) => \"0:06:58\"."
  (let ((seconds (floor (or seconds 0))))
    (format "%d:%02d:%02d"
            (/ seconds 3600)
            (% (/ seconds 60) 60)
            (% seconds 60))))

(defvar ytr-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "TAB") #'ytr-forward-button)
    (define-key map [backtab] #'ytr-backward-button)
    map)
  "Keymap on ytr buttons.
Like `button-map' but routes TAB and backtab through ytr's navigation
so point lands on the button glyph rather than its padding.")

(defun ytr--insert-button (id label command &optional no-box)
  "Insert a button identified by ID showing LABEL.
Runs COMMAND when pressed.  Draws a box around it unless NO-BOX is
non-nil.  Buttons are not highlighted, including on mouse hover."
  (insert-text-button
   (format " %s " label)
   'action (lambda (_button) (call-interactively command))
   'follow-link t
   'mouse-face nil
   'ytr-button-id id
   'keymap ytr-button-map
   'face (if no-box 'default '(:box (:line-width (1 . 1))))))

(defun ytr--button-position (id)
  "Return the buffer position of the button with `ytr-button-id' ID, or nil."
  (let ((pos (point-min)))
    (catch 'found
      (while (setq pos (next-single-property-change pos 'ytr-button-id))
        (when (eq (get-text-property pos 'ytr-button-id) id)
          (throw 'found pos)))
      nil)))

(defun ytr--skip-button-padding ()
  "Move point past the button's leading padding to its first glyph."
  (when-let* ((button (button-at (point))))
    (skip-chars-forward " " (button-end button))))

(defun ytr--goto-button (id)
  "Move point onto the button identified by ID, if present.

Used so a command lands point on its own button before redrawing; the
redraw then restores point there (see `ytr--render')."
  (when-let* ((buffer (get-buffer "*ytr*"))
              (pos (with-current-buffer buffer (ytr--button-position id))))
    (with-current-buffer buffer
      (goto-char pos)
      (ytr--skip-button-padding))))

(defun ytr--download-thumbnail (url file)
  "Download URL to FILE quietly and return FILE, or nil on failure.

A missing YouTube thumbnail is served as a tiny placeholder image
rather than an HTTP error, so a download under 2 KB is treated as
absent.  Flushes FILE from the image cache so freshly downloaded
content is not masked by a previously rasterized version."
  (ignore-errors
    (let ((inhibit-message t)
          (message-log-max nil))
      (url-copy-file url file t))
    (when (and (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 2048))
      (clear-image-cache file)
      file)))

(defun ytr--thumbnail-file (track)
  "Return a local file holding TRACK's thumbnail, downloading it once.

Builds the full-resolution YouTube thumbnail from the track id and
caches it under the ytr directory, falling back to a lower resolution
when the full one is unavailable.  Returns nil when every download
fails."
  (when-let* ((id (map-elt track :id))
              (file (locate-user-emacs-file
                     (file-name-concat "ytr/thumbnails" (concat id ".jpg")))))
    (if (file-exists-p file)
        file
      (make-directory (file-name-directory file) t)
      (or (ytr--download-thumbnail
           (format "https://i.ytimg.com/vi/%s/maxresdefault.jpg" id) file)
          (ytr--download-thumbnail
           (format "https://i.ytimg.com/vi/%s/hqdefault.jpg" id) file)))))

(defun ytr--render ()
  "Render the currently playing track into the `*ytr*' buffer.

Targets the buffer explicitly so it is safe to call from a process
sentinel, where the current buffer is unrelated."
  (when-let* ((buffer (get-buffer "*ytr*")))
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (status (map-nested-elt ytr--state '(:player :status)))
             (track (if (memq status '(loading playing paused))
                        (map-nested-elt ytr--state '(:player :current))
                      (ytr--track (map-elt ytr--state :last-played))))
             (button (or (get-text-property (point) 'ytr-button-id) 'play)))
        (erase-buffer)
        (insert "\n")
        (if (not track)
            (progn
              (ytr--set-marquees nil)
              (insert "  Nothing playing.  Press / to play, + to add a channel.\n"))
          (let* ((file (and (display-graphic-p) (ytr--thumbnail-file track)))
                 (image (and file (create-image file nil nil
                                                :max-height ytr--thumbnail-max-height)))
                 (cols (or (and image (ytr--image-columns image)) 40))
                 (title (or (map-elt track :title) ""))
                 (channel (ytr--channel-name
                           (map-nested-elt ytr--state
                                           `(:channels ,(map-elt track :channel-id)))))
                 ;; reserve room for the elapsed field and the equalizer
                 (time-width (string-width (ytr--format-duration
                                            (map-elt track :duration))))
                 (anim-reserve (1+ ytr--analyzer-bar-count))
                 (marquees nil))
            (when image
              (insert "  ")
              (insert-image image)
              (insert "\n\n"))
            (insert "  ")
            (push (ytr--insert-field 'title title (max 1 (- cols time-width 1)) 'bold)
                  marquees)
            (insert " " (propertize (ytr--elapsed-string)
                                    'ytr-elapsed t 'face 'shadow)
                    "\n")
            (insert "  ")
            (push (ytr--insert-field 'channel channel (max 1 (- cols anim-reserve))
                                     'shadow)
                  marquees)
            (cond ((eq status 'loading)
                   (insert " " (propertize (ytr--loading-frame)
                                           'ytr-loading t 'face 'shadow)))
                  ((eq status 'playing)
                   (insert " " (propertize (ytr--analyzer-bars)
                                           'ytr-analyzer t 'face 'shadow))))
            (insert "\n\n")
            (ytr--set-marquees (delq nil marquees)))
          (insert "  ")
          (ytr--insert-button 'previous ytr-previous-icon #'ytr-previous)
          (insert " ")
          (ytr--insert-button
           'play
           (if (memq status '(loading playing)) ytr-stop-icon ytr-play-icon)
           #'ytr-toggle-play)
          (insert " ")
          (ytr--insert-button 'next ytr-next-icon #'ytr-next)
          (insert " ")
          (ytr--insert-button 'open ytr-open-icon #'ytr-open-in-browser)
          (insert " ")
          (ytr--insert-button 'search ytr-search-icon #'ytr-play-track t)
          (ytr--insert-button 'channels ytr-channels-icon #'ytr-play-channel t)
          (ytr--insert-button 'add ytr-add-icon #'ytr-add-channel t)
          ;; bottom padding, mirroring the leading newline
          (insert "\n"))
        (goto-char (or (ytr--button-position button)
                       (ytr--button-position 'play)
                       (point-min)))
        (ytr--skip-button-padding)))))

(defun ytr-add-channel ()
  "Prompt for a channel URL, fetch it, add it to the state and play it."
  (interactive)
  (let ((url (read-string "Add YouTube channel (URL): ")))
    (message "Fetching %s..." url)
    (let ((channel (ytr--youtube-fetch-channel :url url)))
      (setf (map-elt (map-elt ytr--state :channels) (map-elt channel :id))
            channel)
      (ytr--save)
      (message "Added %s (%d tracks)"
               (ytr--channel-name channel)
               (map-length (map-elt channel :tracks)))
      (if-let* ((track (seq-first (map-values (map-elt channel :tracks)))))
          (ytr--play track)
        (ytr--render)))))

(defun ytr-play-track ()
  "Select a track from all channels and play it."
  (interactive)
  (let ((choices (seq-map (lambda (track)
                            (cons (ytr--track-label track) track))
                          (ytr--tracks))))
    (unless choices
      (user-error "No tracks yet.  Press + to add a channel"))
    (ytr--play (map-elt choices (completing-read "Play: " choices nil t)))))

(defun ytr-play-channel ()
  "Select a channel and play its first track."
  (interactive)
  (let ((choices (seq-map (lambda (channel)
                            (cons (ytr--channel-name channel) channel))
                          (map-values (map-elt ytr--state :channels)))))
    (unless choices
      (user-error "No channels yet.  Press + to add a channel"))
    (let ((track (seq-first
                  (map-values
                   (map-elt (map-elt choices
                                     (completing-read "Play channel: " choices nil t))
                            :tracks)))))
      (unless track
        (user-error "Channel has no tracks"))
      (ytr--play track))))

(defun ytr-remove-channel ()
  "Select a channel and remove it, with its tracks, from the catalog.

When the removed channel is the one playing, stop and load another
channel's first track (ready to play, not auto-played).  When no
channel remains, close the player."
  (interactive)
  (let ((choices (seq-map (lambda (channel)
                            (cons (ytr--channel-name channel) channel))
                          (map-values (map-elt ytr--state :channels)))))
    (unless choices
      (user-error "No channels to remove"))
    (when-let* ((channel (map-elt choices
                                  (completing-read "Remove channel: " choices nil t)))
                ((yes-or-no-p (format "Remove %s? " (ytr--channel-name channel)))))
      (let ((playing (equal (map-nested-elt ytr--state '(:player :current :channel-id))
                            (map-elt channel :id))))
        (setf (map-elt ytr--state :channels)
              (map-delete (map-elt ytr--state :channels) (map-elt channel :id)))
        (ytr--save)
        (message "Removed %s" (ytr--channel-name channel))
        (cond ((seq-empty-p (ytr--tracks))
               ;; Nothing left to display: close the player.
               (ytr--stop)
               (ytr--delete-frame))
              (playing
               ;; Was playing the removed channel: stop and load another
               ;; channel's first track (ready to play, not auto-played).
               (ytr--stop)
               (setf (map-elt ytr--state :last-played)
                     (map-elt (seq-first (ytr--tracks)) :id))
               (ytr--render))
              (t (ytr--render)))))))

(defvar ytr--loading-frames '(".  " ".. " "..." " .." "  .")
  "Frames cycled in place of the equalizer while a track is loading.")

(defvar ytr--loading-index 0
  "Index of the current `ytr--loading-frames' frame.")

(defun ytr--loading-frame ()
  "Return the current loading frame."
  (nth (mod ytr--loading-index (length ytr--loading-frames))
       ytr--loading-frames))

(defun ytr--displayed-p ()
  "Return non-nil when the player buffer is shown in some window.
Animation ticks skip their work otherwise, so nothing animates or
polls while the player is dismissed."
  (and (get-buffer "*ytr*")
       (get-buffer-window "*ytr*" t)))

(defun ytr--loading-tick ()
  "Advance the loading animation and update it in place."
  (when-let* (((ytr--displayed-p))
              (buffer (get-buffer "*ytr*")))
    (setq ytr--loading-index (1+ ytr--loading-index))
    (with-current-buffer buffer
      (when-let* ((beg (text-property-any (point-min) (point-max) 'ytr-loading t))
                  (end (or (next-single-property-change beg 'ytr-loading) (point-max)))
                  (inhibit-read-only t))
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert (propertize (ytr--loading-frame)
                              'ytr-loading t 'face 'shadow)))))))

(defun ytr--start-loading ()
  "Begin animating the loading indicator."
  (ytr--stop-loading)
  (setf (map-elt (map-elt ytr--state :player) :loading-timer)
        (run-at-time 0 0.15 #'ytr--loading-tick)))

(defun ytr--stop-loading ()
  "Cancel the loading animation timer, if any."
  (when-let* ((timer (map-nested-elt ytr--state '(:player :loading-timer))))
    (cancel-timer timer))
  (setf (map-elt (map-elt ytr--state :player) :loading-timer) nil)
  (setq ytr--loading-index 0))

(defconst ytr--analyzer-blocks "▁▂▃▄▅▆"
  "Bar glyphs, from shortest to tallest, for the equalizer animation.")

(defconst ytr--analyzer-bar-count 3
  "Number of equalizer bars to animate.")

(defun ytr--analyzer-bars ()
  "Return a string of `ytr--analyzer-bar-count' random equalizer bars."
  (mapconcat (lambda (_)
               (char-to-string
                (aref ytr--analyzer-blocks (random (length ytr--analyzer-blocks)))))
             (make-list ytr--analyzer-bar-count nil)
             ""))

(defun ytr--analyzer-tick ()
  "Replace the equalizer bars in place with a fresh frame."
  (when-let* (((ytr--displayed-p))
              (buffer (get-buffer "*ytr*")))
    (with-current-buffer buffer
      (when-let* ((beg (text-property-any (point-min) (point-max) 'ytr-analyzer t))
                  (end (or (next-single-property-change beg 'ytr-analyzer) (point-max)))
                  (inhibit-read-only t))
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert (propertize (ytr--analyzer-bars)
                              'ytr-analyzer t 'face 'shadow)))))))

(defun ytr--start-analyzer ()
  "Begin animating the equalizer bars."
  (ytr--stop-analyzer)
  (setf (map-elt (map-elt ytr--state :player) :analyzer-timer)
        (run-at-time 0.15 0.15 #'ytr--analyzer-tick)))

(defun ytr--stop-analyzer ()
  "Cancel the equalizer animation timer, if any."
  (when-let* ((timer (map-nested-elt ytr--state '(:player :analyzer-timer))))
    (cancel-timer timer))
  (setf (map-elt (map-elt ytr--state :player) :analyzer-timer) nil))

(defvar ytr--marquees nil
  "Active marquee regions.
A list of (ID TEXT WIDTH FACE) entries, one per overflowing field.")

(defvar ytr--marquee-offset 0
  "Shared marquee scroll offset, in columns.")

(defun ytr--image-columns (image)
  "Return the width of IMAGE in character columns, or nil if unmeasurable."
  (condition-case nil
      (ceiling (car (image-size image)))
    (error nil)))

(defconst ytr--marquee-pause 6
  "Ticks to hold at each end before reversing the marquee.")

(defun ytr--marquee-window (text width tick)
  "Return WIDTH display columns of TEXT for animation TICK.

Scrolls back and forth: from the start to where the end is visible,
pausing at each end, like a music player's now-playing title."
  (let* ((overflow (max 0 (- (string-width text) width)))
         (period (+ (* 2 overflow) (* 2 ytr--marquee-pause)))
         (p (if (zerop period) 0 (mod tick period)))
         (offset (cond ((<= overflow 0) 0)
                       ((< p overflow) p)
                       ((< p (+ overflow ytr--marquee-pause)) overflow)
                       ((< p (+ (* 2 overflow) ytr--marquee-pause))
                        (- (+ (* 2 overflow) ytr--marquee-pause) p))
                       (t 0))))
    (truncate-string-to-width text (+ offset width) offset ?\s)))

(defun ytr--insert-field (id text width face)
  "Insert TEXT in FACE, fitting WIDTH columns.

When TEXT fits, insert it as is and return nil.  Otherwise insert a
scrolling window tagged for animation and return its (ID TEXT WIDTH
FACE) marquee entry."
  (if (<= (string-width text) width)
      (progn (insert (propertize text 'face face)) nil)
    (insert (propertize (ytr--marquee-window text width ytr--marquee-offset)
                        'ytr-marquee id 'face face))
    (list id text width face)))

(defun ytr--marquee-tick ()
  "Scroll every active marquee one column and update it in place."
  (when-let* (((ytr--displayed-p))
              (buffer (get-buffer "*ytr*")))
    (setq ytr--marquee-offset (1+ ytr--marquee-offset))
    (with-current-buffer buffer
      (dolist (entry ytr--marquees)
        (pcase-let ((`(,id ,text ,width ,face) entry))
          (when-let* ((beg (text-property-any (point-min) (point-max)
                                              'ytr-marquee id))
                      (end (or (next-single-property-change beg 'ytr-marquee)
                               (point-max)))
                      (inhibit-read-only t))
            (save-excursion
              (delete-region beg end)
              (goto-char beg)
              (insert (propertize (ytr--marquee-window text width
                                                       ytr--marquee-offset)
                                  'ytr-marquee id 'face face)))))))))

(defun ytr--set-marquees (marquees)
  "Install MARQUEES (a list of entries) and manage the animation timer.
Resets the offset when the set of marquees changes."
  (unless (equal marquees ytr--marquees)
    (setq ytr--marquee-offset 0))
  (setq ytr--marquees marquees)
  (if (null marquees)
      (ytr--stop-marquee)
    (unless (map-nested-elt ytr--state '(:player :marquee-timer))
      (setf (map-elt (map-elt ytr--state :player) :marquee-timer)
            (run-at-time 0.25 0.25 #'ytr--marquee-tick)))))

(defun ytr--stop-marquee ()
  "Stop all marquee animation."
  (when-let* ((timer (map-nested-elt ytr--state '(:player :marquee-timer))))
    (cancel-timer timer))
  (setf (map-elt (map-elt ytr--state :player) :marquee-timer) nil)
  (setq ytr--marquees nil
        ytr--marquee-offset 0))

(defun ytr--elapsed-string ()
  "Return the current track's elapsed time as H:MM:SS."
  (ytr--format-duration (or (map-nested-elt ytr--state '(:player :position)) 0)))

(defun ytr--update-elapsed ()
  "Replace the elapsed-time field in the `*ytr*' buffer in place."
  (when-let* ((buffer (get-buffer "*ytr*")))
    (with-current-buffer buffer
      (when-let* ((beg (text-property-any (point-min) (point-max) 'ytr-elapsed t))
                  (end (or (next-single-property-change beg 'ytr-elapsed) (point-max)))
                  (inhibit-read-only t))
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert (propertize (ytr--elapsed-string) 'ytr-elapsed t 'face 'shadow)))))))

(defun ytr--poll-position ()
  "Ask mpv for the elapsed time and refresh the displayed field.
Skips the request while the player is not displayed."
  (when (ytr--displayed-p)
    (ytr--mpv-request
     (list "get_property" "time-pos")
     (lambda (data)
       (setf (map-elt (map-elt ytr--state :player) :position)
             (if (numberp data) (floor data) 0))
       (ytr--update-elapsed)))))

(defun ytr--start-position ()
  "Begin polling mpv for the elapsed time each second."
  (ytr--stop-position)
  (setf (map-elt (map-elt ytr--state :player) :position-timer)
        (run-at-time 0 1 #'ytr--poll-position)))

(defun ytr--stop-position ()
  "Cancel the elapsed-time polling timer, if any."
  (when-let* ((timer (map-nested-elt ytr--state '(:player :position-timer))))
    (cancel-timer timer))
  (setf (map-elt (map-elt ytr--state :player) :position-timer) nil))

(defun ytr--progress-bar (position duration)
  "Return a text progress bar of POSITION within DURATION seconds.

Sized to the parent frame, since the player's echo area is the
parent's.  For example:

  0:00:00 ┄┄┄┄ 0:00:30 ┄┄┄┄ 0:01:00"
  (let* ((start (ytr--format-duration 0))
         (now (ytr--format-duration position))
         (end (ytr--format-duration duration))
         (reserved (+ (length start) (length now) (length end) 4))
         (width (max 4 (- (frame-width (frame-parent ytr--frame)) reserved)))
         (left (if (zerop duration) 0
                 (round (* (/ (float position) duration) width)))))
    (concat start " " (make-string left ?┄)
            " " now " " (make-string (- width left) ?┄)
            " " end)))

(defvar ytr--flash-timer nil
  "Timer that clears the last `ytr--flash' message, or nil.")

(defun ytr--flash (text seconds)
  "Show TEXT in the echo area for SECONDS, then clear it if unchanged.

A single timer is reused across calls (cancelling the previous one),
so rapid repeated flashes neither pile up timers nor have a stale one
clear a newer message.  The `string=' guard leaves any unrelated
message shown since untouched."
  (when (timerp ytr--flash-timer)
    (cancel-timer ytr--flash-timer))
  (let ((message-log-max nil))
    (message "%s" text)
    (setq ytr--flash-timer
          (run-at-time seconds nil
                       (lambda ()
                         (when (string= (current-message) text)
                           (message ""))
                         (setq ytr--flash-timer nil))))))

(defun ytr--set-status (status)
  "Set the player STATUS and redraw.
Stops the loading animation unless STATUS is loading, and runs the
equalizer animation and elapsed-time polling only while playing."
  (unless (eq status 'loading)
    (ytr--stop-loading))
  (setf (map-elt (map-elt ytr--state :player) :status) status)
  (if (eq status 'playing)
      (progn (ytr--start-analyzer) (ytr--start-position))
    (ytr--stop-analyzer)
    (ytr--stop-position))
  (ytr--render))

(defun ytr--stop ()
  "Stop playback, close the IPC connection and clear the player.

Clears the process references before deleting them so the
synchronously-invoked sentinel sees them gone and does not advance
\(otherwise advancing to the next track flashes the stopped state)."
  (ytr--stop-loading)
  (ytr--stop-analyzer)
  (ytr--stop-marquee)
  (ytr--stop-position)
  (let ((ipc (map-nested-elt ytr--state '(:player :ipc-process)))
        (process (map-nested-elt ytr--state '(:player :process))))
    (setf (map-elt (map-elt ytr--state :player) :process) nil
          (map-elt (map-elt ytr--state :player) :ipc-process) nil
          (map-elt (map-elt ytr--state :player) :socket) nil
          (map-elt (map-elt ytr--state :player) :status) 'stopped)
    (when (process-live-p ipc)
      (delete-process ipc))
    (when (process-live-p process)
      (delete-process process))))

(defun ytr--next-track (track)
  "Return the track following TRACK across all channels, or nil at the end.

Walks all channels' tracks in channel then listing order.

For example, with two tracks t1 and t2 in the catalog,
\(ytr--next-track t1-alist) returns the t2 alist."
  (seq-first
   (cdr (seq-drop-while
         (lambda (other)
           (not (equal (map-elt other :id) (map-elt track :id))))
         (ytr--tracks)))))

(defun ytr--previous-track (track)
  "Return the track preceding TRACK across all channels, or nil at the start.

Walks all channels' tracks in channel then listing order.

For example, with two tracks t1 and t2 in the catalog,
\(ytr--previous-track t2-alist) returns the t1 alist."
  (seq-first
   (seq-reverse
    (seq-take-while
     (lambda (other)
       (not (equal (map-elt other :id) (map-elt track :id))))
     (ytr--tracks)))))

(defun ytr--sentinel (process _event)
  "Advance to the next track, or stop, when the mpv PROCESS exits.

Ignores PROCESS when it is no longer the player's process, so
starting a new track does not clobber the new state.  Advances only
on a clean finish (exit status 0); a user-stopped or killed process
leaves playback stopped."
  (when (and (not (process-live-p process))
             (eq process (map-nested-elt ytr--state '(:player :process))))
    (if-let* ((next (and (eq (process-exit-status process) 0)
                         (ytr--next-track
                          (map-nested-elt ytr--state '(:player :current))))))
        (ytr--play next)
      (ytr--stop)
      (ytr--render))))

(defun ytr--play (track)
  "Play TRACK with mpv (audio only) and redraw.

Stops any track already playing.  Starts mpv with an IPC socket, then
connects to it so playback status is driven by mpv events.  The echo
area shows a loading message until mpv reports it is actually playing,
or a failure when mpv cannot play the track."
  (unless (executable-find "mpv")
    (error "Cannot find mpv in `exec-path'"))
  (ytr--stop)
  (let ((socket (make-temp-name
                 (file-name-concat temporary-file-directory "ytr-mpv-")))
        (process nil))
    (setq process (start-process "ytr-mpv" nil "mpv" "--no-video"
                                 (concat "--input-ipc-server=" socket)
                                 (map-elt track :url)))
    (set-process-sentinel process #'ytr--sentinel)
    (setf (map-elt (map-elt ytr--state :player) :process) process
          (map-elt (map-elt ytr--state :player) :socket) socket
          (map-elt (map-elt ytr--state :player) :current) track
          (map-elt (map-elt ytr--state :player) :status) 'loading
          (map-elt (map-elt ytr--state :player) :position) 0
          (map-elt ytr--state :last-played) (map-elt track :id))
    (ytr--start-loading)
    (ytr--mpv-connect socket process 0))
  (ytr--save)
  (ytr--render))

(defun ytr--mpv-connect (socket process attempt)
  "Connect to mpv's SOCKET for the mpv PROCESS, retrying until ready.

The socket appears a fraction of a second after mpv starts, so this
retries roughly every 50ms (up to ATTEMPT 40).  Does nothing once
PROCESS is no longer the player's current process."
  (when (and (< attempt 40)
             (process-live-p process)
             (eq process (map-nested-elt ytr--state '(:player :process))))
    (condition-case nil
        (let ((ipc (make-network-process
                    :name "ytr-mpv-ipc"
                    :family 'local
                    :service socket
                    :coding 'utf-8
                    :noquery t
                    :filter #'ytr--mpv-filter)))
          (process-put ipc 'pending "")
          (process-put ipc 'request-id 0)
          (process-put ipc 'callbacks (make-hash-table :test 'eql))
          (setf (map-elt (map-elt ytr--state :player) :ipc-process) ipc)
          (ytr--mpv-send (list "observe_property" 1 "pause"))
          (ytr--mpv-send (list "observe_property" 2 "core-idle")))
      (error
       (run-at-time 0.05 nil #'ytr--mpv-connect socket process (1+ attempt))))))

(defun ytr--mpv-send (command)
  "Send COMMAND to mpv over the persistent IPC connection.

COMMAND is a list such as \\='(\"cycle\" \"pause\").  Does nothing when
the connection is not established."
  (when-let* ((ipc (map-nested-elt ytr--state '(:player :ipc-process)))
              ((process-live-p ipc)))
    (process-send-string
     ipc (concat (json-encode (list (cons 'command command))) "\n"))))

(defun ytr--mpv-request (command callback)
  "Send COMMAND to mpv and call CALLBACK with the reply data.

COMMAND is a list such as \\='(\"get_property\" \"duration\").  CALLBACK
receives the reply `data' (nil on error).  Does nothing when the
connection is not established."
  (when-let* ((ipc (map-nested-elt ytr--state '(:player :ipc-process)))
              ((process-live-p ipc))
              (id (1+ (process-get ipc 'request-id))))
    (process-put ipc 'request-id id)
    (puthash id callback (process-get ipc 'callbacks))
    (process-send-string
     ipc (concat (json-encode (list (cons 'command command)
                                    (cons 'request_id id)))
                 "\n"))))

(defun ytr--mpv-filter (process output)
  "Parse newline-delimited JSON from mpv PROCESS in OUTPUT.

Buffers partial lines, since a socket read may split or batch
messages independently of how mpv writes them."
  (let ((buffer (concat (process-get process 'pending) output)))
    (while (string-match "\n" buffer)
      (ytr--mpv-dispatch process (substring buffer 0 (match-beginning 0)))
      (setq buffer (substring buffer (match-end 0))))
    (process-put process 'pending buffer)))

(defun ytr--mpv-dispatch (process line)
  "Handle one JSON LINE from mpv PROCESS.

A message with a `request_id' is a command reply (routed to its
callback); a message with an `event' is an async notification."
  (when-let* ((msg (ignore-errors
                     (json-parse-string line :object-type 'alist
                                        :null-object nil :false-object nil))))
    (cond ((map-elt msg 'request_id)
           (when-let* ((callbacks (process-get process 'callbacks))
                       (callback (gethash (map-elt msg 'request_id) callbacks)))
             (remhash (map-elt msg 'request_id) callbacks)
             (funcall callback (map-elt msg 'data))))
          ((map-elt msg 'event)
           (ytr--mpv-event (map-elt msg 'event) msg)))))

(defun ytr--capitalize (string)
  "Return STRING with only its first character upcased.

Unlike `capitalize', the rest of STRING is left unchanged.  For
example, (ytr--capitalize \"unrecognized file format\") =>
\"Unrecognized file format\"."
  (if (string-empty-p string)
      string
    (concat (upcase (substring string 0 1)) (substring string 1))))

(defun ytr--mpv-event (event msg)
  "Mirror mpv EVENT (full alist MSG) into the player status.

`core-idle' going false means audio is flowing, so leave the loading
or paused state for `playing'.  `pause' becoming true means paused.
An `end-file' with reason \"error\" means the track could not be
played; its `file_error' is reported in the echo area."
  (pcase event
    ("property-change"
     (pcase (map-elt msg 'name)
       ("core-idle"
        (when (and (not (map-elt msg 'data))
                   (memq (map-nested-elt ytr--state '(:player :status))
                         '(loading paused)))
          (ytr--set-status 'playing)))
       ("pause"
        (when (map-elt msg 'data)
          (ytr--set-status 'paused)))))
    ("end-file"
     (when (equal (map-elt msg 'reason) "error")
       (ytr--set-status 'stopped)
       (message "%s" (ytr--capitalize (or (map-elt msg 'file_error)
                                          "playback error")))))))

(defun ytr-toggle-play ()
  "Toggle play/pause, or resume the last played track.

When a track is playing, toggle its pause state (the resulting status
follows mpv's events).  Otherwise play the last played track, if any."
  (interactive)
  (ytr--goto-button 'play)
  (cond ((process-live-p (map-nested-elt ytr--state '(:player :process)))
         (ytr--mpv-send (list "cycle" "pause")))
        ((ytr--track (map-elt ytr--state :last-played))
         (ytr--play (ytr--track (map-elt ytr--state :last-played))))
        (t
         (user-error "Nothing to play.  Press / to pick a track"))))

(defun ytr--current-or-last-track ()
  "Return the playing track, or the last played one, or nil."
  (or (map-nested-elt ytr--state '(:player :current))
      (ytr--track (map-elt ytr--state :last-played))))

(defun ytr-next ()
  "Play the next track in the catalog."
  (interactive)
  (ytr--goto-button 'next)
  (let ((track (ytr--current-or-last-track)))
    (unless track
      (user-error "Nothing to play.  Press / to pick a track"))
    (if-let* ((next (ytr--next-track track)))
        (ytr--play next)
      (user-error "No next track"))))

(defun ytr-previous ()
  "Play the previous track in the catalog."
  (interactive)
  (ytr--goto-button 'previous)
  (let ((track (ytr--current-or-last-track)))
    (unless track
      (user-error "Nothing to play.  Press / to pick a track"))
    (if-let* ((previous (ytr--previous-track track)))
        (ytr--play previous)
      (user-error "No previous track"))))

(defun ytr-seek-forward (seconds)
  "Seek forward SECONDS seconds, 5 by default.

With a plain prefix argument, seek in multiples of 60 seconds; with a
numeric prefix, seek that many seconds.  Flashes a progress bar in the
echo area afterwards."
  (interactive "P")
  (unless (process-live-p (map-nested-elt ytr--state '(:player :ipc-process)))
    (user-error "Nothing playing"))
  (unless seconds
    (setq seconds 5))
  (unless (numberp seconds)
    (setq seconds (* 60 (/ (prefix-numeric-value seconds) 4))))
  (ytr--mpv-send (list "seek" seconds))
  ;; Advance the position locally for an instant, flicker-free bar; the
  ;; one-second poll corrects any drift.
  (let ((duration (or (map-nested-elt ytr--state '(:player :current :duration)) 0)))
    (setf (map-elt (map-elt ytr--state :player) :position)
          (min duration (max 0 (+ (or (map-nested-elt ytr--state '(:player :position)) 0)
                                  seconds))))
    (ytr--update-elapsed)
    (ytr--flash (ytr--progress-bar
                 (map-nested-elt ytr--state '(:player :position))
                 duration)
                2)))

(defun ytr-seek-backward (seconds)
  "Seek backward SECONDS seconds.  See `ytr-seek-forward'."
  (interactive "P")
  (unless seconds
    (setq seconds 5))
  (unless (numberp seconds)
    (setq seconds (* 60 (/ (prefix-numeric-value seconds) 4))))
  (ytr-seek-forward (- seconds)))

(defun ytr-forward-button ()
  "Move point to the next button, wrapping around."
  (interactive)
  (forward-button 1 t)
  (ytr--skip-button-padding))

(defun ytr-backward-button ()
  "Move point to the previous button, wrapping around."
  (interactive)
  (backward-button 1 t)
  (ytr--skip-button-padding))

(defun ytr-open-in-browser ()
  "Open the current or last played track in a web browser."
  (interactive)
  (if-let* ((track (ytr--current-or-last-track)))
      (browse-url (map-elt track :url))
    (user-error "Nothing to open")))

(defun ytr-quit ()
  "Hide the player frame, or bury its window when there is no frame.
Playback continues."
  (interactive)
  (if (frame-live-p ytr--frame)
      (ytr--delete-frame)
    (quit-window)))

;;; Child frame

(defvar ytr--frame nil
  "The child frame showing the player, or nil.")

(defconst ytr--frame-border-width 1
  "Width in pixels of the child frame's border.")

(defun ytr--delete-frame ()
  "Delete the player child frame, if any, and refocus its parent."
  (when (frame-live-p ytr--frame)
    (let ((parent (frame-parent ytr--frame)))
      (delete-frame ytr--frame)
      (when (frame-live-p parent)
        (select-frame-set-input-focus parent))))
  (setq ytr--frame nil))

(defun ytr--buffer-image (buffer)
  "Return the image displayed in BUFFER, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (image)
        (while (and (not image) (not (eobp)))
          (when-let* ((display (get-text-property (point) 'display))
                      ((eq (car-safe display) 'image)))
            (setq image display))
          (goto-char (or (next-single-property-change (point) 'display)
                         (point-max))))
        image))))

(defun ytr--fit-frame (frame buffer)
  "Size FRAME to fit BUFFER, padded equally on every side.

Derives the size from the thumbnail's character dimensions (via
`image-size', which needs no realized frame) plus the surrounding text
lines, so the thumbnail is never clipped.  Falls back to a plain text
fit when BUFFER shows no image."
  (if-let* ((image (ytr--buffer-image buffer)))
      (let ((dimensions (image-size image nil frame))
            ;; `line-number-at-pos' of `point-max', not `count-lines',
            ;; so the trailing empty line is counted and the whole buffer
            ;; fits (anything off-screen would be scrollable).
            (lines (with-current-buffer buffer
                     (line-number-at-pos (point-max)))))
        (set-frame-size frame
                        ;; image columns + two spaces on each side
                        (+ (ceiling (car dimensions)) 4)
                        ;; the image's rows replace its single text line
                        (+ (1- lines) (ceiling (cdr dimensions)))))
    (make-frame-visible frame)
    (fit-frame-to-buffer frame)
    (set-frame-width frame (+ 2 (frame-width frame)))))

(defun ytr--show-frame (buffer)
  "Show BUFFER in a child frame at the bottom-right of the selected frame.

The frame is sized to fit BUFFER (with matching padding on every side),
floats above the parent's windows, and takes focus."
  (ytr--delete-frame)
  (let* ((parent (selected-frame))
         (frame-resize-pixelwise t)
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (minibuffer . nil)
                   (undecorated . t)
                   (skip-taskbar . t)
                   (no-other-frame . t)
                   (unsplittable . t)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (internal-border-width . 0)
                   (child-frame-border-width . ,ytr--frame-border-width)
                   (visibility . nil)))))
    ;; A visible, theme-aware border (the face is the background colour by
    ;; default, which would be invisible).
    (set-face-background 'child-frame-border
                         (or (face-foreground 'shadow nil t) "gray50")
                         frame)
    (set-window-buffer (frame-root-window frame) buffer)
    (set-window-dedicated-p (frame-root-window frame) t)
    (ytr--fit-frame frame buffer)
    (ytr--position-frame frame)
    (make-frame-visible frame)
    (setq ytr--frame frame)
    (select-frame-set-input-focus frame)
    frame))

(defun ytr--position-frame (frame)
  "Place FRAME at the bottom-right of its parent frame.

The bottom edge sits above the parent's mode line, clearing both the
mode line (measured, not estimated) and the echo area so neither is
covered.  External margins mirror the internal padding: two columns
horizontally, one line vertically."
  (when-let* ((parent (frame-parent frame)))
    (let ((content-bottom (- (frame-pixel-height parent)
                             (window-pixel-height (minibuffer-window parent))
                             (window-mode-line-height
                              (car (window-at-side-list parent 'bottom)))))
          (x-margin (* 2 (frame-char-width parent)))
          (y-margin (frame-char-height parent)))
      (set-frame-position frame
                          (max 0 (- (frame-pixel-width parent)
                                    (frame-pixel-width frame) x-margin))
                          (max 0 (- content-bottom
                                    (frame-pixel-height frame) y-margin))))))

(defun ytr--reposition-on-resize (frame)
  "Re-pin the player child frame when its parent FRAME changes size.
Added to `window-size-change-functions', which runs with the frame
whose size changed."
  (when (and (frame-live-p ytr--frame)
             (eq frame (frame-parent ytr--frame)))
    (ytr--position-frame ytr--frame)))

(add-hook 'window-size-change-functions #'ytr--reposition-on-resize)

;;; Entry point

(defvar ytr--loaded nil
  "Non-nil once `ytr--state' has been loaded from disk this session.")

;;;###autoload
(defun ytr ()
  "Toggle the YouTube radio.

When the player frame is showing, dismiss it (playback continues).
Otherwise load persisted state, prompt to add a channel when none
exists, resume the last played track across sessions (falling back to
the first) when nothing is playing, and show the player as a focused
child frame at the bottom-right corner.

The player relies on images and child frames, so it requires a
graphical Emacs, plus the yt-dlp and mpv programs in variable
`exec-path'."
  (interactive)
  (unless (display-graphic-p)
    (user-error "Must be running Emacs in GUI mode"))
  (unless (executable-find "yt-dlp")
    (user-error "Cannot find yt-dlp in `exec-path'"))
  (unless (executable-find "mpv")
    (user-error "Cannot find mpv in `exec-path'"))
  (if (frame-live-p ytr--frame)
      (ytr--delete-frame)
    (unless ytr--loaded
      (ytr--load)
      (setq ytr--loaded t))
    (when (map-empty-p (map-elt ytr--state :channels))
      (ytr-add-channel))
    ;; Only show the player when there is something to display.
    (unless (seq-empty-p (ytr--tracks))
      (let ((buffer (get-buffer-create "*ytr*")))
        (with-current-buffer buffer
          (unless (derived-mode-p 'ytr-mode)
            (ytr-mode)))
        (unless (memq (map-nested-elt ytr--state '(:player :status))
                      '(playing paused))
          (when-let* ((track (or (ytr--track (map-elt ytr--state :last-played))
                                 (seq-first (ytr--tracks)))))
            (ytr--play track)))
        (ytr--render)
        (ytr--show-frame buffer)))))

(provide 'ytr)

;;; ytr.el ends here
