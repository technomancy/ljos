;; This module contains all the state for the whole editor.
;; It shouldn't be considered part of the public API; to change the
;; state, use the functions in the polywell module.
(local frontend (require :polywell.frontend))

{:kill-ring {}

 :activate-patterns {}

 ;; allows for multi-step key bindings a la Emacs ctrl-x ctrl-s
 :active-prefix nil
 :active-prefix-deactivate nil

 :windows []
 :window 1

 :coroutines []

 ;; echo messages show feedback while in the editor, until a new key is pressed
 :echo-message nil
 :echo-message-new false

 ;; for the default value in interactive buffer switching
 :last-buffer nil ; TODO: use a buffer history ring

 ;; where does print go? (essentially used as dynamic scope)
 :output nil

 ;; current buffer
 :b nil
 ;; all buffers, indexed by path
 :buffers {}

 ;; if you want to write to a filesystem that isn't the disk, provide a new
 ;; table with these three elements as a 4th arg to polywell.init.
 :fs {:read frontend.read :write frontend.write
      :type frontend.type :ls frontend.ls}

 :cwd (os.getenv "PWD")

 ;; colors! you can change these; themeing I guess?
 :colors {
  :mark (frontend.normalize-color [0 125 0])
  :point (frontend.normalize-color [0 125 0])
  :point-line (frontend.normalize-color [0 50 0 190])
  :minibuffer-bg (frontend.normalize-color [0 200 0])
  :minibuffer-fg (frontend.normalize-color [0 0 0])
  :scroll-bar (frontend.normalize-color [0 150 0])
  :background (frontend.normalize-color [0 0 0 240])
  :text (frontend.normalize-color [0 175 0] :text)
  ;; for programming
  :keyword (frontend.normalize-color [0 255 0] :keyword)
  :str (frontend.normalize-color [200 100 0] :str)
  :number (frontend.normalize-color [50 175 120] :number)
  :comment (frontend.normalize-color [0 100 0] :comment)
  }

 ;; added thru the add-mode function, defined in config/
 :modes {}
 }
