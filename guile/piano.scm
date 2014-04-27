(use-modules (ice-9 popen))

(weechat:register "piano" "Caleb Smith" "0.1" "GPL3" "Control piano bar client" "" "")


(define subcommand-patterns '(
    (1 "start" piano-start "")
    (1 "quit" piano-quit "")
    (2 "send" piano-send "")
    (2 "station" piano-send "s")
    (1 "pause" piano-send "S")
    (1 "play" piano-send "P")
    (1 "+" piano-send "+")
    (1 "-" piano-send "-")
    (1 "next" piano-send "n")))


; Derive the tab completion string for the subcommands. E.g. ||start ||quit ...
(define subcommand-tab-completions
    (apply string-append (map (lambda (i) (string-append "|| " i))
        (map (lambda (i) (list-ref i 1)) subcommand-patterns))))


(weechat:hook_command
    "piano" "Control pianobar (a CLI client for Pandora)"
    "/piano command [arg]"
    (string-append
        "Must have pianobar installed and configured to automatically login"
        "\nUse `/piano start`, to begin. Commands include:"
        "\n/piano start - Start Pianobar"
        "\n/piano pause - pause track"
        "\n/piano play - Resume playing"
        "\n/piano next - next track"
        "\n/piano + - Love song"
        "\n/piano - - Ban song"
        "\n/piano station <number> - Switch to station <number>"
        "\n/piano send <command> - Send command directly to pianobar process"
        "\n/piano quit - Quit Pianobar")
    subcommand-tab-completions
    "main" "")

; Stop pianobar when exiting weechat
(weechat:hook_signal "quit" "piano-kill" "")


; Begin with no subprocess running
(define pianobar-pipe '())

; Mappings of command patterns to functions and signals to send to pianobar
; Format: length, subcommand function pianbar-command

; Unpacks args and passes the given command to command-handler
(define (main . args)
    (command-handler (list-ref args 2))
    weechat:WEECHAT_RC_OK)


; Handle the IRC command given by the user
(define (command-handler command)
    (map force (filter promise?
         (map (lambda (pattern)
             (build-promise command pattern)) subcommand-patterns)))
        weechat:WEECHAT_RC_OK)


; Start a pianobar subprocess
(define (piano-start message)
    (set! pianobar-pipe (open-input-output-pipe "pianobar"))
    weechat:WEECHAT_RC_OK)


; Close a pianobar subprocess
(define (piano-kill . args)
    (if (not (null? pianobar-pipe))
        (begin
            (display "q\n" pianobar-pipe)
            (set! pianobar-pipe '())))
    weechat:WEECHAT_RC_OK)

; Close pianobar and print a qutting message
(define (piano-quit message)
    (piano-kill)
    (weechat:print "" "Piano - Quitting pianobar")
    weechat:WEECHAT_RC_OK)


; Send input to the current pianobar subprocess
(define (piano-send message)
    (if (not (null? pianobar-pipe))
        (display message pianobar-pipe)
        (weechat:print "" "Pianobar is not running. use `/piano start` to start"))
    weechat:WEECHAT_RC_OK)


;; Predicate that checks if the given `command` has the given `length` and
;; whose first word matches the given `input` string
(define (matches-pattern? command len input)
    (let* ((cmd command)
          (cmd-l (string-tokenize command))
          (cmd-length (length cmd-l)))
        (if (> cmd-length 0)
            (and (string=? (car cmd-l) input) (= cmd-length len))
            (and (string=? cmd input) (= cmd-length len)))))


;; Given a `command` from IRC, returns the string that should be echoed to
;; pianobar. Eg:
;;     (build-output "station 9" "s")
;;     > "s9\n"
(define (build-output-command command output)
    (let* ((cmd-l (string-tokenize command))
           (cmd-length (length cmd-l)))
        (cond ((<= cmd-length 1)
            (string-append output "\n"))
        (else
            (string-append output (apply string-append (cdr cmd-l)) "\n")))))


;; Given a `command` from IRC, and a subcommand `pattern`, return #f if the
;; command does not match the pattern, otherwise return a promise that calls
;; the pattern's function, with its output
(define (build-promise command pattern)
    (let ((len (list-ref pattern 0))
         (input (list-ref pattern 1))
         (f (list-ref pattern 2))
         (output (list-ref pattern 3)))
        (if (matches-pattern? command len input)
            (delay ((eval f (interaction-environment))
                (build-output-command command output)))
            #f)))
