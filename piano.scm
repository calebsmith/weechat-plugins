(use-modules (ice-9 popen))

(weechat:register "piano" "MCConsALot" "0.1" "GPL3" "Control piano bar client" "" "")

(weechat:hook_command
    "piano" "Control pianobar"
    "[command]" "command to send to pianobar. Use /piano start, to begin"
    "start next quit station send"
    "main" "")


; Begin with no subprocess running
(define pianobar-pipe '())

; Mappings of command patterns to functions and signals to send to pianobar
; Format: length, subcommand function pianbar-command
(define subcommand-patterns '(
    (list 1 "start" piano-start "")
    (list 1 "quit" piano-quit "")
    (list 2 "send" piano-send "")
    (list 2 "station" piano-send "s")
    (list 1 "next" piano-send "n")))


; Unpacks args and passes the given command to command-handler
(define (main . args)
    (command-handler (list-ref (car args) 2))
    weechat:WEECHAT_RC_OK)


; Handle the IRC command given by the user
(define (command-handler command)
    (let ((p (filter promise?
             (map (lambda (pattern)
                 (build-promise command pattern))
                 subcommand-patterns))))
        (if (> (length p) 0)
            (force (car p))
            (weechat:print "" "Not a valid command. Try /help piano")))
        weechat:WEECHAT_RC_OK)


; Start a pianobar subprocess
(define (piano-start message)
    (set! pianobar-pipe (open-input-output-pipe "pianobar"))
    weechat:WEECHAT_RC_OK)


; Close a pianobar subprocess
(define (piano-quit message)
    (if (not (null? pianobar-pipe))
        (begin
            (display "q\n" pianobar-pipe)
            (display "Piano - Quitting pianobar")
            (newline)))
            (set! pianobar-pipe #f)
    weechat:WEECHAT_RC_OK)


; Send input to the current pianobar subprocess
(define (piano-send message)
    (if (not (null? pianobar-pipe))
        (display message pianobar-pipe)
        (begin
            (display "Pianobar is not running. use `/piano start` to start")
            (newline)))
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
    (let ((len (list-ref pattern 1))
         (input (list-ref pattern 2))
         (f (list-ref pattern 3))
         (output (list-ref pattern 4)))
        (if (matches-pattern? command len input)
            (delay ((eval f (interaction-environment))
                (build-output-command command output)))
            #f)))
