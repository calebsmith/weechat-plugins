(weechat:register "emote" "Caleb Smith" "0.1" "GPL3" "Emote" "" "")

; Mappings of command patterns to functions and their arguments
; Format: length, subcommand function message
(define subcommand-patterns '(
    ("tableflip" "(╯° °）╯︵ ┻━┻)")
    ("lookofdisapproval" "ಠ_ಠ")
))


; Derive the tab completion string for the subcommands.
(define subcommand-tab-completions
    (apply string-append (map (lambda (i) (string-append "|| " i))
        (map (lambda (i) (list-ref i 0)) subcommand-patterns))))


(weechat:hook_command
    "emote" "Emote"
    "/emote command"
    (string-append
        ""
        "\nUse `/emote command`. Commands include:"
        "\n/emote tableflip - flip"
        "\n/emote loo - the look")
    subcommand-tab-completions
    "main" "")


; Unpacks args and passes the input buffer and given command to command-handler
(define (main . args)
    (command-handler (list-ref (car args) 1) (list-ref (car args) 2))
    weechat:WEECHAT_RC_OK)


; Handle the IRC command given by the user
(define (command-handler buffer command)
     (map (lambda (pattern)
         (build-promise buffer command pattern)) subcommand-patterns)
        weechat:WEECHAT_RC_OK)


; Set users input buffer to whatever matched
(define (insert_input buffer new-input)
    (weechat:buffer_set buffer "input" new-input)
    weechat:WEECHAT_RC_OK)


;; Given an inpupt `buffer`, `command` from IRC, and a subcommand `pattern`,
;; return #f if the command does not match the pattern, otherwise return a
;; promise that calls the pattern's function, with its corresponding arguments
;; in the pattern
(define (build-promise buffer command pattern)
    (let ((input (list-ref pattern 0))
         (arg (list-ref pattern 1)))
        (if (string=? command input)
            (insert_input buffer arg))))
