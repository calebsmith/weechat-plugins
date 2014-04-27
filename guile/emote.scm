(use-modules (srfi srfi-69))

(weechat:register "emote" "Caleb Smith" "0.1" "GPL3" "Emote" "" "")

; Mappings of words with their emoticons
(define patterns (alist->hash-table '(
    ("tableflip" . "(╯° °）╯︵ ┻━┻)")
    ("rageflip" . "(ノಠ益ಠ)ノ彡┻━┻")
    ("doubleflip" . "┻━┻ ︵ヽ(`Д´)ﾉ︵ ┻━┻")
    ("lookofdisapproval" . "ಠ_ಠ")
    ("sun" . "☼")
    ("kitaa" . "キタ━━━(゜∀゜)━━━!!!!!")
)))


; Derive the tab completion string for the subcommands.
(define tab-completions
    (apply string-append
        (map (lambda (i) (string-append "|| " i))
            (hash-table-keys patterns))))


(weechat:hook_command
    "emote" "Emote"
    "/emote command"
    (string-append
        ""
        "\nUse `/emote phrase`. Words in phrase will be replaced with their"
        "\nemoticons:"
        "\n/tableflip - flip"
        "\n/look - the look")
    tab-completions
    "main" "")


; Unpacks args and passes the input buffer and given command to command-handler
(define (main . args)
    (command-handler (list-ref args 1) (list-ref args 2))
    weechat:WEECHAT_RC_OK)


; Handle the IRC command given by the user. Sets input buffer as a side-effect
(define (command-handler buffer command)
    (weechat:buffer_set buffer "input"
        (apply string-append (map (lambda (c)
            (string-append (hash-table-ref/default patterns c c) " "))
            (string-tokenize command))))
    weechat:WEECHAT_RC_OK)
