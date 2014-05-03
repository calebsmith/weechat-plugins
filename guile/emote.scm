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
    ("joy" . "◕‿◕")
    ("nyancat" . "~=[,,_,,]:3")
)))


; Derive the tab completion string for the subcommands.
(define tab-completions
    (apply string-append
        (map (lambda (i) (string-append "|| " i))
            (hash-table-keys patterns))))


; Hook main function up to the /emote command
(weechat:hook_command
    "emote" "Emote" "/emote phrase"
    (string-append
        ""
        "\nUse `/emote phrase`. Words in phrase will be replaced with their"
        "\nemoticons:"
        "\n"
        "\nExamples:"
        "\n    /emote tableflip - (╯° °）╯︵ ┻━┻)"
        "\n    /emote look - ಠ_ಠ")
    tab-completions
    "main" "")


; Handle the IRC command given by the user. Sets input buffer as a side-effect
(define (main data buffer command)
    (weechat:buffer_set buffer "input"
        (apply string-append (map (lambda (c)
            (string-append (hash-table-ref/default patterns c c) " "))
            (string-tokenize command))))
    weechat:WEECHAT_RC_OK)
