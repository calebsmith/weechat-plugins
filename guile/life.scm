(weechat:register "life" "Caleb Smith" "0.1" "GPL3" "Conway's Game of Life" "" "")
(weechat:hook_command "life" "Conway's Game of Life" "/life" "" "" "main" "")

(weechat:hook_timer 500 0 0 "tick" "")


(define (curry fun . args)
    (lambda x
        (apply fun (append args x))))


(define board-width 32)
(define board-height 32)


(define board (make-vector (* board-width board-height) 0))


(define (board-set! board state x y)
    (vector-set! board (+ (* y board-width) x) state))


(board-set! board 1 5 3)
(board-set! board 1 5 4)
(board-set! board 1 5 5)
(board-set! board 1 4 5)
(board-set! board 1 3 4)


(define (board-number->display-string num)
    (if (= num 0)
        " "
        "o"))


(define (board-get-cell board xin yin)
    (let ((x (modulo xin board-width))
        (y (modulo yin board-height)))
        (if (zero? (vector-ref board (+ (* y board-width) x)))
            0
            1)))


(define (board-count-neighbor board x y)
    (let ((get-cell (curry board-get-cell board)))
        (apply + (map (lambda (x-offset y-offset)
            (apply get-cell (list (+ x x-offset) (+ y y-offset))))
                '(-1 0 1 -1 1 -1 0 1)
                '(-1 -1 -1 0 0 1 1 1)))))


(define (board-neighbor->cell c n)
    (if (= c 0)
        (if (= n 3)
            1
            0)
        (if (or (= n 2) (= n 3))
            1
            0)))


(define (board-generate board)
    (define new-board (make-vector (* board-width board-height)))
    (map (lambda (y)
       (map (lambda (x)
            (let ((num-neighbors (board-count-neighbor board x y))
                  (current (board-get-cell board x y)))
                (board-set! new-board (board-neighbor->cell current num-neighbors) x y)))
            (iota board-width)))
       (iota board-height))
    new-board)


(define (board-display board)
    (weechat:buffer_clear life-buffer)
    (let ((row (make-vector (1+ board-width) 0)))
        (map (lambda (row-index)
             (let ((start (* row-index board-width)))
                  (vector-move-left! board start (+ start board-width) row 0)
                  (weechat:print_y life-buffer row-index
                      (string-drop-right (apply string-append (map board-number->display-string (vector->list row))) 1))))
                  (iota board-height))))


(define life-buffer '())


(define (life-input-buffer . args)
    weechat:WEECHAT_RC_OK)


(define (life-init)
    (set! life-buffer (weechat:buffer_new "life" "life-input-buffer" "" "" ""))
    (weechat:buffer_set life-buffer "type" "free")
    (weechat:buffer_set life-buffer "title" "Life")
    (weechat:buffer_set life-buffer "display" "1"))


(define (main . args)
    (life-init)
    (board-display board)
    weechat:WEECHAT_RC_OK)


(define (tick . args)
    (if (not (null? life-buffer))
        (begin
            (set! board (board-generate board))
            (board-display board)))
    weechat:WEECHAT_RC_OK)
