(defpackage :ugo
  (:use :cl)
  (:export
   ;; board and 数气制 scoring
   #:*board*
   #:*dot*
   #:*white*
   #:*black*
   #:score-qi
   #:score-qi-of
   #:qi-winner
   #:show-qi-score
   ;; AI
   #:ai-move
   #:ai-best-move
   #:mcts-best-move
   #:minimax-best-move
   #:greedy-best-move
   #:ai-self-play
   #:qi-margin
   #:qi-outcome
   #:*ai-engine*
   #:*ai-random-state*
   #:*ai-simulations*
   #:*ai-time-limit*
   #:*uct-c*
   #:*rollout-depth*
   #:*rollout-samples*
   #:*rollout-policy*
   #:*score-scale*
   #:*passes-to-end*
   ;; practice mode
   #:practice))
