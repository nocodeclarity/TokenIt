;; TokenIt
;; Cultivated By NoCodeClarity, Inc.
;; Dedicated to the Builders and All of the Real Estate Equity Seekers 

(define-data-var token-price ufixed)

(define-data-var total-tokens uint)

(define-data-var lockup-period uint)

(define-map token-holders
  ((address buffer) uint))

(define-read-only (get-token-price)
  token-price)

(define-read-only (get-total-tokens)
  total-tokens)

(define-read-only (get-lockup-period)
  lockup-period)

(define-read-only (get-token-holders)
  (map to-json (into {} (map (lambda (holder)
                              (list (standard-lib:serialize-buffer holder) (get token-holders holder)))
                            (keys token-holders)))))


(define-public (buy-tokens (tokens uint))
  (require (> tokens 0) "Number of tokens must be greater than 0")
  (require (ft-get-balance tx-sender) (>= (ufixed-mul (uint-to-ufixed tokens) token-price) (ft-balance)))
  (ft-transfer tx-sender (this-contract) (ufixed-mul (uint-to-ufixed tokens) token-price))
  (let ((holder-balance (or (get token-holders tx-sender) 0)))
    (token-holders{tx-sender} (+ holder-balance tokens))
    (set total-tokens (+ total-tokens tokens))))


(define-private (transfer-tokens (to-address address) (tokens uint))
  (require (>= (get-block-height) (+ (get-lockup-period) 1))) ; ensure lockup period is over
  (let ((sender-balance (or (get token-holders tx-sender) 0)))
    (require (>= sender-balance tokens) "Insufficient tokens to transfer")
    (token-holders{tx-sender} (- sender-balance tokens))
    (let ((recipient-balance (or (get token-holders to-address) 0)))
      (token-holders{to-address} (+ recipient-balance tokens)))))


(define-private (burn-tokens (tokens uint))
  (let ((total-tokens-after-burn (- total-tokens tokens)))
    (require (>= total-tokens-after-burn 0) "Burned too many tokens")
    (set total-tokens total-tokens-after-burn)))


(define-public (sell-tokens (tokens uint))
  (let ((sender-balance (or (get token-holders tx-sender) 0)))
    (require (>= (get-block-height) (+ (get-lockup-period) 1))) ; ensure lockup period is over
    (require (>= sender-balance tokens) "Insufficient tokens to sell")
    (ft-transfer (this-contract) tx-sender (ufixed-mul (uint-to-ufixed tokens) token-price))
    (token-holders{tx-sender} (- sender-balance tokens))
    (burn-tokens tokens)))


(define-read-only (is-lockup-period-over?)
  (>= (get-block-height) (+ (get-lockup-period) 1)))


(define-read-only (get-token-holder-balance (holder-address address))
  (get token-holders holder-address))


(define-read-only (get-token-holder-index (holder-address address))
  (let ((holders (keys token-holders)))
    (let loop ((index 0) (holders holders))
      (cond
        ((null? holders) -1)
        ((equal? holder-address (standard-lib:serialize-buffer (car holders))) index)
        (else (loop (+ index 1) (cdr holders)))))))

        
(define-public (withdraw (amount uint))
(require (== tx-sender (get-contract-owner)))
(ft-transfer tx-sender (ufixed-mul (uint-to-ufixed amount) token-price))
(set token-price (ufixed-div (ufixed-mul token-price (uint-to-ufixed (get-total-tokens))) (uint-to-ufixed (- (get-total-tokens) amount))))
(burn-tokens amount))

(define-public (update-token-price (price ufixed))
(require (== tx-sender (get-contract-owner)))
(set token-price price))

(define-public (update-lockup-period (period uint))
(require (== tx-sender (get-contract-owner)))
(set lockup-period period))
