;; Auto-Deposit Wallet Contract
;; Automatically forwards all incoming deposits to a predefined cold wallet

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-address (err u101))
(define-constant err-transfer-failed (err u102))
(define-constant err-insufficient-balance (err u103))

;; Data Variables
(define-data-var cold-wallet-address (optional principal) none)
(define-data-var is-active bool true)

;; Read-only functions
(define-read-only (get-cold-wallet-address)
  (var-get cold-wallet-address)
)

(define-read-only (is-contract-active)
  (var-get is-active)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Owner-only functions
(define-public (set-cold-wallet (new-address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set cold-wallet-address (some new-address))
    (ok true)
  )
)

(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set is-active (not (var-get is-active)))
    (ok (var-get is-active))
  )
)

;; Main deposit function
(define-public (deposit)
  (let (
    (cold-wallet (var-get cold-wallet-address))
    (contract-balance (stx-get-balance (as-contract tx-sender)))
  )
    ;; Check if contract is active
    (asserts! (var-get is-active) (err u104))
    
    ;; Check if cold wallet is set
    (asserts! (is-some cold-wallet) err-invalid-address)
    
    ;; Check if there's a balance to forward
    (asserts! (> contract-balance u0) err-insufficient-balance)
    
    ;; Forward all funds to cold wallet
    (match (as-contract (stx-transfer? contract-balance tx-sender (unwrap-panic cold-wallet)))
      success (ok contract-balance)
      error err-transfer-failed
    )
  )
)

;; Auto-forward function (can be called by anyone to trigger forwarding)
(define-public (auto-forward)
  (let (
    (cold-wallet (var-get cold-wallet-address))
    (contract-balance (stx-get-balance (as-contract tx-sender)))
  )
    ;; Check if contract is active
    (asserts! (var-get is-active) (err u104))
    
    ;; Check if cold wallet is set
    (asserts! (is-some cold-wallet) err-invalid-address)
    
    ;; Only forward if there's a meaningful balance (> 1 STX to account for fees)
    (asserts! (> contract-balance u1000000) err-insufficient-balance)
    
    ;; Forward funds minus a small amount for fees
    (let ((transfer-amount (- contract-balance u100000)))
      (match (as-contract (stx-transfer? transfer-amount tx-sender (unwrap-panic cold-wallet)))
        success (ok transfer-amount)
        error err-transfer-failed
      )
    )
  )
)

;; Receive STX function (payable)
(define-public (receive-stx (amount uint))
  (begin
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; If auto-forwarding is enabled and cold wallet is set, forward immediately
    (if (and (var-get is-active) (is-some (var-get cold-wallet-address)))
      (match (auto-forward)
        success (ok amount)
        error (ok amount) ;; Even if auto-forward fails, deposit succeeds
      )
      (ok amount)
    )
  )
)

;; Emergency withdrawal (owner only)
(define-public (emergency-withdraw)
  (let (
    (contract-balance (stx-get-balance (as-contract tx-sender)))
  )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> contract-balance u0) err-insufficient-balance)
    
    (match (as-contract (stx-transfer? contract-balance tx-sender contract-owner))
      success (ok contract-balance)
      error err-transfer-failed
    )
  )
)

;; Initialize contract with cold wallet address
(define-public (initialize (cold-wallet principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-none (var-get cold-wallet-address)) (err u105)) ;; Already initialized
    (var-set cold-wallet-address (some cold-wallet))
    (ok true)
  )
)