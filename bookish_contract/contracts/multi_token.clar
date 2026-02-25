;; Multi-Token Wallet Smart Contract
;; Supports multiple token types and tracks balances per token

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-token (err u102))
(define-constant err-transfer-failed (err u103))
(define-constant err-unauthorized (err u104))

;; Data Variables
(define-data-var wallet-nonce uint u0)

;; Data Maps
;; Track balances: (wallet-address, token-contract) -> balance
(define-map token-balances 
  { wallet: principal, token: principal } 
  uint
)

;; Track supported tokens
(define-map supported-tokens principal bool)

;; Track wallet owners
(define-map wallet-owners principal bool)

;; Events (using print for logging)
(define-private (log-deposit (wallet principal) (token principal) (amount uint))
  (print {
    event: "deposit",
    wallet: wallet,
    token: token,
    amount: amount,
    block-height: block-height
  })
)

(define-private (log-withdrawal (wallet principal) (token principal) (amount uint) (recipient principal))
  (print {
    event: "withdrawal",
    wallet: wallet,
    token: token,
    amount: amount,
    recipient: recipient,
    block-height: block-height
  })
)

;; Read-only functions

;; Get balance for a specific wallet and token
(define-read-only (get-balance (wallet principal) (token principal))
  (default-to u0 (map-get? token-balances { wallet: wallet, token: token }))
)

;; Get all balances for a wallet (note: this would need to be called per token in practice)
(define-read-only (get-wallet-balance (wallet principal) (token principal))
  (get-balance wallet token)
)

;; Check if token is supported
(define-read-only (is-token-supported (token principal))
  (default-to false (map-get? supported-tokens token))
)

;; Check if address is a wallet owner
(define-read-only (is-wallet-owner (wallet principal))
  (default-to false (map-get? wallet-owners wallet))
)

;; Private functions

;; Update balance for a wallet and token
(define-private (set-balance (wallet principal) (token principal) (amount uint))
  (map-set token-balances { wallet: wallet, token: token } amount)
)

;; Add to balance
(define-private (add-balance (wallet principal) (token principal) (amount uint))
  (let ((current-balance (get-balance wallet token)))
    (set-balance wallet token (+ current-balance amount))
  )
)

;; Subtract from balance
(define-private (subtract-balance (wallet principal) (token principal) (amount uint))
  (let ((current-balance (get-balance wallet token)))
    (if (>= current-balance amount)
        (begin
          (set-balance wallet token (- current-balance amount))
          (ok true)
        )
        err-insufficient-balance
    )
  )
)

;; Public functions

;; Initialize wallet for a user
(define-public (initialize-wallet)
  (begin
    (map-set wallet-owners tx-sender true)
    (ok true)
  )
)

;; Add supported token (only contract owner)
(define-public (add-supported-token (token principal))
  (if (is-eq tx-sender contract-owner)
      (begin
        (map-set supported-tokens token true)
        (ok true)
      )
      err-owner-only
  )
)

;; Remove supported token (only contract owner)
(define-public (remove-supported-token (token principal))
  (if (is-eq tx-sender contract-owner)
      (begin
        (map-delete supported-tokens token)
        (ok true)
      )
      err-owner-only
  )
)

;; Deposit STX to wallet
(define-public (deposit-stx (amount uint))
  (begin
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update balance
    (add-balance tx-sender .STX amount)
    ;; Log event
    (log-deposit tx-sender .STX amount)
    (ok true)
  )
)

;; Deposit SIP-010 token to wallet
(define-public (deposit-token (token-contract <sip-010-trait>) (amount uint))
  (let ((token-principal (contract-of token-contract)))
    ;; Check if token is supported
    (asserts! (is-token-supported token-principal) err-invalid-token)
    ;; Transfer token from sender to contract
    (try! (contract-call? token-contract transfer amount tx-sender (as-contract tx-sender) none))
    ;; Update balance
    (add-balance tx-sender token-principal amount)
    ;; Log event
    (log-deposit tx-sender token-principal amount)
    (ok true)
  )
)

;; Withdraw STX from wallet
(define-public (withdraw-stx (amount uint) (recipient principal))
  (begin
    ;; Check if sender has sufficient balance
    (try! (subtract-balance tx-sender .STX amount))
    ;; Transfer STX from contract to recipient
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    ;; Log event
    (log-withdrawal tx-sender .STX amount recipient)
    (ok true)
  )
)

;; Withdraw SIP-010 token from wallet
(define-public (withdraw-token (token-contract <sip-010-trait>) (amount uint) (recipient principal))
  (let ((token-principal (contract-of token-contract)))
    ;; Check if token is supported
    (asserts! (is-token-supported token-principal) err-invalid-token)
    ;; Check if sender has sufficient balance
    (try! (subtract-balance tx-sender token-principal amount))
    ;; Transfer token from contract to recipient
    (try! (as-contract (contract-call? token-contract transfer amount tx-sender recipient none)))
    ;; Log event
    (log-withdrawal tx-sender token-principal amount recipient)
    (ok true)
  )
)

;; Transfer tokens between wallets (internal transfer)
(define-public (internal-transfer (token principal) (amount uint) (recipient principal))
  (begin
    ;; Check if token is supported
    (asserts! (is-token-supported token) err-invalid-token)
    ;; Check if sender has sufficient balance
    (try! (subtract-balance tx-sender token amount))
    ;; Add to recipient balance
    (add-balance recipient token amount)
    ;; Log transfer events
    (log-withdrawal tx-sender token amount recipient)
    (log-deposit recipient token amount)
    (ok true)
  )
)

;; Batch operations for efficiency

;; Get balances for multiple tokens
(define-read-only (get-balances (wallet principal) (tokens (list 10 principal)))
  (map get-balance-pair 
    (map create-balance-pair tokens (list wallet wallet wallet wallet wallet wallet wallet wallet wallet wallet))
  )
)

(define-private (create-balance-pair (token principal) (wallet principal))
  { wallet: wallet, token: token }
)

(define-private (get-balance-pair (pair { wallet: principal, token: principal }))
  {
    token: (get token pair),
    balance: (get-balance (get wallet pair) (get token pair))
  }
)

;; Emergency functions (only contract owner)

;; Emergency withdrawal of STX
(define-public (emergency-withdraw-stx (amount uint))
  (if (is-eq tx-sender contract-owner)
      (as-contract (stx-transfer? amount tx-sender contract-owner))
      err-owner-only
  )
)

;; Emergency withdrawal of tokens
(define-public (emergency-withdraw-token (token-contract <sip-010-trait>) (amount uint))
  (if (is-eq tx-sender contract-owner)
      (as-contract (contract-call? token-contract transfer amount tx-sender contract-owner none))
      err-owner-only
  )
)

;; Initialize with STX as supported token
(map-set supported-tokens .STX true)

;; SIP-010 trait definition (for reference)
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)