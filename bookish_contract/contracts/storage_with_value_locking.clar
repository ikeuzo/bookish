;; Enhanced Storage with Value Locking Smart Contract
;; Advanced storage contract with multiple values, history tracking, and flexible access control

;; Define contract owner
(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-value-locked (err u101))
(define-constant err-invalid-key (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-already-locked (err u104))
(define-constant err-invalid-time (err u105))
(define-constant err-not-found (err u106))

;; Maximum number of history entries to keep
(define-constant max-history-entries u50)

;; Data variables for main storage
(define-data-var stored-value uint u0)
(define-data-var is-locked bool false)
(define-data-var lock-timestamp (optional uint) none)

;; Counter for history entries
(define-data-var history-counter uint u0)

;; Maps for extended functionality
(define-map key-value-store 
  { key: (string-ascii 64) } 
  { value: uint, locked: bool, owner: principal }
)

(define-map authorized-users principal bool)

(define-map value-history 
  { index: uint } 
  { value: uint, timestamp: uint, changed-by: principal }
)

(define-map lock-schedule 
  { key: (string-ascii 64) } 
  { lock-time: uint, auto-lock: bool }
)

;; Helper functions
(define-private (is-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-authorized)
  (or (is-owner) 
      (default-to false (map-get? authorized-users tx-sender))
  )
)

(define-private (add-to-history (value uint))
  (let ((current-counter (var-get history-counter)))
    (if (< current-counter max-history-entries)
      (begin
        (map-set value-history 
          { index: current-counter }
          { value: value, timestamp: block-height, changed-by: tx-sender })
        (var-set history-counter (+ current-counter u1)))
      ;; If at max, overwrite oldest entry (circular buffer)
      (begin
        (map-set value-history 
          { index: (mod current-counter max-history-entries) }
          { value: value, timestamp: block-height, changed-by: tx-sender })
        (var-set history-counter (+ current-counter u1))
      )
    )
  )
)

;; Enhanced value setting with history tracking
(define-public (set-value (new-value uint))
  (begin
    (asserts! (is-authorized) err-unauthorized)
    (asserts! (not (var-get is-locked)) err-value-locked)
    (var-set stored-value new-value)
    (add-to-history new-value)
    (ok true)
  )
)

;; Set value with automatic lock after specified blocks
(define-public (set-value-with-auto-lock (new-value uint) (lock-after-blocks uint))
  (begin
    (asserts! (is-authorized) err-unauthorized)
    (asserts! (not (var-get is-locked)) err-value-locked)
    (asserts! (> lock-after-blocks u0) err-invalid-time)
    (var-set stored-value new-value)
    (add-to-history new-value)
    (map-set lock-schedule 
      { key: "main" }
      { lock-time: (+ block-height lock-after-blocks), auto-lock: true })
    (ok true)
  )
)

;; Lock the main value
(define-public (lock-value)
  (begin
    (asserts! (is-owner) err-owner-only)
    (asserts! (not (var-get is-locked)) err-already-locked)
    (var-set is-locked true)
    (var-set lock-timestamp (some block-height))
    (ok true)
  )
)

;; Emergency unlock (owner only, for special circumstances)
(define-public (emergency-unlock)
  (begin
    (asserts! (is-owner) err-owner-only)
    (var-set is-locked false)
    (var-set lock-timestamp none)
    (ok true)
  )
)

;; Key-value store operations
(define-public (set-key-value (key (string-ascii 64)) (value uint))
  (let ((existing (map-get? key-value-store { key: key })))
    (begin
      (asserts! (is-authorized) err-unauthorized)
      ;; Check if key exists and is locked
      (asserts! (match existing
        entry (not (get locked entry))
        true) err-value-locked)
      (map-set key-value-store 
        { key: key } 
        { value: value, locked: false, owner: tx-sender })
      (ok true)
    )
  )
)

(define-public (lock-key (key (string-ascii 64)))
  (let ((existing (map-get? key-value-store { key: key })))
    (begin
      (asserts! (is-authorized) err-unauthorized)
      (asserts! (is-some existing) err-not-found)
      (asserts! (or (is-owner) 
                   (is-eq tx-sender (get owner (unwrap-panic existing)))) err-unauthorized)
      (map-set key-value-store 
        { key: key } 
        (merge (unwrap-panic existing) { locked: true }))
      (ok true)
    )
  )
)

;; Access control functions
(define-public (authorize-user (user principal))
  (begin
    (asserts! (is-owner) err-owner-only)
    (map-set authorized-users user true)
    (ok true)
  )
)

(define-public (revoke-user (user principal))
  (begin
    (asserts! (is-owner) err-owner-only)
    (map-delete authorized-users user)
    (ok true)
  )
)

;; Batch operations
(define-public (batch-set-values (values (list 10 { key: (string-ascii 64), value: uint })))
  (begin
    (asserts! (is-authorized) err-unauthorized)
    (fold check-and-set values (ok true))
  )
)

(define-private (check-and-set (item { key: (string-ascii 64), value: uint }) (prev-result (response bool uint)))
  (match prev-result
    ok-val (set-key-value (get key item) (get value item))
    err-val (err err-val)
  )
)

;; Auto-lock checker (should be called periodically)
(define-public (process-auto-locks)
  (let ((schedule (map-get? lock-schedule { key: "main" })))
    (match schedule
      entry (if (and (get auto-lock entry) 
                    (>= block-height (get lock-time entry))
                    (not (var-get is-locked)))
              (begin
                (var-set is-locked true)
                (var-set lock-timestamp (some block-height))
                (map-delete lock-schedule { key: "main" })
                (ok true))
              (ok false))
      (ok false)
    )
  )
)

;; Read-only functions
(define-read-only (get-value-and-status)
  {
    value: (var-get stored-value),
    locked: (var-get is-locked),
    lock-timestamp: (var-get lock-timestamp),
    history-count: (var-get history-counter)
  }
)

(define-read-only (get-key-value (key (string-ascii 64)))
  (map-get? key-value-store { key: key })
)

(define-read-only (get-history-entry (index uint))
  (map-get? value-history { index: index })
)

(define-read-only (get-recent-history (count uint))
  (let ((current-counter (var-get history-counter))
        (start-index (if (>= current-counter count) (- current-counter count) u0)))
    (map get-history-entry-safe (list u0 u1 u2 u3 u4))
  )
)

(define-private (get-history-entry-safe (offset uint))
  (let ((current-counter (var-get history-counter)))
    (if (> current-counter offset)
      (map-get? value-history { index: (- current-counter offset u1) })
      none
    )
  )
)

(define-read-only (is-user-authorized (user principal))
  (or (is-eq user contract-owner)
      (default-to false (map-get? authorized-users user))
  )
)

(define-read-only (get-lock-schedule (key (string-ascii 64)))
  (map-get? lock-schedule { key: key })
)

(define-read-only (get-contract-stats)
  {
    owner: contract-owner,
    main-value: (var-get stored-value),
    main-locked: (var-get is-locked),
    total-history: (var-get history-counter),
    current-block: block-height
  }
)

;; Utility functions
(define-read-only (get-value)
  (var-get stored-value)
)

(define-read-only (is-value-locked)
  (var-get is-locked)
)

(define-read-only (get-owner)
  contract-owner)