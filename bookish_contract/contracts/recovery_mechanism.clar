;; RecoveryMechanism Smart Contract
;; Implements multi-signature and social recovery for file access

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-FILE (err u101))
(define-constant ERR-RECOVERY-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-RECOVERED (err u103))
(define-constant ERR-INSUFFICIENT-SIGNATURES (err u104))
(define-constant ERR-INVALID-GUARDIAN (err u105))
(define-constant ERR-RECOVERY-EXPIRED (err u106))
(define-constant ERR-DUPLICATE-SIGNATURE (err u107))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant RECOVERY-TIMEOUT u144) ;; blocks (approximately 24 hours)
(define-constant MIN-GUARDIANS u3)
(define-constant MIN-SIGNATURES u2)

;; Data structures for file metadata
(define-map file-metadata
    { file-id: (buff 32) }
    {
        owner: principal,
        encrypted-metadata: (buff 256),
        guardians: (list 10 principal),
        required-signatures: uint,
        created-at: uint,
        is-active: bool
    }
)

;; Recovery requests
(define-map recovery-requests
    { request-id: (buff 32) }
    {
        file-id: (buff 32),
        requester: principal,
        guardian-signatures: (list 10 principal),
        signature-count: uint,
        created-at: uint,
        expires-at: uint,
        is-completed: bool,
        new-owner: principal
    }
)

;; Guardian verification status
(define-map guardian-verifications
    { request-id: (buff 32), guardian: principal }
    { verified: bool, timestamp: uint }
)

;; User guardian settings
(define-map user-guardians
    { user: principal }
    {
        guardians: (list 10 principal),
        required-signatures: uint,
        last-updated: uint
    }
)

;; Events
(define-data-var recovery-counter uint u0)

;; Helper functions
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (generate-request-id)
    (let ((counter (+ (var-get recovery-counter) u1)))
        (var-set recovery-counter counter)
        (hash160 (concat (unwrap-panic (to-consensus-buff? counter)) 
                        (unwrap-panic (to-consensus-buff? block-height))))
    )
)

(define-private (is-valid-guardian (file-id (buff 32)) (guardian principal))
    (match (map-get? file-metadata { file-id: file-id })
        file-data (is-some (index-of (get guardians file-data) guardian))
        false
    )
)

(define-private (has-already-signed (request-id (buff 32)) (guardian principal))
    (match (map-get? guardian-verifications { request-id: request-id, guardian: guardian })
        verification (get verified verification)
        false
    )
)

;; Public functions

;; Register file metadata with recovery settings
(define-public (register-file 
    (file-id (buff 32))
    (encrypted-metadata (buff 256))
    (guardians (list 10 principal))
    (required-signatures uint))
    (begin
        (asserts! (>= (len guardians) MIN-GUARDIANS) ERR-INVALID-GUARDIAN)
        (asserts! (>= required-signatures MIN-SIGNATURES) ERR-INSUFFICIENT-SIGNATURES)
        (asserts! (<= required-signatures (len guardians)) ERR-INSUFFICIENT-SIGNATURES)
        
        (map-set file-metadata
            { file-id: file-id }
            {
                owner: tx-sender,
                encrypted-metadata: encrypted-metadata,
                guardians: guardians,
                required-signatures: required-signatures,
                created-at: block-height,
                is-active: true
            }
        )
        (ok file-id)
    )
)

;; Set up guardian configuration for a user
(define-public (setup-guardians 
    (guardians (list 10 principal))
    (required-signatures uint))
    (begin
        (asserts! (>= (len guardians) MIN-GUARDIANS) ERR-INVALID-GUARDIAN)
        (asserts! (>= required-signatures MIN-SIGNATURES) ERR-INSUFFICIENT-SIGNATURES)
        (asserts! (<= required-signatures (len guardians)) ERR-INSUFFICIENT-SIGNATURES)
        
        (map-set user-guardians
            { user: tx-sender }
            {
                guardians: guardians,
                required-signatures: required-signatures,
                last-updated: block-height
            }
        )
        (ok true)
    )
)

;; Initiate recovery request
(define-public (initiate-recovery 
    (file-id (buff 32))
    (new-owner principal))
    (let ((request-id (generate-request-id)))
        (match (map-get? file-metadata { file-id: file-id })
            file-data
            (begin
                (asserts! (get is-active file-data) ERR-INVALID-FILE)
                
                (map-set recovery-requests
                    { request-id: request-id }
                    {
                        file-id: file-id,
                        requester: tx-sender,
                        guardian-signatures: (list),
                        signature-count: u0,
                        created-at: block-height,
                        expires-at: (+ block-height RECOVERY-TIMEOUT),
                        is-completed: false,
                        new-owner: new-owner
                    }
                )
                (ok request-id)
            )
            ERR-INVALID-FILE
        )
    )
)

;; Guardian verification of recovery request
(define-public (verify-recovery 
    (request-id (buff 32)))
    (match (map-get? recovery-requests { request-id: request-id })
        request-data
        (let ((file-id (get file-id request-data)))
            (begin
                (asserts! (< block-height (get expires-at request-data)) ERR-RECOVERY-EXPIRED)
                (asserts! (not (get is-completed request-data)) ERR-ALREADY-RECOVERED)
                (asserts! (is-valid-guardian file-id tx-sender) ERR-INVALID-GUARDIAN)
                (asserts! (not (has-already-signed request-id tx-sender)) ERR-DUPLICATE-SIGNATURE)
                
                ;; Record guardian verification
                (map-set guardian-verifications
                    { request-id: request-id, guardian: tx-sender }
                    { verified: true, timestamp: block-height }
                )
                
                ;; Update request with new signature
                (let ((new-signature-count (+ (get signature-count request-data) u1))
                      (updated-signatures (unwrap-panic (as-max-len? 
                          (append (get guardian-signatures request-data) tx-sender) u10))))
                    (map-set recovery-requests
                        { request-id: request-id }
                        (merge request-data {
                            guardian-signatures: updated-signatures,
                            signature-count: new-signature-count
                        })
                    )
                    (ok new-signature-count)
                )
            )
        )
        ERR-RECOVERY-NOT-FOUND
    )
)

;; Complete recovery process
(define-public (complete-recovery 
    (request-id (buff 32)))
    (match (map-get? recovery-requests { request-id: request-id })
        request-data
        (let ((file-id (get file-id request-data)))
            (match (map-get? file-metadata { file-id: file-id })
                file-data
                (begin
                    (asserts! (< block-height (get expires-at request-data)) ERR-RECOVERY-EXPIRED)
                    (asserts! (not (get is-completed request-data)) ERR-ALREADY-RECOVERED)
                    (asserts! (>= (get signature-count request-data) 
                                (get required-signatures file-data)) ERR-INSUFFICIENT-SIGNATURES)
                    
                    ;; Update file ownership
                    (map-set file-metadata
                        { file-id: file-id }
                        (merge file-data {
                            owner: (get new-owner request-data)
                        })
                    )
                    
                    ;; Mark recovery as completed
                    (map-set recovery-requests
                        { request-id: request-id }
                        (merge request-data { is-completed: true })
                    )
                    
                    (ok (get new-owner request-data))
                )
                ERR-INVALID-FILE
            )
        )
        ERR-RECOVERY-NOT-FOUND
    )
)

;; Read-only functions

;; Get file metadata (without private key requirement)
(define-read-only (get-file-metadata (file-id (buff 32)))
    (map-get? file-metadata { file-id: file-id })
)

;; Get recovery request details
(define-read-only (get-recovery-request (request-id (buff 32)))
    (map-get? recovery-requests { request-id: request-id })
)

;; Check if guardian has verified a request
(define-read-only (has-guardian-verified (request-id (buff 32)) (guardian principal))
    (match (map-get? guardian-verifications { request-id: request-id, guardian: guardian })
        verification (get verified verification)
        false
    )
)

;; Get user's guardian configuration
(define-read-only (get-user-guardians (user principal))
    (map-get? user-guardians { user: user })
)

;; Check recovery request status
(define-read-only (get-recovery-status (request-id (buff 32)))
    (match (map-get? recovery-requests { request-id: request-id })
        request-data
        (let ((file-id (get file-id request-data)))
            (match (map-get? file-metadata { file-id: file-id })
                file-data
                (some {
                    signatures-collected: (get signature-count request-data),
                    signatures-required: (get required-signatures file-data),
                    is-expired: (>= block-height (get expires-at request-data)),
                    is-completed: (get is-completed request-data),
                    can-complete: (and 
                        (>= (get signature-count request-data) (get required-signatures file-data))
                        (< block-height (get expires-at request-data))
                        (not (get is-completed request-data))
                    )
                })
                none
            )
        )
        none
    )
)

;; Administrative functions

;; Emergency pause (contract owner only)
(define-public (emergency-pause-file (file-id (buff 32)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (match (map-get? file-metadata { file-id: file-id })
            file-data
            (begin
                (map-set file-metadata
                    { file-id: file-id }
                    (merge file-data { is-active: false })
                )
                (ok true)
            )
            ERR-INVALID-FILE
        )
    )
)

;; Reactivate file (contract owner only)
(define-public (reactivate-file (file-id (buff 32)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (match (map-get? file-metadata { file-id: file-id })
            file-data
            (begin
                (map-set file-metadata
                    { file-id: file-id }
                    (merge file-data { is-active: true })
                )
                (ok true)
            )
            ERR-INVALID-FILE
        )
    )
)